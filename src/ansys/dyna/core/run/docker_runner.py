# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""Module for defining the PyDyna Docker runner."""

import logging
import os

# Subprocess is used to run LS-DYNA commands, excluding bandit warning
import subprocess  # nosec: B404
import sys

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision

try:
    import docker
except ImportError:
    docker = None

logger = logging.getLogger(__name__)

_SMP_EXECUTABLES = "ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"
_MPP_EXECUTABLES = "ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib"


class DockerRunner(BaseRunner):
    """Docker implementation to Run LS-DYNA.

    Designed to work with the unified PyDyna Docker image containing both
    SMP and MPP executables. Auto-detects and selects the appropriate executable
    based on the requested solver mode.

    Also compatible with custom executables and unified Ansys installations.

    Parameters
    ----------
    container : str
        Name of the Docker container image to use.
    container_env : dict, optional
        Environment variables to pass to the container.
    stream : bool, optional
        Whether to stream output to stdout. Default is True.
    activate_case : bool, optional
        Whether to activate CASE support. Default is False.
    case_ids : list[int], optional
        List of case IDs to run. If provided with activate_case=True,
        appends CASE=... to the solver arguments.
    executable_name : str, optional
        Name of the LS-DYNA executable. If not provided, auto-detects
        based on solver options.
    **kwargs
        Additional arguments passed to BaseRunner.
    """

    def __init__(self, **kwargs):
        """Initialize DockerRunner.

        Parameters
        ----------
        container : str
            Name of the Docker container image to use.
        container_env : dict, optional
            Environment variables to pass to the container.
        stream : bool, optional
            Whether to stream output to stdout. Default is True.
        activate_case : bool, optional
            Whether to activate CASE support. Default is False.
        case_ids : list[int], optional
            List of case IDs to run. If provided with activate_case=True,
            appends CASE=... to the solver arguments.
        executable_name : str, optional
            Name of the LS-DYNA executable. If not provided, auto-detects
            based on solver options.
        **kwargs
            Additional arguments passed to BaseRunner.

        Raises
        ------
        ImportError
            If Docker SDK for Python is not installed.
        Exception
            If Docker daemon is not running or image is not found.
        """
        if docker is None:
            raise ImportError("Docker SDK for Python is not installed. Install it with: pip install docker")

        super().__init__(**kwargs)
        try:
            self._client: docker.client.DockerClient = docker.from_env()
        except Exception as e:
            logger.error(f"Failed to connect to Docker: {e}")
            raise Exception(f"Cannot connect to Docker daemon. Make sure Docker is running. Error: {e}")

        self.__ensure_image(kwargs["container"])
        self._container_env = kwargs.get("container_env", dict())
        self._stream = kwargs.get("stream", True)
        self.activate_case = kwargs.get("activate_case", False)
        self.case_ids = kwargs.get("case_ids", None)
        self.executable_name = kwargs.get("executable_name", None)

        # discover available executables in the container (for future use)
        self._discovered_executable: str | None = None

    def __ensure_image(self, name):
        """Verify that the specified Docker image exists.

        Parameters
        ----------
        name : str
            Docker image name to verify.

        Raises
        ------
        RuntimeError
            If the Docker image is not found.
        """
        self._name = name
        try:
            _ = self._client.images.get(name)
            logger.info(f"Docker image {name} found")
        except Exception as e:
            logger.error(f"Docker image {name} not found: {e}")
            raise Exception(f"Exception in DockerRunner, container image {name} not found!")

    def set_input(self, input_file: str, working_directory: str) -> None:
        """Set the input file and working directory for the run.

        Parameters
        ----------
        input_file : str
            Path to the LS-DYNA input file (relative to working_directory).
        working_directory : str
            Absolute or relative path to the working directory.

        Raises
        ------
        ValueError
            If working_directory is not a valid directory.
        """
        self._input_file = input_file
        self._working_directory = os.path.abspath(working_directory)
        if not os.path.isdir(self._working_directory):
            raise Exception("`working directory` is not a directory")

    def _create_container(self, volumes: list[str]) -> "docker.models.containers.Container":
        """Create a new Docker container for running LS-DYNA.

        Parameters
        ----------
        volumes : list[str]
            List of volume mount specifications (e.g., "host_path:container_path").

        Returns
        -------
        docker.models.containers.Container
            The created and running container.

        Raises
        ------
        RuntimeError
            If the container fails to start.
        """
        env = self._build_env()
        logger.info(f"Creating Docker container from image {self._name}")
        container = self._client.containers.run(
            self._name,
            command="sleep infinity",
            environment=env,
            volumes=volumes,
            working_dir="/run",
            detach=True,
            remove=False,
        )
        container.reload()
        if container.status != "running":
            raise Exception(f"Container {container.short_id} failed to start (status={container.status})")
        logger.info(f"Container {container.short_id} started")
        return container

    def _build_env(self) -> dict:
        """Build environment variables for the container."""
        env = {
            "OMPI_ALLOW_RUN_AS_ROOT": "1",
            "OMPI_ALLOW_RUN_AS_ROOT_CONFIRM": "1",
        }
        if self._container_env:
            env.update(self._container_env)
        else:
            for k in ("LSTC_LICENSE", "ANSYSLI_SERVERS", "ANSYSLMD_LICENSE_FILE"):
                if k in os.environ:
                    env[k] = os.environ[k]
        return env

    def _get_solver_option(self) -> str:
        """Get the solver option string for the specific LS-DYNA executable."""
        solver_option = {
            (MpiOption.SMP, Precision.SINGLE): "SMP",
            (MpiOption.SMP, Precision.DOUBLE): "SMP",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "MPP",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "MPP",
        }[(self.mpi_option, self.precision)]
        return solver_option

    def _discover_executables(self) -> str:
        """Based on the solver options, discover available LS-DYNA executables in the container.

        For the unified PyDyna Docker image, both SMP and MPP executables are available
        in /opt/dyna/. This method will auto-select the appropriate one or fall back
        to the alternative if requested mode is not available.
        """
        if self._discovered_executable is not None:
            return self._discovered_executable

        expected_executables = {
            "SMP": _SMP_EXECUTABLES,
            "MPP": _MPP_EXECUTABLES,
        }
        solver_option = self._get_solver_option()
        expected_basename = expected_executables.get(solver_option)

        # Search in known locations first, then fall back to full search
        find_cmd = (
            "find /opt/dyna /usr/local /opt -maxdepth 3 -type f -name 'ls-dyna*' 2>/dev/null || "
            "find / -maxdepth 8 -type f -name 'ls-dyna*' 2>/dev/null"
        )
        logger.info(f"Discovering LS-DYNA executables in container for {solver_option} mode")
        container = self._create_container(volumes=[f"{self._working_directory}:/run"])
        exec_log = container.exec_run(["/bin/bash", "-c", find_cmd])
        all_found = [
            line
            for line in exec_log.output.decode("utf-8").splitlines()
            if line.strip() and not line.strip().endswith(".l2a")  # exclude license auth files
        ]
        logger.info(f"Found LS-DYNA executables in container: {all_found}")

        # Match by basename against the expected executable name
        matched = [p for p in all_found if os.path.basename(p) == expected_basename]
        if matched:
            chosen = matched[0]
            logger.info(f"Found expected {solver_option} executable: {chosen}")
        elif all_found:
            # Auto-detect and adjust mpi_option if there's a SMP/MPP mismatch
            # This happens when using legacy single-mode images or custom images
            discovered_basename = os.path.basename(all_found[0])
            if solver_option == "MPP" and _SMP_EXECUTABLES in discovered_basename:
                logger.warning(
                    f"MPP solver requested but only SMP executable found in container '{self._name}'. "
                    f"Auto-switching to SMP mode (unified image should have both executables)."
                )
                self.mpi_option = MpiOption.SMP
                chosen = all_found[0]
            elif solver_option == "SMP" and _MPP_EXECUTABLES in discovered_basename:
                logger.info(
                    f"SMP solver requested but MPP executable found first in container '{self._name}'. "
                    f"Will use MPP in SMP mode (ncpu=1)."
                )
                # Don't auto-switch for this case - MPP can run in SMP mode
                chosen = all_found[0]
            else:
                # Neither SMP/MPP conflict - generic fallback
                chosen = all_found[0]
                logger.warning(
                    f"Expected executable '{expected_basename}' not found by name. "
                    f"Using discovered executable: {chosen}"
                )
        else:
            raise Exception(
                f"No LS-DYNA executable found in container '{self._name}'. "
                f"Ensure the image contains LS-DYNA or set `executable_name` explicitly."
            )

        self._discovered_executable = chosen
        # Clean up the discovery container
        try:
            container.stop()
            container.remove()
        except Exception:
            pass
        return self._discovered_executable

    def _build_command(self, executable: str) -> str:
        """Build the command to run LS-DYNA based on the configuration."""
        case_option = ""
        if self.activate_case:
            if self.case_ids and isinstance(self.case_ids, list) and self.case_ids:
                case_option = f"CASE={','.join(str(cid) for cid in self.case_ids)}"
            else:
                case_option = "CASE"

        args = [f"i={self._input_file}", f"memory={self.get_memory_string()}"]
        if case_option:
            args.append(case_option)

        if self.mpi_option == MpiOption.SMP:
            if self.ncpu > 1:
                args.append(f"ncpu={self.ncpu}")
            command = " ".join([executable] + args)
        else:
            command = " ".join(["mpirun", "-np", str(self.ncpu), executable] + args)

        logger.info(f"Built command to run in container: {command}")
        return command

    def _exec_in_container(self, container: "docker.models.containers.Container", command: str) -> int:
        """Execute a command in the container and stream output.

        Parameters
        ----------
        container : docker.models.containers.Container
            The running container.
        command : str
            Shell command to execute.

        Returns
        -------
        int
            Exit code of the command.
        """
        env = self._build_env()
        env_args = " ".join(f"{k}={v}" for k, v in env.items())
        shell_cmd = f"/bin/bash -c 'export {env_args} && {command}'"

        if self._stream:
            exec_id = self._client.api.exec_create(
                container.id,
                shell_cmd,
                stdout=True,
                stderr=True,
                workdir="/run",
            )
            output = self._client.api.exec_start(exec_id, stream=True, demux=False)
            for chunk in output:
                sys.stdout.write(chunk.decode("utf-8", errors="replace"))
                sys.stdout.flush()
            inspect = self._client.api.exec_inspect(exec_id["Id"])
            return inspect["ExitCode"]
        else:
            exit_code, output = container.exec_run(
                shell_cmd,
                stdout=True,
                stderr=True,
                workdir="/run",
            )
            if output:
                sys.stdout.write(output.decode("utf-8", errors="replace"))
            return exit_code

    def run(self) -> str:
        """Run LS-DYNA using the specified configuration.

        Returns
        -------
        str
            The working directory path
        """
        if not hasattr(self, "_working_directory") or not hasattr(self, "_input_file"):
            raise Exception("Input file and working directory must be set before running")

        volumes = [f"{self._working_directory}:/run"]
        container = self._create_container(volumes=volumes)

        if self.executable_name:
            executable = self.executable_name
        else:
            executable = self._discover_executables()

        command = self._build_command(executable=executable)
        logger.info(f"Running command in container {container.short_id}: {command}")

        try:
            exit_code = self._exec_in_container(container, command)
        finally:
            container.stop()
            container.remove()
            logger.info(f"Container {container.short_id} stopped and removed")

        if exit_code != 0:
            # Subprocess is used to run LS-DYNA commands, excluding bandit warning
            raise subprocess.CalledProcessError(
                exit_code,
                command,
                output=f"LS-DYNA execution failed with exit code {exit_code}",
            )

        return self._working_directory
