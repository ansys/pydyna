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
import sys

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision

try:
    import docker
except ImportError:
    docker = None

logger = logging.getLogger(__name__)


class DockerRunner(BaseRunner):
    """Docker implementation to Run LS-DYNA.

    Tested with a custom executable and when LS-DYNA is
    installed as part of the unified Ansys installation.
    """

    def __init__(self, **kwargs):
        """Initialize DockerRunner.

        Parameters
        ----------
        case_ids : list[int] or None
            If provided, appends CASE or CASE=... to the DYNA_ARGS for *CASE support.
        executable_name : str, optional
            Name of the LS-DYNA executable. Default is auto-detected based on solver options.
        """
        if docker is None:
            raise ImportError("Docker SDK for Python is not installed. Install it with: pip install docker>=6.0.0")

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

    def __ensure_image(self, name):
        self._name = name
        try:
            _ = self._client.images.get(name)
            logger.info(f"Docker image {name} found")
        except Exception as e:
            logger.error(f"Docker image {name} not found: {e}")
            raise Exception(f"Exception in DockerRunner, container image {name} not found!")

    def set_input(self, input_file: str, working_directory: str) -> None:
        """Set the input file and working directory for the run."""
        self._input_file = input_file
        self._working_directory = os.path.abspath(working_directory)
        if not os.path.isdir(self._working_directory):
            raise Exception("`working directory` is not a directory")

    def _get_solver_option(self) -> str:
        """Get the solver option string for the specific LS-DYNA executable."""
        solver_option = {
            (MpiOption.SMP, Precision.SINGLE): "SP",
            (MpiOption.SMP, Precision.DOUBLE): "DP",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "SP_MPP",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "DP_MPP",
        }[(self.mpi_option, self.precision)]
        return solver_option

    def _get_executable_name(self) -> str:
        """Get the LS-DYNA executable name based on solver configuration."""
        if self.executable_name:
            return self.executable_name

        # Get the executable name based on the MPI option and precision
        # if self.mpi_option == MpiOption.SMP:
        #     if self.precision == Precision.SINGLE:
        #         main_executable = "ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2"
        #     else:  # DOUBLE precision (default)
        #         main_executable = "ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"
        # elif self.mpi_option == MpiOption.MPP_INTEL_MPI:
        #     if self.precision == Precision.SINGLE:
        #         main_executable = "ls-dyna_mpp_s_R16_1_1_x64_centos79_ifort190_sse2_intelmpi-2018"
        #     else:  # DOUBLE precision (default)
        #         main_executable = "ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_intelmpi-2018"
        # else:
        #     # Default to SMP double precision if unknown MPI option
        #     main_executable = "ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2"
        main_executable = "lls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib"

        # TODO: In the future, we could check what executables are actually available in the container
        return main_executable

    def run(self) -> str:
        """Run LS-DYNA using the specified configuration.

        Returns
        -------
        str
            The working directory path
        """
        if not hasattr(self, "_working_directory") or not hasattr(self, "_input_file"):
            raise Exception("Input file and working directory must be set before running")

        # Build the LS-DYNA command
        executable = self._get_executable_name()

        # Build command arguments
        args = [f"i={self._input_file}", f"memory={self.get_memory_string()}"]

        # Add NCPU argument for SMP runs
        if self.mpi_option == MpiOption.SMP and self.ncpu > 1:
            args.append(f"ncpu={self.ncpu}")

        # Add CASE option if enabled
        if self.activate_case:
            if self.case_ids and isinstance(self.case_ids, list) and self.case_ids:
                args.append(f"CASE={','.join(str(cid) for cid in self.case_ids)}")
            else:
                args.append("CASE")

        # Build the full command with MPI wrapper if needed
        if self.mpi_option == MpiOption.MPP_INTEL_MPI:
            # For MPP runs, use mpirun with the specified number of processors
            # Source Intel MPI environment and then run
            mpi_setup = "source /opt/intel/oneapi/mpi/latest/env/vars.sh &&"
            mpi_args = [mpi_setup, "mpirun", "-np", str(self.ncpu), executable] + args
            command = " ".join(mpi_args)
        else:
            # For SMP runs, use direct execution
            command = " ".join([executable] + args)
        logger.info(f"Running LS-DYNA command: {command}")

        # Set up environment variables
        if self._container_env == {}:
            self._container_env = dict(
                (k, os.environ[k]) for k in ("LSTC_LICENSE", "ANSYSLI_SERVERS", "ANSYSLMD_LICENSE_FILE")
            )

        env = {}
        env.update(self._container_env)

        # Set up volumes to mount the working directory
        volumes = [f"{self._working_directory}:/run"]

        # Change working directory inside container to /run
        working_dir = "/run"

        if self._stream:
            cont: docker.models.containers.Container = self._client.containers.run(
                self._name, command=command, environment=env, volumes=volumes, working_dir=working_dir, detach=True
            )

            logger.info(f"Started container {cont.id[:12]} running LS-DYNA")

            # Stream the logs
            logs: docker.types.daemon.CancellableStream = cont.logs(stream=True, follow=True)
            try:
                for log in logs:
                    sys.stdout.write(log.decode("utf-8"))
                    sys.stdout.flush()
            except KeyboardInterrupt:
                logger.info("Stopping container due to keyboard interrupt")
                cont.stop()
                raise

            # Wait for container to finish and get exit code
            result = cont.wait()
            exit_code = result["StatusCode"]

            if exit_code != 0:
                logger.error(f"LS-DYNA exited with code {exit_code}")
                # Get any remaining logs
                final_logs = cont.logs(tail=50)
                if final_logs:
                    logger.error("Final container logs:")
                    logs_str = final_logs.decode("utf-8")
                    sys.stderr.write(logs_str)

                    # Check for common issues and provide helpful error messages
                    if "libmpifort" in logs_str or "libmpi" in logs_str:
                        error_msg = (
                            f"LS-DYNA execution failed with exit code {exit_code}. "
                            "This appears to be an Intel MPI library dependency issue. "
                            "The Docker container may be missing Intel MPI runtime libraries. "
                            "Ensure the Docker image includes Intel oneAPI MPI runtime."
                        )
                    elif "cannot open shared object file" in logs_str:
                        error_msg = (
                            f"LS-DYNA execution failed with exit code {exit_code}. "
                            "Missing shared library dependencies. Check that all required "
                            "runtime libraries are installed in the Docker container."
                        )
                    elif exit_code == 127:
                        error_msg = (
                            f"LS-DYNA execution failed with exit code {exit_code}. "
                            "Command not found or library dependency missing. "
                            "Verify LS-DYNA executable and its dependencies are properly installed."
                        )
                    else:
                        error_msg = f"LS-DYNA execution failed with exit code {exit_code}"
                else:
                    error_msg = f"LS-DYNA execution failed with exit code {exit_code}"

                raise Exception(error_msg)

            # Clean up the container
            cont.remove()
            logger.info("Container execution completed successfully")

        else:
            result = self._client.containers.run(
                self._name, command=command, environment=env, volumes=volumes, working_dir=working_dir, remove=True
            )
            logger.info("Container execution completed (non-streaming mode)")

        return self._working_directory
