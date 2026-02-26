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

"""Module for defining the PyDyna local solver."""

import logging
import os
from pathlib import Path
import tempfile
from typing import TYPE_CHECKING

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.run.linux_runner import LinuxRunner
from ansys.dyna.core.run.windows_runner import WindowsRunner

if TYPE_CHECKING:
    from ansys.dyna.core.run.base_runner import BaseRunner

try:
    from ansys.dyna.core.run.docker_runner import DockerRunner

    HAS_DOCKER = True
except ImportError:
    HAS_DOCKER = False

logger = logging.getLogger(__name__)


def __make_temp_dir() -> str:
    """Create a temporary directory for the job.

    Returns
    -------
    str
        Path to the created temporary directory.
    """
    job_folder = Path(tempfile.gettempdir()) / "ansys" / "pydyna" / "jobs"
    job_folder.mkdir(parents=True, exist_ok=True)
    return tempfile.mkdtemp(dir=str(job_folder))


def _check_case_keywords(input: str | Deck, wdir: str) -> bool:
    """Check if input deck contains *CASE keywords.

    This function checks for any of the CASE-related keywords:
    - *CASE
    - *CASE_BEGIN_n
    - *CASE_END_n

    Parameters
    ----------
    input : str or Deck
        Either a filename (str) or a Deck object to check.
    wdir : str
        Working directory where the input file is located.

    Returns
    -------
    bool
        True if any CASE keywords are found, False otherwise.
    """
    if isinstance(input, str):
        try:
            with (Path(wdir) / input).open("r") as f:
                for line in f:
                    line_upper = line.strip().upper()
                    if line_upper.startswith(("*CASE", "*CASE_BEGIN", "*CASE_END")):
                        return True
            return False
        except Exception as e:
            logger.warning(f"Could not read input file '{input}' to check for CASE keywords: {e}")
            return False
    elif isinstance(input, Deck):
        try:
            return bool(list(input.get_kwds_by_type("CASE")))
        except Exception as e:
            logger.warning(f"Could not check Deck for CASE keywords: {e}")
            return False

    return False


def __prepare(input: str | Deck, **kwargs) -> tuple[str, str]:
    """Prepare the working directory and input file for solver execution.

    Parameters
    ----------
    input : str or Deck
        Either a path to a DYNA keyword file or a Deck object.
    **kwargs : dict
        Additional keyword arguments, including:
        - working_directory: Optional working directory path
        - activate_case: Whether to activate CASE support

    Returns
    -------
    tuple[str, str]
        A tuple of (working_directory, input_file_path).

    Raises
    ------
    ValueError
        If *CASE keywords are detected but activate_case is not True.
    """
    wdir = kwargs.get("working_directory", None)
    if isinstance(input, str):
        input_file = input
        if wdir is None:
            wdir = str(Path(input_file).parent.resolve())
        elif not Path(wdir).is_dir():
            p = Path(wdir)
            p.mkdir(parents=True, exist_ok=True)

    needs_case_keywords = _check_case_keywords(input, wdir=wdir)
    if needs_case_keywords and not kwargs.get("activate_case", False):
        raise ValueError(
            "*CASE keyword detected in input file, but `activate_case` is not set to True. "
            "The solver may fail to run correctly. To enable *CASE support, set `activate_case=True`."
        )
    if isinstance(input, Deck):
        # Write the deck to a file in the working directory
        if wdir is None:
            wdir = __make_temp_dir()
            logger.info(f"Launching the DYNA solver in '{wdir}'")
        input_file = str(Path(wdir) / "input.k")
        input.export_file(input_file)

    return wdir, input_file


def get_runner(**kwargs) -> "BaseRunner":
    """Return the appropriate runner for the job based on configuration.

    Parameters
    ----------
    **kwargs : dict
        Configuration options including:
        - container: Docker container name (optional)
        - Other runner-specific options

    Returns
    -------
    BaseRunner
        An instance of DockerRunner, WindowsRunner, or LinuxRunner.

    Raises
    ------
    ImportError
        If Docker container is requested but Docker SDK is not installed.
    """
    container = kwargs.get("container", None)
    if container is not None:
        if not HAS_DOCKER:
            raise ImportError(
                "Cannot run in container - Docker SDK for Python is not installed. Install it with: pip install docker"
            )
        return DockerRunner(**kwargs)
    if os.name == "nt":
        return WindowsRunner(**kwargs)
    else:
        return LinuxRunner(**kwargs)


def run_dyna(input: str | Deck, **kwargs) -> str:
    """Run the LS-DYNA solver with the given input file.

    Parameters
    ----------
    input : str or Deck
        Either the path to a DYNA keyword file or an instance of
        ``ansys.dyna.core.lib.deck.Deck``.
    **kwargs : dict
        Configuration options:

        mpi_option : int, optional
            The MPI option to use. Choose from the values defined in ``MpiOption``.
            Defaults to ``MpiOption.SMP``.
        precision : int, optional
            Floating point precision. Choose from the values defined in ``Precision``.
            Defaults to ``Precision.DOUBLE``.
        version : str, optional
            Version of Ansys Unified installation to use.
        executable : str, optional
            Linux-only: The name or full path of the DYNA solver executable.
            Default is based on the value of the ``mpi_option`` argument.
            On Linux, ansys-tools-path can be used to save a custom location.
        ncpu : int, optional
            Number of CPUs. Defaults to 1.
        memory : int, optional
            Amount of memory units (as defined by ``memory_unit``) for DYNA to use.
            Defaults to 20.
        memory_unit : int, optional
            Memory unit. Choose from the values defined in ``MemoryUnit``.
            Defaults to ``MemoryUnit.MB``.
        working_directory : str, optional
            Working directory path.
            If the ``input`` parameter is a file path, defaults to that file's directory.
            Otherwise, the job runs in a new folder under ``$TMP/ansys/pydyna/jobs``.
        container : str, optional
            Docker container name to run LS-DYNA in.
        container_env : dict, optional
            Environment variables to pass into the Docker container.
        stream : bool, optional
            Currently only affects runs using the ``container`` option.
            If True, solver stdout is streamed to Python's stdout during the solve.
            If False, solver stdout is printed once after the container exits.
            Defaults to True.
        activate_case : bool, optional
            If True, appends CASE command line option for *CASE keywords support.
            Defaults to False.
        case_ids : list[int], optional
            If provided, appends CASE=... to the LS-DYNA command line for *CASE support.

    Returns
    -------
    str
        The working directory where the solver is launched.
        If ``stream=False`` and ``container`` is set, returns the stdout of the run.

    Raises
    ------
    ValueError
        If *CASE keywords are detected but ``activate_case`` is not True.
    ImportError
        If Docker container is requested but Docker SDK is not installed.
    """
    # TODO: jobname => jobid={jobname}
    # TODO: override => clear all generated files before running (like in launch_mapdl)
    # TODO: additional_switches => literal string to add to the command line of the dyna solver
    # TODO: cleanup_on_exit => maybe delete some unneeded files to save space
    # TODO: license_server_check, license_type => as in pymapdl
    wdir, input_file = __prepare(input, **kwargs)

    # Check for container configuration in environment variables
    if "container" not in kwargs:
        container = os.environ.get("PYDYNA_RUN_CONTAINER", None)
        if container is not None:
            kwargs["container"] = container
            if "container_env" not in kwargs:
                kwargs["container_env"] = {
                    k: os.environ[k]
                    for k in ("LSTC_LICENSE", "ANSYSLI_SERVERS", "ANSYSLMD_LICENSE_FILE")
                    if k in os.environ
                }

    # Check for stream configuration in environment variables
    if "stream" not in kwargs:
        stream = os.environ.get("PYDYNA_RUN_STREAM", None)
        if stream is not None:
            kwargs["stream"] = bool(int(stream))

    runner = get_runner(**kwargs)
    runner.set_input(input_file, wdir)

    result = runner.run()

    # Return result for non-streaming container runs, otherwise return working directory
    container = kwargs.get("container", None)
    if container is not None and kwargs.get("stream", True) is False:
        return result
    return wdir
