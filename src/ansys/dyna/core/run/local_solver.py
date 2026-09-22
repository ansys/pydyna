# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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
import pathlib
import tempfile
import typing

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.run.linux_runner import LinuxRunner
from ansys.dyna.core.run.windows_runner import WindowsRunner

try:
    from ansys.dyna.core.run.docker_runner import DockerRunner

    HAS_DOCKER = True
except ImportError:
    HAS_DOCKER = False


def __make_temp_dir():
    """Create a temporary directory for the job."""
    job_folder = os.path.join(tempfile.gettempdir(), "ansys", "pydyna", "jobs")
    pathlib.Path(job_folder).mkdir(parents=True, exist_ok=True)
    return tempfile.mkdtemp(dir=job_folder)


def _check_case_keywords(input: typing.Union[str, Deck], wdir: str) -> bool:
    """Check if input deck contains *CASE keywords.

    This function checks for any of the CASE-related keywords:
    - *CASE
    - *CASE_BEGIN_n
    - *CASE_END_n

    Returns
    -------
    bool
        True if any CASE keywords are found, False otherwise.
    """
    if isinstance(input, str):
        try:
            with open(pathlib.Path(wdir) / input, "r") as f:
                for line in f:
                    line = line.strip().upper()
                    if line.startswith("*CASE") or line.startswith("*CASE_BEGIN") or line.startswith("*CASE_END"):
                        return True
            return False
        except Exception as e:
            logging.warning(f"Could not read input file {input} to check for CASE keywords: {e}")
            return False
    elif isinstance(input, Deck):
        try:
            if len(list(input.get_kwds_by_type("CASE"))) > 0:
                return True
            return False
        except Exception as e:
            logging.warning(f"Could not check Deck for CASE keywords: {e}")
            return False


def __prepare(input: typing.Union[str, Deck], **kwargs) -> typing.Tuple[str, str]:
    """Return the working directory and input file from a launch_dyna input."""
    wdir = kwargs.get("working_directory", None)
    if isinstance(input, str):
        input_file = input
        if wdir is None:
            wdir = str(pathlib.Path(input_file).parent.resolve())
        elif not os.path.isdir(wdir):
            p = pathlib.Path(wdir)
            p.mkdir(parents=True)

    needs_case_keywords = _check_case_keywords(input, wdir=wdir)
    if needs_case_keywords:
        if not kwargs.get("activate_case", False):
            raise UserWarning(
                "*CASE keyword detected in input file, but `activate_case` is not set to True. "
                "The solver may fail to run correctly. To enable *CASE support, set `activate_case=True`."
            )
    if isinstance(input, Deck):
        # write the deck to a file in the working directory.
        if wdir is None:
            wdir = __make_temp_dir()
            logging.log(logging.INFO, f"launching the dyna solver in {wdir}")
        input_file = os.path.join(wdir, "input.k")
        input.export_file(input_file)

    return wdir, input_file


def get_runner(**kwargs) -> typing.Any:
    """Return the runner for the job."""
    container = kwargs.get("container", None)
    if container != None:
        if not HAS_DOCKER:
            raise Exception("Cannot run in container, `docker` is not installed.")
        return DockerRunner(**kwargs)
    if os.name == "nt":
        return WindowsRunner(**kwargs)
    else:
        return LinuxRunner(**kwargs)


def run_dyna(input: typing.Union[str, object], **kwargs) -> str:
    """Run the Ls-Dyna solver with the given input file.

    Parameters
    ----------
    input : str or object
        Either the path to a dyna keyword file or an instance of
        ``ansys.dyna.core.lib.deck.Deck``.
    **kwargs : dict
        mpi_option : int
            The mpi option to use. Choose from the values defined in ``MpiOption``.
            Defaults to MpiOption.SMP.
        precision : int
            Floating point precision. Choose from the values defined in ``Precision``.
            Defaults to Precision.DOUBLE.
        version : int
            Integer identifying the Ansys release to use, for example ``241``
            for 2024 R1. Used when neither an explicit executable nor a saved
            solver path is selected. If version is also omitted, the latest
            discoverable Ansys installation is used.
        executable : str
            Full path to the LS-DYNA solver executable. Accepted on both
            Windows and Linux. The file must exist. When omitted, both
            platforms try ``get_dyna_path(find=True, allow_input=False)``,
            then the requested version, and then the latest installation.
            Windows checks that the returned saved path exists.
            On Windows, an environment script under ``lsprepost*/LS-Run``
            is used when a matching directory is found. Without that directory,
            the solver is launched directly with a warning. MPI modes require
            a working MPI command and libraries in the process environment.
        ncpu : int
            Number of CPU cores.
            Defaults to 1.
        memory : int
            Amount of memory units, as defined by `memory_unit` for DYNA to use.
            Defaults to 20.
        memory_unit : int
            Memory unit. Use ``MemoryUnit.MB`` (default) or ``MemoryUnit.GB``.
        working_directory : str
            Working directory.
            If the `input` parameter is a path to the input file,
            defaults to the same folder as that file.  Otherwise, the job is run
            in a new folder under $TMP/ansys/pydyna/jobs.
        container : str
            DockerContainer to run LS-DYNA in.
        container_env : dict()
            Environment variables to pass into the docker container.
        stream : bool
            Currently only affects runs using the `container` option.
            If True, the stdout of solver is streamed to python's stdout during the solve.
            If False, the solver stdout is printed once after the container exits.
            Defaults to True.
        activate_case : bool
            If provided, appends CASE command line for ``*CASE`` keywords support.
        case_ids : list[int] or None
            If provided, appends CASE or CASE=... to the LS-DYNA command line for ``*CASE`` support.

    Returns
    -------
    result : str
        The working directory where the solver is launched.
        If `stream` is `False` and `container` is set, returns the stdout of the run

    """
    # TODO: jobname => jobid={jobname}
    # TODO: override => clear all generated files before running (like in launch_mapdl)
    # TODO: additional_switches => literal string to add to the command line of the dyna solver
    # TODO: cleanup_on_exit => maybe delete some unneeded files to save space
    # TODO: license_server_check, license_type => as in pymapdl
    wdir, input_file = __prepare(input, **kwargs)

    container = kwargs.get("container", None)
    if "container" not in kwargs:
        container = os.environ.get("PYDYNA_RUN_CONTAINER", None)
        if container is not None:
            kwargs["container"] = container
            if "container_env" not in kwargs:
                kwargs["container_env"] = dict(
                    (k, os.environ[k]) for k in ("LSTC_LICENSE", "ANSYSLI_SERVERS", "ANSYSLMD_LICENSE_FILE")
                )

    if "stream" not in kwargs:
        stream = os.environ.get("PYDYNA_RUN_STREAM", None)
        if stream is not None:
            kwargs["stream"] = bool(int(stream))

    runner = get_runner(**kwargs)
    runner.set_input(input_file, wdir)

    result = runner.run()
    if container is not None and kwargs.get("stream", True) is False:
        return result
    return wdir
