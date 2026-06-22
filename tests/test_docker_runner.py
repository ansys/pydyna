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

"""Tests for Docker runner functionality."""

import logging
import os
import tempfile
from unittest.mock import MagicMock, patch

import pytest

from ansys.dyna.core.run import run_dyna
from ansys.dyna.core.run.docker_runner import DockerRunner, _MPP_EXECUTABLES, _SMP_EXECUTABLES
from ansys.dyna.core.run.options import MpiOption, Precision

logger = logging.getLogger(__name__)

# These tests should only run when Docker is available and the container image is present
docker_available = True
container_image = os.environ.get("PYDYNA_RUN_CONTAINER", None)

try:
    import docker

    client = docker.from_env()
    if container_image:
        client.images.get(container_image)
        logger.info(f"Docker image {container_image} found for testing")
    else:
        docker_available = False
        logger.warning("PYDYNA_RUN_CONTAINER not set - Docker tests will be skipped")
except Exception as e:
    docker_available = False
    logger.warning(f"Docker not available for testing: {e}")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_runner(image="test-image:latest", **kwargs):
    """Create a DockerRunner with a fully-mocked Docker client."""
    mock_client = MagicMock()
    mock_client.images.get.return_value = MagicMock()  # image exists
    with patch("docker.from_env", return_value=mock_client):
        runner = DockerRunner(container=image, **kwargs)
    return runner, mock_client


# ---------------------------------------------------------------------------
# Unit tests – no real Docker daemon required
# ---------------------------------------------------------------------------


@pytest.mark.run
class TestDockerRunnerUnit:
    """Unit tests for DockerRunner using mocked Docker SDK."""

    def test_import_error_when_docker_not_installed(self):
        """DockerRunner raises ImportError when the docker package is absent."""
        import ansys.dyna.core.run.docker_runner as dr_module

        original = dr_module.docker
        try:
            dr_module.docker = None
            with pytest.raises(ImportError, match="Docker SDK for Python is not installed"):
                DockerRunner(container="any-image")
        finally:
            dr_module.docker = original

    def test_initialization_defaults(self):
        """Test default attribute values after construction."""
        runner, _ = _make_runner()
        assert runner._name == "test-image:latest"
        assert runner.mpi_option == MpiOption.SMP
        assert runner.precision == Precision.DOUBLE
        assert runner._stream is True
        assert runner.activate_case is False
        assert runner.case_ids is None
        assert runner.executable_name is None
        assert runner._container_env == {}
        assert runner._discovered_executable is None

    def test_initialization_with_custom_options(self):
        """Test DockerRunner construction with explicit options."""
        runner, _ = _make_runner(
            mpi_option=MpiOption.MPP_INTEL_MPI,
            precision=Precision.SINGLE,
            ncpu=4,
            memory=100,
            stream=False,
            activate_case=True,
            case_ids=[1, 2],
            executable_name="my_dyna",
            container_env={"MY_VAR": "value"},
        )
        assert runner.mpi_option == MpiOption.MPP_INTEL_MPI
        assert runner.precision == Precision.SINGLE
        assert runner.ncpu == 4
        assert runner.memory == 100
        assert runner._stream is False
        assert runner.activate_case is True
        assert runner.case_ids == [1, 2]
        assert runner.executable_name == "my_dyna"
        assert runner._container_env == {"MY_VAR": "value"}

    def test_image_not_found_raises(self):
        """DockerRunner raises if the container image does not exist locally."""
        mock_client = MagicMock()
        mock_client.images.get.side_effect = Exception("No such image")
        with patch("docker.from_env", return_value=mock_client):
            with pytest.raises(Exception, match="container image .* not found"):
                DockerRunner(container="missing-image:latest")

    def test_docker_daemon_not_running_raises(self):
        """DockerRunner raises a descriptive error when Docker daemon is unreachable."""
        with patch("docker.from_env", side_effect=Exception("connection refused")):
            with pytest.raises(Exception, match="Cannot connect to Docker daemon"):
                DockerRunner(container="any-image")

    # -- set_input ------------------------------------------------------------

    def test_set_input_valid(self):
        """set_input stores the file and resolved working directory."""
        runner, _ = _make_runner()
        with tempfile.TemporaryDirectory() as tmpdir:
            runner.set_input("test.k", tmpdir)
            assert runner._input_file == "test.k"
            assert runner._working_directory == os.path.abspath(tmpdir)

    def test_set_input_invalid_directory(self):
        """set_input raises when the working directory does not exist."""
        runner, _ = _make_runner()
        with pytest.raises(Exception, match="`working directory` is not a directory"):
            runner.set_input("test.k", "/non/existent/path")

    # -- _build_env -----------------------------------------------------------

    def test_build_env_uses_container_env_when_set(self):
        """_build_env uses container_env dict when provided."""
        runner, _ = _make_runner(container_env={"CUSTOM": "val"})
        env = runner._build_env()
        assert env["CUSTOM"] == "val"
        assert "OMPI_ALLOW_RUN_AS_ROOT" in env

    def test_build_env_falls_back_to_os_environ(self, monkeypatch):
        """_build_env copies licence vars from os.environ when container_env is empty."""
        monkeypatch.setenv("LSTC_LICENSE", "ansys")
        monkeypatch.setenv("ANSYSLI_SERVERS", "2325@host")
        runner, _ = _make_runner()
        env = runner._build_env()
        assert env["LSTC_LICENSE"] == "ansys"
        assert env["ANSYSLI_SERVERS"] == "2325@host"

    def test_build_env_ompi_flags_always_present(self):
        """OMPI root flags are always set in the container env."""
        runner, _ = _make_runner()
        env = runner._build_env()
        assert env["OMPI_ALLOW_RUN_AS_ROOT"] == "1"
        assert env["OMPI_ALLOW_RUN_AS_ROOT_CONFIRM"] == "1"

    # -- _get_solver_option ---------------------------------------------------

    @pytest.mark.parametrize(
        "mpi_option,precision,expected",
        [
            (MpiOption.SMP, Precision.DOUBLE, "SMP"),
            (MpiOption.SMP, Precision.SINGLE, "SMP"),
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE, "MPP"),
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE, "MPP"),
        ],
    )
    def test_get_solver_option(self, mpi_option, precision, expected):
        """_get_solver_option returns the correct SMP/MPP label."""
        runner, _ = _make_runner(mpi_option=mpi_option, precision=precision)
        assert runner._get_solver_option() == expected

    # -- _build_command -------------------------------------------------------

    def test_build_command_smp_single_cpu(self):
        """SMP command with 1 CPU omits ncpu argument."""
        runner, _ = _make_runner()
        runner.set_input.__func__  # ensure method exists; set attrs directly
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"
        cmd = runner._build_command("/path/to/ls-dyna")
        assert cmd.startswith("/path/to/ls-dyna")
        assert "i=model.k" in cmd
        assert "ncpu=" not in cmd  # ncpu=1 is omitted for SMP

    def test_build_command_smp_multi_cpu(self):
        """SMP command with ncpu > 1 includes ncpu argument."""
        runner, _ = _make_runner(ncpu=4)
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"
        cmd = runner._build_command("/path/to/ls-dyna")
        assert "ncpu=4" in cmd
        assert "mpirun" not in cmd

    def test_build_command_mpp(self):
        """MPP command is prefixed with mpirun -np <ncpu>."""
        runner, _ = _make_runner(mpi_option=MpiOption.MPP_INTEL_MPI, ncpu=8)
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"
        cmd = runner._build_command("/path/to/ls-dyna")
        assert cmd.startswith("mpirun -np 8")
        assert "/path/to/ls-dyna" in cmd

    def test_build_command_activate_case_no_ids(self):
        """activate_case=True with no case_ids appends bare CASE token."""
        runner, _ = _make_runner(activate_case=True)
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"
        cmd = runner._build_command("/path/to/ls-dyna")
        assert "CASE" in cmd
        assert "CASE=" not in cmd

    def test_build_command_activate_case_with_ids(self):
        """activate_case=True with case_ids appends CASE=id1,id2."""
        runner, _ = _make_runner(activate_case=True, case_ids=[1, 3])
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"
        cmd = runner._build_command("/path/to/ls-dyna")
        assert "CASE=1,3" in cmd

    # -- _discover_executables ------------------------------------------------

    def test_discover_executables_matches_smp(self):
        """_discover_executables picks the expected SMP executable when present."""
        runner, mock_client = _make_runner()
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"

        mock_container = MagicMock()
        mock_container.status = "running"
        mock_container.short_id = "abc123"
        mock_container.exec_run.return_value = MagicMock(
            output=f"/opt/dyna/{_SMP_EXECUTABLES}\n".encode()
        )
        mock_client.containers.run.return_value = mock_container

        result = runner._discover_executables()
        assert result == f"/opt/dyna/{_SMP_EXECUTABLES}"
        # Second call should return cached value without creating another container
        mock_client.containers.run.reset_mock()
        result2 = runner._discover_executables()
        assert result2 == f"/opt/dyna/{_SMP_EXECUTABLES}"
        mock_client.containers.run.assert_not_called()

    def test_discover_executables_falls_back_when_no_match(self):
        """_discover_executables falls back to first found executable on name mismatch."""
        runner, mock_client = _make_runner()
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"

        mock_container = MagicMock()
        mock_container.status = "running"
        mock_container.short_id = "abc123"
        mock_container.exec_run.return_value = MagicMock(
            output=b"/opt/dyna/ls-dyna_custom_build\n"
        )
        mock_client.containers.run.return_value = mock_container

        result = runner._discover_executables()
        assert result == "/opt/dyna/ls-dyna_custom_build"

    def test_discover_executables_raises_when_none_found(self):
        """_discover_executables raises when no executable is found in the container."""
        runner, mock_client = _make_runner()
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"

        mock_container = MagicMock()
        mock_container.status = "running"
        mock_container.short_id = "abc123"
        mock_container.exec_run.return_value = MagicMock(output=b"")
        mock_client.containers.run.return_value = mock_container

        with pytest.raises(Exception, match="No LS-DYNA executable found"):
            runner._discover_executables()

    def test_discover_executables_auto_switches_mpp_to_smp(self):
        """_discover_executables auto-switches from MPP to SMP when only SMP is available."""
        runner, mock_client = _make_runner(mpi_option=MpiOption.MPP_INTEL_MPI)
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"

        mock_container = MagicMock()
        mock_container.status = "running"
        mock_container.short_id = "abc123"
        mock_container.exec_run.return_value = MagicMock(
            output=f"/opt/dyna/{_SMP_EXECUTABLES}\n".encode()
        )
        mock_client.containers.run.return_value = mock_container

        result = runner._discover_executables()
        assert result == f"/opt/dyna/{_SMP_EXECUTABLES}"
        assert runner.mpi_option == MpiOption.SMP  # Auto-switched

    def test_discover_executables_uses_mpp_for_smp_mode(self):
        """_discover_executables uses MPP executable for SMP mode (MPP can run in SMP mode)."""
        runner, mock_client = _make_runner(mpi_option=MpiOption.SMP)
        runner._input_file = "model.k"
        runner._working_directory = "/tmp"

        mock_container = MagicMock()
        mock_container.status = "running"
        mock_container.short_id = "abc123"
        mock_container.exec_run.return_value = MagicMock(
            output=f"/opt/dyna/{_MPP_EXECUTABLES}\n".encode()
        )
        mock_client.containers.run.return_value = mock_container

        result = runner._discover_executables()
        assert result == f"/opt/dyna/{_MPP_EXECUTABLES}"
        assert runner.mpi_option == MpiOption.SMP  # No auto-switch - MPP can run in SMP mode

    # -- run() guard ----------------------------------------------------------

    def test_run_raises_without_set_input(self):
        """run() raises if set_input has not been called."""
        runner, _ = _make_runner()
        with pytest.raises(Exception, match="Input file and working directory must be set"):
            runner.run()

    def test_run_uses_explicit_executable_name(self):
        """run() skips discovery and uses executable_name when provided."""
        runner, mock_client = _make_runner(executable_name="/opt/dyna/my_dyna")
        with tempfile.TemporaryDirectory() as tmpdir:
            runner.set_input("model.k", tmpdir)

            mock_container = MagicMock()
            mock_container.status = "running"
            mock_container.short_id = "ctr1"
            mock_client.containers.run.return_value = mock_container
            mock_client.api.exec_create.return_value = {"Id": "exec1"}
            mock_client.api.exec_start.return_value = iter([b"done"])
            mock_client.api.exec_inspect.return_value = {"ExitCode": 0}

            result = runner.run()
            assert result == os.path.abspath(tmpdir)
            # _discover_executables should NOT have been called
            assert runner._discovered_executable is None


# ---------------------------------------------------------------------------
# Integration tests – require real Docker + container image
# ---------------------------------------------------------------------------


@pytest.mark.run
@pytest.mark.skipif(not docker_available or not container_image, reason="Docker or container image not available")
class TestDockerRunnerExecution:
    """Integration test suite for DockerRunner execution (requires real Docker + container image)."""

    def test_docker_runner_simple_execution(self, file_utils):
        """Test basic Docker runner execution with a simple input file."""
        input_file = file_utils.testfiles_folder / "run" / "i.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        runner = DockerRunner(container=container_image)
        runner.set_input(input_filename, example_folder)

        try:
            result_dir = runner.run()
            assert result_dir == example_folder

            # Check that output files were created
            assert os.path.isfile(os.path.join(example_folder, "d3plot"))

        except Exception as e:
            logger.error(f"Docker runner execution failed: {e}")
            raise
        finally:
            generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except Exception:
                    pass

    def test_docker_runner_case_option(self, file_utils):
        """Test Docker runner with CASE option via DockerRunner directly."""
        input_file = file_utils.testfiles_folder / "run" / "case-keywords" / "projectile.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        runner = DockerRunner(container=container_image, activate_case=True)
        runner.set_input(input_filename, example_folder)

        try:
            runner.run()

            # Check that output files from both cases were created
            d3plot_files = [f for f in os.listdir(example_folder) if f.endswith(".d3plot")]
            assert len(d3plot_files) > 0
            assert any("ZERO_VELOCITY" in f for f in d3plot_files)
            assert any("LOW_VELOCITY" in f for f in d3plot_files)

        except Exception as e:
            logger.error(f"Docker runner CASE execution failed: {e}")
            raise
        finally:
            generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except Exception:
                    pass

    def test_docker_runner_through_run_dyna(self, file_utils):
        """Test Docker runner through the run_dyna interface."""
        input_file = file_utils.testfiles_folder / "run" / "i.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        try:
            wdir = run_dyna(input_filename, working_directory=example_folder, container=container_image)
            assert wdir == example_folder
            assert os.path.isfile(os.path.join(example_folder, "d3plot"))

        except Exception as e:
            logger.error(f"Docker runner via run_dyna failed: {e}")
            raise
        finally:
            generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except Exception:
                    pass