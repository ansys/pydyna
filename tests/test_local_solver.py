"""Tests for the run_dyna entry point.

These tests do not launch a solver or a container: the runner factory is replaced by a mock,
so only the argument handling of run_dyna is exercised.
"""

from unittest.mock import MagicMock

import pytest

from ansys.dyna.core.run import local_solver

CONTAINER_ENV_VARS = ("LSTC_LICENSE", "ANSYSLI_SERVERS", "ANSYSLMD_LICENSE_FILE")


@pytest.fixture()
def input_file(tmp_path):
    """Path to a minimal keyword file."""
    path = tmp_path / "input.k"
    path.write_text("*KEYWORD\n", encoding="utf-8")
    return str(path)


@pytest.fixture(autouse=True)
def clean_run_environment(monkeypatch):
    """Remove the environment variables that run_dyna reads by default."""
    monkeypatch.delenv("PYDYNA_RUN_CONTAINER", raising=False)
    monkeypatch.delenv("PYDYNA_RUN_STREAM", raising=False)
    for name in CONTAINER_ENV_VARS:
        monkeypatch.delenv(name, raising=False)


def _patch_runner(monkeypatch, stdout="container stdout"):
    """Replace get_runner with a factory returning a mock runner.

    Returns the runner and the kwargs that the factory was called with.
    """
    runner = MagicMock()
    runner.run.return_value = stdout
    call_kwargs = {}

    def factory(**kwargs):
        call_kwargs.update(kwargs)
        return runner

    monkeypatch.setattr(local_solver, "get_runner", factory)
    return runner, call_kwargs


def test_run_dyna_container_stream_false_returns_stdout(monkeypatch, input_file, tmp_path):
    """An explicit container with stream=False returns the solver stdout."""
    runner, call_kwargs = _patch_runner(monkeypatch)

    result = local_solver.run_dyna(input_file, container="img", working_directory=str(tmp_path), stream=False)

    assert result == "container stdout"
    assert call_kwargs["container"] == "img"
    runner.set_input.assert_called_once_with(input_file, str(tmp_path))


def test_run_dyna_container_stream_true_returns_working_directory(monkeypatch, input_file, tmp_path):
    """An explicit container with the default stream returns the working directory."""
    _patch_runner(monkeypatch)

    result = local_solver.run_dyna(input_file, container="img", working_directory=str(tmp_path))

    assert result == str(tmp_path)


def test_run_dyna_without_container_returns_working_directory(monkeypatch, input_file, tmp_path):
    """Without a container, the working directory is returned even when stream is False."""
    _patch_runner(monkeypatch)

    result = local_solver.run_dyna(input_file, working_directory=str(tmp_path), stream=False)

    assert result == str(tmp_path)


def test_run_dyna_reads_container_from_environment(monkeypatch, input_file, tmp_path):
    """PYDYNA_RUN_CONTAINER fills the container argument and the license environment."""
    runner, call_kwargs = _patch_runner(monkeypatch)
    monkeypatch.setenv("PYDYNA_RUN_CONTAINER", "img-from-env")
    monkeypatch.setenv("LSTC_LICENSE", "1055@server")
    monkeypatch.setenv("ANSYSLI_SERVERS", "2325@server")
    monkeypatch.setenv("ANSYSLMD_LICENSE_FILE", "1055@server")
    monkeypatch.setenv("PYDYNA_RUN_STREAM", "0")

    result = local_solver.run_dyna(input_file, working_directory=str(tmp_path))

    assert call_kwargs["container"] == "img-from-env"
    assert call_kwargs["container_env"] == {
        "LSTC_LICENSE": "1055@server",
        "ANSYSLI_SERVERS": "2325@server",
        "ANSYSLMD_LICENSE_FILE": "1055@server",
    }
    assert call_kwargs["stream"] is False
    assert result == "container stdout"
    runner.run.assert_called_once()


def test_run_dyna_ignores_environment_when_container_is_given(monkeypatch, input_file, tmp_path):
    """An explicit container argument takes precedence over the environment variable."""
    _, call_kwargs = _patch_runner(monkeypatch)
    monkeypatch.setenv("PYDYNA_RUN_CONTAINER", "img-from-env")

    local_solver.run_dyna(input_file, container="img-from-args", working_directory=str(tmp_path))

    assert call_kwargs["container"] == "img-from-args"
