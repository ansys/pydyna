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

"""Tests for the download_utilities module.

The module wraps ``ansys-tools-common``'s ``download_manager`` singleton and
exposes ``EXAMPLES_PATH`` / ``USER_DATA_PATH`` convenience constants.

Test organisation
-----------------
``TestModuleExports``       – fast, no network (constants & singleton identity)
``TestDownloadManagerUnit`` – fast, no network (behaviour mocked at method level)
``test_download_*``         – integration, requires network (real example files)
"""

import os
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

# Upstream singleton used to verify identity
from ansys.tools.common.example_download import download_manager as _upstream_dm

from ansys.dyna.core.utils.download_utilities import EXAMPLES_PATH, USER_DATA_PATH, download_manager


# Real example files used by the pydyna example scripts (examples/ directory).
# Format: (filename, directory-under-example-data-repo)
_EXAMPLE_FILES = [
    pytest.param("mesh.k", "ls-dyna/Buckling_Beer_Can", id="buckling_beer_can"),
    pytest.param("nodes.k", "ls-dyna/John_Reid_Pendulum", id="john_reid_pendulum"),
    pytest.param("nodes.k", "ls-dyna/John_Reid_Pipe", id="john_reid_pipe"),
    pytest.param("taylor_bar_mesh.k", "ls-dyna/Taylor_Bar", id="taylor_bar"),
    pytest.param("bar_impact_mesh.k", "ls-dyna/Bar_Impact", id="bar_impact"),
]

# Module-level exports

class TestModuleExports:
    """Verify that the module re-exports the expected symbols with correct types."""

    def test_download_manager_is_upstream_singleton(self):
        """``download_manager`` must be the same object as the upstream singleton."""
        assert download_manager is _upstream_dm

    def test_all_exports_defined(self):
        import ansys.dyna.core.utils.download_utilities as du

        assert set(du.__all__) == {"download_manager", "EXAMPLES_PATH", "USER_DATA_PATH"}

    def test_user_data_path_is_string(self):
        assert isinstance(USER_DATA_PATH, str)

    def test_examples_path_is_string(self):
        assert isinstance(EXAMPLES_PATH, str)

    def test_examples_path_is_examples_subdir_of_user_data_path(self):
        assert Path(EXAMPLES_PATH) == Path(USER_DATA_PATH) / "examples"

    def test_examples_path_directory_exists(self):
        assert os.path.isdir(EXAMPLES_PATH)

    def test_user_data_path_directory_exists(self):
        assert os.path.isdir(USER_DATA_PATH)



# Unit tests – network calls mocked at the private-method boundary

class TestDownloadManagerUnit:
    """Behaviour tests that do not touch the network."""

    def test_download_file_is_callable(self):
        assert callable(download_manager.download_file)

    def test_clear_download_cache_is_callable(self):
        assert callable(download_manager.clear_download_cache)

    def test_download_file_creates_missing_destination_directory(self, tmp_path):
        dest = tmp_path / "new_example_dir"
        assert not dest.exists()

        with (
            patch.object(
                download_manager,
                "_download_file_git_based",
                side_effect=RuntimeError("git unavailable"),
            ),
            patch.object(
                download_manager,
                "_download_file_http_based",
                return_value=str(dest / "mesh.k"),
            ),
            patch.object(download_manager, "_add_file"),
        ):
            download_manager.download_file("mesh.k", "ls-dyna/Buckling_Beer_Can", destination=str(dest))

        assert dest.is_dir()

    def test_download_file_returns_local_path(self, tmp_path):
        expected = str(tmp_path / "mesh.k")

        with (
            patch.object(
                download_manager,
                "_download_file_git_based",
                side_effect=RuntimeError("git unavailable"),
            ),
            patch.object(
                download_manager,
                "_download_file_http_based",
                return_value=expected,
            ),
            patch.object(download_manager, "_add_file"),
        ):
            result = download_manager.download_file(
                "mesh.k", "ls-dyna/Buckling_Beer_Can", destination=str(tmp_path)
            )

        assert result == expected

    def test_download_file_force_bypasses_cache(self, tmp_path):
        """force=True must reach the HTTP download even when a cached file exists."""
        cached = tmp_path / "mesh.k"
        cached.write_bytes(b"stale content")
        expected = str(cached)

        with (
            patch.object(
                download_manager,
                "_download_file_git_based",
                side_effect=RuntimeError("git unavailable"),
            ),
            patch.object(
                download_manager,
                "_download_file_http_based",
                return_value=expected,
            ) as mock_http,
            patch.object(download_manager, "_add_file"),
        ):
            download_manager.download_file(
                "mesh.k", "ls-dyna/Buckling_Beer_Can", destination=str(tmp_path), force=True
            )

        mock_http.assert_called_once()

    def test_download_file_no_force_skips_network_for_cached_file(self, tmp_path):
        """force=False must not issue any HTTP request when the file is already cached.

        The caching check in ``_retrieve_data`` returns the cached path before
        calling ``requests.get``, so no network I/O should occur.
        """
        import requests

        cached = tmp_path / "mesh.k"
        cached.write_bytes(b"cached content")

        with (
            patch.object(
                download_manager,
                "_download_file_git_based",
                side_effect=RuntimeError("git unavailable"),
            ),
            patch.object(download_manager, "_add_file"),
            patch.object(requests, "get") as mock_get,
        ):
            result = download_manager.download_file(
                "mesh.k", "ls-dyna/Buckling_Beer_Can", destination=str(tmp_path), force=False
            )

        mock_get.assert_not_called()
        assert result == str(cached)

    def test_clear_download_cache_removes_tracked_files(self, tmp_path):
        """clear_download_cache() must delete all files in the internal tracking list."""
        tracked = tmp_path / "tracked.k"
        tracked.write_bytes(b"content")

        original_list = list(download_manager._downloads_list)
        download_manager._downloads_list.clear()
        download_manager._downloads_list.append(str(tracked))

        try:
            download_manager.clear_download_cache()
            assert not tracked.exists()
            assert len(download_manager._downloads_list) == 0
        finally:
            # Restore the list so other tests are unaffected
            download_manager._downloads_list.extend(original_list)



# Integration tests – require network access to github.com/ansys/example-data

@pytest.mark.parametrize("filename,directory", _EXAMPLE_FILES)
def test_download_example_file(filename, directory, tmp_path):
    """Download each real example file referenced by pydyna example scripts."""
    dest = tmp_path / directory.split("/")[-1]

    local_path = download_manager.download_file(filename, directory, destination=str(dest))

    assert os.path.isfile(local_path), f"Expected file not found: {local_path}"
    assert os.path.getsize(local_path) > 0, f"Downloaded file is empty: {local_path}"
    assert Path(local_path).name == filename


def test_download_file_caches_result(tmp_path):
    """A second non-forced download of the same file must reuse the cached copy."""
    filename = "mesh.k"
    directory = "ls-dyna/Buckling_Beer_Can"
    dest = tmp_path / "cache_test"

    path1 = download_manager.download_file(filename, directory, destination=str(dest))
    mtime1 = os.path.getmtime(path1)

    path2 = download_manager.download_file(filename, directory, destination=str(dest), force=False)
    mtime2 = os.path.getmtime(path2)

    assert path1 == path2
    assert mtime1 == mtime2, "Cached file was unexpectedly overwritten"


def test_download_file_force_refreshes_cached_file(tmp_path):
    """force=True must overwrite a pre-existing file and return its path."""
    filename = "mesh.k"
    directory = "ls-dyna/Buckling_Beer_Can"
    dest = tmp_path / "force_test"

    # Populate cache with a stale placeholder
    dest.mkdir()
    stale = dest / filename
    stale.write_bytes(b"stale")
    mtime_before = os.path.getmtime(str(stale))

    refreshed_path = download_manager.download_file(filename, directory, destination=str(dest), force=True)

    assert os.path.isfile(refreshed_path)
    assert os.path.getsize(refreshed_path) > len(b"stale")
    assert os.path.getmtime(refreshed_path) >= mtime_before
