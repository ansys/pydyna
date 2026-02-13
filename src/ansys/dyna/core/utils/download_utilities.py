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

"""Module provides a thread-safe singleton download manager for handling example file downloads."""

import os
from pathlib import Path
from threading import Lock
from typing import Optional
from urllib.parse import urljoin, urlparse
import urllib.request

try:
    import appdirs

    USER_DATA_PATH = Path(
        os.getenv(
            "PYDYNA_USER_DATA",
            appdirs.user_data_dir(appname="pydyna", appauthor=False),
        )
    )
except ModuleNotFoundError:
    import tempfile

    USER_NAME = os.getenv("USERNAME", os.getenv("USER", "pydyna"))
    USER_DATA_PATH = Path(
        os.getenv(
            "PYDYNA_USER_DATA",
            Path(tempfile.gettempdir()) / USER_NAME,
        )
    )

# Ensure directories exist
USER_DATA_PATH.mkdir(parents=True, exist_ok=True)
EXAMPLES_PATH = USER_DATA_PATH / "examples"
EXAMPLES_PATH.mkdir(parents=True, exist_ok=True)

__all__ = ["DownloadManager"]


class DownloadManagerMeta(type):
    """Thread-safe Singleton metaclass."""

    _instances = {}
    _lock: Lock = Lock()

    def __call__(cls, *args, **kwargs):
        with cls._lock:
            if cls not in cls._instances:
                instance = super().__call__(*args, **kwargs)
                cls._instances[cls] = instance
        return cls._instances[cls]


class DownloadManager(metaclass=DownloadManagerMeta):
    """Manages downloads of example files."""

    def __init__(self):
        self.downloads_list: list[Path] = []

    def add_file(self, file_path: str | Path):
        """Track downloaded file path."""
        self.downloads_list.append(Path(file_path))

    def clear_download_cache(self):
        """Remove downloaded example files."""
        for file in self.downloads_list:
            if file.exists():
                file.unlink()
        self.downloads_list.clear()

    def download_file(
        self,
        filename: str,
        *directory: str,
        destination: Optional[str | Path] = None,
        force: bool = False,
    ) -> str:
        """Download an example file from the PyAnsys GitHub examples repository."""
        if destination is not None:
            destination = Path(destination)
            destination.mkdir(parents=True, exist_ok=True)
            if not destination.is_dir():
                raise ValueError("destination directory provided does not exist")

        url = self._get_filepath_on_default_server(filename, *directory)
        local_path = self._retrieve_data(url, filename, dest=destination, force=force)

        self.add_file(local_path)
        return str(local_path)

    def _joinurl(self, base: str, *paths: str) -> str:
        for path in paths:
            if not base.endswith("/"):
                base += "/"
            base = urljoin(base, path)
        return base

    def _get_default_server_and_joiner(self):
        return "https://github.com/ansys/example-data/raw/main", self._joinurl

    def _get_filepath_on_default_server(self, filename: str, *directory: str):
        server, joiner = self._get_default_server_and_joiner()
        if directory:
            return joiner(server, *directory, filename)
        return joiner(server, filename)

    def _retrieve_url(self, url: str, dest: Path) -> Path:
        parsed_url = urlparse(url)
        allowed_schemes = {"http", "https", "ftp"}

        if parsed_url.scheme not in allowed_schemes:
            raise ValueError(f"URL scheme '{parsed_url.scheme}' not allowed for download.")

        saved_file, _ = urllib.request.urlretrieve(url, filename=str(dest))  # nosec: B310
        return Path(saved_file)

    def _retrieve_data(
        self,
        url: str,
        filename: str,
        dest: Optional[Path] = None,
        force: bool = False,
    ) -> Path:
        if dest is None:
            dest = EXAMPLES_PATH

        local_path = dest / Path(filename).name

        if not force and local_path.is_file():
            return local_path

        return self._retrieve_url(url, local_path)
