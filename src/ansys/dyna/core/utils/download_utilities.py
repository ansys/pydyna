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

import os
from threading import Lock
from typing import Optional
from urllib.parse import urljoin, urlparse
import urllib.request

from ansys.dyna.core.pre.internals.defaults import EXAMPLES_PATH

__all__ = ["DownloadManager"]


class DownloadManagerMeta(type):
    """
    Provides a thread-safe implementation of ``Singleton`` from
    https://refactoring.guru/design-patterns/singleton/python/example#example-1.
    """

    _instances = {}
    _lock: Lock = Lock()

    def __call__(cls, *args, **kwargs):
        """
        Possible changes to the value of the ``__init__`` argument do not affect
        the returned instance.
        """
        with cls._lock:
            if cls not in cls._instances:
                instance = super().__call__(*args, **kwargs)
                cls._instances[cls] = instance
        return cls._instances[cls]


class DownloadManager(metaclass=DownloadManagerMeta):
    """Manages downloads of example files.

    Local paths are saved in this class so that a global cleanup
    of example files can be performed when the client is closed.
    """

    def __init__(self):
        self.downloads_list = []

    def add_file(self, file_path: str):
        """Add the path for a downloaded example file to a list.

        This list keeps track of where example files are
        downloaded so that a global cleanup of these files can be
        performed when the client is closed.

        Parameters
        ----------
        file_path : str
            Local path of the downloaded example file.
        """
        self.downloads_list.append(file_path)

    def clear_download_cache(self):
        """Remove downloaded example files from the local path."""
        for file in self.downloads_list:
            os.remove(file)
        self.downloads_list.clear()

    def download_file(
        self, filename: str, *directory: str, destination: Optional[str] = None, force: bool = False
    ) -> str:
        """Download an example file from the PyPrimeMesh repository.

        Parameters
        ----------
        filename : str
            Name of the example file to download.
        destination : str, optional
            Path to download the example file to. The default
            is ``None``, in which case the default path for app data
            is used.
        force : bool, optional
            Whether to always download the example file. The default is
            ``False``, in which case if the example file is cached, it
            is reused.
        directory : tuple[str]
            Path under the PyAnsys Github examples repository.

        Returns
        -------
        tuple[str, str]
            Tuple containing the filepath to use and the local filepath of the downloaded
            directory. The two are different in case of containers.

        """
        # if destination is not a dir create it
        if destination is not None and not os.path.isdir(destination):
            os.mkdir(destination)

        # check if it was able to create the dir
        if destination is not None and not os.path.isdir(destination):
            raise ValueError("destination directory provided does not exist")

        url = self._get_filepath_on_default_server(filename, *directory)
        local_path = self._retrieve_data(url, filename, dest=destination, force=force)

        # add path to downloaded files
        self.add_file(local_path)
        return local_path

    def _joinurl(self, base, *paths):
        for path in paths:
            if base[-1] != "/":
                base += "/"
            base = urljoin(base, path)
        return base

    def _get_default_server_and_joiner(self):
        return "https://github.com/ansys/example-data/raw/main", self._joinurl

    def _get_filepath_on_default_server(self, filename: str, *directory: str):
        server, joiner = self._get_default_server_and_joiner()
        if directory:
            return joiner(server, *directory, filename)
        else:
            return joiner(server, filename)

    def _retrieve_url(self, url, dest):
        parsed_url = urlparse(url)
        allowed_schemes = {"http", "https", "ftp"}
        if parsed_url.scheme not in allowed_schemes:
            raise ValueError(f"URL scheme '{parsed_url.scheme}' not allowed for download.")
        # Ignore the B310 warning as this is a safe use of urllib
        # as it is used to download files from a trusted source.
        saved_file, _ = urllib.request.urlretrieve(url, filename=dest)  # nosec: B310
        return saved_file

    def _retrieve_data(self, url: str, filename: str, dest: str = None, force: bool = False):
        if dest == None:
            dest = EXAMPLES_PATH
        local_path = os.path.join(dest, os.path.basename(filename))
        if not force and os.path.isfile(local_path):
            return local_path
        local_path = self._retrieve_url(url, local_path)
        return local_path
