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

"""
Keywords-Based DynaSolution
===========================

Module providing a keywords-based implementation of DynaSolution.
This allows running pre examples without requiring a gRPC server.
"""

import logging
import os
import shutil
from typing import List, Optional

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.pre.keywords_backend import KeywordsBackend
from ansys.dyna.core.pre.model import Model

logger = logging.getLogger(__name__)


class KeywordsDynaSolution:
    """Keywords-based implementation of DynaSolution.

    This class provides the same interface as DynaSolution but uses
    the local keywords module instead of gRPC calls.
    """

    # Class-level attributes for compatibility with existing code
    stub = None
    termination_time = 0

    def __init__(self, working_dir: Optional[str] = None):
        """Initialize the keywords-based solution.

        Parameters
        ----------
        working_dir : str, optional
            Working directory for output files. Defaults to current directory.
        """
        logger.info("Initializing KeywordsDynaSolution")

        self._backend = KeywordsBackend()
        self._working_dir = working_dir or os.getcwd()
        self._backend.set_working_dir(self._working_dir)

        self.object_list = []
        self.mainname = ""
        self._path = None
        self._default_model: Optional[Model] = None
        self._loaded_files: List[str] = []

        # Store a reference for compatibility
        KeywordsDynaSolution.stub = self._backend
        KeywordsDynaSolution.termination_time = 0

        # PIM client attributes (not used in keywords backend)
        self.pim_client = None
        self.remote_instance = None

    @property
    def deck(self) -> Deck:
        """Get the underlying Deck instance."""
        return self._backend.deck

    @property
    def model(self) -> Model:
        """Get model associated with the solution."""
        if self._default_model is None:
            # Create a model that works with the keywords backend
            self._default_model = KeywordsModel(self._backend)
        return self._default_model

    @staticmethod
    def get_stub():
        """Get the stub of the solution object."""
        return KeywordsDynaSolution.stub

    def add(self, obj):
        """Add a case to the solution.

        Parameters
        ----------
        obj :
            A DynaBase subclass instance (e.g., DynaMech).
        """
        obj.set_parent(self)
        self.object_list.append(obj)

    def open_files(self, filenames: List[str]) -> bool:
        """Open initial model files.

        Parameters
        ----------
        filenames : list
            List of filenames. The main file is ``[0]``. The others are subfiles.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if not filenames:
            logger.warning("No files provided to open_files")
            return False

        # Set main filename
        self.mainname = os.path.basename(filenames[0])
        self._backend.set_main_filename(self.mainname)

        # Load files into the deck
        for filename in filenames:
            if os.path.exists(filename):
                logger.info(f"Loading file: {filename}")
                self._backend.load_file(filename)
                self._loaded_files.append(filename)
            else:
                logger.warning(f"File not found: {filename}")

        return True

    def set_termination(self, termination_time: float) -> bool:
        """Set time for terminating the job.

        Parameters
        ----------
        termination_time : float
            Termination time.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        KeywordsDynaSolution.termination_time = termination_time
        self._backend.create_termination(termination_time)
        logger.info(f"Setting termination time to {termination_time}")
        return True

    def create_database_binary(
        self,
        filetype: str = "D3PLOT",
        dt: float = 0,
        maxint: int = 3,
        ieverp: int = 0,
        dcomp: int = 1,
        nintsld: int = 1,
    ) -> bool:
        """Request binary output.

        Parameters
        ----------
        filetype : str, optional
           Type of file. The default is ``"D3PLOT"``.
        dt : float, optional
            Time interval between output states. The default is ``0``.
        maxint : int, optional
            Number of shell and thick shell through-thickness integration points.
        ieverp : int, optional
            How to plot output states on plot files.
        dcomp : int, optional
            Data compression to eliminate rigid body data.
        nintsld : int, optional
            Number of solid element integration points.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self._backend.create_database_binary(
            filetype=filetype,
            dt=dt,
            maxint=maxint,
            ieverp=ieverp,
            dcomp=dcomp,
            nintsld=nintsld,
        )
        logger.info("DB Binary Created...")
        return True

    def create_database_ascii(
        self,
        type: str,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 0,
    ) -> bool:
        """Obtain output files containing result information.

        Parameters
        ----------
        type : string
            Type of the database.
        dt : float, optional
            Time interval between outputs.
        binary : int, optional
            Flag for whether to generate binary output.
        lcur : int, optional
            Curve ID specifying the time interval between outputs.
        ioopt : int, optional
            Flag for governing the behavior of the output frequency.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self._backend.create_database_ascii(
            db_type=type,
            dt=dt,
            binary=binary,
            lcur=lcur,
            ioopt=ioopt,
        )
        logger.info("DB Ascii Created...")
        return True

    def set_output_database(
        self,
        matsum: float = 0,
        glstat: float = 0,
        elout: float = 0,
        nodout: float = 0,
        nodfor: float = 0,
        rbdout: float = 0,
        rcforc: float = 0,
        secforc: float = 0,
        rwforc: float = 0,
        abstat: float = 0,
        bndout: float = 0,
        sleout: float = 0,
        sphmassflow: float = 0,
    ) -> bool:
        """Obtain output files containing result information.

        Parameters
        ----------
        matsum : float, optional
            Time interval between outputs of part energies.
        glstat : float, optional
            Time interval between outputs of global statistics and energies.
        elout : float, optional
            Time interval for element output.
        nodout : float, optional
            Time interval for nodal output.
        nodfor : float, optional
            Time interval for nodal force output.
        rbdout : float, optional
            Time interval for rigid body output.
        rcforc : float, optional
            Time interval for resultant contact force output.
        secforc : float, optional
            Time interval for section force output.
        rwforc : float, optional
            Time interval for rigid wall force output.
        abstat : float, optional
            Time interval for airbag statistics.
        bndout : float, optional
            Time interval for boundary output.
        sleout : float, optional
            Time interval for sliding interface energy output.
        sphmassflow : float, optional
            Time interval for SPH mass flow output.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if matsum > 0:
            self._backend.create_database_ascii("MATSUM", dt=matsum)
        if glstat > 0:
            self._backend.create_database_ascii("GLSTAT", dt=glstat)
        if elout > 0:
            self._backend.create_database_ascii("ELOUT", dt=elout)
        if nodout > 0:
            self._backend.create_database_ascii("NODOUT", dt=nodout)
        if nodfor > 0:
            self._backend.create_database_ascii("NODFOR", dt=nodfor)
        if rbdout > 0:
            self._backend.create_database_ascii("RBDOUT", dt=rbdout)
        if rcforc > 0:
            self._backend.create_database_ascii("RCFORC", dt=rcforc)
        if secforc > 0:
            self._backend.create_database_ascii("SECFORC", dt=secforc)
        if rwforc > 0:
            self._backend.create_database_ascii("RWFORC", dt=rwforc)
        if abstat > 0:
            self._backend.create_database_ascii("ABSTAT", dt=abstat)
        if bndout > 0:
            self._backend.create_database_ascii("BNDOUT", dt=bndout)
        if sleout > 0:
            self._backend.create_database_ascii("SLEOUT", dt=sleout)
        if sphmassflow > 0:
            self._backend.create_database_ascii("SPHMASSFLOW", dt=sphmassflow)

        logger.info("Output Setting...")
        return True

    def save_file(self) -> str:
        """Save keyword files.

        Returns
        -------
        str
            The path where files were saved.
        """
        # Let each object save its data to the deck
        for obj in self.object_list:
            obj.save_file()

        # Save the deck to file
        output_path = self._backend.save_file(self.mainname)
        logger.info(f"{self.mainname} is outputted to {output_path}")
        return output_path

    def download(self, remote_name: str, local_name: str) -> None:
        """Download files from the server (copies local file for keywords backend).

        Parameters
        ----------
        remote_name : str
            Source file path.
        local_name : str
            Destination file path.
        """
        # For keywords backend, the file is already local
        # Just copy it to the requested location
        source_path = remote_name
        if not os.path.isabs(source_path):
            source_path = os.path.join(self._working_dir, remote_name)

        if os.path.exists(source_path):
            os.makedirs(os.path.dirname(local_name), exist_ok=True)
            shutil.copy2(source_path, local_name)
            logger.info(f"Copied {source_path} to {local_name}")
        else:
            # The deck content can be written directly
            deck_content = self._backend.deck.write()
            os.makedirs(os.path.dirname(local_name), exist_ok=True)
            with open(local_name, "w") as f:
                f.write(deck_content)
            logger.info(f"Wrote deck to {local_name}")

    def quit(self) -> None:
        """Cleanup (no-op for keywords backend)."""
        logger.debug("KeywordsDynaSolution quit called (no-op)")

    def get_file_chunks(self, filename: str):
        """Get file chunks (for compatibility)."""
        with open(filename, "rb") as f:
            while True:
                piece = f.read(1024 * 1024)
                if len(piece) == 0:
                    return
                yield type("Chunk", (), {"buffer": piece})()

    def upload(self, stub_, filename: str) -> None:
        """Upload files (no-op for keywords backend)."""
        # For local backend, we load files directly
        pass


class KeywordsModel(Model):
    """Keywords-based implementation of Model.

    This provides a Model interface that works with the keywords backend.
    """

    def __init__(self, backend: KeywordsBackend):
        """Initialize the model with a backend.

        Parameters
        ----------
        backend : KeywordsBackend
            The keywords backend instance.
        """
        self._backend = backend
        # Don't call parent __init__ as it requires a gRPC stub

    def get_nodes(self):
        """Get nodes from the deck."""
        # Return nodes from the loaded deck
        node_keywords = list(self._backend.deck.nodes)
        return node_keywords

    def get_elements(self):
        """Get elements from the deck."""
        element_keywords = list(self._backend.deck.elements)
        return element_keywords

    def get_parts(self):
        """Get parts from the deck."""
        part_keywords = list(self._backend.deck.parts)
        return part_keywords
