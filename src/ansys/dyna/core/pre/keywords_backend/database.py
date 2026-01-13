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

"""Database keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class DatabaseKeywordsMixin:
    """Mixin class providing database keyword creation methods."""

    def create_database_binary(
        self,
        filetype: str = "D3PLOT",
        dt: float = 0,
        maxint: int = 3,
        ieverp: int = 0,
        dcomp: int = 1,
        nintsld: int = 1,
    ) -> bool:
        """Create a DATABASE_BINARY keyword.

        Parameters
        ----------
        filetype : str
            Type of file (e.g., "D3PLOT").
        dt : float
            Time interval between outputs.
        maxint : int
            Number of integration points.
        ieverp : int
            Output state flag.
        dcomp : int
            Data compression flag.
        nintsld : int
            Number of solid integration points.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        # Store settings
        self._db_binary_settings = {
            "filetype": filetype,
            "dt": dt,
            "maxint": maxint,
            "ieverp": ieverp,
            "dcomp": dcomp,
            "nintsld": nintsld,
        }

        # Create DATABASE_BINARY_D3PLOT
        kw = keywords.DatabaseBinaryD3Plot()
        kw.dt = dt

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_BINARY_{filetype} with dt={dt}")

        # Create DATABASE_EXTENT_BINARY
        extent_kw = keywords.DatabaseExtentBinary()
        extent_kw.maxint = maxint
        extent_kw.ieverp = ieverp
        extent_kw.dcomp = dcomp
        extent_kw.nintsld = nintsld
        # Match pre server defaults
        extent_kw.ialemat = 0
        extent_kw.sclp = float("nan")  # Leave unset like pre server

        self._deck.append(extent_kw)
        logger.debug("Created DATABASE_EXTENT_BINARY")
        return True

    def create_database_ascii(
        self,
        db_type: str,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 1,
    ) -> bool:
        """Create a DATABASE_ASCII keyword.

        Parameters
        ----------
        db_type : str
            Type of database (e.g., "GLSTAT", "MATSUM").
        dt : float
            Time interval between outputs.
        binary : int
            Binary output flag.
        lcur : int
            Curve ID for time interval.
        ioopt : int
            Output frequency flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        self._db_ascii_settings.append({"type": db_type, "dt": dt, "binary": binary, "lcur": lcur, "ioopt": ioopt})

        # Map database types to keyword classes
        db_type_map = {
            "GLSTAT": keywords.DatabaseGlstat,
            "MATSUM": keywords.DatabaseMatsum,
            "NODFOR": keywords.DatabaseNodfor,
            "RCFORC": keywords.DatabaseRcforc,
            "SLEOUT": keywords.DatabaseSleout,
            "BNDOUT": keywords.DatabaseBndout,
            "ELOUT": keywords.DatabaseElout,
            "NODOUT": keywords.DatabaseNodout,
            "RBDOUT": keywords.DatabaseRbdout,
            "SECFORC": keywords.DatabaseSecforc,
            "RWFORC": keywords.DatabaseRwforc,
            "ABSTAT": keywords.DatabaseAbstat,
        }

        if db_type.upper() in db_type_map:
            kw_class = db_type_map[db_type.upper()]
            kw = kw_class()
            kw.dt = dt
            kw.binary = binary
            kw.lcur = lcur
            kw.ioopt = ioopt

            self._deck.append(kw)
            logger.debug(f"Created DATABASE_{db_type.upper()} with dt={dt}")
            return True
        else:
            logger.warning(f"Unknown database type: {db_type}")
            return False

    def create_database_glstat(
        self,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 0,
    ) -> bool:
        """Create a DATABASE_GLSTAT keyword.

        Parameters
        ----------
        dt : float, optional
            Time interval between outputs. Default is 0.0.
        binary : int, optional
            Binary file flag. Default is 1.
        lcur : int, optional
            Load curve ID. Default is 0.
        ioopt : int, optional
            Output option. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DatabaseGlstat()
        kw.dt = dt
        kw.binary = binary
        kw.lcur = lcur
        kw.ioopt = ioopt

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_GLSTAT with dt={dt}")
        return True

    def create_database_matsum(
        self,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 0,
    ) -> bool:
        """Create a DATABASE_MATSUM keyword.

        Parameters
        ----------
        dt : float, optional
            Time interval between outputs. Default is 0.0.
        binary : int, optional
            Binary file flag. Default is 1.
        lcur : int, optional
            Load curve ID. Default is 0.
        ioopt : int, optional
            Output option. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DatabaseMatsum()
        kw.dt = dt
        kw.binary = binary
        kw.lcur = lcur
        kw.ioopt = ioopt

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_MATSUM with dt={dt}")
        return True

    def create_database_binary_d3plot(
        self,
        dt: float = 0.0,
        lcdt: int = 0,
        beam: int = 0,
        npltc: int = 0,
        psetid: int = 0,
    ) -> bool:
        """Create a DATABASE_BINARY_D3PLOT keyword.

        Parameters
        ----------
        dt : float, optional
            Time interval between outputs. Default is 0.0.
        lcdt : int, optional
            Load curve for output time. Default is 0.
        beam : int, optional
            Beam element data flag. Default is 0.
        npltc : int, optional
            Number of plot states. Default is 0.
        psetid : int, optional
            Part set ID. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DatabaseBinaryD3Plot()
        kw.dt = dt
        kw.lcdt = lcdt
        kw.beam = beam
        kw.npltc = npltc
        kw.psetid = psetid

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_BINARY_D3PLOT with dt={dt}")
        return True

    def create_database_sale(self, switch: int = 1) -> bool:
        """Create a DATABASE_SALE keyword.

        Parameters
        ----------
        switch : int, optional
            Switch to turn on/off SALE database output. Default is 1 (on).

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.DatabaseSale()
        kw.on_off = switch

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_SALE with switch={switch}")
        return True
