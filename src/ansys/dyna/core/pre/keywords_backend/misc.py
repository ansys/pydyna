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

"""Miscellaneous keyword creation methods for the keywords backend."""

import logging
from typing import List

logger = logging.getLogger(__name__)


class MiscKeywordsMixin:
    """Mixin class providing miscellaneous keyword creation methods."""

    def create_define_curve(
        self,
        sfo: float = 1.0,
        abscissa: List[float] = None,
        ordinate: List[float] = None,
        title: str = "",
    ) -> int:
        """Create a DEFINE_CURVE keyword.

        Parameters
        ----------
        sfo : float
            Scale factor for ordinate values.
        abscissa : List[float]
            X values (time/independent variable).
        ordinate : List[float]
            Y values (dependent variable).
        title : str
            Curve title.

        Returns
        -------
        int
            The curve ID.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        if abscissa is None:
            abscissa = []
        if ordinate is None:
            ordinate = []

        curve_id = self.next_id("curve")

        kw = keywords.DefineCurve()
        kw.lcid = curve_id
        kw.sfo = sfo
        kw.title = title

        # Add curve points - build DataFrame properly (append removed in pandas 2.0)
        curve_data = [[a, o] for a, o in zip(abscissa, ordinate)]
        if curve_data:
            kw.curves = pd.DataFrame(curve_data, columns=["a1", "o1"])

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_CURVE with id={curve_id}")
        return curve_id

    def create_define_curve_function(
        self,
        function: str,
        sfo: float = 1.0,
        title: str = "",
    ) -> int:
        """Create a DEFINE_CURVE_FUNCTION keyword.

        Parameters
        ----------
        function : str
            Mathematical function expression (e.g., "sin(TIME)").
        sfo : float
            Scale factor for ordinate values.
        title : str
            Curve title.

        Returns
        -------
        int
            The curve ID.
        """
        from ansys.dyna.core.keywords import keywords

        curve_id = self.next_id("curve")

        logger.debug(f"Creating DEFINE_CURVE_FUNCTION: lcid={curve_id}, function={function}")

        kw = keywords.DefineCurveFunction()
        kw.lcid = curve_id
        kw.sfo = sfo
        kw.function = function
        if title:
            kw.title = title

        self._deck.append(kw)
        logger.info(f"Created DEFINE_CURVE_FUNCTION keyword with lcid={curve_id}")

        return curve_id

    def create_part(
        self,
        pid: int,
        secid: int = 0,
        mid: int = 0,
        title: str = "",
    ) -> bool:
        """Create a PART keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        secid : int
            Section ID.
        mid : int
            Material ID.
        title : str
            Part title.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.Part()
        kw.pid = pid
        kw.secid = secid
        kw.mid = mid
        kw.heading = title

        self._deck.append(kw)
        logger.debug(f"Created PART with pid={pid}")
        return True

    def set_part_property(
        self,
        pid: int,
        secid: int = 0,
        mid: int = 0,
        eosid: int = 0,
        hgid: int = 0,
        grav: int = 0,
        adpopt: int = 0,
        tmid: int = 0,
    ) -> bool:
        """Set properties for an existing PART or create a new one.

        Parameters
        ----------
        pid : int
            Part ID.
        secid : int
            Section ID.
        mid : int
            Material ID.
        eosid : int
            Equation of state ID.
        hgid : int
            Hourglass ID.
        grav : int
            Gravity load flag.
        adpopt : int
            Adaptive meshing flag.
        tmid : int
            Thermal material ID.

        Returns
        -------
        bool
            True if successful.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        # Look for an existing Part keyword and update the row with matching pid
        for kw in self._deck.all_keywords:
            if isinstance(kw, keywords.Part):
                # Part keyword has a DataFrame in kw.parts
                if not kw.parts.empty:
                    # Find row with matching pid
                    mask = kw.parts["pid"] == pid
                    if mask.any():
                        # Update the existing row
                        kw.parts.loc[mask, "secid"] = secid
                        kw.parts.loc[mask, "mid"] = mid
                        kw.parts.loc[mask, "eosid"] = eosid
                        kw.parts.loc[mask, "hgid"] = hgid
                        kw.parts.loc[mask, "grav"] = grav
                        kw.parts.loc[mask, "adpopt"] = adpopt
                        kw.parts.loc[mask, "tmid"] = tmid
                        logger.debug(f"Updated PART properties for pid={pid}")
                        return True

        # If not found in existing keywords, create a new PART keyword
        kw = keywords.Part()
        # Add a row to the parts dataframe
        new_row = pd.DataFrame(
            {
                "heading": [""],
                "pid": [pid],
                "secid": [secid],
                "mid": [mid],
                "eosid": [eosid],
                "hgid": [hgid],
                "grav": [grav],
                "adpopt": [adpopt],
                "tmid": [tmid],
            }
        )
        kw.parts = pd.concat([kw.parts, new_row], ignore_index=True)

        self._deck.append(kw)
        logger.debug(f"Created PART with pid={pid}")
        return True

    def create_contact(
        self,
        contact_type: str,
        ssid: int = 0,
        msid: int = 0,
        sstyp: int = 0,
        mstyp: int = 0,
        fs: float = 0.0,
        fd: float = 0.0,
    ) -> int:
        """Create a CONTACT keyword.

        Parameters
        ----------
        contact_type : str
            Contact type (e.g., "AUTOMATIC_SINGLE_SURFACE").
        ssid : int
            Slave set ID.
        msid : int
            Master set ID.
        sstyp : int
            Slave set type.
        mstyp : int
            Master set type.
        fs : float
            Static friction coefficient.
        fd : float
            Dynamic friction coefficient.

        Returns
        -------
        int
            The contact ID.
        """
        from ansys.dyna.core.keywords import keywords

        contact_id = self.next_id("contact")

        # Map contact types to keyword classes
        contact_type_map = {
            "AUTOMATIC_SINGLE_SURFACE": keywords.ContactAutomaticSingleSurface,
            "AUTOMATIC_SURFACE_TO_SURFACE": keywords.ContactAutomaticSurfaceToSurface,
            "NODES_TO_SURFACE": keywords.ContactNodesToSurface,
        }

        if contact_type.upper() in contact_type_map:
            kw_class = contact_type_map[contact_type.upper()]
            kw = kw_class()
            kw.ssid = ssid
            kw.msid = msid
            kw.sstyp = sstyp
            kw.mstyp = mstyp
            kw.fs = fs
            kw.fd = fd

            self._deck.append(kw)
            logger.debug(f"Created CONTACT_{contact_type} with id={contact_id}")
            return contact_id
        else:
            logger.warning(f"Unknown contact type: {contact_type}")
            return 0

    def create_load_body_y(self, lcid: int = 0, sf: float = 1.0) -> None:
        """Create LOAD_BODY_Y keyword for body load in Y direction.

        Parameters
        ----------
        lcid : int, optional
            Load curve ID for the body load versus time. Default is 0.
        sf : float, optional
            Scale factor for the load curve. Default is 1.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating LOAD_BODY_Y: lcid={lcid}, sf={sf}")

        kw = keywords.LoadBodyY()
        kw.lcid = lcid
        kw.sf = sf

        self._deck.append(kw)
        logger.info(f"Created LOAD_BODY_Y keyword with lcid={lcid}")

    def create_load_body_z(self, lcid: int = 0, sf: float = 1.0) -> None:
        """Create LOAD_BODY_Z keyword for body load in Z direction.

        Parameters
        ----------
        lcid : int, optional
            Load curve ID for the body load versus time. Default is 0.
        sf : float, optional
            Scale factor for the load curve. Default is 1.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating LOAD_BODY_Z: lcid={lcid}, sf={sf}")

        kw = keywords.LoadBodyZ()
        kw.lcid = lcid
        kw.sf = sf

        self._deck.append(kw)
        logger.info(f"Created LOAD_BODY_Z keyword with lcid={lcid}")

