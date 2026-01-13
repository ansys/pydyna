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
        lcid: int = None,
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
        lcid : int, optional
            Curve ID. If None, auto-generated.

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

        curve_id = lcid if lcid is not None else self.next_id("curve")

        kw = keywords.DefineCurve()
        kw.lcid = curve_id
        kw.sfo = sfo
        kw.title = title

        # Add curve points - build DataFrame properly (append removed in pandas 2.0)
        curve_data = [[a, o] for a, o in zip(abscissa, ordinate)]
        if curve_data:
            kw.curves = pd.DataFrame(curve_data, columns=["a1", "o1"])

        self._deck.append(kw)
        # Register the keyword for later lookup by ID
        self.register_id("curve", curve_id, kw)
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
        # Register the keyword for later lookup by ID
        self.register_id("curve", curve_id, kw)
        logger.info(f"Created DEFINE_CURVE_FUNCTION keyword with lcid={curve_id}")

        return curve_id

    def create_define_function(
        self,
        function: str,
        fid: int = None,
        heading: str = "",
    ) -> int:
        """Create a DEFINE_FUNCTION keyword.

        Parameters
        ----------
        function : str
            C-like function code (multi-line supported).
        fid : int, optional
            Function ID. If None, auto-generated.
        heading : str, optional
            Function heading/title.

        Returns
        -------
        int
            The function ID.
        """
        from ansys.dyna.core.keywords import keywords

        if fid is None:
            fid = self.next_id("function")

        logger.debug(f"Creating DEFINE_FUNCTION: fid={fid}")

        kw = keywords.DefineFunction()
        kw.fid = fid
        kw.heading = heading
        kw.function = function

        self._deck.append(kw)
        logger.info(f"Created DEFINE_FUNCTION keyword with fid={fid}")

        return fid

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
        sfsa: float = 1.0,
        sfsb: float = 1.0,
        bt: float = 0.0,
        dt: float = 0.0,
    ) -> int:
        """Create a CONTACT keyword.

        Parameters
        ----------
        contact_type : str
            Contact type (e.g., "AUTOMATIC_SINGLE_SURFACE", "NODES_TO_SURFACE").
        ssid : int
            Slave set ID (surfa).
        msid : int
            Master set ID (surfb).
        sstyp : int
            Slave set type (surfatyp).
        mstyp : int
            Master set type (surfbtyp).
        fs : float
            Static friction coefficient.
        fd : float
            Dynamic friction coefficient.
        sfsa : float
            Slave penalty scale factor.
        sfsb : float
            Master penalty scale factor.
        bt : float
            Birth time.
        dt : float
            Death time.

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
            # Card 0: surfa, surfb, surfatyp, surfbtyp
            kw.surfa = ssid
            kw.surfb = msid
            kw.surfatyp = sstyp
            kw.surfbtyp = mstyp
            # Card 1: fs, fd, bt, dt
            kw.fs = fs
            kw.fd = fd
            if bt:
                kw.bt = bt
            if dt:
                kw.dt = dt
            # Card 2: sfsa, sfsb
            kw.sfsa = sfsa
            kw.sfsb = sfsb

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

    def create_define_transformation(
        self,
        transforms: List[dict] = None,
        tranid: int = None,
    ) -> int:
        """Create a DEFINE_TRANSFORMATION keyword.

        Parameters
        ----------
        transforms : List[dict]
            List of transformation operations. Each dict should have:
            - option: str (e.g., "MIRROR", "SCALE", "ROTATE", "TRANSL")
            - a1-a7: float values for transformation parameters
        tranid : int, optional
            Transformation ID. If None, auto-generated.

        Returns
        -------
        int
            The transformation ID.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        if transforms is None:
            transforms = []

        trans_id = tranid if tranid is not None else self.next_id("transformation")

        logger.debug(f"Creating DEFINE_TRANSFORMATION: tranid={trans_id}")

        kw = keywords.DefineTransformation()
        kw.tranid = trans_id

        # Build transforms table as DataFrame
        if transforms:
            data = []
            for t in transforms:
                row = [
                    t.get("option", ""),
                    t.get("a1", 0.0),
                    t.get("a2", 0.0),
                    t.get("a3", 0.0),
                    t.get("a4", 0.0),
                    t.get("a5", 0.0),
                    t.get("a6", 0.0),
                    t.get("a7", 0.0),
                ]
                data.append(row)
            kw.transforms = pd.DataFrame(data, columns=["option", "a1", "a2", "a3", "a4", "a5", "a6", "a7"])

        self._deck.append(kw)
        logger.info(f"Created DEFINE_TRANSFORMATION keyword with tranid={trans_id}")
        return trans_id

    def create_include_transform(
        self,
        filename: str,
        idnoff: int = 0,
        ideoff: int = 0,
        idpoff: int = 0,
        idmoff: int = 0,
        idsoff: int = 0,
        idfoff: int = 0,
        iddoff: int = 0,
        idroff: int = 0,
        prefix: str = "",
        suffix: str = "",
        fctmas: float = 0.0,
        fcttim: float = 0.0,
        fctlen: float = 0.0,
        fcttem: str = "",
        incout1: int = 0,
        fctchg: float = 0.0,
        tranid: int = 0,
    ) -> None:
        """Create an INCLUDE_TRANSFORM keyword.

        Parameters
        ----------
        filename : str
            Name of the file to include.
        idnoff : int
            Node ID offset.
        ideoff : int
            Element ID offset.
        idpoff : int
            Part ID offset.
        idmoff : int
            Material ID offset.
        idsoff : int
            Section ID offset.
        idfoff : int
            Function ID offset.
        iddoff : int
            Define ID offset.
        idroff : int
            Set ID offset.
        prefix : str
            Prefix added to titles in included file.
        suffix : str
            Suffix added to titles in included file.
        fctmas : float
            Mass transformation factor.
        fcttim : float
            Time transformation factor.
        fctlen : float
            Length transformation factor.
        fcttem : str
            Temperature transformation factor.
        incout1 : int
            Output control (1 to create DYNA.INC file).
        fctchg : float
            Electric charge transformation factor.
        tranid : int
            Transformation ID (references DEFINE_TRANSFORMATION).
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating INCLUDE_TRANSFORM: filename={filename}, tranid={tranid}")

        kw = keywords.IncludeTransform()
        kw.filename = filename
        kw.idnoff = idnoff
        kw.ideoff = ideoff
        kw.idpoff = idpoff
        kw.idmoff = idmoff
        kw.idsoff = idsoff
        kw.idfoff = idfoff
        kw.iddoff = iddoff
        kw.idroff = idroff
        if prefix:
            kw.prefix = prefix
        if suffix:
            kw.suffix = suffix
        if fctmas:
            kw.fctmas = fctmas
        if fcttim:
            kw.fcttim = fcttim
        if fctlen:
            kw.fctlen = fctlen
        if fcttem:
            kw.fcttem = fcttem
        # incout1 accepts 0, 1, or None - explicitly set it
        kw.incout1 = incout1 if incout1 else None
        if fctchg:
            kw.fctchg = fctchg
        kw.tranid = tranid

        self._deck.append(kw)
        logger.info(f"Created INCLUDE_TRANSFORM keyword: filename={filename}")

    def create_rigidwall_planar(
        self,
        nsid: int = 0,
        nsidex: int = 0,
        boxid: int = 0,
        fric: float = 0.5,
        xt: float = 0.0,
        yt: float = 0.0,
        zt: float = 0.0,
        xh: float = 0.0,
        yh: float = 0.0,
        zh: float = 0.0,
    ) -> int:
        """Create a RIGIDWALL_PLANAR_ID keyword.

        Parameters
        ----------
        nsid : int
            Node set ID containing tracked nodes.
        nsidex : int
            Node set ID containing exempted nodes.
        boxid : int
            Box ID for tracked nodes.
        fric : float
            Coulomb friction coefficient.
        xt : float
            X-coordinate of tail of normal vector.
        yt : float
            Y-coordinate of tail of normal vector.
        zt : float
            Z-coordinate of tail of normal vector.
        xh : float
            X-coordinate of head of normal vector.
        yh : float
            Y-coordinate of head of normal vector.
        zh : float
            Z-coordinate of head of normal vector.

        Returns
        -------
        int
            The rigidwall ID.
        """
        from ansys.dyna.core.keywords import keywords

        wall_id = self.next_id("rigidwall")

        logger.debug(f"Creating RIGIDWALL_PLANAR_ID: id={wall_id}")

        kw = keywords.RigidwallPlanarId()
        kw.id = wall_id
        kw.nsid = nsid
        kw.nsidex = nsidex
        kw.boxid = boxid
        kw.xt = xt
        kw.yt = yt
        kw.zt = zt
        kw.xh = xh
        kw.yh = yh
        kw.zh = zh
        kw.fric = fric

        self._deck.append(kw)
        logger.info(f"Created RIGIDWALL_PLANAR_ID keyword with id={wall_id}")
        return wall_id

    def create_airbag_model(
        self,
        sid: int,
        sidtyp: int = 0,
        cv: float = 0.0,
        cp: float = 0.0,
        t: float = 0.0,
        lcid: int = 0,
        mu: float = 0.0,
        area: float = 0.0,
        pe: float = 0.0,
        ro: float = 0.0,
    ) -> None:
        """Create an AIRBAG_SIMPLE_AIRBAG_MODEL keyword.

        Parameters
        ----------
        sid : int
            Set ID (segment or part set).
        sidtyp : int
            Set type (0=segment, 1=part IDs).
        cv : float
            Heat capacity at constant volume.
        cp : float
            Heat capacity at constant pressure.
        t : float
            Temperature of input gas.
        lcid : int
            Load curve ID for mass flow rate.
        mu : float
            Shape factor for exit hole.
        area : float
            Optional airbag reference area.
        pe : float
            Ambient pressure.
        ro : float
            Ambient density.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating AIRBAG_SIMPLE_AIRBAG_MODEL: sid={sid}")

        kw = keywords.AirbagSimpleAirbagModel()
        kw.sid = sid
        kw.sidtyp = sidtyp
        kw.cv = cv
        kw.cp = cp
        kw.t = t
        kw.lcid = lcid
        kw.mu = mu
        kw.area = area
        kw.pe = pe
        kw.ro = ro

        self._deck.append(kw)
        logger.info(f"Created AIRBAG_SIMPLE_AIRBAG_MODEL keyword with sid={sid}")
