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

"""Boundary condition keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class BoundaryKeywordsMixin:
    """Mixin class providing boundary condition keyword creation methods."""

    def create_boundary_spc_node(
        self,
        nid: int,
        dofx: int = 0,
        dofy: int = 0,
        dofz: int = 0,
        dofrx: int = 0,
        dofry: int = 0,
        dofrz: int = 0,
    ) -> bool:
        """Create a BOUNDARY_SPC_NODE keyword.

        Parameters
        ----------
        nid : int
            Node ID.
        dofx, dofy, dofz : int
            Translational DOF constraints (0=free, 1=constrained).
        dofrx, dofry, dofrz : int
            Rotational DOF constraints (0=free, 1=constrained).

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.BoundarySpcNode()
        kw.nid = nid
        kw.dofx = dofx
        kw.dofy = dofy
        kw.dofz = dofz
        kw.dofrx = dofrx
        kw.dofry = dofry
        kw.dofrz = dofrz

        self._deck.append(kw)
        logger.debug(f"Created BOUNDARY_SPC_NODE for nid={nid}")
        return True

    def create_boundary_prescribed_motion_rigid(
        self,
        pid: int,
        dof: int = 1,
        vad: int = 2,
        lcid: int = 0,
        sf: float = 1.0,
        death: float = 0.0,
        birth: float = 0.0,
    ) -> bool:
        """Create or add to a BOUNDARY_PRESCRIBED_MOTION_RIGID keyword.

        If a BOUNDARY_PRESCRIBED_MOTION_RIGID keyword already exists, this will
        add a new entry to it. Otherwise, creates a new keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        dof : int
            Degree of freedom.
        vad : int
            Velocity/acceleration/displacement flag.
        lcid : int
            Load curve ID.
        sf : float
            Scale factor.
        death : float
            Death time.
        birth : float
            Birth time.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        # Look for existing BOUNDARY_PRESCRIBED_MOTION_RIGID keyword
        existing_kw = None
        for kw in self._deck:
            if hasattr(kw, "keyword") and kw.keyword == "*BOUNDARY_PRESCRIBED_MOTION_RIGID":
                existing_kw = kw
                break

        if existing_kw is not None:
            # Add entry to existing keyword using duplicate card mechanism
            # The keyword uses a duplicate card structure - we add via the table
            import pandas as pd

            new_row = {
                "pid": pid,
                "dof": dof,
                "vad": vad,
                "lcid": lcid,
                "sf": sf,
                "vid": 0,
                "death": death,
                "birth": birth,
            }
            if hasattr(existing_kw, "motions") and existing_kw.motions is not None:
                existing_kw.motions = pd.concat([existing_kw.motions, pd.DataFrame([new_row])], ignore_index=True)
            else:
                existing_kw.motions = pd.DataFrame([new_row])
            logger.debug(f"Added entry to existing BOUNDARY_PRESCRIBED_MOTION_RIGID: pid={pid}, dof={dof}")
        else:
            # Create new keyword
            kw = keywords.BoundaryPrescribedMotionRigid()
            kw.pid = pid
            kw.dof = dof
            kw.vad = vad
            kw.lcid = lcid
            kw.sf = sf
            kw.death = death
            kw.birth = birth

            self._deck.append(kw)
            logger.debug(f"Created BOUNDARY_PRESCRIBED_MOTION_RIGID with pid={pid}, dof={dof}")
        return True

    def create_boundary_spc_set(
        self,
        nsid: int,
        cid: int = 0,
        dofx: int = 0,
        dofy: int = 0,
        dofz: int = 0,
        dofrx: int = 0,
        dofry: int = 0,
        dofrz: int = 0,
    ) -> bool:
        """Create a BOUNDARY_SPC_SET keyword.

        Parameters
        ----------
        nsid : int
            Node set ID.
        cid : int, optional
            Coordinate system ID. Default is 0.
        dofx : int, optional
            Constrain X translation. Default is 0.
        dofy : int, optional
            Constrain Y translation. Default is 0.
        dofz : int, optional
            Constrain Z translation. Default is 0.
        dofrx : int, optional
            Constrain X rotation. Default is 0.
        dofry : int, optional
            Constrain Y rotation. Default is 0.
        dofrz : int, optional
            Constrain Z rotation. Default is 0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.BoundarySpcSet()
        kw.nsid = nsid
        kw.cid = cid
        kw.dofx = dofx
        kw.dofy = dofy
        kw.dofz = dofz
        kw.dofrx = dofrx
        kw.dofry = dofry
        kw.dofrz = dofrz

        self._deck.append(kw)
        logger.debug(f"Created BOUNDARY_SPC_SET with nsid={nsid}")
        return True

    def create_boundary_temperature_set(
        self,
        nsid: int,
        lcid: int = 0,
        cmult: float = 1.0,
        loc: int = 0,
        tdeath: float = 0.0,
        tbirth: float = 0.0,
    ) -> bool:
        """Create a BOUNDARY_TEMPERATURE_SET keyword.

        Parameters
        ----------
        nsid : int
            Node set ID.
        lcid : int
            Load curve ID for temperature vs time. Use 0 for constant.
        cmult : float
            Curve multiplier (temperature value when lcid=0).
        loc : int
            Location flag.
        tdeath : float
            Death time.
        tbirth : float
            Birth time.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.BoundaryTemperatureSet()
        kw.nsid = nsid
        kw.lcid = lcid
        kw.cmult = cmult
        kw.loc = loc
        kw.tdeath = tdeath
        kw.tbirth = tbirth

        self._deck.append(kw)
        logger.debug(f"Created BOUNDARY_TEMPERATURE_SET with nsid={nsid}, cmult={cmult}")
        return True
