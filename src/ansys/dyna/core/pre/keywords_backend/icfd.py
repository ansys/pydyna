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
ICFD Keywords Mixin
===================

Mixin class providing ICFD (Incompressible CFD) keyword functionality.
"""

import logging
from typing import List, Optional, Set

import pandas as pd

logger = logging.getLogger(__name__)


class ICFDKeywordsMixin:
    """Mixin class providing ICFD keyword creation methods."""

    def _get_icfd_sections_created(self) -> Set[int]:
        """Get the set of created ICFD section IDs (lazily initialized)."""
        if not hasattr(self, "_icfd_sections_created"):
            self._icfd_sections_created: Set[int] = set()
        return self._icfd_sections_created

    def create_icfd_control_time(
        self,
        ttm: float = 1e28,
        dt: float = 0.0,
        cfl: float = 1.0,
        lcidsf: int = 0,
        dtmin: float = 0.0,
        dtmax: float = 0.0,
        dtinit: float = 0.0,
        tdeath: float = 0.0,
    ) -> None:
        """Create ICFD_CONTROL_TIME keyword.

        Parameters
        ----------
        ttm : float, optional
            Total time of simulation for the fluid problem. Default is 1e28.
        dt : float, optional
            Time step for fluid problem. If 0, auto-computed. Default is 0.0.
        cfl : float, optional
            CFL number for time step calculation. Default is 1.0.
        lcidsf : int, optional
            Load curve ID for scaling factor. Default is 0.
        dtmin : float, optional
            Minimum time step. Default is 0.0.
        dtmax : float, optional
            Maximum time step. Default is 0.0.
        dtinit : float, optional
            Initial time step. Default is 0.0.
        tdeath : float, optional
            Death time for ICFD solver. Default is 0.0.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(
            f"Creating ICFD_CONTROL_TIME: ttm={ttm}, dt={dt}, cfl={cfl}, "
            f"lcidsf={lcidsf}, dtmin={dtmin}, dtmax={dtmax}, dtinit={dtinit}, tdeath={tdeath}"
        )

        kw = keywords.IcfdControlTime()
        kw.ttm = ttm
        kw.dt = dt
        kw.cfl = cfl
        kw.lcidsf = lcidsf
        kw.dtmin = dtmin
        kw.dtmax = dtmax
        kw.dtinit = dtinit
        kw.tdeath = tdeath

        self._deck.append(kw)
        logger.info("Created ICFD_CONTROL_TIME keyword")

    def create_icfd_section(self, sid: int) -> None:
        """Create ICFD_SECTION keyword.

        Skips creation if a section with the same sid has already been created.

        Parameters
        ----------
        sid : int
            Section identification.
        """
        # Check if this section already exists to avoid duplicates
        created = self._get_icfd_sections_created()
        if sid in created:
            logger.debug(f"ICFD_SECTION with sid={sid} already exists, skipping")
            return

        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_SECTION: sid={sid}")

        kw = keywords.IcfdSection()
        kw.sid = sid

        self._deck.append(kw)
        created.add(sid)
        logger.info(f"Created ICFD_SECTION keyword with sid={sid}")

    def create_icfd_part_vol(
        self,
        pid: int,
        secid: int,
        mid: int = 0,
        spids: Optional[List[int]] = None,
    ) -> int:
        """Create ICFD_PART_VOL keyword.

        Parameters
        ----------
        pid : int
            Part identification for volume.
        secid : int
            Section ID defined in *ICFD_SECTION.
        mid : int, optional
            Material ID. Default is 0.
        spids : list of int, optional
            Surface part IDs that define the volume. Default is None.

        Returns
        -------
        int
            The part ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating ICFD_PART_VOL: pid={pid}, secid={secid}, mid={mid}, spids={spids}")

        kw = keywords.IcfdPartVol()
        kw.pid = pid
        kw.secid = secid
        kw.mid = mid

        # Set the surface part IDs in the table card
        if spids:
            # Pad the list to 8 elements
            padded_spids = spids + [0] * (8 - len(spids)) if len(spids) < 8 else spids[:8]
            df = pd.DataFrame(
                [padded_spids],
                columns=["spid1", "spid2", "spid3", "spid4", "spid5", "spid6", "spid7", "spid8"],
            )
            kw.nodes = df

        self._deck.append(kw)
        logger.info(f"Created ICFD_PART_VOL keyword with pid={pid}")
        return pid

    def create_mesh_volume(
        self,
        volid: int,
        pids: Optional[List[int]] = None,
    ) -> int:
        """Create MESH_VOLUME keyword.

        Parameters
        ----------
        volid : int
            ID assigned to the new volume.
        pids : list of int, optional
            Surface element/part IDs. Default is None.

        Returns
        -------
        int
            The volume ID assigned.
        """
        from ansys.dyna.core.keywords import keywords

        logger.debug(f"Creating MESH_VOLUME: volid={volid}, pids={pids}")

        kw = keywords.MeshVolume()
        kw.volid = volid

        # Set the surface part IDs using the elements property (SeriesCard)
        if pids:
            kw.elements = pids

        self._deck.append(kw)
        logger.info(f"Created MESH_VOLUME keyword with volid={volid}")
        return volid
