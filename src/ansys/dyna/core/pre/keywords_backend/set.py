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

"""Set keyword creation methods for the keywords backend."""

import logging
from typing import List

logger = logging.getLogger(__name__)


class SetKeywordsMixin:
    """Mixin class providing set keyword creation methods."""

    def create_set_node_list_with_solver(
        self,
        sid: int,
        nodes: List[int],
        solver: str = "MECH",
    ) -> bool:
        """Create a SET_NODE_LIST keyword with solver option.

        Parameters
        ----------
        sid : int
            Set ID.
        nodes : List[int]
            List of node IDs.
        solver : str
            Solver type (MECH, THER, etc.).

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetNodeList()
        kw.sid = sid
        kw.solver = solver
        kw.its = None  # Match reference file behavior

        # Add nodes to the set using the SeriesCard's append method
        for nid in nodes:
            kw.nodes.append(nid)

        self._deck.append(kw)
        logger.debug(f"Created SET_NODE_LIST with sid={sid}, solver={solver}")
        return True

    def create_set_node_list(self, sid: int, nodes: List[int]) -> bool:
        """Create a SET_NODE_LIST keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        nodes : List[int]
            List of node IDs.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetNodeList()
        kw.sid = sid

        # Add nodes to the set using SeriesCard's append method
        for nid in nodes:
            kw.nodes.append(nid)

        self._deck.append(kw)
        logger.debug(f"Created SET_NODE_LIST with sid={sid}")
        return True

    def create_set_part_list(self, sid: int, parts: List[int]) -> bool:
        """Create a SET_PART_LIST keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        parts : List[int]
            List of part IDs.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetPartList()
        kw.sid = sid

        # Add parts to the set
        for pid in parts:
            kw.parts.append(pid)

        self._deck.append(kw)
        logger.debug(f"Created SET_PART_LIST with sid={sid}, {len(parts)} parts")
        return True

    def create_set_segment(
        self,
        sid: int,
        segments: List[tuple],
        solver: str = "MECH",
    ) -> bool:
        """Create a SET_SEGMENT keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        segments : List[tuple]
            List of segment tuples (n1, n2, n3, n4).
        solver : str
            Solver type.

        Returns
        -------
        bool
            True if successful.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetSegment()
        kw.sid = sid
        kw.solver = solver

        # Build rows for the dataframe
        rows = []
        for seg in segments:
            if len(seg) >= 4:
                rows.append({"n1": seg[0], "n2": seg[1], "n3": seg[2], "n4": seg[3]})
            elif len(seg) == 3:
                # Triangle - n4 = n3
                rows.append({"n1": seg[0], "n2": seg[1], "n3": seg[2], "n4": seg[2]})

        if rows:
            new_df = pd.DataFrame(rows)
            kw.segments = pd.concat([kw.segments, new_df], ignore_index=True)

        self._deck.append(kw)
        logger.debug(f"Created SET_SEGMENT with sid={sid}, {len(segments)} segments")
        return True
