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

"""Initial condition keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class InitialKeywordsMixin:
    """Mixin class providing initial condition keyword creation methods."""

    def create_initial_temperature(
        self,
        option: str = "SET",
        nsid: int = 0,
        temp: float = 0.0,
    ) -> bool:
        """Create an INITIAL_TEMPERATURE keyword.

        Parameters
        ----------
        option : str
            Option: "SET" for nodal set or "NODE" for single node.
        nsid : int
            Node set ID or node ID.
        temp : float
            Initial temperature.

        Returns
        -------
        bool
            True if successful.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        if option.upper() == "NODE":
            # Look for existing InitialTemperatureNode keyword and append to it
            for kw in self._deck.all_keywords:
                if isinstance(kw, keywords.InitialTemperatureNode):
                    # Add a new row to the existing keyword's DataFrame
                    new_row = pd.DataFrame({"nid": [nsid], "temp": [temp], "loc": [0]})
                    kw.nodes = pd.concat([kw.nodes, new_row], ignore_index=True)
                    logger.debug(f"Added node {nsid} to existing INITIAL_TEMPERATURE_NODE")
                    return True

            # Create new keyword if not found
            kw = keywords.InitialTemperatureNode()
            kw.nodes = pd.DataFrame({"nid": [nsid], "temp": [temp], "loc": [0]})
        else:
            kw = keywords.InitialTemperatureSet()
            kw.nsid = nsid
            kw.temp = temp

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_TEMPERATURE_{option} with nsid={nsid}, temp={temp}")
        return True

    def create_initial_temperature_set(
        self,
        nsid: int = 0,
        temp: float = 0.0,
    ) -> bool:
        """Create an INITIAL_TEMPERATURE_SET keyword.

        Parameters
        ----------
        nsid : int
            Node set ID (0 for all nodes).
        temp : float
            Initial temperature.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.InitialTemperatureSet()
        kw.nsid = nsid
        kw.temp = temp

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_TEMPERATURE_SET with nsid={nsid}, temp={temp}")
        return True

    def create_initial_velocity_node(
        self,
        nid: int,
        vx: float = 0.0,
        vy: float = 0.0,
        vz: float = 0.0,
    ) -> bool:
        """Create an INITIAL_VELOCITY_NODE keyword.

        Parameters
        ----------
        nid : int
            Node ID.
        vx, vy, vz : float
            Velocity components.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.InitialVelocityNode()
        kw.nid = nid
        kw.vx = vx
        kw.vy = vy
        kw.vz = vz

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_VELOCITY_NODE for nid={nid}")
        return True
