# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""Module providing the IcfdControlTurbulence class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLTURBULENCE_CARD0 = (
    FieldSchema("tmod", int, 0, 10, 0),
    FieldSchema("submod", int, 10, 10, 1),
    FieldSchema("wlaw", int, 20, 10, 1),
    FieldSchema("ks", float, 30, 10, 0.0),
    FieldSchema("cs", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("twlaw", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_ICFDCONTROLTURBULENCE_CARD1 = (
    FieldSchema("cs", float, 0, 10, 0.18),
)

class IcfdControlTurbulence(KeywordBase):
    """DYNA ICFD_CONTROL_TURBULENCE keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TURBULENCE"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlTurbulence class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD1,
                active_func=lambda: self.tmod==2 or self.tmod==3,
                **kwargs,
            ),
        ]
    @property
    def tmod(self) -> int:
        """Get or set the Indicates which turbulence model to use.
        EQ.0: Turbulence model based on a variational multiscale approach default.
        EQ.1: RANS k-epsilon approach.
        EQ.2: LES Smagorinsky sub-grid scale model.
        EQ.3: LES Wall adapting local eddy-viscosity (WALE) model.
        EQ.4: RANS k-omega approach.
        EQ.5: RANS Spalart Allmaras approach.
        """ # nopep8
        return self._cards[0].get_value("tmod")

    @tmod.setter
    def tmod(self, value: int) -> None:
        """Set the tmod property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""tmod must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("tmod", value)

    @property
    def submod(self) -> int:
        """Get or set the Turbulence submodel.For RANS k-epsilon approach (TMOD = 1):
        EQ.1:Standard model.
        EQ.2:Realizable model.
        For LES Smagorinsky or dynamic sub-grid model (TMOD = 2):
        EQ.1: Smagorinsky model(see Remark 6)
        EQ.2: Dynamic model(see Remark 7).
        For RANS k-omega approach (TMOD = 4):
        EQ.1:Standard Wilcox 98 model.
        EQ.2:Standard Wilcox 06 model.
        EQ.3:SST Menter 2003.
        """ # nopep8
        return self._cards[0].get_value("submod")

    @submod.setter
    def submod(self, value: int) -> None:
        """Set the submod property."""
        self._cards[0].set_value("submod", value)

    @property
    def wlaw(self) -> int:
        """Get or set the Law of the wall ID if a RANS turbulence model is selected (see Remark 4):
        EQ.1: Standard classic law of the wall(default for TMOD = 1) with linear blending between the logand linear regions.
        EQ.2: Standard Launder and Spalding law of the wall
        EQ.4: Nonequilibrium Launder and Spalding law of the wall
        EQ.5: Automatic classic law of the wall.
        """ # nopep8
        return self._cards[0].get_value("wlaw")

    @wlaw.setter
    def wlaw(self, value: int) -> None:
        """Set the wlaw property."""
        if value not in [1, 2, 4, 5, None]:
            raise Exception("""wlaw must be `None` or one of {1,2,4,5}.""")
        self._cards[0].set_value("wlaw", value)

    @property
    def ks(self) -> float:
        """Get or set the Roughness physical height, only used for RANS turbulence models.
        """ # nopep8
        return self._cards[0].get_value("ks")

    @ks.setter
    def ks(self, value: float) -> None:
        """Set the ks property."""
        self._cards[0].set_value("ks", value)

    @property
    def cs(self) -> float:
        """Get or set the Roughness constant, only used for RANS turbulence models.
        """ # nopep8
        return self._cards[0].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[0].set_value("cs", value)

    @property
    def twlaw(self) -> typing.Optional[int]:
        """Get or set the Thermal law of the wall flag (see Remark 8):
        EQ.0: No thermal law of the wall activated.
        EQ.1: Thermal law of the wall.
        """ # nopep8
        return self._cards[0].get_value("twlaw")

    @twlaw.setter
    def twlaw(self, value: int) -> None:
        """Set the twlaw property."""
        self._cards[0].set_value("twlaw", value)

    @property
    def cs(self) -> float:
        """Get or set the Smagorinsky constant if TMOD = 2 and SUBM = 1 or WALE constant if TMOD = 3
        """ # nopep8
        return self._cards[1].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[1].set_value("cs", value)

