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

"""Module providing the IcfdControlLevelset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLLEVELSET_CARD0 = (
    FieldSchema("lsrst", int, 0, 10, 20),
    FieldSchema("lsinl", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("lsmth", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("sgc", int, 60, 10, 0),
    FieldSchema("ast", int, 70, 10, 0),
)

_ICFDCONTROLLEVELSET_CARD1 = (
    FieldSchema("srl", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("lshref", float, 50, 10, 0.0),
    FieldSchema("lsw", int, 60, 10, 0),
    FieldSchema("unused", int, 70, 10, None),
)

class IcfdControlLevelset(KeywordBase):
    """DYNA ICFD_CONTROL_LEVELSET keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_LEVELSET"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlLevelset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLLEVELSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLLEVELSET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def lsrst(self) -> int:
        """Get or set the Frequency the level set distance function is re-initialized. The default value is every 20 time steps.
        """ # nopep8
        return self._cards[0].get_value("lsrst")

    @lsrst.setter
    def lsrst(self, value: int) -> None:
        """Set the lsrst property."""
        self._cards[0].set_value("lsrst", value)

    @property
    def lsinl(self) -> int:
        """Get or set the Set the level set to be positive at a velocity inlet:
        EQ.0: Let the level set algorithm compute the value(default).
        EQ.1: Force a positive level set value at the inlet.
        """ # nopep8
        return self._cards[0].get_value("lsinl")

    @lsinl.setter
    def lsinl(self, value: int) -> None:
        """Set the lsinl property."""
        if value not in [0, 1, None]:
            raise Exception("""lsinl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("lsinl", value)

    @property
    def lsmth(self) -> float:
        """Get or set the Scale factor for level set smoothness.
        GT.0: Add smoothness to the free surface.Small values like 0.1 are reasonable.The optimal value could be problem dependent.
        """ # nopep8
        return self._cards[0].get_value("lsmth")

    @lsmth.setter
    def lsmth(self, value: float) -> None:
        """Set the lsmth property."""
        self._cards[0].set_value("lsmth", value)

    @property
    def sgc(self) -> int:
        """Get or set the Smoother approximation of gradient and curvature:
        EQ.0: No smoothing added.
        EQ.1: Linear gradient and curvature.
        EQ.-1: Least-squares based gradient and curvature.
        EQ.2: Least - squares gradient smoothing.
        """ # nopep8
        return self._cards[0].get_value("sgc")

    @sgc.setter
    def sgc(self, value: int) -> None:
        """Set the sgc property."""
        if value not in [0, 1, -1, 2, None]:
            raise Exception("""sgc must be `None` or one of {0,1,-1,2}.""")
        self._cards[0].set_value("sgc", value)

    @property
    def ast(self) -> int:
        """Get or set the Advanced formulation for surface tension:
        EQ.0: Smoothed Dirac.
        EQ.1: Smoothed Heaviside.
        EQ.2: Laplace - Beltrami
        """ # nopep8
        return self._cards[0].get_value("ast")

    @ast.setter
    def ast(self, value: int) -> None:
        """Set the ast property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ast must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ast", value)

    @property
    def srl(self) -> int:
        """Get or set the Smoother reinitialization method for the level set:
        EQ.0: Standard, geometric based.
        EQ.1: Linear closest point algorithm.
        EQ.2: Higher-order closest point algorithm.
        """ # nopep8
        return self._cards[1].get_value("srl")

    @srl.setter
    def srl(self, value: int) -> None:
        """Set the srl property."""
        if value not in [0, 1, None]:
            raise Exception("""srl must be `None` or one of {0,1}.""")
        self._cards[1].set_value("srl", value)

    @property
    def lshref(self) -> float:
        """Get or set the Reference size for the narrow-band approach. Level-set transport is done in a standard way up to distances to interfaces that are smaller than 4 times LSHREF (Peng et al. [1999]). This field is relevant only if semi-Lagrangian advection of the level set is enabled. See SLLS on *ICFD_CONTROL_ADVECTION.
        EQ.0.0:	Disable narrow - band approach.
        """ # nopep8
        return self._cards[1].get_value("lshref")

    @lshref.setter
    def lshref(self, value: float) -> None:
        """Set the lshref property."""
        self._cards[1].set_value("lshref", value)

    @property
    def lsw(self) -> int:
        """Get or set the Weighted Essentially Non-Oscillatory scheme for advection and reinitialization of the level set. This field is relevant only if higher-order advection (see SLLS = 2 on *ICFD_CONTROL_ADVECTION) and higher-order reinitialization (SRL = 2)  of the level set are enabled.
        EQ.0:	Off
        EQ.1 : On
        """ # nopep8
        return self._cards[1].get_value("lsw")

    @lsw.setter
    def lsw(self, value: int) -> None:
        """Set the lsw property."""
        if value not in [0, 1, None]:
            raise Exception("""lsw must be `None` or one of {0,1}.""")
        self._cards[1].set_value("lsw", value)

