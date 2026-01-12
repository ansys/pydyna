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

"""Module providing the LoadSsa class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADSSA_CARD0 = (
    FieldSchema("vs", float, 0, 10, None),
    FieldSchema("ds", float, 10, 10, None),
    FieldSchema("refl", float, 20, 10, 0.0),
    FieldSchema("zb", float, 30, 10, 0.0),
    FieldSchema("zsurf", float, 40, 10, 0.0),
    FieldSchema("fpsid", int, 50, 10, 0),
    FieldSchema("psid", int, 60, 10, 0),
)

_LOADSSA_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("alpha", float, 10, 10, None),
    FieldSchema("gamma", float, 20, 10, None),
    FieldSchema("ktheta", float, 30, 10, None),
    FieldSchema("kappa", float, 40, 10, None),
)

_LOADSSA_CARD2 = (
    FieldSchema("xs", float, 0, 10, None),
    FieldSchema("ys", float, 10, 10, None),
    FieldSchema("zs", float, 20, 10, None),
    FieldSchema("w", float, 30, 10, None),
    FieldSchema("tdely", float, 40, 10, None),
    FieldSchema("rad", float, 50, 10, None),
    FieldSchema("cz", float, 60, 10, None),
)

class LoadSsa(KeywordBase):
    """DYNA LOAD_SSA keyword"""

    keyword = "LOAD"
    subkeyword = "SSA"

    def __init__(self, **kwargs):
        """Initialize the LoadSsa class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSSA_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSSA_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSSA_CARD2,
                **kwargs,
            ),        ]
    @property
    def vs(self) -> typing.Optional[float]:
        """Get or set the Sound speed in fluid.
        """ # nopep8
        return self._cards[0].get_value("vs")

    @vs.setter
    def vs(self, value: float) -> None:
        """Set the vs property."""
        self._cards[0].set_value("vs", value)

    @property
    def ds(self) -> typing.Optional[float]:
        """Get or set the Density of fluid.
        """ # nopep8
        return self._cards[0].get_value("ds")

    @ds.setter
    def ds(self, value: float) -> None:
        """Set the ds property."""
        self._cards[0].set_value("ds", value)

    @property
    def refl(self) -> float:
        """Get or set the Consider reflections from sea floor:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("refl")

    @refl.setter
    def refl(self, value: float) -> None:
        """Set the refl property."""
        if value not in [0, 1, None]:
            raise Exception("""refl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("refl", value)

    @property
    def zb(self) -> float:
        """Get or set the z-coordinate of sea floor. Define only if REFL=1.
        """ # nopep8
        return self._cards[0].get_value("zb")

    @zb.setter
    def zb(self, value: float) -> None:
        """Set the zb property."""
        self._cards[0].set_value("zb", value)

    @property
    def zsurf(self) -> float:
        """Get or set the z-coordinate of sea surface.
        """ # nopep8
        return self._cards[0].get_value("zsurf")

    @zsurf.setter
    def zsurf(self, value: float) -> None:
        """Set the zsurf property."""
        self._cards[0].set_value("zsurf", value)

    @property
    def fpsid(self) -> int:
        """Get or set the Part set ID of parts subject to flood control. Use the *PART_SET_COLUMN option where the parameters A1 and A2 must be defined as follows:
        Parameter A1: Flooding status:
        EQ.1.0: Fluid on both sides,
        EQ.2.0: Fluid outside, air inside,
        EQ.3.0: Air outside, fluid inside,
        EQ.4.0: Material or part is ignored.
        Parameter A2:
        Tubular outer diameter of beam elements. For shell elements this input must be greater than zero for loading.
        """ # nopep8
        return self._cards[0].get_value("fpsid")

    @fpsid.setter
    def fpsid(self, value: int) -> None:
        """Set the fpsid property."""
        self._cards[0].set_value("fpsid", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID of parts defining the wet surface. The elements defining these parts must have their outward normals pointing into the fluid.
        EQ.0: all parts are included,
        GT.0: define n part ID's in the *SET_PART_COLUMN keyword.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Shock pressure parameter.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Shock pressure parameter.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Time constant parameter.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[1].set_value("gamma", value)

    @property
    def ktheta(self) -> typing.Optional[float]:
        """Get or set the Time constant parameter.
        """ # nopep8
        return self._cards[1].get_value("ktheta")

    @ktheta.setter
    def ktheta(self, value: float) -> None:
        """Set the ktheta property."""
        self._cards[1].set_value("ktheta", value)

    @property
    def kappa(self) -> typing.Optional[float]:
        """Get or set the Ratio of specific heat capacities.
        """ # nopep8
        return self._cards[1].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        """Set the kappa property."""
        self._cards[1].set_value("kappa", value)

    @property
    def xs(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("xs")

    @xs.setter
    def xs(self, value: float) -> None:
        """Set the xs property."""
        self._cards[2].set_value("xs", value)

    @property
    def ys(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        """Set the ys property."""
        self._cards[2].set_value("ys", value)

    @property
    def zs(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("zs")

    @zs.setter
    def zs(self, value: float) -> None:
        """Set the zs property."""
        self._cards[2].set_value("zs", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Weight of charge.
        """ # nopep8
        return self._cards[2].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[2].set_value("w", value)

    @property
    def tdely(self) -> typing.Optional[float]:
        """Get or set the Time delay before charge detonates.
        """ # nopep8
        return self._cards[2].get_value("tdely")

    @tdely.setter
    def tdely(self, value: float) -> None:
        """Set the tdely property."""
        self._cards[2].set_value("tdely", value)

    @property
    def rad(self) -> typing.Optional[float]:
        """Get or set the Charge radius.
        """ # nopep8
        return self._cards[2].get_value("rad")

    @rad.setter
    def rad(self, value: float) -> None:
        """Set the rad property."""
        self._cards[2].set_value("rad", value)

    @property
    def cz(self) -> typing.Optional[float]:
        """Get or set the Water depth.
        """ # nopep8
        return self._cards[2].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        """Set the cz property."""
        self._cards[2].set_value("cz", value)

