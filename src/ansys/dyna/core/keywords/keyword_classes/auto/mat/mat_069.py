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

"""Module providing the Mat069 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT069_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("st", float, 20, 10, None),
    FieldSchema("d", float, 30, 10, None),
    FieldSchema("r", float, 40, 10, None),
    FieldSchema("h", float, 50, 10, None),
    FieldSchema("k", float, 60, 10, None),
    FieldSchema("c", float, 70, 10, None),
)

_MAT069_CARD1 = (
    FieldSchema("c3", float, 0, 10, None),
    FieldSchema("stf", float, 10, 10, None),
    FieldSchema("rhof", float, 20, 10, None),
    FieldSchema("c1", float, 30, 10, None),
    FieldSchema("c2", float, 40, 10, None),
    FieldSchema("lcidf", float, 50, 10, 0.0),
    FieldSchema("lcidd", float, 60, 10, 0.0),
    FieldSchema("s0", float, 70, 10, None),
)

_MAT069_CARD2 = (
    FieldSchema("orfloc", float, 0, 10, None),
    FieldSchema("orfrad", float, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("dc", float, 30, 10, None),
)

_MAT069_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat069(KeywordBase):
    """DYNA MAT_069 keyword"""

    keyword = "MAT"
    subkeyword = "069"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcidf": LinkType.DEFINE_CURVE,
        "lcidd": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat069 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT069_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT069_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT069_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat069.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT069_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume on *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Piston stroke. St must equal or exceed the length of the beam element.
        """ # nopep8
        return self._cards[0].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        """Set the st property."""
        self._cards[0].set_value("st", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Piston diameter.
        """ # nopep8
        return self._cards[0].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[0].set_value("d", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Default orifice radius.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Orifice controller position.
        """ # nopep8
        return self._cards[0].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[0].set_value("h", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Damping constant.
        LT.0.0: |K| is the load curve number ID, see *DEFINE_CURVE, defining the damping coefficient as a function of the absolute value of the relative velocity.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Coefficient for fluid inertia term.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[1].set_value("c3", value)

    @property
    def stf(self) -> typing.Optional[float]:
        """Get or set the k, stiffness coefficient if piston bottoms out.
        """ # nopep8
        return self._cards[1].get_value("stf")

    @stf.setter
    def stf(self, value: float) -> None:
        """Set the stf property."""
        self._cards[1].set_value("stf", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[1].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        """Set the rhof property."""
        self._cards[1].set_value("rhof", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the C1, coefficient for linear velocity term.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the C2, coefficient for quadratic velocity term.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def lcidf(self) -> float:
        """Get or set the Load curve number ID defining force versus piston displacement. Compressive behavior is defined in the positive quadrant of the force displacement curve. Displacements falling outside of the defined force displacement curve are extrapolated. Care must be taken to ensure that extrapolated values are reasonable.
        """ # nopep8
        return self._cards[1].get_value("lcidf")

    @lcidf.setter
    def lcidf(self, value: float) -> None:
        """Set the lcidf property."""
        self._cards[1].set_value("lcidf", value)

    @property
    def lcidd(self) -> float:
        """Get or set the Load curve number ID defining damping coefficient versus piston displacement. Displacements falling outside the defined curve are extrapolated. Care must be taken to ensure that extrapolated values are reasonable.
        """ # nopep8
        return self._cards[1].get_value("lcidd")

    @lcidd.setter
    def lcidd(self, value: float) -> None:
        """Set the lcidd property."""
        self._cards[1].set_value("lcidd", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the Initial displacement s0 , typically set to zero. A positive displacement corresponds to compressive behavior.
        """ # nopep8
        return self._cards[1].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        """Set the s0 property."""
        self._cards[1].set_value("s0", value)

    @property
    def orfloc(self) -> typing.Optional[float]:
        """Get or set the Orifice location of i'th orifice relative to the fixed end.
        """ # nopep8
        return self._cards[2].get_value("orfloc")

    @orfloc.setter
    def orfloc(self, value: float) -> None:
        """Set the orfloc property."""
        self._cards[2].set_value("orfloc", value)

    @property
    def orfrad(self) -> typing.Optional[float]:
        """Get or set the Orifice radius of i'th orifice, if zero the default radius is used.
        """ # nopep8
        return self._cards[2].get_value("orfrad")

    @orfrad.setter
    def orfrad(self, value: float) -> None:
        """Set the orfrad property."""
        self._cards[2].set_value("orfrad", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor on calculated force. (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[2].set_value("sf", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Linear viscous damping coefficient used after damper bottoms out either in tension or compression.
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[2].set_value("dc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcidf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidf:
                return kwd
        return None

    @lcidf_link.setter
    def lcidf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidf."""
        self.lcidf = value.lcid

    @property
    def lcidd_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidd."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidd:
                return kwd
        return None

    @lcidd_link.setter
    def lcidd_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidd."""
        self.lcidd = value.lcid

