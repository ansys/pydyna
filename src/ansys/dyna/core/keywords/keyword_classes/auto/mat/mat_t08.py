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

"""Module providing the MatT08 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATT08_CARD0 = (
    FieldSchema("tmid", int, 0, 10, None),
    FieldSchema("tro", float, 10, 10, None),
    FieldSchema("tgrlc", int, 20, 10, None),
    FieldSchema("tgmult", float, 30, 10, None),
    FieldSchema("aopt", float, 40, 10, 0.0),
    FieldSchema("tlat", float, 50, 10, None),
    FieldSchema("hlat", float, 60, 10, None),
)

_MATT08_CARD1 = (
    FieldSchema("lcc", int, 0, 10, None),
    FieldSchema("lck1", int, 10, 10, None),
    FieldSchema("lck2", int, 20, 10, None),
    FieldSchema("lck3", int, 30, 10, None),
    FieldSchema("ilcchsv", int, 40, 10, None),
    FieldSchema("ilckhsv", float, 50, 10, None),
    FieldSchema("itghsv", int, 60, 10, None),
)

_MATT08_CARD2 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATT08_CARD3 = (
    FieldSchema("d1", float, 0, 10, None),
    FieldSchema("d2", float, 10, 10, None),
    FieldSchema("d3", float, 20, 10, None),
)

_MATT08_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatT08(KeywordBase):
    """DYNA MAT_T08 keyword"""

    keyword = "MAT"
    subkeyword = "T08"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "tgrlc": LinkType.DEFINE_CURVE,
        "lcc": LinkType.DEFINE_CURVE,
        "lck1": LinkType.DEFINE_CURVE,
        "lck2": LinkType.DEFINE_CURVE,
        "lck3": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatT08 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATT08_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATT08_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATT08_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATT08_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatT08.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATT08_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        """Set the tmid property."""
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:
        EQ.0.0: default to structural density
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        """Set the tro property."""
        self._cards[0].set_value("tro", value)

    @property
    def tgrlc(self) -> typing.Optional[int]:
        """Get or set the Thermal generation rate curve / table ID(see * DEFINE_‌CURVE) :
        GT.0 : Load curve giving thermal generation rate as a function of the mechanical history variable specified by ITGHSV.
        EQ.0 : Use mechanical history variable specified by ITGHSV times constant multiplier value TGMULT.
        LT.0 : Table of load curves for different temperatures.Each curve gives the thermal generation rate as a function of the mechanical history variable specified by ITGHSV
        """ # nopep8
        return self._cards[0].get_value("tgrlc")

    @tgrlc.setter
    def tgrlc(self, value: int) -> None:
        """Set the tgrlc property."""
        self._cards[0].set_value("tgrlc", value)

    @property
    def tgmult(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
        EQ.0.0: no heat generation
        """ # nopep8
        return self._cards[0].get_value("tgmult")

    @tgmult.setter
    def tgmult(self, value: float) -> None:
        """Set the tgmult property."""
        self._cards[0].set_value("tgmult", value)

    @property
    def aopt(self) -> float:
        """Get or set the Material axes definition:
        EQ.0.0: locally orthotropic with material axes by element nodes N1, N2 and N4,
        EQ.1.0: locally orthotropic with material axes determined by a point in space and global location of element center,
        EQ.2.0: globally orthotropic with material axes determined by vectors.
        EQ.3.0:	Locally orthotropic with first material axis orthogonal to element normal (defined by element nodes N1, N2 and N4) and to a vector d- Third material direction corresponds to element normal.
        EQ.4.0:	Local orthogonal in cylindrical coordinates with the material axes determined by a vector d,and an originating point, P, which define the centerline axis.
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0, None]:
            raise Exception("""aopt must be `None` or one of {0.0,1.0,2.0,3.0,4.0}.""")
        self._cards[0].set_value("aopt", value)

    @property
    def tlat(self) -> typing.Optional[float]:
        """Get or set the Phase change temperature
        """ # nopep8
        return self._cards[0].get_value("tlat")

    @tlat.setter
    def tlat(self, value: float) -> None:
        """Set the tlat property."""
        self._cards[0].set_value("tlat", value)

    @property
    def hlat(self) -> typing.Optional[float]:
        """Get or set the Latent heat
        """ # nopep8
        return self._cards[0].get_value("hlat")

    @hlat.setter
    def hlat(self, value: float) -> None:
        """Set the hlat property."""
        self._cards[0].set_value("hlat", value)

    @property
    def lcc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining specific heat as a function of temperature, or if |ILCCHSV| > 0:
        GT.0:	Load curve as function of mechanical history variable specified by ILCCHSV.
        LT.0 : Table of load curves for different temperatures.Each curve is a function of the mechanical history variable specified by ILCCHSV.
        """ # nopep8
        return self._cards[1].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        """Set the lcc property."""
        self._cards[1].set_value("lcc", value)

    @property
    def lck1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
        GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        """ # nopep8
        return self._cards[1].get_value("lck1")

    @lck1.setter
    def lck1(self, value: int) -> None:
        """Set the lck1 property."""
        self._cards[1].set_value("lck1", value)

    @property
    def lck2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
        GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        """ # nopep8
        return self._cards[1].get_value("lck2")

    @lck2.setter
    def lck2(self, value: int) -> None:
        """Set the lck2 property."""
        self._cards[1].set_value("lck2", value)

    @property
    def lck3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
        GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
        """ # nopep8
        return self._cards[1].get_value("lck3")

    @lck3.setter
    def lck3(self, value: int) -> None:
        """Set the lck3 property."""
        self._cards[1].set_value("lck3", value)

    @property
    def ilcchsv(self) -> typing.Optional[int]:
        """Get or set the Optional:
        GT.0.0:	Mechanical history variable # used by LCC
        LT.0.0:	As above but | ILCCHSV |= 1 - 6 points to the six stress components, | ILCCHSV |= 7 to plastic strain,and | ILCCHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("ilcchsv")

    @ilcchsv.setter
    def ilcchsv(self, value: int) -> None:
        """Set the ilcchsv property."""
        self._cards[1].set_value("ilcchsv", value)

    @property
    def ilckhsv(self) -> typing.Optional[float]:
        """Get or set the Optional:
        GT.0.0:	Mechanical history variable # used by LCK1, LCK2, LCK3
        LT.0.0:	As above but | ILCKHSV |= 1 - 6 points to the six stress components, | ILCKHSV |= 7 to plastic strain,and | ILCKHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("ilckhsv")

    @ilckhsv.setter
    def ilckhsv(self, value: float) -> None:
        """Set the ilckhsv property."""
        self._cards[1].set_value("ilckhsv", value)

    @property
    def itghsv(self) -> typing.Optional[int]:
        """Get or set the Optional:
        GT.0.0:	Mechanical history variable # used by TGRLC
        LT.0.0:	As above but | ITGHSV |= 1 - 6 points to the six stress components, | ITGHSV |= 7 to plastic strain,and | ITGHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("itghsv")

    @itghsv.setter
    def itghsv(self, value: int) -> None:
        """Set the itghsv property."""
        self._cards[1].set_value("itghsv", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2,3 and 4
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2,3 nd 4
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2,3 and 4
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[3].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def tgrlc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tgrlc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tgrlc:
                return kwd
        return None

    @tgrlc_link.setter
    def tgrlc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tgrlc."""
        self.tgrlc = value.lcid

    @property
    def lcc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcc:
                return kwd
        return None

    @lcc_link.setter
    def lcc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcc."""
        self.lcc = value.lcid

    @property
    def lck1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lck1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lck1:
                return kwd
        return None

    @lck1_link.setter
    def lck1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lck1."""
        self.lck1 = value.lcid

    @property
    def lck2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lck2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lck2:
                return kwd
        return None

    @lck2_link.setter
    def lck2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lck2."""
        self.lck2 = value.lcid

    @property
    def lck3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lck3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lck3:
                return kwd
        return None

    @lck3_link.setter
    def lck3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lck3."""
        self.lck3 = value.lcid

