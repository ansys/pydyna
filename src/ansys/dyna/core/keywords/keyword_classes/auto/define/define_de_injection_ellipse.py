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

"""Module providing the DefineDeInjectionEllipse class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINEDEINJECTIONELLIPSE_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("xc", float, 20, 10, 0.0),
    FieldSchema("yc", float, 30, 10, 0.0),
    FieldSchema("zc", float, 40, 10, 0.0),
    FieldSchema("xl", float, 50, 10, 0.0),
    FieldSchema("yl", float, 60, 10, 0.0),
    FieldSchema("cid", int, 70, 10, 0),
)

_DEFINEDEINJECTIONELLIPSE_CARD1 = (
    FieldSchema("rmass", float, 0, 10, None),
    FieldSchema("rmin", float, 10, 10, None),
    FieldSchema("rmax", float, 20, 10, None),
    FieldSchema("vx", float, 30, 10, 0.0),
    FieldSchema("vy", float, 40, 10, 0.0),
    FieldSchema("vz", float, 50, 10, 0.0),
    FieldSchema("tbeg", float, 60, 10, 0.0),
    FieldSchema("tend", float, 70, 10, 1e+20),
)

_DEFINEDEINJECTIONELLIPSE_CARD2 = (
    FieldSchema("ifunc", int, 0, 10, 0),
    FieldSchema("nid", int, 10, 10, None),
    FieldSchema("imulti", int, 20, 10, None),
    FieldSchema("lcvx", int, 30, 10, None),
    FieldSchema("lcvy", int, 40, 10, None),
    FieldSchema("lcvz", int, 50, 10, None),
)

_DEFINEDEINJECTIONELLIPSE_CARD3 = (
    FieldSchema("r1", float, 0, 10, 0.0),
    FieldSchema("p1", float, 10, 10, 0.0),
    FieldSchema("r2", float, 20, 10, 0.0),
    FieldSchema("p2", float, 30, 10, 0.0),
    FieldSchema("r3", float, 40, 10, 0.0),
    FieldSchema("p3", float, 50, 10, 0.0),
    FieldSchema("r4", float, 60, 10, 0.0),
    FieldSchema("p4", float, 70, 10, 0.0),
)

_DEFINEDEINJECTIONELLIPSE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeInjectionEllipse(KeywordBase):
    """DYNA DEFINE_DE_INJECTION_ELLIPSE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECTION_ELLIPSE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcvx": LinkType.DEFINE_CURVE,
        "lcvy": LinkType.DEFINE_CURVE,
        "lcvz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineDeInjectionEllipse class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTIONELLIPSE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTIONELLIPSE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTIONELLIPSE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTIONELLIPSE_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeInjectionEllipse.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEINJECTIONELLIPSE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def xc(self) -> float:
        """Get or set the X coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def xl(self) -> float:
        """Get or set the Length of the rectangular injection plane along X-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Length of the rectangular injection plane along Y-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[0].set_value("yl", value)

    @property
    def cid(self) -> int:
        """Get or set the Optional local coordinate system ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def rmass(self) -> typing.Optional[float]:
        """Get or set the Mass flow rate
        """ # nopep8
        return self._cards[1].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        """Set the rmass property."""
        self._cards[1].set_value("rmass", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the Minimum DES radius (ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        """Set the rmin property."""
        self._cards[1].set_value("rmin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum DES radius.(ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[1].set_value("rmax", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Death time.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def ifunc(self) -> int:
        """Get or set the Distribution of particle radii (ignored if IMULTI > 1):
        EQ.0: Uniform distribution(Default)
        EQ.1: Gaussian distribution (see Remarks)
        """ # nopep8
        return self._cards[2].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        """Set the ifunc property."""
        if value not in [0, 1, None]:
            raise Exception("""ifunc must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ifunc", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the An optional node ID. If defined, the center of injection plane follows the motion of this node
        """ # nopep8
        return self._cards[2].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[2].set_value("nid", value)

    @property
    def imulti(self) -> typing.Optional[int]:
        """Get or set the Flag for giving a specified mass distribution of injected particles with given radii:
        EQ.1:	Inject the particles with distribution IFUNC using the radii specified with RMINand RMAX(default).
        GT.1 : Inject particles with IMULTI different radii, Ri, with each different size having a specified mass distribution, Pi, given in Card 3.1.IMULTI cannot be greater than 4.
        """ # nopep8
        return self._cards[2].get_value("imulti")

    @imulti.setter
    def imulti(self, value: int) -> None:
        """Set the imulti property."""
        self._cards[2].set_value("imulti", value)

    @property
    def lcvx(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in x-direction
        """ # nopep8
        return self._cards[2].get_value("lcvx")

    @lcvx.setter
    def lcvx(self, value: int) -> None:
        """Set the lcvx property."""
        self._cards[2].set_value("lcvx", value)

    @property
    def lcvy(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in y-direction
        """ # nopep8
        return self._cards[2].get_value("lcvy")

    @lcvy.setter
    def lcvy(self, value: int) -> None:
        """Set the lcvy property."""
        self._cards[2].set_value("lcvy", value)

    @property
    def lcvz(self) -> typing.Optional[int]:
        """Get or set the Load curve defines initial injection velocity in z-direction
        """ # nopep8
        return self._cards[2].get_value("lcvz")

    @lcvz.setter
    def lcvz(self, value: int) -> None:
        """Set the lcvz property."""
        self._cards[2].set_value("lcvz", value)

    @property
    def r1(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[3].set_value("r1", value)

    @property
    def p1(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[3].set_value("p1", value)

    @property
    def r2(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[3].set_value("r2", value)

    @property
    def p2(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[3].set_value("p2", value)

    @property
    def r3(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        """Set the r3 property."""
        self._cards[3].set_value("r3", value)

    @property
    def p3(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[3].set_value("p3", value)

    @property
    def r4(self) -> float:
        """Get or set the Injected particle radius.IMULTI radii may be specified
        """ # nopep8
        return self._cards[3].get_value("r4")

    @r4.setter
    def r4(self, value: float) -> None:
        """Set the r4 property."""
        self._cards[3].set_value("r4", value)

    @property
    def p4(self) -> float:
        """Get or set the The mass percentage of injected particle with radius Ri
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[3].set_value("p4", value)

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
    def lcvx_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvx:
                return kwd
        return None

    @lcvx_link.setter
    def lcvx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvx."""
        self.lcvx = value.lcid

    @property
    def lcvy_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvy:
                return kwd
        return None

    @lcvy_link.setter
    def lcvy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvy."""
        self.lcvy = value.lcid

    @property
    def lcvz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvz:
                return kwd
        return None

    @lcvz_link.setter
    def lcvz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvz."""
        self.lcvz = value.lcid

