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

"""Module providing the DefineDeToSurfaceCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINEDETOSURFACECOUPLING_CARD0 = (
    FieldSchema("desid", int, 0, 10, 0),
    FieldSchema("surfid", int, 10, 10, 0),
    FieldSchema("destyp", int, 20, 10, 0),
    FieldSchema("surftyp", int, 30, 10, 0),
    FieldSchema("isoft", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("sbox", int, 70, 10, None),
)

_DEFINEDETOSURFACECOUPLING_CARD1 = (
    FieldSchema("frics", float, 0, 10, None),
    FieldSchema("fricd", float, 10, 10, None),
    FieldSchema("damp", float, 20, 10, None),
    FieldSchema("bsort", int, 30, 10, 100),
    FieldSchema("lcvx", int, 40, 10, 0),
    FieldSchema("lcvy", int, 50, 10, 0),
    FieldSchema("lcvz", int, 60, 10, 0),
    FieldSchema("wearc", float, 70, 10, 0.0),
)

_DEFINEDETOSURFACECOUPLING_CARD2 = (
    FieldSchema("w1", float, 0, 10, None),
    FieldSchema("w2", float, 10, 10, None),
    FieldSchema("w3", float, 20, 10, None),
    FieldSchema("w4", float, 30, 10, None),
    FieldSchema("w5", float, 40, 10, None),
    FieldSchema("w6", float, 50, 10, None),
    FieldSchema("w7", float, 60, 10, None),
    FieldSchema("w8", float, 70, 10, None),
)

_DEFINEDETOSURFACECOUPLING_CARD3 = (
    FieldSchema("sfp", float, 0, 10, 1.0),
    FieldSchema("sft", float, 10, 10, 1.0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("ideact", int, 40, 10, None),
    FieldSchema("cid_rcf", int, 50, 10, 0),
    FieldSchema("bt", float, 60, 10, 0.0),
    FieldSchema("dt", float, 70, 10, 1e+20),
)

_DEFINEDETOSURFACECOUPLING_CARD4 = (
    FieldSchema("ht_trsf", int, 0, 10, 0),
    FieldSchema("th_cnd", float, 10, 10, None),
    FieldSchema("surf_ht", float, 20, 10, None),
)

_DEFINEDETOSURFACECOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_DE_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_SURFACE_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcvx": LinkType.DEFINE_CURVE,
        "lcvy": LinkType.DEFINE_CURVE,
        "lcvz": LinkType.DEFINE_CURVE,
        "sbox": LinkType.DEFINE_BOX,
        "cid_rcf": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineDeToSurfaceCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD4,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeToSurfaceCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDETOSURFACECOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def desid(self) -> int:
        """Get or set the Node set ID, node ID, part set ID or part ID specifying the DES in the coupling. DESTYP below indicates the ID type
        """ # nopep8
        return self._cards[0].get_value("desid")

    @desid.setter
    def desid(self, value: int) -> None:
        """Set the desid property."""
        self._cards[0].set_value("desid", value)

    @property
    def surfid(self) -> int:
        """Get or set the Part set ID or part ID specifying the surface. SURFID below indicates the ID type.
        """ # nopep8
        return self._cards[0].get_value("surfid")

    @surfid.setter
    def surfid(self, value: int) -> None:
        """Set the surfid property."""
        self._cards[0].set_value("surfid", value)

    @property
    def destyp(self) -> int:
        """Get or set the Type for DESID:
        EQ.0: Node set
        EQ.1: Node
        EQ.2: Part setn
        EQ.3: Part
        """ # nopep8
        return self._cards[0].get_value("destyp")

    @destyp.setter
    def destyp(self, value: int) -> None:
        """Set the destyp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""destyp must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("destyp", value)

    @property
    def surftyp(self) -> int:
        """Get or set the SURFID type:
        EQ.0: Part set
        EQ.1: Part
        EQ.2: Segment set (TRANSDUCER keyword option only; see Remark 5)
        """ # nopep8
        return self._cards[0].get_value("surftyp")

    @surftyp.setter
    def surftyp(self, value: int) -> None:
        """Set the surftyp property."""
        if value not in [0, 1, None]:
            raise Exception("""surftyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("surftyp", value)

    @property
    def isoft(self) -> int:
        """Get or set the Contact stiffness evaluation:
        EQ.0: Default is based on DES properties only
        EQ.1: Geometric mean is based on both DES and contact segment's properties.
        EQ.2: Soft option is based on mass of the system and time step size.
        """ # nopep8
        return self._cards[0].get_value("isoft")

    @isoft.setter
    def isoft(self, value: int) -> None:
        """Set the isoft property."""
        self._cards[0].set_value("isoft", value)

    @property
    def sbox(self) -> typing.Optional[int]:
        """Get or set the BOX ID.Exclude segments belonging to SURFID that are outside of the box during initialization.
        """ # nopep8
        return self._cards[0].get_value("sbox")

    @sbox.setter
    def sbox(self, value: int) -> None:
        """Set the sbox property."""
        self._cards[0].set_value("sbox", value)

    @property
    def frics(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient
        """ # nopep8
        return self._cards[1].get_value("frics")

    @frics.setter
    def frics(self, value: float) -> None:
        """Set the frics property."""
        self._cards[1].set_value("frics", value)

    @property
    def fricd(self) -> typing.Optional[float]:
        """Get or set the Rolling friction coefficient
        """ # nopep8
        return self._cards[1].get_value("fricd")

    @fricd.setter
    def fricd(self, value: float) -> None:
        """Set the fricd property."""
        self._cards[1].set_value("fricd", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def bsort(self) -> int:
        """Get or set the Number of cycles between bucket sort. Number of cycles between bucket sorts; the default value is 100. For blast simulations with very high DEM particle velocity, we suggest setting BSORT = 20 or smaller.
        LT.0: | BSORT | is the minimum number of cycles between bucket sorts.This value can be increased during runtime by tracking the velocity of potential coupling pairs.This feature only works with MPP currently.
        """ # nopep8
        return self._cards[1].get_value("bsort")

    @bsort.setter
    def bsort(self, value: int) -> None:
        """Set the bsort property."""
        self._cards[1].set_value("bsort", value)

    @property
    def lcvx(self) -> int:
        """Get or set the Load curve defining surface velocity in X direction.
        """ # nopep8
        return self._cards[1].get_value("lcvx")

    @lcvx.setter
    def lcvx(self, value: int) -> None:
        """Set the lcvx property."""
        self._cards[1].set_value("lcvx", value)

    @property
    def lcvy(self) -> int:
        """Get or set the Load curve defining surface velocity in Y direction.
        """ # nopep8
        return self._cards[1].get_value("lcvy")

    @lcvy.setter
    def lcvy(self, value: int) -> None:
        """Set the lcvy property."""
        self._cards[1].set_value("lcvy", value)

    @property
    def lcvz(self) -> int:
        """Get or set the Load curve defining surface velocity in Z direction.
        """ # nopep8
        return self._cards[1].get_value("lcvz")

    @lcvz.setter
    def lcvz(self, value: int) -> None:
        """Set the lcvz property."""
        self._cards[1].set_value("lcvz", value)

    @property
    def wearc(self) -> float:
        """Get or set the WEARC is the wear coefficient.
        GT.0: Archard's Wear Law; see Remark 1.
        EQ. - 1: Finnie Wear Law; an additional card is required
        LE. - 100: User - defined wear model; an additional card is required.
        """ # nopep8
        return self._cards[1].get_value("wearc")

    @wearc.setter
    def wearc(self, value: float) -> None:
        """Set the wearc property."""
        self._cards[1].set_value("wearc", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[2].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[2].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[2].set_value("w3", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[2].set_value("w4", value)

    @property
    def w5(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[2].set_value("w5", value)

    @property
    def w6(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[2].set_value("w6", value)

    @property
    def w7(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[2].set_value("w7", value)

    @property
    def w8(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[2].set_value("w8", value)

    @property
    def sfp(self) -> float:
        """Get or set the Scale factor on contact stiffness.
        """ # nopep8
        return self._cards[3].get_value("sfp")

    @sfp.setter
    def sfp(self, value: float) -> None:
        """Set the sfp property."""
        self._cards[3].set_value("sfp", value)

    @property
    def sft(self) -> float:
        """Get or set the Scale factor for surface thickness (scales true thickness). True thickness is the element thickness of the shell elements. This option applies only to contact with shell elements.
        """ # nopep8
        return self._cards[3].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[3].set_value("sft", value)

    @property
    def ideact(self) -> typing.Optional[int]:
        """Get or set the DES particles will be automatically deactivated after contacting with surface when IDEACT = 1.
        """ # nopep8
        return self._cards[3].get_value("ideact")

    @ideact.setter
    def ideact(self, value: int) -> None:
        """Set the ideact property."""
        self._cards[3].set_value("ideact", value)

    @property
    def cid_rcf(self) -> int:
        """Get or set the Coordinate system ID to output demrcf force resultants in a local system.
        """ # nopep8
        return self._cards[3].get_value("cid_rcf")

    @cid_rcf.setter
    def cid_rcf(self, value: int) -> None:
        """Set the cid_rcf property."""
        self._cards[3].set_value("cid_rcf", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time
        """ # nopep8
        return self._cards[3].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[3].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time
        """ # nopep8
        return self._cards[3].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[3].set_value("dt", value)

    @property
    def ht_trsf(self) -> int:
        """Get or set the Flag to enable heat transfer between DES and the surface:
        EQ.0:	No heat transfer between DES and the surface
        EQ.1 : Consider heat transfer between DES and the surface
        """ # nopep8
        return self._cards[4].get_value("ht_trsf")

    @ht_trsf.setter
    def ht_trsf(self, value: int) -> None:
        """Set the ht_trsf property."""
        if value not in [0, 1, None]:
            raise Exception("""ht_trsf must be `None` or one of {0,1}.""")
        self._cards[4].set_value("ht_trsf", value)

    @property
    def th_cnd(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity between DES and the surface in W/(m K).
        """ # nopep8
        return self._cards[4].get_value("th_cnd")

    @th_cnd.setter
    def th_cnd(self, value: float) -> None:
        """Set the th_cnd property."""
        self._cards[4].set_value("th_cnd", value)

    @property
    def surf_ht(self) -> typing.Optional[float]:
        """Get or set the Constant surface temperature in K or �C
        """ # nopep8
        return self._cards[4].get_value("surf_ht")

    @surf_ht.setter
    def surf_ht(self, value: float) -> None:
        """Set the surf_ht property."""
        self._cards[4].set_value("surf_ht", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcvx_link(self) -> typing.Optional[DefineCurve]:
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
    def lcvy_link(self) -> typing.Optional[DefineCurve]:
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
    def lcvz_link(self) -> typing.Optional[DefineCurve]:
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

    @property
    def sbox_link(self) -> typing.Optional[DefineBox]:
        """Get the DefineBox object for sbox."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.sbox:
                return kwd
        return None

    @sbox_link.setter
    def sbox_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for sbox."""
        self.sbox = value.boxid

    @property
    def cid_rcf_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid_rcf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid_rcf:
                return kwd
        return None

    @cid_rcf_link.setter
    def cid_rcf_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid_rcf."""
        self.cid_rcf = value.cid

