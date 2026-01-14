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

"""Module providing the MatForceLimited class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATFORCELIMITED_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("df", float, 40, 10, None),
    FieldSchema("iaflc", int, 50, 10, 0),
    FieldSchema("ytflag", float, 60, 10, 0.0),
    FieldSchema("asoft", float, 70, 10, None),
)

_MATFORCELIMITED_CARD1 = (
    FieldSchema("m1", float, 0, 10, None),
    FieldSchema("m2", float, 10, 10, None),
    FieldSchema("m3", float, 20, 10, None),
    FieldSchema("m4", float, 30, 10, None),
    FieldSchema("m5", float, 40, 10, None),
    FieldSchema("m6", float, 50, 10, None),
    FieldSchema("m7", float, 60, 10, None),
    FieldSchema("m8", float, 70, 10, None),
)

_MATFORCELIMITED_CARD2 = (
    FieldSchema("lc1", int, 0, 10, None),
    FieldSchema("lc2", int, 10, 10, 0),
    FieldSchema("lc3", int, 20, 10, 0),
    FieldSchema("lc4", int, 30, 10, 0),
    FieldSchema("lc5", int, 40, 10, 0),
    FieldSchema("lc6", int, 50, 10, 0),
    FieldSchema("lc7", int, 60, 10, 0),
    FieldSchema("lc8", int, 70, 10, 0),
)

_MATFORCELIMITED_CARD3 = (
    FieldSchema("lps1", int, 0, 10, 0),
    FieldSchema("sfs1", float, 10, 10, 1.0),
    FieldSchema("lps2", int, 20, 10, 0),
    FieldSchema("sfs2", float, 30, 10, 1.0),
    FieldSchema("yms1", float, 40, 10, 1e+20),
    FieldSchema("yms2", float, 50, 10, None),
)

_MATFORCELIMITED_CARD4 = (
    FieldSchema("lpt1", int, 0, 10, 0),
    FieldSchema("sft1", float, 10, 10, 1.0),
    FieldSchema("lpt2", int, 20, 10, 0),
    FieldSchema("sft2", float, 30, 10, 1.0),
    FieldSchema("ymt1", float, 40, 10, 1e+20),
    FieldSchema("ymt2", float, 50, 10, None),
)

_MATFORCELIMITED_CARD5 = (
    FieldSchema("lpr", int, 0, 10, 0),
    FieldSchema("sfr", float, 10, 10, 1.0),
    FieldSchema("ymr", float, 20, 10, 1e+20),
)

_MATFORCELIMITED_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatForceLimited(KeywordBase):
    """DYNA MAT_FORCE_LIMITED keyword"""

    keyword = "MAT"
    subkeyword = "FORCE_LIMITED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
        "lc3": LinkType.DEFINE_CURVE,
        "lc4": LinkType.DEFINE_CURVE,
        "lc5": LinkType.DEFINE_CURVE,
        "lc6": LinkType.DEFINE_CURVE,
        "lc7": LinkType.DEFINE_CURVE,
        "lc8": LinkType.DEFINE_CURVE,
        "lps1": LinkType.DEFINE_CURVE,
        "lps2": LinkType.DEFINE_CURVE,
        "lpt1": LinkType.DEFINE_CURVE,
        "lpt2": LinkType.DEFINE_CURVE,
        "lpr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatForceLimited class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATFORCELIMITED_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatForceLimited.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATFORCELIMITED_OPTION0_CARD0,
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def df(self) -> typing.Optional[float]:
        """Get or set the Damping factor. A proper control for the timestep must be maintained by the user.!
        """ # nopep8
        return self._cards[0].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        """Set the df property."""
        self._cards[0].set_value("df", value)

    @property
    def iaflc(self) -> int:
        """Get or set the Axial force load curve option:
        EQ.0: axial load curves are force as a function of strain.
        EQ.1: axial load curves are force as a function of change in length.
        """ # nopep8
        return self._cards[0].get_value("iaflc")

    @iaflc.setter
    def iaflc(self, value: int) -> None:
        """Set the iaflc property."""
        if value not in [0, 1, None]:
            raise Exception("""iaflc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iaflc", value)

    @property
    def ytflag(self) -> float:
        """Get or set the Flag to allow beam to yield in tension:
        EQ.0.0: beam does not yield in tension (default),
        EQ.1.0: beam can yield in tension.
        """ # nopep8
        return self._cards[0].get_value("ytflag")

    @ytflag.setter
    def ytflag(self, value: float) -> None:
        """Set the ytflag property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ytflag must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ytflag", value)

    @property
    def asoft(self) -> typing.Optional[float]:
        """Get or set the Axial elastic softening factor applied once hinge has formed. When a hinge has formed the stuffness is reduced by this factor.
        EQ.0: ASOFT is ignored.
        """ # nopep8
        return self._cards[0].get_value("asoft")

    @asoft.setter
    def asoft(self, value: float) -> None:
        """Set the asoft property."""
        self._cards[0].set_value("asoft", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the First end moment for force versus (strain/change in length) curve. Define at least one.
        """ # nopep8
        return self._cards[1].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        """Set the m1 property."""
        self._cards[1].set_value("m1", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Second end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        """Set the m2 property."""
        self._cards[1].set_value("m2", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Third end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        """Set the m3 property."""
        self._cards[1].set_value("m3", value)

    @property
    def m4(self) -> typing.Optional[float]:
        """Get or set the Fourth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m4")

    @m4.setter
    def m4(self, value: float) -> None:
        """Set the m4 property."""
        self._cards[1].set_value("m4", value)

    @property
    def m5(self) -> typing.Optional[float]:
        """Get or set the Fifth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m5")

    @m5.setter
    def m5(self, value: float) -> None:
        """Set the m5 property."""
        self._cards[1].set_value("m5", value)

    @property
    def m6(self) -> typing.Optional[float]:
        """Get or set the Sixth end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m6")

    @m6.setter
    def m6(self, value: float) -> None:
        """Set the m6 property."""
        self._cards[1].set_value("m6", value)

    @property
    def m7(self) -> typing.Optional[float]:
        """Get or set the Seventh end moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m7")

    @m7.setter
    def m7(self, value: float) -> None:
        """Set the m7 property."""
        self._cards[1].set_value("m7", value)

    @property
    def m8(self) -> typing.Optional[float]:
        """Get or set the Eight moment for force versus (strain/change in length) curve.
        """ # nopep8
        return self._cards[1].get_value("m8")

    @m8.setter
    def m8(self, value: float) -> None:
        """Set the m8 property."""
        self._cards[1].set_value("m8", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the first end moment.
        """ # nopep8
        return self._cards[2].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[2].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the secomd end moment.
        """ # nopep8
        return self._cards[2].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[2].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the third end moment.
        """ # nopep8
        return self._cards[2].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[2].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the fourth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        """Set the lc4 property."""
        self._cards[2].set_value("lc4", value)

    @property
    def lc5(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the fifth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc5")

    @lc5.setter
    def lc5(self, value: int) -> None:
        """Set the lc5 property."""
        self._cards[2].set_value("lc5", value)

    @property
    def lc6(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the sixth end moment.
        """ # nopep8
        return self._cards[2].get_value("lc6")

    @lc6.setter
    def lc6(self, value: int) -> None:
        """Set the lc6 property."""
        self._cards[2].set_value("lc6", value)

    @property
    def lc7(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the seventh end moment.
        """ # nopep8
        return self._cards[2].get_value("lc7")

    @lc7.setter
    def lc7(self, value: int) -> None:
        """Set the lc7 property."""
        self._cards[2].set_value("lc7", value)

    @property
    def lc8(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length for the eight end moment.
        """ # nopep8
        return self._cards[2].get_value("lc8")

    @lc8.setter
    def lc8(self, value: int) -> None:
        """Set the lc8 property."""
        self._cards[2].set_value("lc8", value)

    @property
    def lps1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 1. If zero, LPS1 is ingored.
        """ # nopep8
        return self._cards[3].get_value("lps1")

    @lps1.setter
    def lps1(self, value: int) -> None:
        """Set the lps1 property."""
        self._cards[3].set_value("lps1", value)

    @property
    def sfs1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 1 (default = 1.0).
        """ # nopep8
        return self._cards[3].get_value("sfs1")

    @sfs1.setter
    def sfs1(self, value: float) -> None:
        """Set the sfs1 property."""
        self._cards[3].set_value("sfs1", value)

    @property
    def lps2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 2  (default = LPS1).
        """ # nopep8
        return self._cards[3].get_value("lps2")

    @lps2.setter
    def lps2(self, value: int) -> None:
        """Set the lps2 property."""
        self._cards[3].set_value("lps2", value)

    @property
    def sfs2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 2 (default = SFS1).
        """ # nopep8
        return self._cards[3].get_value("sfs2")

    @sfs2.setter
    def sfs2(self, value: float) -> None:
        """Set the sfs2 property."""
        self._cards[3].set_value("sfs2", value)

    @property
    def yms1(self) -> float:
        """Get or set the Yield moment about s-axis at node 1 for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[3].get_value("yms1")

    @yms1.setter
    def yms1(self, value: float) -> None:
        """Set the yms1 property."""
        self._cards[3].set_value("yms1", value)

    @property
    def yms2(self) -> typing.Optional[float]:
        """Get or set the Yield moment about s-axis at node 2 for interaction calculations (default = YMS1).
        """ # nopep8
        return self._cards[3].get_value("yms2")

    @yms2.setter
    def yms2(self, value: float) -> None:
        """Set the yms2 property."""
        self._cards[3].set_value("yms2", value)

    @property
    def lpt1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 1. If zero, LPT1 is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt1")

    @lpt1.setter
    def lpt1(self, value: int) -> None:
        """Set the lpt1 property."""
        self._cards[4].set_value("lpt1", value)

    @property
    def sft1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 1 (default = 1.0).
        """ # nopep8
        return self._cards[4].get_value("sft1")

    @sft1.setter
    def sft1(self, value: float) -> None:
        """Set the sft1 property."""
        self._cards[4].set_value("sft1", value)

    @property
    def lpt2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 2 (default = LPT1).
        """ # nopep8
        return self._cards[4].get_value("lpt2")

    @lpt2.setter
    def lpt2(self, value: int) -> None:
        """Set the lpt2 property."""
        self._cards[4].set_value("lpt2", value)

    @property
    def sft2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 2 (default = SFS1).
        """ # nopep8
        return self._cards[4].get_value("sft2")

    @sft2.setter
    def sft2(self, value: float) -> None:
        """Set the sft2 property."""
        self._cards[4].set_value("sft2", value)

    @property
    def ymt1(self) -> float:
        """Get or set the Yield moment about t-axis at node 1 for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[4].get_value("ymt1")

    @ymt1.setter
    def ymt1(self, value: float) -> None:
        """Set the ymt1 property."""
        self._cards[4].set_value("ymt1", value)

    @property
    def ymt2(self) -> typing.Optional[float]:
        """Get or set the Yield moment about t-axis at node 2 for interaction calculations (default = YMT1).
        """ # nopep8
        return self._cards[4].get_value("ymt2")

    @ymt2.setter
    def ymt2(self, value: float) -> None:
        """Set the ymt2 property."""
        self._cards[4].set_value("ymt2", value)

    @property
    def lpr(self) -> int:
        """Get or set the Load curve ID for plastic torsional moment versus rotation. If zero, LPR is ignored.
        """ # nopep8
        return self._cards[5].get_value("lpr")

    @lpr.setter
    def lpr(self, value: int) -> None:
        """Set the lpr property."""
        self._cards[5].set_value("lpr", value)

    @property
    def sfr(self) -> float:
        """Get or set the Scale factor for plastic torsional moment versus rotation (default = 1.0).
        """ # nopep8
        return self._cards[5].get_value("sfr")

    @sfr.setter
    def sfr(self, value: float) -> None:
        """Set the sfr property."""
        self._cards[5].set_value("sfr", value)

    @property
    def ymr(self) -> float:
        """Get or set the Torsional yield moment for interaction calculations (default = 1.0E+20).
        """ # nopep8
        return self._cards[5].get_value("ymr")

    @ymr.setter
    def ymr(self, value: float) -> None:
        """Set the ymr property."""
        self._cards[5].set_value("ymr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lc1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

    @property
    def lc3_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc3:
                return kwd
        return None

    @lc3_link.setter
    def lc3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc3."""
        self.lc3 = value.lcid

    @property
    def lc4_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc4:
                return kwd
        return None

    @lc4_link.setter
    def lc4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc4."""
        self.lc4 = value.lcid

    @property
    def lc5_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc5:
                return kwd
        return None

    @lc5_link.setter
    def lc5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc5."""
        self.lc5 = value.lcid

    @property
    def lc6_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc6:
                return kwd
        return None

    @lc6_link.setter
    def lc6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc6."""
        self.lc6 = value.lcid

    @property
    def lc7_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc7:
                return kwd
        return None

    @lc7_link.setter
    def lc7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc7."""
        self.lc7 = value.lcid

    @property
    def lc8_link(self) -> DefineCurve:
        """Get the DefineCurve object for lc8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc8:
                return kwd
        return None

    @lc8_link.setter
    def lc8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc8."""
        self.lc8 = value.lcid

    @property
    def lps1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lps1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lps1:
                return kwd
        return None

    @lps1_link.setter
    def lps1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lps1."""
        self.lps1 = value.lcid

    @property
    def lps2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lps2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lps2:
                return kwd
        return None

    @lps2_link.setter
    def lps2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lps2."""
        self.lps2 = value.lcid

    @property
    def lpt1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lpt1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpt1:
                return kwd
        return None

    @lpt1_link.setter
    def lpt1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpt1."""
        self.lpt1 = value.lcid

    @property
    def lpt2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lpt2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpt2:
                return kwd
        return None

    @lpt2_link.setter
    def lpt2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpt2."""
        self.lpt2 = value.lcid

    @property
    def lpr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lpr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpr:
                return kwd
        return None

    @lpr_link.setter
    def lpr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpr."""
        self.lpr = value.lcid

