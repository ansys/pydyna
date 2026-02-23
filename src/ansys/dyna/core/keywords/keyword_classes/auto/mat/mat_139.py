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

"""Module providing the Mat139 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT139_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("df", float, 40, 10, None),
    FieldSchema("iaflc", int, 50, 10, 0),
    FieldSchema("ytflag", float, 60, 10, 0.0),
    FieldSchema("asoft", float, 70, 10, None),
)

_MAT139_CARD1 = (
    FieldSchema("m1", float, 0, 10, None),
    FieldSchema("m2", float, 10, 10, None),
    FieldSchema("m3", float, 20, 10, None),
    FieldSchema("m4", float, 30, 10, None),
    FieldSchema("m5", float, 40, 10, None),
    FieldSchema("m6", float, 50, 10, None),
    FieldSchema("m7", float, 60, 10, None),
    FieldSchema("m8", float, 70, 10, None),
)

_MAT139_CARD2 = (
    FieldSchema("lc1", int, 0, 10, None),
    FieldSchema("lc2", int, 10, 10, 0),
    FieldSchema("lc3", int, 20, 10, 0),
    FieldSchema("lc4", int, 30, 10, 0),
    FieldSchema("lc5", int, 40, 10, 0),
    FieldSchema("lc6", int, 50, 10, 0),
    FieldSchema("lc7", int, 60, 10, 0),
    FieldSchema("lc8", int, 70, 10, 0),
)

_MAT139_CARD3 = (
    FieldSchema("lps1", int, 0, 10, 0),
    FieldSchema("sfs1", float, 10, 10, 1.0),
    FieldSchema("lps2", int, 20, 10, 0),
    FieldSchema("sfs2", float, 30, 10, 1.0),
    FieldSchema("yms1", float, 40, 10, 1e+20),
    FieldSchema("yms2", float, 50, 10, 1e+20),
)

_MAT139_CARD4 = (
    FieldSchema("lpt1", int, 0, 10, 0),
    FieldSchema("sft1", float, 10, 10, 1.0),
    FieldSchema("lpt2", int, 20, 10, 0),
    FieldSchema("sft2", float, 30, 10, 1.0),
    FieldSchema("ymt1", float, 40, 10, 1e+20),
    FieldSchema("ymt2", float, 50, 10, 1e+20),
)

_MAT139_CARD5 = (
    FieldSchema("lpr", int, 0, 10, 0),
    FieldSchema("sfr", float, 10, 10, 1.0),
    FieldSchema("ymr", float, 20, 10, 1e+20),
)

_MAT139_CARD6 = (
    FieldSchema("lys1", int, 0, 10, 0),
    FieldSchema("sys1", float, 10, 10, 1.0),
    FieldSchema("lys2", int, 20, 10, 0),
    FieldSchema("sys2", float, 30, 10, 1.0),
    FieldSchema("lyt1", int, 40, 10, 0),
    FieldSchema("syt1", float, 50, 10, 1.0),
    FieldSchema("lyt2", int, 60, 10, 0),
    FieldSchema("syt2", float, 70, 10, 1.0),
)

_MAT139_CARD7 = (
    FieldSchema("lyr", int, 0, 10, 0),
    FieldSchema("syr", float, 10, 10, 1.0),
)

_MAT139_CARD8 = (
    FieldSchema("hms1_1", float, 0, 10, None),
    FieldSchema("hms1_2", float, 10, 10, None),
    FieldSchema("hms1_3", float, 20, 10, None),
    FieldSchema("hms1_4", float, 30, 10, None),
    FieldSchema("hms1_5", float, 40, 10, None),
    FieldSchema("hms1_6", float, 50, 10, None),
    FieldSchema("hms1_7", float, 60, 10, None),
    FieldSchema("hms1_8", float, 70, 10, None),
)

_MAT139_CARD9 = (
    FieldSchema("lpms1_1", int, 0, 10, None),
    FieldSchema("lpms1_2", int, 10, 10, None),
    FieldSchema("lpms1_3", int, 20, 10, None),
    FieldSchema("lpms1_4", int, 30, 10, None),
    FieldSchema("lpms1_5", int, 40, 10, None),
    FieldSchema("lpms1_6", int, 50, 10, None),
    FieldSchema("lpms1_7", int, 60, 10, None),
    FieldSchema("lpms1_8", int, 70, 10, None),
)

_MAT139_CARD10 = (
    FieldSchema("hms2_1", float, 0, 10, None),
    FieldSchema("hms2_2", float, 10, 10, None),
    FieldSchema("hms2_3", float, 20, 10, None),
    FieldSchema("hms2_4", float, 30, 10, None),
    FieldSchema("hms2_5", float, 40, 10, None),
    FieldSchema("hms2_6", float, 50, 10, None),
    FieldSchema("hms2_7", float, 60, 10, None),
    FieldSchema("hms2_8", float, 70, 10, None),
)

_MAT139_CARD11 = (
    FieldSchema("lpms2_1", int, 0, 10, None),
    FieldSchema("lpms2_2", int, 10, 10, None),
    FieldSchema("lpms2_3", int, 20, 10, None),
    FieldSchema("lpms2_4", int, 30, 10, None),
    FieldSchema("lpms2_5", int, 40, 10, None),
    FieldSchema("lpms2_6", int, 50, 10, None),
    FieldSchema("lpms2_7", int, 60, 10, None),
    FieldSchema("lpms2_8", int, 70, 10, None),
)

_MAT139_CARD12 = (
    FieldSchema("hmt1_1", float, 0, 10, None),
    FieldSchema("hmt1_2", float, 10, 10, None),
    FieldSchema("hmt1_3", float, 20, 10, None),
    FieldSchema("hmt1_4", float, 30, 10, None),
    FieldSchema("hmt1_5", float, 40, 10, None),
    FieldSchema("hmt1_6", float, 50, 10, None),
    FieldSchema("hmt1_7", float, 60, 10, None),
    FieldSchema("hmt1_8", float, 70, 10, None),
)

_MAT139_CARD13 = (
    FieldSchema("lpmt1_1", int, 0, 10, None),
    FieldSchema("lpmt1_2", int, 10, 10, None),
    FieldSchema("lpmt1_3", int, 20, 10, None),
    FieldSchema("lpmt1_4", int, 30, 10, None),
    FieldSchema("lpmt1_5", int, 40, 10, None),
    FieldSchema("lpmt1_6", int, 50, 10, None),
    FieldSchema("lpmt1_7", int, 60, 10, None),
    FieldSchema("lpmt1_8", int, 70, 10, None),
)

_MAT139_CARD14 = (
    FieldSchema("hmt2_1", float, 0, 10, None),
    FieldSchema("hmt2_2", float, 10, 10, None),
    FieldSchema("hmt2_3", float, 20, 10, None),
    FieldSchema("hmt2_4", float, 30, 10, None),
    FieldSchema("hmt2_5", float, 40, 10, None),
    FieldSchema("hmt2_6", float, 50, 10, None),
    FieldSchema("hmt2_7", float, 60, 10, None),
    FieldSchema("hmt2_8", float, 70, 10, None),
)

_MAT139_CARD15 = (
    FieldSchema("lpmt2_1", int, 0, 10, None),
    FieldSchema("lpmt2_2", int, 10, 10, None),
    FieldSchema("lpmt2_3", int, 20, 10, None),
    FieldSchema("lpmt2_4", int, 30, 10, None),
    FieldSchema("lpmt2_5", int, 40, 10, None),
    FieldSchema("lpmt2_6", int, 50, 10, None),
    FieldSchema("lpmt2_7", int, 60, 10, None),
    FieldSchema("lpmt2_8", int, 70, 10, None),
)

_MAT139_CARD16 = (
    FieldSchema("hmr_1", float, 0, 10, None),
    FieldSchema("hmr_2", float, 10, 10, None),
    FieldSchema("hmr_3", float, 20, 10, None),
    FieldSchema("hmr_4", float, 30, 10, None),
    FieldSchema("hmr_5", float, 40, 10, None),
    FieldSchema("hmr_6", float, 50, 10, None),
    FieldSchema("hmr_7", float, 60, 10, None),
    FieldSchema("hmr_8", float, 70, 10, None),
)

_MAT139_CARD17 = (
    FieldSchema("lpmr_1", int, 0, 10, 0),
    FieldSchema("lpmr_2", int, 10, 10, 0),
    FieldSchema("lpmr_3", int, 20, 10, 0),
    FieldSchema("lpmr_4", int, 30, 10, 0),
    FieldSchema("lpmr_5", int, 40, 10, 0),
    FieldSchema("lpmr_6", int, 50, 10, 0),
    FieldSchema("lpmr_7", int, 60, 10, 0),
    FieldSchema("lpmr_8", int, 70, 10, 0),
)

_MAT139_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat139(KeywordBase):
    """DYNA MAT_139 keyword"""

    keyword = "MAT"
    subkeyword = "139"
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
        "lys1": LinkType.DEFINE_CURVE,
        "lys2": LinkType.DEFINE_CURVE,
        "lyt1": LinkType.DEFINE_CURVE,
        "lyt2": LinkType.DEFINE_CURVE,
        "lyr": LinkType.DEFINE_CURVE,
        "lpms1_1": LinkType.DEFINE_CURVE,
        "lpms1_2": LinkType.DEFINE_CURVE,
        "lpms1_3": LinkType.DEFINE_CURVE,
        "lpms1_4": LinkType.DEFINE_CURVE,
        "lpms1_5": LinkType.DEFINE_CURVE,
        "lpms1_6": LinkType.DEFINE_CURVE,
        "lpms1_7": LinkType.DEFINE_CURVE,
        "lpms1_8": LinkType.DEFINE_CURVE,
        "lpms2_1": LinkType.DEFINE_CURVE,
        "lpms2_2": LinkType.DEFINE_CURVE,
        "lpms2_3": LinkType.DEFINE_CURVE,
        "lpms2_4": LinkType.DEFINE_CURVE,
        "lpms2_5": LinkType.DEFINE_CURVE,
        "lpms2_6": LinkType.DEFINE_CURVE,
        "lpms2_7": LinkType.DEFINE_CURVE,
        "lpms2_8": LinkType.DEFINE_CURVE,
        "lpmt1_1": LinkType.DEFINE_CURVE,
        "lpmt1_2": LinkType.DEFINE_CURVE,
        "lpmt1_3": LinkType.DEFINE_CURVE,
        "lpmt1_4": LinkType.DEFINE_CURVE,
        "lpmt1_5": LinkType.DEFINE_CURVE,
        "lpmt1_6": LinkType.DEFINE_CURVE,
        "lpmt1_7": LinkType.DEFINE_CURVE,
        "lpmt1_8": LinkType.DEFINE_CURVE,
        "lpmt2_1": LinkType.DEFINE_CURVE,
        "lpmt2_2": LinkType.DEFINE_CURVE,
        "lpmt2_3": LinkType.DEFINE_CURVE,
        "lpmt2_4": LinkType.DEFINE_CURVE,
        "lpmt2_5": LinkType.DEFINE_CURVE,
        "lpmt2_6": LinkType.DEFINE_CURVE,
        "lpmt2_7": LinkType.DEFINE_CURVE,
        "lpmt2_8": LinkType.DEFINE_CURVE,
        "lpmr_1": LinkType.DEFINE_CURVE,
        "lpmr_2": LinkType.DEFINE_CURVE,
        "lpmr_3": LinkType.DEFINE_CURVE,
        "lpmr_4": LinkType.DEFINE_CURVE,
        "lpmr_5": LinkType.DEFINE_CURVE,
        "lpmr_6": LinkType.DEFINE_CURVE,
        "lpmr_7": LinkType.DEFINE_CURVE,
        "lpmr_8": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat139 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT139_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD10,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD11,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD12,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD13,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD14,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD15,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD16,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT139_CARD17,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat139.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT139_OPTION0_CARD0,
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
        """Get or set the Damping factor, see definition in notes below. A proper control for the timestep has to be maintained by the user!
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
        EQ.0.0: beam does not yield in tension,
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
        """Get or set the Axial elastic softening factor applied once hinge has formed. When a hinge has formed the stiffness is reduced by this factor. If zero, this factor is ignored.
        """ # nopep8
        return self._cards[0].get_value("asoft")

    @asoft.setter
    def asoft(self, value: float) -> None:
        """Set the asoft property."""
        self._cards[0].set_value("asoft", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. At least one must be defined. A maximum of 8 moments can be defined. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        """Set the m1 property."""
        self._cards[1].set_value("m1", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        """Set the m2 property."""
        self._cards[1].set_value("m2", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        """Set the m3 property."""
        self._cards[1].set_value("m3", value)

    @property
    def m4(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m4")

    @m4.setter
    def m4(self, value: float) -> None:
        """Set the m4 property."""
        self._cards[1].set_value("m4", value)

    @property
    def m5(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m5")

    @m5.setter
    def m5(self, value: float) -> None:
        """Set the m5 property."""
        self._cards[1].set_value("m5", value)

    @property
    def m6(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m6")

    @m6.setter
    def m6(self, value: float) -> None:
        """Set the m6 property."""
        self._cards[1].set_value("m6", value)

    @property
    def m7(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m7")

    @m7.setter
    def m7(self, value: float) -> None:
        """Set the m7 property."""
        self._cards[1].set_value("m7", value)

    @property
    def m8(self) -> typing.Optional[float]:
        """Get or set the Applied end moment for force versus (strain/change in length) curve. The values should be in ascending order.
        """ # nopep8
        return self._cards[1].get_value("m8")

    @m8.setter
    def m8(self, value: float) -> None:
        """Set the m8 property."""
        self._cards[1].set_value("m8", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment. Define the same number as end moments. Each curve must contain the same number of points.
        """ # nopep8
        return self._cards[2].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[2].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[2].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[2].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        """Set the lc4 property."""
        self._cards[2].set_value("lc4", value)

    @property
    def lc5(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc5")

    @lc5.setter
    def lc5(self, value: int) -> None:
        """Set the lc5 property."""
        self._cards[2].set_value("lc5", value)

    @property
    def lc6(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc6")

    @lc6.setter
    def lc6(self, value: int) -> None:
        """Set the lc6 property."""
        self._cards[2].set_value("lc6", value)

    @property
    def lc7(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc7")

    @lc7.setter
    def lc7(self, value: int) -> None:
        """Set the lc7 property."""
        self._cards[2].set_value("lc7", value)

    @property
    def lc8(self) -> int:
        """Get or set the Load curve ID defining axial force versus strain/change in length (see AOPT) for the corresponding applied end moment.
        """ # nopep8
        return self._cards[2].get_value("lc8")

    @lc8.setter
    def lc8(self, value: int) -> None:
        """Set the lc8 property."""
        self._cards[2].set_value("lc8", value)

    @property
    def lps1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 1. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[3].get_value("lps1")

    @lps1.setter
    def lps1(self, value: int) -> None:
        """Set the lps1 property."""
        self._cards[3].set_value("lps1", value)

    @property
    def sfs1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 1. Default = 1.0.
        """ # nopep8
        return self._cards[3].get_value("sfs1")

    @sfs1.setter
    def sfs1(self, value: float) -> None:
        """Set the sfs1 property."""
        self._cards[3].set_value("sfs1", value)

    @property
    def lps2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about s-axis at node 2. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[3].get_value("lps2")

    @lps2.setter
    def lps2(self, value: int) -> None:
        """Set the lps2 property."""
        self._cards[3].set_value("lps2", value)

    @property
    def sfs2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about s-axis at node 2. Default = 1.0.
        """ # nopep8
        return self._cards[3].get_value("sfs2")

    @sfs2.setter
    def sfs2(self, value: float) -> None:
        """Set the sfs2 property."""
        self._cards[3].set_value("sfs2", value)

    @property
    def yms1(self) -> float:
        """Get or set the Yield moment about s-axis at node 1 for interaction calculations (default set to 1.0E+20 to prevent interaction).
        """ # nopep8
        return self._cards[3].get_value("yms1")

    @yms1.setter
    def yms1(self, value: float) -> None:
        """Set the yms1 property."""
        self._cards[3].set_value("yms1", value)

    @property
    def yms2(self) -> float:
        """Get or set the Yield moment about s-axis at node 2 for interaction calculations (default set to YMS1).
        """ # nopep8
        return self._cards[3].get_value("yms2")

    @yms2.setter
    def yms2(self, value: float) -> None:
        """Set the yms2 property."""
        self._cards[3].set_value("yms2", value)

    @property
    def lpt1(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 1. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt1")

    @lpt1.setter
    def lpt1(self, value: int) -> None:
        """Set the lpt1 property."""
        self._cards[4].set_value("lpt1", value)

    @property
    def sft1(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 1. Default = 1.0.
        """ # nopep8
        return self._cards[4].get_value("sft1")

    @sft1.setter
    def sft1(self, value: float) -> None:
        """Set the sft1 property."""
        self._cards[4].set_value("sft1", value)

    @property
    def lpt2(self) -> int:
        """Get or set the Load curve ID for plastic moment versus rotation about t-axis at node 2. If zero, this load curve is ignored.
        """ # nopep8
        return self._cards[4].get_value("lpt2")

    @lpt2.setter
    def lpt2(self, value: int) -> None:
        """Set the lpt2 property."""
        self._cards[4].set_value("lpt2", value)

    @property
    def sft2(self) -> float:
        """Get or set the Scale factor for plastic moment versus rotation curve about t-axis at node 2. Default = 1.0.
        """ # nopep8
        return self._cards[4].get_value("sft2")

    @sft2.setter
    def sft2(self, value: float) -> None:
        """Set the sft2 property."""
        self._cards[4].set_value("sft2", value)

    @property
    def ymt1(self) -> float:
        """Get or set the Yield moment about t-axis at node 1 for interaction calculations (default set to 1.0E+20 to prevent interactions)
        """ # nopep8
        return self._cards[4].get_value("ymt1")

    @ymt1.setter
    def ymt1(self, value: float) -> None:
        """Set the ymt1 property."""
        self._cards[4].set_value("ymt1", value)

    @property
    def ymt2(self) -> float:
        """Get or set the Yield moment about t-axis at node 2 for interaction calculations (default set to YMT1)
        """ # nopep8
        return self._cards[4].get_value("ymt2")

    @ymt2.setter
    def ymt2(self, value: float) -> None:
        """Set the ymt2 property."""
        self._cards[4].set_value("ymt2", value)

    @property
    def lpr(self) -> int:
        """Get or set the Load curve ID for plastic torsional moment versus rotation. If zero, this load curve is ignored.
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
        """Get or set the Torsional yield moment for interaction calculations (default set to 1.0E+20 to prevent interaction)
        """ # nopep8
        return self._cards[5].get_value("ymr")

    @ymr.setter
    def ymr(self, value: float) -> None:
        """Set the ymr property."""
        self._cards[5].set_value("ymr", value)

    @property
    def lys1(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the s-axis at node 1.
        """ # nopep8
        return self._cards[6].get_value("lys1")

    @lys1.setter
    def lys1(self, value: int) -> None:
        """Set the lys1 property."""
        self._cards[6].set_value("lys1", value)

    @property
    def sys1(self) -> float:
        """Get or set the Scale factor applied to load curve LYS1.
        """ # nopep8
        return self._cards[6].get_value("sys1")

    @sys1.setter
    def sys1(self, value: float) -> None:
        """Set the sys1 property."""
        self._cards[6].set_value("sys1", value)

    @property
    def lys2(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the s-axis at node 2.
        """ # nopep8
        return self._cards[6].get_value("lys2")

    @lys2.setter
    def lys2(self, value: int) -> None:
        """Set the lys2 property."""
        self._cards[6].set_value("lys2", value)

    @property
    def sys2(self) -> float:
        """Get or set the Scale factor applied to load curve LYS2.
        """ # nopep8
        return self._cards[6].get_value("sys2")

    @sys2.setter
    def sys2(self, value: float) -> None:
        """Set the sys2 property."""
        self._cards[6].set_value("sys2", value)

    @property
    def lyt1(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the t-axis at node 1.
        """ # nopep8
        return self._cards[6].get_value("lyt1")

    @lyt1.setter
    def lyt1(self, value: int) -> None:
        """Set the lyt1 property."""
        self._cards[6].set_value("lyt1", value)

    @property
    def syt1(self) -> float:
        """Get or set the Scale factor applied to load curve LYT1.
        """ # nopep8
        return self._cards[6].get_value("syt1")

    @syt1.setter
    def syt1(self, value: float) -> None:
        """Set the syt1 property."""
        self._cards[6].set_value("syt1", value)

    @property
    def lyt2(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the t-axis at node 2.
        """ # nopep8
        return self._cards[6].get_value("lyt2")

    @lyt2.setter
    def lyt2(self, value: int) -> None:
        """Set the lyt2 property."""
        self._cards[6].set_value("lyt2", value)

    @property
    def syt2(self) -> float:
        """Get or set the Scale factor applied to load curve LYT2.
        """ # nopep8
        return self._cards[6].get_value("syt2")

    @syt2.setter
    def syt2(self, value: float) -> None:
        """Set the syt2 property."""
        self._cards[6].set_value("syt2", value)

    @property
    def lyr(self) -> int:
        """Get or set the ID of curve defining yield moment as a function of axial force for the torsional axis.
        """ # nopep8
        return self._cards[7].get_value("lyr")

    @lyr.setter
    def lyr(self, value: int) -> None:
        """Set the lyr property."""
        self._cards[7].set_value("lyr", value)

    @property
    def syr(self) -> float:
        """Get or set the Scale factor applied to load curve LYR.
        """ # nopep8
        return self._cards[7].get_value("syr")

    @syr.setter
    def syr(self, value: float) -> None:
        """Set the syr property."""
        self._cards[7].set_value("syr", value)

    @property
    def hms1_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_1")

    @hms1_1.setter
    def hms1_1(self, value: float) -> None:
        """Set the hms1_1 property."""
        self._cards[8].set_value("hms1_1", value)

    @property
    def hms1_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_2")

    @hms1_2.setter
    def hms1_2(self, value: float) -> None:
        """Set the hms1_2 property."""
        self._cards[8].set_value("hms1_2", value)

    @property
    def hms1_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_3")

    @hms1_3.setter
    def hms1_3(self, value: float) -> None:
        """Set the hms1_3 property."""
        self._cards[8].set_value("hms1_3", value)

    @property
    def hms1_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_4")

    @hms1_4.setter
    def hms1_4(self, value: float) -> None:
        """Set the hms1_4 property."""
        self._cards[8].set_value("hms1_4", value)

    @property
    def hms1_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_5")

    @hms1_5.setter
    def hms1_5(self, value: float) -> None:
        """Set the hms1_5 property."""
        self._cards[8].set_value("hms1_5", value)

    @property
    def hms1_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_6")

    @hms1_6.setter
    def hms1_6(self, value: float) -> None:
        """Set the hms1_6 property."""
        self._cards[8].set_value("hms1_6", value)

    @property
    def hms1_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_7")

    @hms1_7.setter
    def hms1_7(self, value: float) -> None:
        """Set the hms1_7 property."""
        self._cards[8].set_value("hms1_7", value)

    @property
    def hms1_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 1.
        """ # nopep8
        return self._cards[8].get_value("hms1_8")

    @hms1_8.setter
    def hms1_8(self, value: float) -> None:
        """Set the hms1_8 property."""
        self._cards[8].set_value("hms1_8", value)

    @property
    def lpms1_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_1
        """ # nopep8
        return self._cards[9].get_value("lpms1_1")

    @lpms1_1.setter
    def lpms1_1(self, value: int) -> None:
        """Set the lpms1_1 property."""
        self._cards[9].set_value("lpms1_1", value)

    @property
    def lpms1_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_2
        """ # nopep8
        return self._cards[9].get_value("lpms1_2")

    @lpms1_2.setter
    def lpms1_2(self, value: int) -> None:
        """Set the lpms1_2 property."""
        self._cards[9].set_value("lpms1_2", value)

    @property
    def lpms1_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_3
        """ # nopep8
        return self._cards[9].get_value("lpms1_3")

    @lpms1_3.setter
    def lpms1_3(self, value: int) -> None:
        """Set the lpms1_3 property."""
        self._cards[9].set_value("lpms1_3", value)

    @property
    def lpms1_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_4
        """ # nopep8
        return self._cards[9].get_value("lpms1_4")

    @lpms1_4.setter
    def lpms1_4(self, value: int) -> None:
        """Set the lpms1_4 property."""
        self._cards[9].set_value("lpms1_4", value)

    @property
    def lpms1_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_5
        """ # nopep8
        return self._cards[9].get_value("lpms1_5")

    @lpms1_5.setter
    def lpms1_5(self, value: int) -> None:
        """Set the lpms1_5 property."""
        self._cards[9].set_value("lpms1_5", value)

    @property
    def lpms1_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_6
        """ # nopep8
        return self._cards[9].get_value("lpms1_6")

    @lpms1_6.setter
    def lpms1_6(self, value: int) -> None:
        """Set the lpms1_6 property."""
        self._cards[9].set_value("lpms1_6", value)

    @property
    def lpms1_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_7
        """ # nopep8
        return self._cards[9].get_value("lpms1_7")

    @lpms1_7.setter
    def lpms1_7(self, value: int) -> None:
        """Set the lpms1_7 property."""
        self._cards[9].set_value("lpms1_7", value)

    @property
    def lpms1_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 1 for hinge moment HMS1_8
        """ # nopep8
        return self._cards[9].get_value("lpms1_8")

    @lpms1_8.setter
    def lpms1_8(self, value: int) -> None:
        """Set the lpms1_8 property."""
        self._cards[9].set_value("lpms1_8", value)

    @property
    def hms2_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_1")

    @hms2_1.setter
    def hms2_1(self, value: float) -> None:
        """Set the hms2_1 property."""
        self._cards[10].set_value("hms2_1", value)

    @property
    def hms2_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_2")

    @hms2_2.setter
    def hms2_2(self, value: float) -> None:
        """Set the hms2_2 property."""
        self._cards[10].set_value("hms2_2", value)

    @property
    def hms2_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_3")

    @hms2_3.setter
    def hms2_3(self, value: float) -> None:
        """Set the hms2_3 property."""
        self._cards[10].set_value("hms2_3", value)

    @property
    def hms2_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_4")

    @hms2_4.setter
    def hms2_4(self, value: float) -> None:
        """Set the hms2_4 property."""
        self._cards[10].set_value("hms2_4", value)

    @property
    def hms2_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_5")

    @hms2_5.setter
    def hms2_5(self, value: float) -> None:
        """Set the hms2_5 property."""
        self._cards[10].set_value("hms2_5", value)

    @property
    def hms2_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_6")

    @hms2_6.setter
    def hms2_6(self, value: float) -> None:
        """Set the hms2_6 property."""
        self._cards[10].set_value("hms2_6", value)

    @property
    def hms2_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_7")

    @hms2_7.setter
    def hms2_7(self, value: float) -> None:
        """Set the hms2_7 property."""
        self._cards[10].set_value("hms2_7", value)

    @property
    def hms2_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for s-axis at node 2.
        """ # nopep8
        return self._cards[10].get_value("hms2_8")

    @hms2_8.setter
    def hms2_8(self, value: float) -> None:
        """Set the hms2_8 property."""
        self._cards[10].set_value("hms2_8", value)

    @property
    def lpms2_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_1
        """ # nopep8
        return self._cards[11].get_value("lpms2_1")

    @lpms2_1.setter
    def lpms2_1(self, value: int) -> None:
        """Set the lpms2_1 property."""
        self._cards[11].set_value("lpms2_1", value)

    @property
    def lpms2_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_2
        """ # nopep8
        return self._cards[11].get_value("lpms2_2")

    @lpms2_2.setter
    def lpms2_2(self, value: int) -> None:
        """Set the lpms2_2 property."""
        self._cards[11].set_value("lpms2_2", value)

    @property
    def lpms2_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_3
        """ # nopep8
        return self._cards[11].get_value("lpms2_3")

    @lpms2_3.setter
    def lpms2_3(self, value: int) -> None:
        """Set the lpms2_3 property."""
        self._cards[11].set_value("lpms2_3", value)

    @property
    def lpms2_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_4
        """ # nopep8
        return self._cards[11].get_value("lpms2_4")

    @lpms2_4.setter
    def lpms2_4(self, value: int) -> None:
        """Set the lpms2_4 property."""
        self._cards[11].set_value("lpms2_4", value)

    @property
    def lpms2_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_5
        """ # nopep8
        return self._cards[11].get_value("lpms2_5")

    @lpms2_5.setter
    def lpms2_5(self, value: int) -> None:
        """Set the lpms2_5 property."""
        self._cards[11].set_value("lpms2_5", value)

    @property
    def lpms2_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_6
        """ # nopep8
        return self._cards[11].get_value("lpms2_6")

    @lpms2_6.setter
    def lpms2_6(self, value: int) -> None:
        """Set the lpms2_6 property."""
        self._cards[11].set_value("lpms2_6", value)

    @property
    def lpms2_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_7
        """ # nopep8
        return self._cards[11].get_value("lpms2_7")

    @lpms2_7.setter
    def lpms2_7(self, value: int) -> None:
        """Set the lpms2_7 property."""
        self._cards[11].set_value("lpms2_7", value)

    @property
    def lpms2_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the s-axis at node 2 for hinge moment HMS2_8
        """ # nopep8
        return self._cards[11].get_value("lpms2_8")

    @lpms2_8.setter
    def lpms2_8(self, value: int) -> None:
        """Set the lpms2_8 property."""
        self._cards[11].set_value("lpms2_8", value)

    @property
    def hmt1_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_1")

    @hmt1_1.setter
    def hmt1_1(self, value: float) -> None:
        """Set the hmt1_1 property."""
        self._cards[12].set_value("hmt1_1", value)

    @property
    def hmt1_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_2")

    @hmt1_2.setter
    def hmt1_2(self, value: float) -> None:
        """Set the hmt1_2 property."""
        self._cards[12].set_value("hmt1_2", value)

    @property
    def hmt1_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_3")

    @hmt1_3.setter
    def hmt1_3(self, value: float) -> None:
        """Set the hmt1_3 property."""
        self._cards[12].set_value("hmt1_3", value)

    @property
    def hmt1_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_4")

    @hmt1_4.setter
    def hmt1_4(self, value: float) -> None:
        """Set the hmt1_4 property."""
        self._cards[12].set_value("hmt1_4", value)

    @property
    def hmt1_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_5")

    @hmt1_5.setter
    def hmt1_5(self, value: float) -> None:
        """Set the hmt1_5 property."""
        self._cards[12].set_value("hmt1_5", value)

    @property
    def hmt1_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_6")

    @hmt1_6.setter
    def hmt1_6(self, value: float) -> None:
        """Set the hmt1_6 property."""
        self._cards[12].set_value("hmt1_6", value)

    @property
    def hmt1_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_7")

    @hmt1_7.setter
    def hmt1_7(self, value: float) -> None:
        """Set the hmt1_7 property."""
        self._cards[12].set_value("hmt1_7", value)

    @property
    def hmt1_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 1.
        """ # nopep8
        return self._cards[12].get_value("hmt1_8")

    @hmt1_8.setter
    def hmt1_8(self, value: float) -> None:
        """Set the hmt1_8 property."""
        self._cards[12].set_value("hmt1_8", value)

    @property
    def lpmt1_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_1
        """ # nopep8
        return self._cards[13].get_value("lpmt1_1")

    @lpmt1_1.setter
    def lpmt1_1(self, value: int) -> None:
        """Set the lpmt1_1 property."""
        self._cards[13].set_value("lpmt1_1", value)

    @property
    def lpmt1_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_2
        """ # nopep8
        return self._cards[13].get_value("lpmt1_2")

    @lpmt1_2.setter
    def lpmt1_2(self, value: int) -> None:
        """Set the lpmt1_2 property."""
        self._cards[13].set_value("lpmt1_2", value)

    @property
    def lpmt1_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_3
        """ # nopep8
        return self._cards[13].get_value("lpmt1_3")

    @lpmt1_3.setter
    def lpmt1_3(self, value: int) -> None:
        """Set the lpmt1_3 property."""
        self._cards[13].set_value("lpmt1_3", value)

    @property
    def lpmt1_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_4
        """ # nopep8
        return self._cards[13].get_value("lpmt1_4")

    @lpmt1_4.setter
    def lpmt1_4(self, value: int) -> None:
        """Set the lpmt1_4 property."""
        self._cards[13].set_value("lpmt1_4", value)

    @property
    def lpmt1_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_5
        """ # nopep8
        return self._cards[13].get_value("lpmt1_5")

    @lpmt1_5.setter
    def lpmt1_5(self, value: int) -> None:
        """Set the lpmt1_5 property."""
        self._cards[13].set_value("lpmt1_5", value)

    @property
    def lpmt1_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_6
        """ # nopep8
        return self._cards[13].get_value("lpmt1_6")

    @lpmt1_6.setter
    def lpmt1_6(self, value: int) -> None:
        """Set the lpmt1_6 property."""
        self._cards[13].set_value("lpmt1_6", value)

    @property
    def lpmt1_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_7
        """ # nopep8
        return self._cards[13].get_value("lpmt1_7")

    @lpmt1_7.setter
    def lpmt1_7(self, value: int) -> None:
        """Set the lpmt1_7 property."""
        self._cards[13].set_value("lpmt1_7", value)

    @property
    def lpmt1_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 1 for hinge moment HMT1_8
        """ # nopep8
        return self._cards[13].get_value("lpmt1_8")

    @lpmt1_8.setter
    def lpmt1_8(self, value: int) -> None:
        """Set the lpmt1_8 property."""
        self._cards[13].set_value("lpmt1_8", value)

    @property
    def hmt2_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_1")

    @hmt2_1.setter
    def hmt2_1(self, value: float) -> None:
        """Set the hmt2_1 property."""
        self._cards[14].set_value("hmt2_1", value)

    @property
    def hmt2_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_2")

    @hmt2_2.setter
    def hmt2_2(self, value: float) -> None:
        """Set the hmt2_2 property."""
        self._cards[14].set_value("hmt2_2", value)

    @property
    def hmt2_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_3")

    @hmt2_3.setter
    def hmt2_3(self, value: float) -> None:
        """Set the hmt2_3 property."""
        self._cards[14].set_value("hmt2_3", value)

    @property
    def hmt2_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_4")

    @hmt2_4.setter
    def hmt2_4(self, value: float) -> None:
        """Set the hmt2_4 property."""
        self._cards[14].set_value("hmt2_4", value)

    @property
    def hmt2_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_5")

    @hmt2_5.setter
    def hmt2_5(self, value: float) -> None:
        """Set the hmt2_5 property."""
        self._cards[14].set_value("hmt2_5", value)

    @property
    def hmt2_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_6")

    @hmt2_6.setter
    def hmt2_6(self, value: float) -> None:
        """Set the hmt2_6 property."""
        self._cards[14].set_value("hmt2_6", value)

    @property
    def hmt2_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_7")

    @hmt2_7.setter
    def hmt2_7(self, value: float) -> None:
        """Set the hmt2_7 property."""
        self._cards[14].set_value("hmt2_7", value)

    @property
    def hmt2_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for t-axis at node 2.
        """ # nopep8
        return self._cards[14].get_value("hmt2_8")

    @hmt2_8.setter
    def hmt2_8(self, value: float) -> None:
        """Set the hmt2_8 property."""
        self._cards[14].set_value("hmt2_8", value)

    @property
    def lpmt2_1(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_1
        """ # nopep8
        return self._cards[15].get_value("lpmt2_1")

    @lpmt2_1.setter
    def lpmt2_1(self, value: int) -> None:
        """Set the lpmt2_1 property."""
        self._cards[15].set_value("lpmt2_1", value)

    @property
    def lpmt2_2(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_2
        """ # nopep8
        return self._cards[15].get_value("lpmt2_2")

    @lpmt2_2.setter
    def lpmt2_2(self, value: int) -> None:
        """Set the lpmt2_2 property."""
        self._cards[15].set_value("lpmt2_2", value)

    @property
    def lpmt2_3(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_3
        """ # nopep8
        return self._cards[15].get_value("lpmt2_3")

    @lpmt2_3.setter
    def lpmt2_3(self, value: int) -> None:
        """Set the lpmt2_3 property."""
        self._cards[15].set_value("lpmt2_3", value)

    @property
    def lpmt2_4(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_4
        """ # nopep8
        return self._cards[15].get_value("lpmt2_4")

    @lpmt2_4.setter
    def lpmt2_4(self, value: int) -> None:
        """Set the lpmt2_4 property."""
        self._cards[15].set_value("lpmt2_4", value)

    @property
    def lpmt2_5(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_5
        """ # nopep8
        return self._cards[15].get_value("lpmt2_5")

    @lpmt2_5.setter
    def lpmt2_5(self, value: int) -> None:
        """Set the lpmt2_5 property."""
        self._cards[15].set_value("lpmt2_5", value)

    @property
    def lpmt2_6(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_6
        """ # nopep8
        return self._cards[15].get_value("lpmt2_6")

    @lpmt2_6.setter
    def lpmt2_6(self, value: int) -> None:
        """Set the lpmt2_6 property."""
        self._cards[15].set_value("lpmt2_6", value)

    @property
    def lpmt2_7(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_7
        """ # nopep8
        return self._cards[15].get_value("lpmt2_7")

    @lpmt2_7.setter
    def lpmt2_7(self, value: int) -> None:
        """Set the lpmt2_7 property."""
        self._cards[15].set_value("lpmt2_7", value)

    @property
    def lpmt2_8(self) -> typing.Optional[int]:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the t-axis at node 2 for hinge moment HMT2_8
        """ # nopep8
        return self._cards[15].get_value("lpmt2_8")

    @lpmt2_8.setter
    def lpmt2_8(self, value: int) -> None:
        """Set the lpmt2_8 property."""
        self._cards[15].set_value("lpmt2_8", value)

    @property
    def hmr_1(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_1")

    @hmr_1.setter
    def hmr_1(self, value: float) -> None:
        """Set the hmr_1 property."""
        self._cards[16].set_value("hmr_1", value)

    @property
    def hmr_2(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_2")

    @hmr_2.setter
    def hmr_2(self, value: float) -> None:
        """Set the hmr_2 property."""
        self._cards[16].set_value("hmr_2", value)

    @property
    def hmr_3(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_3")

    @hmr_3.setter
    def hmr_3(self, value: float) -> None:
        """Set the hmr_3 property."""
        self._cards[16].set_value("hmr_3", value)

    @property
    def hmr_4(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_4")

    @hmr_4.setter
    def hmr_4(self, value: float) -> None:
        """Set the hmr_4 property."""
        self._cards[16].set_value("hmr_4", value)

    @property
    def hmr_5(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_5")

    @hmr_5.setter
    def hmr_5(self, value: float) -> None:
        """Set the hmr_5 property."""
        self._cards[16].set_value("hmr_5", value)

    @property
    def hmr_6(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_6")

    @hmr_6.setter
    def hmr_6(self, value: float) -> None:
        """Set the hmr_6 property."""
        self._cards[16].set_value("hmr_6", value)

    @property
    def hmr_7(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_7")

    @hmr_7.setter
    def hmr_7(self, value: float) -> None:
        """Set the hmr_7 property."""
        self._cards[16].set_value("hmr_7", value)

    @property
    def hmr_8(self) -> typing.Optional[float]:
        """Get or set the Hinge moment for the torsional axis.
        """ # nopep8
        return self._cards[16].get_value("hmr_8")

    @hmr_8.setter
    def hmr_8(self, value: float) -> None:
        """Set the hmr_8 property."""
        self._cards[16].set_value("hmr_8", value)

    @property
    def lpmr_1(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_1
        """ # nopep8
        return self._cards[17].get_value("lpmr_1")

    @lpmr_1.setter
    def lpmr_1(self, value: int) -> None:
        """Set the lpmr_1 property."""
        self._cards[17].set_value("lpmr_1", value)

    @property
    def lpmr_2(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_2
        """ # nopep8
        return self._cards[17].get_value("lpmr_2")

    @lpmr_2.setter
    def lpmr_2(self, value: int) -> None:
        """Set the lpmr_2 property."""
        self._cards[17].set_value("lpmr_2", value)

    @property
    def lpmr_3(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_3
        """ # nopep8
        return self._cards[17].get_value("lpmr_3")

    @lpmr_3.setter
    def lpmr_3(self, value: int) -> None:
        """Set the lpmr_3 property."""
        self._cards[17].set_value("lpmr_3", value)

    @property
    def lpmr_4(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_4
        """ # nopep8
        return self._cards[17].get_value("lpmr_4")

    @lpmr_4.setter
    def lpmr_4(self, value: int) -> None:
        """Set the lpmr_4 property."""
        self._cards[17].set_value("lpmr_4", value)

    @property
    def lpmr_5(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_5
        """ # nopep8
        return self._cards[17].get_value("lpmr_5")

    @lpmr_5.setter
    def lpmr_5(self, value: int) -> None:
        """Set the lpmr_5 property."""
        self._cards[17].set_value("lpmr_5", value)

    @property
    def lpmr_6(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_6
        """ # nopep8
        return self._cards[17].get_value("lpmr_6")

    @lpmr_6.setter
    def lpmr_6(self, value: int) -> None:
        """Set the lpmr_6 property."""
        self._cards[17].set_value("lpmr_6", value)

    @property
    def lpmr_7(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_7
        """ # nopep8
        return self._cards[17].get_value("lpmr_7")

    @lpmr_7.setter
    def lpmr_7(self, value: int) -> None:
        """Set the lpmr_7 property."""
        self._cards[17].set_value("lpmr_7", value)

    @property
    def lpmr_8(self) -> int:
        """Get or set the ID of curve defining plastic moment as a function of plastic rotation for the torsional axis for hinge moment HMR_8
        """ # nopep8
        return self._cards[17].get_value("lpmr_8")

    @lpmr_8.setter
    def lpmr_8(self, value: int) -> None:
        """Set the lpmr_8 property."""
        self._cards[17].set_value("lpmr_8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[18].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[18].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lc1_link(self) -> typing.Optional[DefineCurve]:
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
    def lc2_link(self) -> typing.Optional[DefineCurve]:
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
    def lc3_link(self) -> typing.Optional[DefineCurve]:
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
    def lc4_link(self) -> typing.Optional[DefineCurve]:
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
    def lc5_link(self) -> typing.Optional[DefineCurve]:
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
    def lc6_link(self) -> typing.Optional[DefineCurve]:
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
    def lc7_link(self) -> typing.Optional[DefineCurve]:
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
    def lc8_link(self) -> typing.Optional[DefineCurve]:
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
    def lps1_link(self) -> typing.Optional[DefineCurve]:
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
    def lps2_link(self) -> typing.Optional[DefineCurve]:
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
    def lpt1_link(self) -> typing.Optional[DefineCurve]:
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
    def lpt2_link(self) -> typing.Optional[DefineCurve]:
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
    def lpr_link(self) -> typing.Optional[DefineCurve]:
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

    @property
    def lys1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lys1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lys1:
                return kwd
        return None

    @lys1_link.setter
    def lys1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lys1."""
        self.lys1 = value.lcid

    @property
    def lys2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lys2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lys2:
                return kwd
        return None

    @lys2_link.setter
    def lys2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lys2."""
        self.lys2 = value.lcid

    @property
    def lyt1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lyt1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lyt1:
                return kwd
        return None

    @lyt1_link.setter
    def lyt1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lyt1."""
        self.lyt1 = value.lcid

    @property
    def lyt2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lyt2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lyt2:
                return kwd
        return None

    @lyt2_link.setter
    def lyt2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lyt2."""
        self.lyt2 = value.lcid

    @property
    def lyr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lyr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lyr:
                return kwd
        return None

    @lyr_link.setter
    def lyr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lyr."""
        self.lyr = value.lcid

    @property
    def lpms1_1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_1:
                return kwd
        return None

    @lpms1_1_link.setter
    def lpms1_1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_1."""
        self.lpms1_1 = value.lcid

    @property
    def lpms1_2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_2:
                return kwd
        return None

    @lpms1_2_link.setter
    def lpms1_2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_2."""
        self.lpms1_2 = value.lcid

    @property
    def lpms1_3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_3:
                return kwd
        return None

    @lpms1_3_link.setter
    def lpms1_3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_3."""
        self.lpms1_3 = value.lcid

    @property
    def lpms1_4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_4:
                return kwd
        return None

    @lpms1_4_link.setter
    def lpms1_4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_4."""
        self.lpms1_4 = value.lcid

    @property
    def lpms1_5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_5:
                return kwd
        return None

    @lpms1_5_link.setter
    def lpms1_5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_5."""
        self.lpms1_5 = value.lcid

    @property
    def lpms1_6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_6:
                return kwd
        return None

    @lpms1_6_link.setter
    def lpms1_6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_6."""
        self.lpms1_6 = value.lcid

    @property
    def lpms1_7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_7:
                return kwd
        return None

    @lpms1_7_link.setter
    def lpms1_7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_7."""
        self.lpms1_7 = value.lcid

    @property
    def lpms1_8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms1_8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms1_8:
                return kwd
        return None

    @lpms1_8_link.setter
    def lpms1_8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms1_8."""
        self.lpms1_8 = value.lcid

    @property
    def lpms2_1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_1:
                return kwd
        return None

    @lpms2_1_link.setter
    def lpms2_1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_1."""
        self.lpms2_1 = value.lcid

    @property
    def lpms2_2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_2:
                return kwd
        return None

    @lpms2_2_link.setter
    def lpms2_2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_2."""
        self.lpms2_2 = value.lcid

    @property
    def lpms2_3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_3:
                return kwd
        return None

    @lpms2_3_link.setter
    def lpms2_3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_3."""
        self.lpms2_3 = value.lcid

    @property
    def lpms2_4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_4:
                return kwd
        return None

    @lpms2_4_link.setter
    def lpms2_4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_4."""
        self.lpms2_4 = value.lcid

    @property
    def lpms2_5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_5:
                return kwd
        return None

    @lpms2_5_link.setter
    def lpms2_5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_5."""
        self.lpms2_5 = value.lcid

    @property
    def lpms2_6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_6:
                return kwd
        return None

    @lpms2_6_link.setter
    def lpms2_6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_6."""
        self.lpms2_6 = value.lcid

    @property
    def lpms2_7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_7:
                return kwd
        return None

    @lpms2_7_link.setter
    def lpms2_7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_7."""
        self.lpms2_7 = value.lcid

    @property
    def lpms2_8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpms2_8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpms2_8:
                return kwd
        return None

    @lpms2_8_link.setter
    def lpms2_8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpms2_8."""
        self.lpms2_8 = value.lcid

    @property
    def lpmt1_1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_1:
                return kwd
        return None

    @lpmt1_1_link.setter
    def lpmt1_1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_1."""
        self.lpmt1_1 = value.lcid

    @property
    def lpmt1_2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_2:
                return kwd
        return None

    @lpmt1_2_link.setter
    def lpmt1_2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_2."""
        self.lpmt1_2 = value.lcid

    @property
    def lpmt1_3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_3:
                return kwd
        return None

    @lpmt1_3_link.setter
    def lpmt1_3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_3."""
        self.lpmt1_3 = value.lcid

    @property
    def lpmt1_4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_4:
                return kwd
        return None

    @lpmt1_4_link.setter
    def lpmt1_4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_4."""
        self.lpmt1_4 = value.lcid

    @property
    def lpmt1_5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_5:
                return kwd
        return None

    @lpmt1_5_link.setter
    def lpmt1_5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_5."""
        self.lpmt1_5 = value.lcid

    @property
    def lpmt1_6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_6:
                return kwd
        return None

    @lpmt1_6_link.setter
    def lpmt1_6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_6."""
        self.lpmt1_6 = value.lcid

    @property
    def lpmt1_7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_7:
                return kwd
        return None

    @lpmt1_7_link.setter
    def lpmt1_7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_7."""
        self.lpmt1_7 = value.lcid

    @property
    def lpmt1_8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt1_8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt1_8:
                return kwd
        return None

    @lpmt1_8_link.setter
    def lpmt1_8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt1_8."""
        self.lpmt1_8 = value.lcid

    @property
    def lpmt2_1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_1:
                return kwd
        return None

    @lpmt2_1_link.setter
    def lpmt2_1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_1."""
        self.lpmt2_1 = value.lcid

    @property
    def lpmt2_2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_2:
                return kwd
        return None

    @lpmt2_2_link.setter
    def lpmt2_2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_2."""
        self.lpmt2_2 = value.lcid

    @property
    def lpmt2_3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_3:
                return kwd
        return None

    @lpmt2_3_link.setter
    def lpmt2_3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_3."""
        self.lpmt2_3 = value.lcid

    @property
    def lpmt2_4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_4:
                return kwd
        return None

    @lpmt2_4_link.setter
    def lpmt2_4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_4."""
        self.lpmt2_4 = value.lcid

    @property
    def lpmt2_5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_5:
                return kwd
        return None

    @lpmt2_5_link.setter
    def lpmt2_5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_5."""
        self.lpmt2_5 = value.lcid

    @property
    def lpmt2_6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_6:
                return kwd
        return None

    @lpmt2_6_link.setter
    def lpmt2_6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_6."""
        self.lpmt2_6 = value.lcid

    @property
    def lpmt2_7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_7:
                return kwd
        return None

    @lpmt2_7_link.setter
    def lpmt2_7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_7."""
        self.lpmt2_7 = value.lcid

    @property
    def lpmt2_8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmt2_8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmt2_8:
                return kwd
        return None

    @lpmt2_8_link.setter
    def lpmt2_8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmt2_8."""
        self.lpmt2_8 = value.lcid

    @property
    def lpmr_1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_1:
                return kwd
        return None

    @lpmr_1_link.setter
    def lpmr_1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_1."""
        self.lpmr_1 = value.lcid

    @property
    def lpmr_2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_2:
                return kwd
        return None

    @lpmr_2_link.setter
    def lpmr_2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_2."""
        self.lpmr_2 = value.lcid

    @property
    def lpmr_3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_3:
                return kwd
        return None

    @lpmr_3_link.setter
    def lpmr_3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_3."""
        self.lpmr_3 = value.lcid

    @property
    def lpmr_4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_4:
                return kwd
        return None

    @lpmr_4_link.setter
    def lpmr_4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_4."""
        self.lpmr_4 = value.lcid

    @property
    def lpmr_5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_5:
                return kwd
        return None

    @lpmr_5_link.setter
    def lpmr_5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_5."""
        self.lpmr_5 = value.lcid

    @property
    def lpmr_6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_6:
                return kwd
        return None

    @lpmr_6_link.setter
    def lpmr_6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_6."""
        self.lpmr_6 = value.lcid

    @property
    def lpmr_7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_7:
                return kwd
        return None

    @lpmr_7_link.setter
    def lpmr_7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_7."""
        self.lpmr_7 = value.lcid

    @property
    def lpmr_8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lpmr_8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lpmr_8:
                return kwd
        return None

    @lpmr_8_link.setter
    def lpmr_8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lpmr_8."""
        self.lpmr_8 = value.lcid

