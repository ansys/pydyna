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

"""Module providing the MatAddBasicIncrementalFailure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDBASICINCREMENTALFAILURE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("numfip", float, 20, 10, 1.0),
    FieldSchema("volfrac", float, 30, 10, 0.5),
    FieldSchema("nerod", float, 40, 10, 0.0),
)

_MATADDBASICINCREMENTALFAILURE_CARD1 = (
    FieldSchema("epsf", float, 0, 10, 0.0),
    FieldSchema("lcss", int, 10, 10, 0),
    FieldSchema("lcregd", int, 20, 10, 0),
    FieldSchema("lcsrs", int, 30, 10, 0),
    FieldSchema("dmgexp", float, 40, 10, 1.0),
)

_MATADDBASICINCREMENTALFAILURE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddBasicIncrementalFailure(KeywordBase):
    """DYNA MAT_ADD_BASIC_INCREMENTAL_FAILURE keyword"""

    keyword = "MAT"
    subkeyword = "ADD_BASIC_INCREMENTAL_FAILURE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "mid": LinkType.MAT,
        "lcss": LinkType.DEFINE_CURVE,
        "lcregd": LinkType.DEFINE_CURVE,
        "lcsrs": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddBasicIncrementalFailure class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDBASICINCREMENTALFAILURE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDBASICINCREMENTALFAILURE_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatAddBasicIncrementalFailure._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDBASICINCREMENTALFAILURE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification for which this erosion definition applies
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
        NUMFIP does not apply to higher-order solid element types 24, 25, 26, 27, 28, and 29; rather, see the variable VOLFRAC.
        Also, when the material is a composite defined with *PART_COMPOSITE with different materials through-the-thickness,
        do not use NUMFIP; use *DEFINE_ELEMENT_EROSION instead.
        GT.0.0: Number of integration points that must fail before an element is deleted.
        LT.0.0: Applies only to shells. | "NUMFIP" | is the percentage of layers that must fail before an element is deleted.
        For shell formulations with 4 integration points per layer, the layer is considered failed if any of the integration points in the layer fails.
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        """Set the numfip property."""
        self._cards[0].set_value("numfip", value)

    @property
    def volfrac(self) -> float:
        """Get or set the Volume fraction required to fail before element deletion. The default is 0.5.
        It is used for higher-order solid element types 24, 25, 26, 27, 28, and 29, and all isogeometric solids and shell elements. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("volfrac")

    @volfrac.setter
    def volfrac(self, value: float) -> None:
        """Set the volfrac property."""
        self._cards[0].set_value("volfrac", value)

    @property
    def nerod(self) -> float:
        """Get or set the Option to turn off element erosion:
        EQ.0.0: Elements erode according to the definition of NUMFIP.
        EQ.1.0: Damage does not affect stresses,and elements do not erode.It could be used solely for post - processing damage.
        """ # nopep8
        return self._cards[0].get_value("nerod")

    @nerod.setter
    def nerod(self, value: float) -> None:
        """Set the nerod property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""nerod must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("nerod", value)

    @property
    def epsf(self) -> float:
        """Get or set the Plastic failure strain under uniaxial tension, meaning at a triaxiality of 1/3 and a Lode parameter of 1.0.
        """ # nopep8
        return self._cards[1].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        """Set the epsf property."""
        self._cards[1].set_value("epsf", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID of the related material's stress-strain curve (hardening curve). If zero, then the appropriate curve
        of the associated material model is automatically picked (currently supported: 3, 24, 36, 81, 120, 123, 124, 133, 187, 224, 243, 251, 324).
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[1].set_value("lcss", value)

    @property
    def lcregd(self) -> int:
        """Get or set the Load curve ID defining the failure strain scaling factor as a function of element size.
        """ # nopep8
        return self._cards[1].get_value("lcregd")

    @lcregd.setter
    def lcregd(self, value: int) -> None:
        """Set the lcregd property."""
        self._cards[1].set_value("lcregd", value)

    @property
    def lcsrs(self) -> int:
        """Get or set the Load curve ID defining the failure strain scaling factor as a function of strain rate.
        If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given
        as a natural logarithm of the strain rate. The curve should not extrapolate to zero; otherwise, failure may occur at low strains.
        """ # nopep8
        return self._cards[1].get_value("lcsrs")

    @lcsrs.setter
    def lcsrs(self, value: int) -> None:
        """Set the lcsrs property."""
        self._cards[1].set_value("lcsrs", value)

    @property
    def dmgexp(self) -> float:
        """Get or set the Exponent for nonlinear damage accumulation
        """ # nopep8
        return self._cards[1].get_value("dmgexp")

    @dmgexp.setter
    def dmgexp(self, value: float) -> None:
        """Set the dmgexp property."""
        self._cards[1].set_value("dmgexp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def mid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

    @property
    def lcss_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcss."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcss:
                return kwd
        return None

    @lcss_link.setter
    def lcss_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcss."""
        self.lcss = value.lcid

    @property
    def lcregd_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcregd."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcregd:
                return kwd
        return None

    @lcregd_link.setter
    def lcregd_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcregd."""
        self.lcregd = value.lcid

    @property
    def lcsrs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsrs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsrs:
                return kwd
        return None

    @lcsrs_link.setter
    def lcsrs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsrs."""
        self.lcsrs = value.lcid

