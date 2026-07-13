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

"""Module providing the Mat125 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT125_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("rbar", float, 40, 10, None),
    FieldSchema("hclid", int, 50, 10, 0),
    FieldSchema("opt", int, 60, 10, 0),
)

_MAT125_CARD1 = (
    FieldSchema("cb", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("sc1", float, 20, 10, None),
    FieldSchema("k", float, 30, 10, None),
    FieldSchema("rsat", float, 40, 10, None),
    FieldSchema("sb", float, 50, 10, None),
    FieldSchema("h", float, 60, 10, None),
    FieldSchema("sc2", float, 70, 10, 0.0),
)

_MAT125_CARD2 = (
    FieldSchema("ea", float, 0, 10, None),
    FieldSchema("coe", float, 10, 10, None),
    FieldSchema("iopt", int, 20, 10, 0),
    FieldSchema("c1", float, 30, 10, None),
    FieldSchema("c2", float, 40, 10, None),
    FieldSchema("ifld", int, 50, 10, None),
)

_MAT125_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat125(KeywordBase):
    """DYNA MAT_125 keyword"""

    keyword = "MAT"
    subkeyword = "125"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "hclid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat125 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT125_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT125_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT125_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat125._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT125_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
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
    def rbar(self) -> typing.Optional[float]:
        """Get or set the Plastic anisotropic parameter r(Lankford coefficient), also commonly referred to as "r-bar value" in sheet metal forming literature. For shell elements, r =R00=R45=R90 is assumed in the plane of the shell.
        """ # nopep8
        return self._cards[0].get_value("rbar")

    @rbar.setter
    def rbar(self, value: float) -> None:
        """Set the rbar property."""
        self._cards[0].set_value("rbar", value)

    @property
    def hclid(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) giving true strain as a function of true stress. This curve is used with OPT below and should not be referenced or used in other keywords.
        Using this parameter is not recommended
        """ # nopep8
        return self._cards[0].get_value("hclid")

    @hclid.setter
    def hclid(self, value: int) -> None:
        """Set the hclid property."""
        self._cards[0].set_value("hclid", value)

    @property
    def opt(self) -> int:
        """Get or set the Error calculation flag. The default value of "0" is recommended.
        EQ.2: Perform the error calculation based on the true stress-strain curve from uniaxial tension, specified by HLCID.
        The corrections will be made to the cyclic load curve, both in the loading and unloading portions. Since, in some cases where loading is more complex,
        the accumulated plastic strain could be large (say more than 30%), the input uniaxial stress-strain curve must have enough strain range to cover
        the maximum expected plastic strain. Note that this variable must be set to a value of "2" if HLCID is specified and a stress-strain curve is used.
        """ # nopep8
        return self._cards[0].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        self._cards[0].set_value("opt", value)

    @property
    def cb(self) -> typing.Optional[float]:
        """Get or set the Uppercase B defined in Yoshida and Uemori [2002]. It is the initial size of the bounding surface. See Equations 5, 7b, 21, and 43a and section 2.5 in the paper.
        """ # nopep8
        return self._cards[1].get_value("cb")

    @cb.setter
    def cb(self, value: float) -> None:
        """Set the cb property."""
        self._cards[1].set_value("cb", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter appearing in Yoshida and Uemori [2002] and Yoshida and Uemori [2003]. It gives the yield surface's radius in the deviatoric stress space. See Equations 4a, 7b, and 43a and section 2.5 in Yoshida and Uemori [2002].
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def sc1(self) -> typing.Optional[float]:
        """Get or set the C_1 in Equation 43a of Yoshida and Uemori [2002]. Note that SC1 must be greater than SC2.
        """ # nopep8
        return self._cards[1].get_value("sc1")

    @sc1.setter
    def sc1(self, value: float) -> None:
        """Set the sc1 property."""
        self._cards[1].set_value("sc1", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter appearing in Equations 11, 12, 13, and 14 and section 2.4 of Yoshida and Uemori [2003]. In Yoshida and Uemori [2002], this parameter is called m and is in Equations 19 and 20 and section 2.5.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def rsat(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter, R_sat , appearing in Yoshida and Uemori [2002] and Yoshida and Uemori [2003]. See Equations 19 and 21 and section 2.5 of Yoshida and Uemori [2002].
        """ # nopep8
        return self._cards[1].get_value("rsat")

    @rsat.setter
    def rsat(self, value: float) -> None:
        """Set the rsat property."""
        self._cards[1].set_value("rsat", value)

    @property
    def sb(self) -> typing.Optional[float]:
        """Get or set the The lowercase b material parameter appearing in Yoshida and Uemori [2002] (see Equation 20 and section 2.5 of the paper) and Yoshida and Uemori [2003]
        """ # nopep8
        return self._cards[1].get_value("sb")

    @sb.setter
    def sb(self, value: float) -> None:
        """Set the sb property."""
        self._cards[1].set_value("sb", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameter associated with work-hardening stagnation. See Equations 26a, 27, and 30a and section 2.5 of Yoshida and Uemori [2002].
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def sc2(self) -> float:
        """Get or set the C_2 in Equation 43b of Yoshida and Uemori [2002]. See the description of SC1. If SC2 equals 0.0, is left blank, or equals SC1, then it turns into the basic model (the one c model).
        """ # nopep8
        return self._cards[1].get_value("sc2")

    @sc2.setter
    def sc2(self, value: float) -> None:
        """Set the sc2 property."""
        self._cards[1].set_value("sc2", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Parameter controlling the change of Young's modulus, E_a. in Equation 42 of Yoshida and Uemori [2002]
        """ # nopep8
        return self._cards[2].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[2].set_value("ea", value)

    @property
    def coe(self) -> typing.Optional[float]:
        """Get or set the Parameter controlling the change of Young's modulus, ξ, in Equation 42 of Yoshida and Uemori [2002]
        """ # nopep8
        return self._cards[2].get_value("coe")

    @coe.setter
    def coe(self, value: float) -> None:
        """Set the coe property."""
        self._cards[2].set_value("coe", value)

    @property
    def iopt(self) -> int:
        """Get or set the Modified kinematic hardening rule flag:
        EQ.0: Original Yoshida and Uemori formulation,
        EQ.1 : Modified formulation.Define C1 and C2 below
        """ # nopep8
        return self._cards[2].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        """Set the iopt property."""
        if value not in [0, 1, None]:
            raise Exception("""iopt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("iopt", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R ̇, so strain hardening will not saturate
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R ̇, so strain hardening will not saturate
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[2].set_value("c2", value)

    @property
    def ifld(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve defining Forming Limit Diagram (FLD) under
        linear strain paths. In the load curve, abscissas represent minor
        strains while ordinates represent major strains. Define only when
        the option NLP is used. See the example in the remarks section
        """ # nopep8
        return self._cards[2].get_value("ifld")

    @ifld.setter
    def ifld(self, value: int) -> None:
        """Set the ifld property."""
        self._cards[2].set_value("ifld", value)

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
    def hclid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for hclid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hclid:
                return kwd
        return None

    @hclid_link.setter
    def hclid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hclid."""
        self.hclid = value.lcid

