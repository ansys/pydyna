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

"""Module providing the MatHystereticReinforcement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATHYSTERETICREINFORCEMENT_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ym", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("lamda", float, 50, 10, None),
    FieldSchema("sbuck", float, 60, 10, None),
    FieldSchema("power", float, 70, 10, 0.5),
)

_MATHYSTERETICREINFORCEMENT_CARD1 = (
    FieldSchema("fracx", float, 0, 10, None),
    FieldSchema("fracy", float, 10, 10, None),
    FieldSchema("lcten", float, 20, 10, None),
    FieldSchema("lccomp", float, 30, 10, None),
    FieldSchema("aopt", float, 40, 10, None),
    FieldSchema("ebu", float, 50, 10, None),
    FieldSchema("downsl", float, 60, 10, 0.1),
)

_MATHYSTERETICREINFORCEMENT_CARD2 = (
    FieldSchema("dbar", float, 0, 10, None),
    FieldSchema("fcdow", float, 10, 10, None),
    FieldSchema("lchard", float, 20, 10, None),
    FieldSchema("unitc", float, 30, 10, 1.0),
    FieldSchema("unitl", float, 40, 10, 1.0),
)

_MATHYSTERETICREINFORCEMENT_CARD3 = (
    FieldSchema("epdam1", float, 0, 10, None),
    FieldSchema("epdam2", float, 10, 10, None),
    FieldSchema("dresid", float, 20, 10, None),
)

_MATHYSTERETICREINFORCEMENT_CARD4 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATHYSTERETICREINFORCEMENT_CARD5 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

class MatHystereticReinforcement(KeywordBase):
    """DYNA MAT_HYSTERETIC_REINFORCEMENT keyword"""

    keyword = "MAT"
    subkeyword = "HYSTERETIC_REINFORCEMENT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatHystereticReinforcement class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHYSTERETICREINFORCEMENT_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatHystereticReinforcement.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        """Set the ym property."""
        self._cards[0].set_value("ym", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's Ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def lamda(self) -> typing.Optional[float]:
        """Get or set the Slenderness ratio
        """ # nopep8
        return self._cards[0].get_value("lamda")

    @lamda.setter
    def lamda(self, value: float) -> None:
        """Set the lamda property."""
        self._cards[0].set_value("lamda", value)

    @property
    def sbuck(self) -> typing.Optional[float]:
        """Get or set the Initial buckling stress (should be positive)
        """ # nopep8
        return self._cards[0].get_value("sbuck")

    @sbuck.setter
    def sbuck(self, value: float) -> None:
        """Set the sbuck property."""
        self._cards[0].set_value("sbuck", value)

    @property
    def power(self) -> float:
        """Get or set the Power law for Bauschinger effect (non-dimensional)
        """ # nopep8
        return self._cards[0].get_value("power")

    @power.setter
    def power(self, value: float) -> None:
        """Set the power property."""
        self._cards[0].set_value("power", value)

    @property
    def fracx(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement at this integration point in local x direction
        """ # nopep8
        return self._cards[1].get_value("fracx")

    @fracx.setter
    def fracx(self, value: float) -> None:
        """Set the fracx property."""
        self._cards[1].set_value("fracx", value)

    @property
    def fracy(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement at this integration point in local y direction
        """ # nopep8
        return self._cards[1].get_value("fracy")

    @fracy.setter
    def fracy(self, value: float) -> None:
        """Set the fracy property."""
        self._cards[1].set_value("fracy", value)

    @property
    def lcten(self) -> typing.Optional[float]:
        """Get or set the Optional curve providing the factor on SIGY versus plastic strain (tension)
        """ # nopep8
        return self._cards[1].get_value("lcten")

    @lcten.setter
    def lcten(self, value: float) -> None:
        """Set the lcten property."""
        self._cards[1].set_value("lcten", value)

    @property
    def lccomp(self) -> typing.Optional[float]:
        """Get or set the Optional curve providing the factor on SBUCK versus plastic strain (compression)
        """ # nopep8
        return self._cards[1].get_value("lccomp")

    @lccomp.setter
    def lccomp(self, value: float) -> None:
        """Set the lccomp property."""
        self._cards[1].set_value("lccomp", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Option for local axis alignment  - see material type 2
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[1].set_value("aopt", value)

    @property
    def ebu(self) -> typing.Optional[float]:
        """Get or set the Optional buckling strain (if defined, overrides LAMBDA)
        """ # nopep8
        return self._cards[1].get_value("ebu")

    @ebu.setter
    def ebu(self, value: float) -> None:
        """Set the ebu property."""
        self._cards[1].set_value("ebu", value)

    @property
    def downsl(self) -> float:
        """Get or set the Initial down-slope of buckling curve as a fraction of YM (dimensionless)
        """ # nopep8
        return self._cards[1].get_value("downsl")

    @downsl.setter
    def downsl(self, value: float) -> None:
        """Set the downsl property."""
        self._cards[1].set_value("downsl", value)

    @property
    def dbar(self) -> typing.Optional[float]:
        """Get or set the Reinforcement bar diameter used for dowel action. See remarks.
        """ # nopep8
        return self._cards[2].get_value("dbar")

    @dbar.setter
    def dbar(self, value: float) -> None:
        """Set the dbar property."""
        self._cards[2].set_value("dbar", value)

    @property
    def fcdow(self) -> typing.Optional[float]:
        """Get or set the Concrete compressive strength used for dowel action. See notes.	This field has units of stress
        """ # nopep8
        return self._cards[2].get_value("fcdow")

    @fcdow.setter
    def fcdow(self, value: float) -> None:
        """Set the fcdow property."""
        self._cards[2].set_value("fcdow", value)

    @property
    def lchard(self) -> typing.Optional[float]:
        """Get or set the Characteristic length for dowel action (length units)
        """ # nopep8
        return self._cards[2].get_value("lchard")

    @lchard.setter
    def lchard(self, value: float) -> None:
        """Set the lchard property."""
        self._cards[2].set_value("lchard", value)

    @property
    def unitc(self) -> float:
        """Get or set the Factor to convert model stress units to MPa, e.g. is model units are Newtons and meters, UNITC = 10e-6 , [UNITC] = 1/[STRESS].
        """ # nopep8
        return self._cards[2].get_value("unitc")

    @unitc.setter
    def unitc(self, value: float) -> None:
        """Set the unitc property."""
        self._cards[2].set_value("unitc", value)

    @property
    def unitl(self) -> float:
        """Get or set the Factor to convert model length units to millimeters, e.g. if model	units are meters, UNITL = 1000, [UNITL] = 1/[LENGTH].
        """ # nopep8
        return self._cards[2].get_value("unitl")

    @unitl.setter
    def unitl(self, value: float) -> None:
        """Set the unitl property."""
        self._cards[2].set_value("unitl", value)

    @property
    def epdam1(self) -> typing.Optional[float]:
        """Get or set the Accumulated plastic strain at which hysteretic damage begins
        """ # nopep8
        return self._cards[3].get_value("epdam1")

    @epdam1.setter
    def epdam1(self, value: float) -> None:
        """Set the epdam1 property."""
        self._cards[3].set_value("epdam1", value)

    @property
    def epdam2(self) -> typing.Optional[float]:
        """Get or set the Accumulated plastic strain at which hysteretic damage is complete
        """ # nopep8
        return self._cards[3].get_value("epdam2")

    @epdam2.setter
    def epdam2(self, value: float) -> None:
        """Set the epdam2 property."""
        self._cards[3].set_value("epdam2", value)

    @property
    def dresid(self) -> typing.Optional[float]:
        """Get or set the Residual factor remaining after hysteretic damage
        """ # nopep8
        return self._cards[3].get_value("dresid")

    @dresid.setter
    def dresid(self, value: float) -> None:
        """Set the dresid property."""
        self._cards[3].set_value("dresid", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see MAT 2)
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[5].set_value("beta", value)

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

