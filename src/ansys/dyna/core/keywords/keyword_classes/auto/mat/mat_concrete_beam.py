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

"""Module providing the MatConcreteBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATCONCRETEBEAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("etan", float, 50, 10, None),
    FieldSchema("fail", float, 60, 10, 1e+21),
    FieldSchema("tdel", float, 70, 10, 1e+21),
)

_MATCONCRETEBEAM_CARD1 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lcss", int, 20, 10, 0),
    FieldSchema("lcsr", int, 30, 10, 0),
)

_MATCONCRETEBEAM_CARD2 = (
    FieldSchema("noten", int, 0, 10, 0),
    FieldSchema("tencut", float, 10, 10, 1000000000000000.0),
    FieldSchema("sdr", float, 20, 10, None),
)

_MATCONCRETEBEAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatConcreteBeam(KeywordBase):
    """DYNA MAT_CONCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "CONCRETE_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatConcreteBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCONCRETEBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCONCRETEBEAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCONCRETEBEAM_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatConcreteBeam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCONCRETEBEAM_OPTION0_CARD0,
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus.
        Ignored if LCSS.GT.0 is defined.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[0].set_value("etan", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure flag:
        LT.0.0: user defined failure subroutine is called to determine failure
        EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many caluculations will be saved (default),
        GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> float:
        """Get or set the Minimum time step size for automatic element deletion.
        Default is set to 10.0E+20
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        """Set the tdel property."""
        self._cards[0].set_value("tdel", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter.
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[1].set_value("p", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID or Table ID. Load curve ID defining effective stress versus effective plastic strain. The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[1].set_value("lcss", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID defining strain rate scaling effect on yield stress.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        """Set the lcsr property."""
        self._cards[1].set_value("lcsr", value)

    @property
    def noten(self) -> int:
        """Get or set the No-tension flag:
        EQ:0 beam takes tension,
        EQ:1 beam takes no tension,
        EQ:2 beam takes tension up to value given by TENCUT.
        """ # nopep8
        return self._cards[2].get_value("noten")

    @noten.setter
    def noten(self, value: int) -> None:
        """Set the noten property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""noten must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("noten", value)

    @property
    def tencut(self) -> float:
        """Get or set the Tension cutoff value.
        """ # nopep8
        return self._cards[2].get_value("tencut")

    @tencut.setter
    def tencut(self, value: float) -> None:
        """Set the tencut property."""
        self._cards[2].set_value("tencut", value)

    @property
    def sdr(self) -> typing.Optional[float]:
        """Get or set the Stiffness degradation factor.
        Default is set to 0.0
        """ # nopep8
        return self._cards[2].get_value("sdr")

    @sdr.setter
    def sdr(self, value: float) -> None:
        """Set the sdr property."""
        self._cards[2].set_value("sdr", value)

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

