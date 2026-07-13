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

"""Module providing the DefineDeBond class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDEBOND_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("bdform", int, 20, 10, 1),
    FieldSchema("idist", int, 30, 10, 0),
    FieldSchema("maxcn", int, 40, 10, 0),
    FieldSchema("binary", int, 50, 10, 0),
)

_DEFINEDEBOND_CARD1 = (
    FieldSchema("pbn", float, 0, 10, None),
    FieldSchema("pbs", float, 10, 10, None),
    FieldSchema("pbns", float, 20, 10, None),
    FieldSchema("pbss", float, 30, 10, None),
    FieldSchema("sfa", float, 40, 10, 1.0),
    FieldSchema("alpha", float, 50, 10, 0.0),
    FieldSchema("bendsf", float, 60, 10, 1.0),
    FieldSchema("maxgap", float, 70, 10, 0.0001),
)

_DEFINEDEBOND_CARD2 = (
    FieldSchema("arsp", float, 0, 10, None),
    FieldSchema("brsp", float, 10, 10, None),
)

_DEFINEDEBOND_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeBond(KeywordBase):
    """DYNA DEFINE_DE_BOND keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_BOND"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeBond class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEBOND_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDEBOND_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDEBOND_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeBond._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEBOND_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set, part set, or part ID for which bond properties apply
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the EQ.0: DES node set
        EQ.2: DES part set
        EQ.3: DES part
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,2,3}.""")
        self._cards[0].set_value("stype", value)

    @property
    def bdform(self) -> int:
        """Get or set the Bond formulation:
        EQ.1: Linear bond formulation(default)
        EQ.3: Bond strengths that increases linearly with deformation rate.See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("bdform")

    @bdform.setter
    def bdform(self, value: int) -> None:
        """Set the bdform property."""
        if value not in [1, 3, None]:
            raise Exception("""bdform must be `None` or one of {1,3}.""")
        self._cards[0].set_value("bdform", value)

    @property
    def idist(self) -> int:
        """Get or set the Distribution of bond properties:
        EQ. - 2: Weibull distribution(non - deterministic).See Remark 5
        EQ. - 1: Gaussian distribution(non - deterministic).See Remark 4
        EQ.0: Single property(default)
        EQ.1: Gaussian distribution(deterministic).See Remark 4
        EQ.2: Weibull distribution(deterministic).See Remark 5
        """ # nopep8
        return self._cards[0].get_value("idist")

    @idist.setter
    def idist(self, value: int) -> None:
        """Set the idist property."""
        if value not in [0, 1, 2, -1, -2, None]:
            raise Exception("""idist must be `None` or one of {0,1,2,-1,-2}.""")
        self._cards[0].set_value("idist", value)

    @property
    def maxcn(self) -> int:
        """Get or set the Maximum coordination number for outputting the DES particles that belong to a bond. If the coordination number of a DES particle that belongs to a bond is less than or equal to this value, the DES particle is output at the initial time state.
        EQ.0: Do not print out any bonded DES particles at the initial time state.
        """ # nopep8
        return self._cards[0].get_value("maxcn")

    @maxcn.setter
    def maxcn(self, value: int) -> None:
        """Set the maxcn property."""
        self._cards[0].set_value("maxcn", value)

    @property
    def binary(self) -> int:
        """Get or set the Output file type for printing the bonded DES particle if the coordination number is less than or equal to MAXCN:
        EQ.0: Do not write the data to a file.
        EQ.1: Write an ASCII file.
        EQ.2: Write the data to binary database binout.
        EQ.3: Write the data to an ASCII file and to the binary database binout.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""binary must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("binary", value)

    @property
    def pbn(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond modulus [Pa]. See Remarks 1 and 2. For IDIST=-1, and 1, this value is the mean. For IDIST=-2, and 2 with MPBN!=0, this value is the scale parameter
        """ # nopep8
        return self._cards[1].get_value("pbn")

    @pbn.setter
    def pbn(self, value: float) -> None:
        """Set the pbn property."""
        self._cards[1].set_value("pbn", value)

    @property
    def pbs(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond stiffness ratio. shear stiffness/normal stiffness. See Remark 2. For IDIST=-1, and 1, this value is the mean. For IDIST=-2, and 2 and MPBN!=0, this value is the scale parameter.
        """ # nopep8
        return self._cards[1].get_value("pbs")

    @pbs.setter
    def pbs(self, value: float) -> None:
        """Set the pbs property."""
        self._cards[1].set_value("pbs", value)

    @property
    def pbns(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress. For IDIST=-1,and 1, this value is the mean. For IDIST=-2, and 2 with MPBN!=0, this value is the scale parameter
        """ # nopep8
        return self._cards[1].get_value("pbns")

    @pbns.setter
    def pbns(self, value: float) -> None:
        """Set the pbns property."""
        self._cards[1].set_value("pbns", value)

    @property
    def pbss(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress. For IDIST=-1, and 1, this value is the mean. For IDIST=-2, and 2 with MPBN!=0, this value is the scale parameter.
        """ # nopep8
        return self._cards[1].get_value("pbss")

    @pbss.setter
    def pbss(self, value: float) -> None:
        """Set the pbss property."""
        self._cards[1].set_value("pbss", value)

    @property
    def sfa(self) -> float:
        """Get or set the Bond radius multiplier.
        """ # nopep8
        return self._cards[1].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[1].set_value("sfa", value)

    @property
    def alpha(self) -> float:
        """Get or set the Numerical damping, 0.0 <= ALPHA <= 1.0
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def bendsf(self) -> float:
        """Get or set the Influence of bending/twisting on bond failure criteria (see Remark 6).
        EQ. - 1.0: No bending / twisting is considered in bond failure criteria.
        EQ.0.0: Defaults to 1.0.
        GT.0.0: Scale factor for the bending / twisting component in bond failure criteria.
        """ # nopep8
        return self._cards[1].get_value("bendsf")

    @bendsf.setter
    def bendsf(self, value: float) -> None:
        """Set the bendsf property."""
        self._cards[1].set_value("bendsf", value)

    @property
    def maxgap(self) -> float:
        """Get or set the Maximum gap between two bonded spheres
        GT.0.0: defines the ratio of the smaller radius of two bonded spheres as the maximum gap, i.e. MAXGAPxmin(r1,r2)
        LT.0.0: absolute value is used as the maximum gap.
        """ # nopep8
        return self._cards[1].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        """Set the maxgap property."""
        self._cards[1].set_value("maxgap", value)

    @property
    def arsp(self) -> typing.Optional[float]:
        """Get or set the Deformation rate sensitivity parameter for parallel-bond maximum normal stress. A zero value ignores deformation rate effects on bond strength
        """ # nopep8
        return self._cards[2].get_value("arsp")

    @arsp.setter
    def arsp(self, value: float) -> None:
        """Set the arsp property."""
        self._cards[2].set_value("arsp", value)

    @property
    def brsp(self) -> typing.Optional[float]:
        """Get or set the Deformation rate sensitivity parameter for parallel-bond maximum shear stress. A zero value ignores deformation rate effects on bond strength. t
        """ # nopep8
        return self._cards[2].get_value("brsp")

    @brsp.setter
    def brsp(self, value: float) -> None:
        """Set the brsp property."""
        self._cards[2].set_value("brsp", value)

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

