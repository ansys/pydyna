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
)

_DEFINEDEBOND_CARD1 = (
    FieldSchema("pbn", float, 0, 10, None),
    FieldSchema("pbs", float, 10, 10, None),
    FieldSchema("pbn_s", float, 20, 10, None),
    FieldSchema("pbs_s", float, 30, 10, None),
    FieldSchema("sfa", float, 40, 10, 1.0),
    FieldSchema("alpha", float, 50, 10, 0.0),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("maxgap", float, 70, 10, 0.0001),
)

_DEFINEDEBOND_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeBond(KeywordBase):
    """DYNA DEFINE_DE_BOND keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_BOND"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeBond class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEBOND_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEBOND_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeBond.option_specs[0],
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
        EQ.1: Linear bond formulation.
        """ # nopep8
        return self._cards[0].get_value("bdform")

    @bdform.setter
    def bdform(self, value: int) -> None:
        """Set the bdform property."""
        if value not in [1, 2, None]:
            raise Exception("""bdform must be `None` or one of {1,2}.""")
        self._cards[0].set_value("bdform", value)

    @property
    def pbn(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond normal stiffness
        """ # nopep8
        return self._cards[1].get_value("pbn")

    @pbn.setter
    def pbn(self, value: float) -> None:
        """Set the pbn property."""
        self._cards[1].set_value("pbn", value)

    @property
    def pbs(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond shear stiffness
        """ # nopep8
        return self._cards[1].get_value("pbs")

    @pbs.setter
    def pbs(self, value: float) -> None:
        """Set the pbs property."""
        self._cards[1].set_value("pbs", value)

    @property
    def pbn_s(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum normal stress.
        """ # nopep8
        return self._cards[1].get_value("pbn_s")

    @pbn_s.setter
    def pbn_s(self, value: float) -> None:
        """Set the pbn_s property."""
        self._cards[1].set_value("pbn_s", value)

    @property
    def pbs_s(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum shear stress.
        """ # nopep8
        return self._cards[1].get_value("pbs_s")

    @pbs_s.setter
    def pbs_s(self, value: float) -> None:
        """Set the pbs_s property."""
        self._cards[1].set_value("pbs_s", value)

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
        """Get or set the Numerical damping.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

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

