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

"""Module providing the DefineCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineCurve(KeywordBase):
    """DYNA DEFINE_CURVE keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCurve class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sidr",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sfa",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sfo",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "offa",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "offo",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dattyp",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcint",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            TableCard(
                [
                    Field("a1", float, 0, 20, 0.0),
                    Field("o1", float, 20, 20, 0.0),
                ],
                None,
                name="curves",
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCurve.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sidr(self) -> int:
        """Get or set the Stress initialization by dynamic relaxation:
        EQ.0: load curve used in transient analysis only or for other applications,
        EQ.1: load curve used in stress initialization but not transient analysis,
        EQ.2: load curve applies to both initialization and transient analysis.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        """Set the sidr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sidr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sidr", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for abcissa value. This is useful for simple modifications.
        EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[0].set_value("sfa", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for ordinate value (function). This is useful for simple modifications.
        EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        """Set the sfo property."""
        self._cards[0].set_value("sfo", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for abcissa values.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        """Set the offa property."""
        self._cards[0].set_value("offa", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for ordinate values (function).
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        """Set the offo property."""
        self._cards[0].set_value("offo", value)

    @property
    def dattyp(self) -> int:
        """Get or set the Data type.This affects how offsets are applied.
        EQ.-2:for fabric stress vs. strain curves(*MAT_FABRIC)as described below.Thickness flag for norminal stress calculation.
        EQ.0:general case for time dependent curves,force versus displacement curves and stress strain curves.
        EQ.1:for general (x,y) data curves whose abscissa values do not increase monotonically.
        EQ.6:for general (r,s) data(coordinates in a 2D parametric space) whose values do not increase momotonically.Use for definition of trimming polygons for trimmed NURBS(*ELEMENT_SHELL_NURBS_PATCH,NL.GT.0).
        EQ.-100:	for defining the proxy, α, from experiments for the chemical shrinkage coefficient as a function of temperature (see *MAT_ADD_CHEM_SHRINKAGE for details)
        """ # nopep8
        return self._cards[0].get_value("dattyp")

    @dattyp.setter
    def dattyp(self, value: int) -> None:
        """Set the dattyp property."""
        if value not in [0, 1, -2, 6, -100, None]:
            raise Exception("""dattyp must be `None` or one of {0,1,-2,6,-100}.""")
        self._cards[0].set_value("dattyp", value)

    @property
    def lcint(self) -> int:
        """Get or set the The number of discretization intervals to use for this curve. If 0 is input, the value of LCINT from *CONTROL_SOLUTION will be used.
        """ # nopep8
        return self._cards[0].get_value("lcint")

    @lcint.setter
    def lcint(self, value: int) -> None:
        """Set the lcint property."""
        self._cards[0].set_value("lcint", value)

    @property
    def curves(self):
        """Get the table of curves."""
        return self._cards[1].table

    @curves.setter
    def curves(self, df):
        """Set curves from the dataframe df"""
        self._cards[1].table = df

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

