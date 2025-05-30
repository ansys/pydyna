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

"""Module providing the InitialStrainShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.card_set import CardSet
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialStrainShellCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nplane",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nthick",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "large",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ilocal",
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
                    Field("epsxx", float, 0, 10, 0.0),
                    Field("epsyy", float, 10, 10, 0.0),
                    Field("epszz", float, 20, 10, 0.0),
                    Field("epsxy", float, 30, 10, 0.0),
                    Field("epsyz", float, 40, 10, 0.0),
                    Field("epszx", float, 50, 10, 0.0),
                    Field("t", float, 60, 10, 0.0),
                ],
                lambda: self.nplane * self.nthick if (self.nplane and self.nthick) else 0,
                lambda: self.large == None or self.large == 0,
                name="strains",
                **kwargs,
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Shell element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the Number of in#plane integration points being output.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        self._cards[0].set_value("nplane", value)

    @property
    def nthick(self) -> typing.Optional[int]:
        """Get or set the Number of integration points through the thickness.
        """ # nopep8
        return self._cards[0].get_value("nthick")

    @nthick.setter
    def nthick(self, value: int) -> None:
        """Set the nthick property."""
        self._cards[0].set_value("nthick", value)

    @property
    def large(self) -> int:
        """Get or set the Large format flag:
        EQ.0:	off
        EQ.1 : on.Each strain field is twice as long for higher precision.
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        """Set the large property."""
        if value not in [0, 1, None]:
            raise Exception("""large must be `None` or one of {0,1}.""")
        self._cards[0].set_value("large", value)

    @property
    def ilocal(self) -> int:
        """Get or set the Flag for coordinate system of strain components:
        EQ.0:	global,
        EQ.1 : local(not supported).
        """ # nopep8
        return self._cards[0].get_value("ilocal")

    @ilocal.setter
    def ilocal(self, value: int) -> None:
        """Set the ilocal property."""
        if value not in [0, 1, None]:
            raise Exception("""ilocal must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ilocal", value)

    @property
    def strains(self):
        """Get the table of strains."""
        return self._cards[1].table

    @strains.setter
    def strains(self, df):
        """Set strains from the dataframe df"""
        self._cards[1].table = df

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent


class InitialStrainShell(KeywordBase):
    """DYNA INITIAL_STRAIN_SHELL keyword"""

    keyword = "INITIAL"
    subkeyword = "STRAIN_SHELL"

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        self._cards = [
            CardSet(
                InitialStrainShellCardSet,
                **kwargs
            ),
        ]

    @property
    def sets(self) -> typing.List[InitialStrainShellCardSet]:
        """Gets the list of sets."""
        return self._cards[0].items()

    def add_set(self, **kwargs):
        """Adds a set to the list of sets."""
        self._cards[0].add_item(**kwargs)

