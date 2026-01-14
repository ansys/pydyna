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
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALSTRAINSHELLLARGECARDSET_CARD0 = (
    FieldSchema("epsxx", float, 0, 10, 0.0),
    FieldSchema("epsyy", float, 10, 10, 0.0),
    FieldSchema("epszz", float, 20, 10, 0.0),
    FieldSchema("epsxy", float, 30, 10, 0.0),
    FieldSchema("epsyz", float, 40, 10, 0.0),
    FieldSchema("epszx", float, 50, 10, 0.0),
    FieldSchema("t", float, 60, 10, 0.0),
)

class InitialStrainShellLargeCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellLargeCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLLARGECARDSET_CARD0,
                **kwargs,
            ),        ]

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        """Set the epsxx property."""
        self._cards[0].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        """Set the epsyy property."""
        self._cards[0].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        """Set the epszz property."""
        self._cards[0].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        """Set the epsxy property."""
        self._cards[0].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        """Set the epsyz property."""
        self._cards[0].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        """Set the epszx property."""
        self._cards[0].set_value("epszx", value)

    @property
    def t(self) -> float:
        """Get or set the Parametric coordinate of through thickness integration point between -1and 1 inclusive.
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[0].set_value("t", value)

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

_INITIALSTRAINSHELLLARGECARDSETLARGE_CARD0 = (
    FieldSchema("epsxx", float, 0, 20, 0.0),
    FieldSchema("epsyy", float, 20, 20, 0.0),
    FieldSchema("epszz", float, 40, 20, 0.0),
    FieldSchema("epsxy", float, 60, 20, 0.0),
    FieldSchema("epsyz", float, 80, 20, 0.0),
)

_INITIALSTRAINSHELLLARGECARDSETLARGE_CARD1 = (
    FieldSchema("epszx", float, 0, 20, 0.0),
    FieldSchema("t", float, 20, 20, 0.0),
)

class InitialStrainShellLargeCardSetLarge(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellLargeCardSetLarge CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLLARGECARDSETLARGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLLARGECARDSETLARGE_CARD1,
                **kwargs,
            ),        ]

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        """Set the epsxx property."""
        self._cards[0].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        """Set the epsyy property."""
        self._cards[0].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        """Set the epszz property."""
        self._cards[0].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        """Set the epsxy property."""
        self._cards[0].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        """Set the epsyz property."""
        self._cards[0].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        """Set the epszx property."""
        self._cards[1].set_value("epszx", value)

    @property
    def t(self) -> float:
        """Get or set the Parametric coordinate of through thickness integration point between -1and 1 inclusive.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

_INITIALSTRAINSHELLCARDSET_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("nplane", int, 10, 10, None),
    FieldSchema("nthick", int, 20, 10, None),
    FieldSchema("large", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("ilocal", int, 70, 10, 0),
)

class InitialStrainShellCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLCARDSET_CARD0,
                **kwargs,
            ),            CardSet(
                InitialStrainShellLargeCardSet,
                length_func = lambda: self.nplane * self.nthick if (self.nplane and self.nthick) else 2,
                active_func = lambda: self.large == None or self.large == 0,
                **kwargs
            ),            CardSet(
                InitialStrainShellLargeCardSetLarge,
                length_func = lambda: self.nplane * self.nthick if (self.nplane and self.nthick) else 2,
                active_func = lambda: self.large == 1,
                **kwargs
            ),        ]

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
    def strains(self) -> typing.List[InitialStrainShellLargeCardSet]:
        """Gets the list of strains."""
        return self._cards[1].items()

    @property
    def large_strains(self) -> typing.List[InitialStrainShellLargeCardSetLarge]:
        """Gets the list of large_strains."""
        return self._cards[2].items()

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
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the eid
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].eid

    @eid.setter
    def eid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].eid = value

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the nplane
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nplane

    @nplane.setter
    def nplane(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nplane = value

    @property
    def nthick(self) -> typing.Optional[int]:
        """Get or set the nthick
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nthick

    @nthick.setter
    def nthick(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nthick = value

    @property
    def large(self) -> int:
        """Get or set the large
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].large

    @large.setter
    def large(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].large = value

    @property
    def ilocal(self) -> int:
        """Get or set the ilocal
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].ilocal

    @ilocal.setter
    def ilocal(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].ilocal = value

    @property
    def sets(self) -> typing.List[InitialStrainShellCardSet]:
        """Gets the list of sets."""
        return self._cards[0].items()

    def add_set(self, **kwargs):
        """Adds a set."""
        self._cards[0].add_item(**kwargs)

