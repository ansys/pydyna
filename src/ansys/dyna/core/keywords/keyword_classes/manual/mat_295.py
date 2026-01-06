# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Manual MAT_295 implementation with fiber families CardSet support.

This is a MANUAL keyword class that was copied from the auto-generated version
and modified to fix GitHub issue #995: MAT_295 write issues with multiple fiber
families having different FTYPE values.

PROBLEM SUMMARY (Issue #995):
-----------------------------
The original auto-generated code used `table-card-group` for cards 7, 8, 9 which
represent repeatable fiber family data. The table-card-group handler creates a
pandas DataFrame with columns from all cards in the group. However, cards 8 and 9
both have an `ftype` field, causing duplicate column names which triggers a pandas
ValueError when trying to write with multiple fiber families of different FTYPE.

SOLUTION IMPLEMENTED:
--------------------
1. Changed manifest.json to use `card-set` instead of `table-card-group` for
   indices [7, 8, 9]. This generates a CardSet with a custom item class.

2. Created FiberFamily class (the CardSet item) with 3 cards:
   - Card 0: theta, a, b (always active)
   - Card 1: ftype=1 params (fcid, k1, k2) - active when ftype==1
   - Card 2: ftype=2 params (flcid, e, r0norm, h0norm) - active when ftype==2

3. Added custom _read_data method to FiberFamily to handle the chicken-and-egg
   problem: the card conditionals depend on ftype, but ftype is IN the card.
   The solution peeks ahead to read ftype before setting card conditionals.

MANIFEST.JSON CHANGES:
----------------------
The manifest entry for MAT_295 was changed from:

    "table-card-group": [
        {"indices": [7, 8, 9], "length-func": "self.nf or 0",
         "active-func": "self.atype and abs(self.atype) == 1"}
    ]

To:

    "card-set": [
        {"source-indices": [7, 8, 9], "target-index": 7, "name": "FiberFamily",
         "length-func": "self.nf or 0",
         "active-func": "self.atype and abs(self.atype) == 1"}
    ]

CARD_SET.PY CHANGES:
--------------------
The CardSet._read_item_cards method was modified to support custom _read_data
methods on CardSet items. If an item has a _read_data method, it's called instead
of the default card iteration. The _read_data method must return a bool indicating
if end-of-keyword was reached.

KEY CLASSES IN THIS FILE:
-------------------------
1. FiberFamily(Cards): CardSet item class for fiber family data
   - 3 cards with conditional activation based on ftype
   - Custom _read_data for peek-ahead ftype detection
   - Unified ftype property that sets value on both cards 1 and 2

2. Mat295(KeywordBase): Main keyword class
   - fiber_families property returns List[FiberFamily] (bounded by nf)
   - before_read method detects "ANISO" and "ACTIVE" option lines
   - Most of the class is copied verbatim from auto-generated code

WHAT COULD BE AUTO-GENERATED VS MANUAL:
---------------------------------------
Most of this file (>95%) is identical to the auto-generated version. Only these
parts required manual changes:

1. FiberFamily class definition (~80 lines):
   - The card definitions with conditionals (lambda: self.ftype == 1/2)
   - The _read_data method for peek-ahead reading (~25 lines)
   - The unified ftype property that sets both cards

2. Mat295 class changes (~15 lines):
   - The fiber_families property (simple accessor to CardSet.items())
   - Import of read_line for the peek-ahead functionality

POTENTIAL CODEGEN IMPROVEMENTS:
-------------------------------
To auto-generate most of this, the codegen system could:

1. For card-set handler: Generate the CardSet item class (FiberFamily) with
   cards from source-indices, applying conditionals from the manifest.

2. For conditional-on-own-field pattern: When a card's active condition depends
   on a field within that same card (or sibling cards in the set), generate a
   custom _read_data method that:
   - Peeks at the line to extract the conditional field value
   - Sets the field on all cards that have it
   - Reads the cards normally

3. The card-set handler could accept a "conditional-field" option in manifest
   that specifies which field determines card activation, enabling automatic
   generation of the peek-ahead logic.

Example manifest enhancement:
    "card-set": [{
        "source-indices": [7, 8, 9],
        "target-index": 7,
        "name": "FiberFamily",
        "length-func": "self.nf or 0",
        "active-func": "self.atype and abs(self.atype) == 1",
        "conditional-field": {"name": "ftype", "position": 0, "width": 10}
    }]

TESTING:
--------
See tests/test_mat295_fiber_families.py for comprehensive tests including:
- Creating fiber families with ftype=1 (Holzapfel-Gasser-Ogden model)
- Creating fiber families with ftype=2 (Freed-Doehring model)
- Multiple families with mixed ftype values
- Round-trip read/write verification

RELATED FILES:
--------------
- codegen/manifest.json: Contains card-set configuration for MAT_295
- src/ansys/dyna/core/lib/card_set.py: CardSet with _read_data hook support
- src/ansys/dyna/core/keywords/keyword_classes/auto/mat_295.py: Original auto-gen
- tests/test_mat295_fiber_families.py: Test coverage for this implementation
"""
import typing

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.kwd_line_formatter import read_line
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec


class FiberFamily(Cards):
    """CardSet item class for MAT_295 fiber family data.

    This class represents a single fiber family in the MAT_295 keyword. Each fiber
    family consists of 2 lines (cards) in the keyword file:

    Line 1 (Card 0 - always present):
        THETA, A, B - fiber orientation and structure tensor parameters

    Line 2 (Card 1 OR Card 2 - mutually exclusive based on FTYPE):
        If FTYPE=1: FTYPE, FCID, K1, K2 (Holzapfel-Gasser-Ogden model)
        If FTYPE=2: FTYPE, FLCID, E, R0NORM, H0NORM (Freed-Doehring model)

    STRUCTURE:
    ----------
    The class has 3 Card objects in self._cards:
        _cards[0]: theta, a, b (always active, no conditional)
        _cards[1]: ftype, fcid, k1, k2 (active when ftype==1)
        _cards[2]: ftype, flcid, e, r0norm, h0norm (active when ftype==2)

    Cards 1 and 2 both have `ftype` field because it's the discriminator that
    determines which card format is used. When writing, only the active card
    writes its ftype value.

    READING CHALLENGE:
    ------------------
    Card conditionals (lambda: self.ftype == 1) depend on ftype, but ftype is
    IN the cards themselves. This creates a chicken-and-egg problem during read.

    Solution: Custom _read_data method that:
    1. Reads card 0 normally
    2. Peeks ahead to extract ftype value from next line
    3. Sets ftype on BOTH cards 1 and 2 (so conditionals evaluate correctly)
    4. Reads both cards (only the active one will actually consume data)

    UNIFIED FTYPE PROPERTY:
    -----------------------
    The `ftype` property getter returns from whichever card is active.
    The `ftype` property setter sets the value on BOTH cards, ensuring the
    conditional activation switches correctly.

    CODEGEN ORIGIN:
    ---------------
    This class structure mirrors what the card-set handler would generate, but
    with manual additions for:
    - Conditional lambdas on cards 1 and 2
    - Custom _read_data method for peek-ahead reading
    - Unified ftype property implementation

    The card definitions (fields, positions, widths) come from the kwd.json
    schema entries at indices 7, 8, 9 for MAT_295.
    """

    def __init__(self, **kwargs):
        """Initialize the FiberFamily CardSet item."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "theta",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ftype",
                        int,
                        0,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "fcid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k1",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k2",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.ftype == 1,
            ),
            Card(
                [
                    Field(
                        "ftype",
                        int,
                        0,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "flcid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "r0norm",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "h0norm",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.ftype == 2,
            ),
        ]

    def _read_data(self, buf: typing.TextIO, parameters) -> bool:
        """Custom read logic to handle ftype-dependent cards.

        The ftype field determines which of cards 1 or 2 should be read, but
        ftype is in those cards themselves. We need to:
        1. Read card 0 (theta, a, b) - always present
        2. Peek at next line to get ftype value
        3. Set ftype so the correct card becomes active
        4. Read the appropriate card

        Returns:
            True if the reader hit the end of the keyword early, False otherwise.
        """
        # Read card 0 (theta, a, b)
        self._cards[0].read(buf, parameters)

        # Peek at next line to determine ftype
        pos = buf.tell()
        line, _ = read_line(buf)
        buf.seek(pos)

        # Parse ftype from position 0-10
        try:
            ftype_str = line[:10].strip()
            ftype_val = int(ftype_str) if ftype_str else 1
        except (ValueError, IndexError):
            ftype_val = 1

        # Set ftype on both cards so conditionals work
        self._cards[1].set_value("ftype", ftype_val)
        self._cards[2].set_value("ftype", ftype_val)

        # Now read the appropriate card (either card 1 or card 2 will be active)
        self._cards[1].read(buf, parameters)
        self._cards[2].read(buf, parameters)

        return False

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Mean fiber family orientation angle with respect to the a material axis in the a-b material plane in degrees."""  # nopep8
        return self._cards[0].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[0].set_value("theta", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the First structure tensor parameter."""  # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Second structure tensor parameter."""  # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def ftype(self) -> int:
        """Get or set the Type of fiber model:
        EQ.1:	Holzapfel-Gasser-Ogden [6]
        EQ.2:	Freed-Doehring [2].
        """  # nopep8
        # Read from card 1 first, fall back to card 2
        val = self._cards[1].get_value("ftype")
        if val is None:
            val = self._cards[2].get_value("ftype")
        return val

    @ftype.setter
    def ftype(self, value: int) -> None:
        """Set the ftype property."""
        if value not in [1, 2, None]:
            raise Exception("""ftype must be `None` or one of {1,2}.""")
        # Set on both cards so conditionals work correctly
        self._cards[1].set_value("ftype", value)
        self._cards[2].set_value("ftype", value)

    @property
    def fcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the fiber stress versus fiber stretch relation, default if nonzero."""  # nopep8
        return self._cards[1].get_value("fcid")

    @fcid.setter
    def fcid(self, value: int) -> None:
        """Set the fcid property."""
        self._cards[1].set_value("fcid", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Gasser-Ogden modulus."""  # nopep8
        return self._cards[1].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[1].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Gasser-Ogden constant."""  # nopep8
        return self._cards[1].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[1].set_value("k2", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the fiber stress versus fiber stretch relation, default if nonzero."""  # nopep8
        return self._cards[2].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        """Set the flcid property."""
        self._cards[2].set_value("flcid", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Fiber modulus."""  # nopep8
        return self._cards[2].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[2].set_value("e", value)

    @property
    def r0norm(self) -> typing.Optional[float]:
        """Get or set the Initial crimp/coil amplitude normalized with respect to the initial fiber radius (R0/r0)."""  # nopep8
        return self._cards[2].get_value("r0norm")

    @r0norm.setter
    def r0norm(self, value: float) -> None:
        """Set the r0norm property."""
        self._cards[2].set_value("r0norm", value)

    @property
    def h0norm(self) -> typing.Optional[float]:
        """Get or set the Initial crimp/coil wavelength normalized with respect to the initial fiber radius (H0/r0)."""  # nopep8
        return self._cards[2].get_value("h0norm")

    @h0norm.setter
    def h0norm(self, value: float) -> None:
        """Set the h0norm property."""
        self._cards[2].set_value("h0norm", value)

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent


class Mat295(KeywordBase):
    """DYNA MAT_295 keyword"""

    keyword = "MAT"
    subkeyword = "295"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat295 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rho",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "aopt",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "title",
                        str,
                        0,
                        10,
                        Field.ReadOnlyValue("ISO"),
                        **kwargs,
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nu",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mu1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mu8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.itype and abs(self.itype) == 1,
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.itype and abs(self.itype) == 1,
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.itype and abs(self.itype) == 2,
            ),
            Card(
                [
                    Field(
                        "k1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.itype and abs(self.itype) == 3,
            ),
            Card(
                [
                    Field(
                        "title",
                        str,
                        0,
                        10,
                        Field.ReadOnlyValue("ANISO"),
                        **kwargs,
                    ),
                    Field(
                        "atype",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "intype",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nf",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1,
            ),
            CardSet(
                FiberFamily,
                length_func=lambda: self.nf or 0,
                active_func=lambda: self.atype and abs(self.atype) == 1,
                **kwargs,
            ),
            Card(
                [
                    Field(
                        "k1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.intype == 1,
            ),
            Card(
                [
                    Field(
                        "title",
                        str,
                        0,
                        10,
                        Field.ReadOnlyValue("ACTIVE"),
                        **kwargs,
                    ),
                    Field(
                        "actype",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "acdir",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "acid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "acthr",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sf",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "ss",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sn",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype in [1, 2, 3, 4, 5],
            ),
            Card(
                [
                    Field(
                        "t0",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ion",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ionm",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "taumax",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "stf",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "l0",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype in [1, 2],
            ),
            Card(
                [
                    Field(
                        "l",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dtmax",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mr",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 1,
            ),
            Card(
                [
                    Field(
                        "l",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eta",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 2,
            ),
            Card(
                [
                    Field(
                        "t0",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ion",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ion50",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigmax",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "f",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "l",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eta",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 3,
            ),
            Card(
                [
                    Field(
                        "t0",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ion50",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ionmax",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigmax",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "f",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ca2ion0",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tca",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 4,
            ),
            Card(
                [
                    Field(
                        "l",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eta",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 4,
            ),
            Card(
                [
                    Field(
                        "fseid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "flid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fvid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alphaid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1 and self.actype == 5,
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "macf",
                        int,
                        60,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1,
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ref",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
                lambda: self.atype and abs(self.atype) == 1,
            ),
            OptionCardSet(
                option_spec=Mat295.option_specs[0],
                cards=[
                    Card(
                        [
                            Field("title", str, 0, 80, kwargs.get("title")),
                        ],
                    ),
                ],
                **kwargs,
            ),
        ]

    def before_read(self, buf: typing.TextIO) -> None:
        """Peek into the buffer before reading to activate conditional cards.

        The ANISO and ACTIVE cards need to be activated before reading because
        their conditional functions depend on atype and actype values which
        are read from those very cards.
        """
        pos = buf.tell()
        while True:
            line, end = read_line(buf)
            if end:
                break
            if line.startswith("ANISO"):
                self.atype = 1
            elif line.startswith("ACTIVE"):
                self.actype = 1
        buf.seek(pos)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the theta"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].theta

    @theta.setter
    def theta(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].theta = value

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the a"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].a

    @a.setter
    def a(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].a = value

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the b"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].b

    @b.setter
    def b(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].b = value

    @property
    def ftype(self) -> int:
        """Get or set the ftype"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].ftype

    @ftype.setter
    def ftype(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].ftype = value

    @property
    def fcid(self) -> typing.Optional[int]:
        """Get or set the fcid"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].fcid

    @fcid.setter
    def fcid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].fcid = value

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the k1"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].k1

    @k1.setter
    def k1(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].k1 = value

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the k2"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].k2

    @k2.setter
    def k2(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].k2 = value

    @property
    def ftype(self) -> int:
        """Get or set the ftype"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].ftype

    @ftype.setter
    def ftype(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].ftype = value

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the flcid"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].flcid

    @flcid.setter
    def flcid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].flcid = value

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the e"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].e

    @e.setter
    def e(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].e = value

    @property
    def r0norm(self) -> typing.Optional[float]:
        """Get or set the r0norm"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].r0norm

    @r0norm.setter
    def r0norm(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].r0norm = value

    @property
    def h0norm(self) -> typing.Optional[float]:
        """Get or set the h0norm"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].h0norm

    @h0norm.setter
    def h0norm(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].h0norm = value

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified."""  # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Mass density."""  # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see *MAT_002 for a more complete description):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.The a - direction is from node 1 to node 2 of the element.The b - direction is orthogonal to the a - direction and is in the plane formed by nodes 1, 2,and 4. For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors a and d input below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then a and b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : | AOPT | is a coordinate system ID(see * DEFINE_COORDINATE_OPTION).
        """  # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[0].set_value("aopt", value)

    @property
    def isotropic_title(self) -> str:
        """Get or set the Module title."""  # nopep8
        return self._cards[1].get_value("title")

    @property
    def itype(self) -> typing.Optional[int]:
        """Get or set the Type of isotropic model (see remarks 1 and 2):
        EQ.-1/+1:	compressible/nearly-incompressible Ogden [12] (see notes 1-3)
        EQ.-2:	Yeoh [13]
        EQ.-3/+3:	compressible/nearly-incompressible Holzapfel-Ogden [1], [7].
        """  # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        self._cards[1].set_value("itype", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Volumetric response function coefficient."""  # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def nu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (see remark 3)."""  # nopep8
        return self._cards[1].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        """Set the nu property."""
        self._cards[1].set_value("nu", value)

    @property
    def mu1(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        """Set the mu1 property."""
        self._cards[2].set_value("mu1", value)

    @property
    def mu2(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        """Set the mu2 property."""
        self._cards[2].set_value("mu2", value)

    @property
    def mu3(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu3")

    @mu3.setter
    def mu3(self, value: float) -> None:
        """Set the mu3 property."""
        self._cards[2].set_value("mu3", value)

    @property
    def mu4(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu4")

    @mu4.setter
    def mu4(self, value: float) -> None:
        """Set the mu4 property."""
        self._cards[2].set_value("mu4", value)

    @property
    def mu5(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu5")

    @mu5.setter
    def mu5(self, value: float) -> None:
        """Set the mu5 property."""
        self._cards[2].set_value("mu5", value)

    @property
    def mu6(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu6")

    @mu6.setter
    def mu6(self, value: float) -> None:
        """Set the mu6 property."""
        self._cards[2].set_value("mu6", value)

    @property
    def mu7(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu7")

    @mu7.setter
    def mu7(self, value: float) -> None:
        """Set the mu7 property."""
        self._cards[2].set_value("mu7", value)

    @property
    def mu8(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8."""  # nopep8
        return self._cards[2].get_value("mu8")

    @mu8.setter
    def mu8(self, value: float) -> None:
        """Set the mu8 property."""
        self._cards[2].set_value("mu8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[3].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[3].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[3].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[3].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[3].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[3].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[3].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8."""  # nopep8
        return self._cards[3].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[3].set_value("alpha8", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3."""  # nopep8
        return self._cards[4].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[4].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3."""  # nopep8
        return self._cards[4].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[4].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3."""  # nopep8
        return self._cards[4].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[4].set_value("c3", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Ogden modulus."""  # nopep8
        return self._cards[5].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[5].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Ogden constant."""  # nopep8
        return self._cards[5].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[5].set_value("k2", value)

    @property
    def anisotropic_title(self) -> str:
        """Get or set the Module title."""  # nopep8
        return self._cards[6].get_value("title")

    @property
    def atype(self) -> typing.Optional[int]:
        """Get or set the Type of anisotropic model:
        EQ.-1/+1:	general structure tensor-based, see Holzapfel et al. [8] (see remark 4 and note 4)
        """  # nopep8
        return self._cards[6].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        """Set the atype property."""
        self._cards[6].set_value("atype", value)

    @property
    def intype(self) -> typing.Optional[int]:
        """Get or set the Type of interaction between the fiber families (see remarks 5 and 6):
        EQ.0:	none
        EQ.1:	Holzapfel-Ogden [1], [5].
        """  # nopep8
        return self._cards[6].get_value("intype")

    @intype.setter
    def intype(self, value: int) -> None:
        """Set the intype property."""
        self._cards[6].set_value("intype", value)

    @property
    def nf(self) -> typing.Optional[int]:
        """Get or set the Number of fiber families (see remark 4)."""  # nopep8
        return self._cards[6].get_value("nf")

    @nf.setter
    def nf(self, value: int) -> None:
        """Set the nf property."""
        self._cards[6].set_value("nf", value)

    @property
    def fiber_families(self) -> typing.List[FiberFamily]:
        """Gets the list of fiber families."""
        return self._cards[7].items()

    @property
    def coupling_k1(self) -> typing.Optional[float]:
        """Get or set the Coupling modulus between the fiber and sheet directions"""  # nopep8
        return self._cards[8].get_value("k1")

    @coupling_k1.setter
    def coupling_k1(self, value: float) -> None:
        """Set the coupling_k1 property."""
        self._cards[8].set_value("k1", value)

    @property
    def coupling_k2(self) -> typing.Optional[float]:
        """Get or set the Coupling constant between the fiber and sheet directions"""  # nopep8
        return self._cards[8].get_value("k2")

    @coupling_k2.setter
    def coupling_k2(self, value: float) -> None:
        """Set the coupling_k2 property."""
        self._cards[8].set_value("k2", value)

    @property
    def active_title(self) -> str:
        """Get or set the Module title."""  # nopep8
        return self._cards[9].get_value("title")

    @property
    def actype(self) -> typing.Optional[int]:
        """Get or set the Type of active model:
        EQ.1:	Guccione-Waldman-McCulloch [4]
        EQ.2:	Guccione-Waldman-McCulloch [4] and Hunter-Nash-Sands [9]
        EQ.3:	Hunter-Nash-Sands	[9]
        EQ.4:	Hunter-Nash-Sands [9] and Hunter-McCulloch-ter Keurs [10].
        EQ.5: Martins-Pato-Pires [14]
        """  # nopep8
        return self._cards[9].get_value("actype")

    @actype.setter
    def actype(self, value: int) -> None:
        """Set the actype property."""
        self._cards[9].set_value("actype", value)

    @property
    def acdir(self) -> int:
        """Get or set the Direction of active tension: GT.0:	Active tension develops along the mean orientation of the ACDIRth fiber family."""  # nopep8
        return self._cards[9].get_value("acdir")

    @acdir.setter
    def acdir(self, value: int) -> None:
        """Set the acdir property."""
        self._cards[9].set_value("acdir", value)

    @property
    def acid(self) -> typing.Optional[int]:
        """Get or set the Activation curve ID (takes priority over T0 for ACTYPE = 1, 2, 3, or 4 when defined, see Remark 8"""  # nopep8
        return self._cards[9].get_value("acid")

    @acid.setter
    def acid(self, value: int) -> None:
        """Set the acid property."""
        self._cards[9].set_value("acid", value)

    @property
    def acthr(self) -> float:
        """Get or set the (De/re)activation threshold (see Remark 8)"""  # nopep8
        return self._cards[9].get_value("acthr")

    @acthr.setter
    def acthr(self, value: float) -> None:
        """Set the acthr property."""
        self._cards[9].set_value("acthr", value)

    @property
    def sf(self) -> float:
        """Get or set the Active stress scaling factor in the fiber direction (see Remark 9)"""  # nopep8
        return self._cards[9].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[9].set_value("sf", value)

    @property
    def ss(self) -> float:
        """Get or set the Active stress scaling factor in the transverse sheet direction (see Remark 9)"""  # nopep8
        return self._cards[9].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[9].set_value("ss", value)

    @property
    def sn(self) -> float:
        """Get or set the Active stress scaling factor in the transverse normal direction (see Remark 9)"""  # nopep8
        return self._cards[9].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[9].set_value("sn", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Starting time of active stress development."""  # nopep8
        return self._cards[10].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[10].set_value("t0", value)
        self._cards[13].set_value("t0", value)
        self._cards[14].set_value("t0", value)

    @property
    def ca2ion(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration"""  # nopep8
        return self._cards[10].get_value("ca2ion")

    @ca2ion.setter
    def ca2ion(self, value: float) -> None:
        """Set the ca2ion property."""
        self._cards[10].set_value("ca2ion", value)
        self._cards[13].set_value("ca2ion", value)

    @property
    def ca2ionm(self) -> typing.Optional[float]:
        """Get or set the Maximum intercellular calcium ion concentration."""  # nopep8
        return self._cards[10].get_value("ca2ionm")

    @ca2ionm.setter
    def ca2ionm(self, value: float) -> None:
        """Set the ca2ionm property."""
        self._cards[10].set_value("ca2ionm", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hill coefficient."""  # nopep8
        return self._cards[10].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[10].set_value("n", value)
        self._cards[13].set_value("n", value)
        self._cards[14].set_value("n", value)

    @property
    def taumax(self) -> typing.Optional[float]:
        """Get or set the Peak isometric tension under maximum activation."""  # nopep8
        return self._cards[10].get_value("taumax")

    @taumax.setter
    def taumax(self, value: float) -> None:
        """Set the taumax property."""
        self._cards[10].set_value("taumax", value)

    @property
    def stf(self) -> typing.Optional[float]:
        """Get or set the Transverse fiber stress scaling factor."""  # nopep8
        return self._cards[10].get_value("stf")

    @stf.setter
    def stf(self, value: float) -> None:
        """Set the stf property."""
        self._cards[10].set_value("stf", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Shape coefficient."""  # nopep8
        return self._cards[10].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[10].set_value("b", value)

    @property
    def l0(self) -> typing.Optional[float]:
        """Get or set the Sarcomere length with no active tension."""  # nopep8
        return self._cards[10].get_value("l0")

    @l0.setter
    def l0(self, value: float) -> None:
        """Set the l0 property."""
        self._cards[10].set_value("l0", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Reference (stress-free) sarcomere length."""  # nopep8
        return self._cards[11].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[11].set_value("l", value)
        self._cards[12].set_value("l", value)
        self._cards[13].set_value("l", value)
        self._cards[15].set_value("l", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Time to peak tension."""  # nopep8
        return self._cards[11].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[11].set_value("dtmax", value)

    @property
    def mr(self) -> typing.Optional[float]:
        """Get or set the Slope of linear relaxation versus sarcomere length relation."""  # nopep8
        return self._cards[11].get_value("mr")

    @mr.setter
    def mr(self, value: float) -> None:
        """Set the mr property."""
        self._cards[11].set_value("mr", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Time intercept of linear relaxation versus sarcomere length relation."""  # nopep8
        return self._cards[11].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        """Set the tr property."""
        self._cards[11].set_value("tr", value)

    @property
    def eta(self) -> typing.Optional[float]:
        """Get or set the Scaling parameter."""  # nopep8
        return self._cards[12].get_value("eta")

    @eta.setter
    def eta(self, value: float) -> None:
        """Set the eta property."""
        self._cards[12].set_value("eta", value)
        self._cards[13].set_value("eta", value)
        self._cards[15].set_value("eta", value)

    @property
    def ca2ion50(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration at half of peak isometric tension."""  # nopep8
        return self._cards[13].get_value("ca2ion50")

    @ca2ion50.setter
    def ca2ion50(self, value: float) -> None:
        """Set the ca2ion50 property."""
        self._cards[13].set_value("ca2ion50", value)
        self._cards[14].set_value("ca2ion50", value)

    @property
    def sigmax(self) -> typing.Optional[float]:
        """Get or set the Peak isometric tension under maximum activation."""  # nopep8
        return self._cards[13].get_value("sigmax")

    @sigmax.setter
    def sigmax(self, value: float) -> None:
        """Set the sigmax property."""
        self._cards[13].set_value("sigmax", value)
        self._cards[14].set_value("sigmax", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Transverse fiber stress scaling factor."""  # nopep8
        return self._cards[13].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[13].set_value("f", value)
        self._cards[14].set_value("f", value)

    @property
    def ca2ionmax(self) -> typing.Optional[float]:
        """Get or set the Maximum intercellular calcium ion concentration."""  # nopep8
        return self._cards[14].get_value("ca2ionmax")

    @ca2ionmax.setter
    def ca2ionmax(self, value: float) -> None:
        """Set the ca2ionmax property."""
        self._cards[14].set_value("ca2ionmax", value)

    @property
    def ca2ion0(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration at rest."""  # nopep8
        return self._cards[14].get_value("ca2ion0")

    @ca2ion0.setter
    def ca2ion0(self, value: float) -> None:
        """Set the ca2ion0 property."""
        self._cards[14].set_value("ca2ion0", value)

    @property
    def tca(self) -> typing.Optional[float]:
        """Get or set the Shape coefficient."""  # nopep8
        return self._cards[14].get_value("tca")

    @tca.setter
    def tca(self, value: float) -> None:
        """Set the tca property."""
        self._cards[14].set_value("tca", value)

    @property
    def fseid(self) -> typing.Optional[int]:
        """Get or set the Serial stress function ID"""  # nopep8
        return self._cards[16].get_value("fseid")

    @fseid.setter
    def fseid(self, value: int) -> None:
        """Set the fseid property."""
        self._cards[16].set_value("fseid", value)

    @property
    def flid(self) -> typing.Optional[int]:
        """Get or set the Normalized force-contractile stretch curve ID"""  # nopep8
        return self._cards[16].get_value("flid")

    @flid.setter
    def flid(self, value: int) -> None:
        """Set the flid property."""
        self._cards[16].set_value("flid", value)

    @property
    def fvid(self) -> typing.Optional[int]:
        """Get or set the Normalized force-contractile stretch rate curve ID"""  # nopep8
        return self._cards[16].get_value("fvid")

    @fvid.setter
    def fvid(self, value: int) -> None:
        """Set the fvid property."""
        self._cards[16].set_value("fvid", value)

    @property
    def alphaid(self) -> typing.Optional[int]:
        """Get or set the Activation curve ID"""  # nopep8
        return self._cards[16].get_value("alphaid")

    @alphaid.setter
    def alphaid(self, value: int) -> None:
        """Set the alphaid property."""
        self._cards[16].set_value("alphaid", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4."""  # nopep8
        return self._cards[17].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[17].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4."""  # nopep8
        return self._cards[17].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[17].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4."""  # nopep8
        return self._cards[17].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[17].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[17].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[17].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[17].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[17].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[17].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[17].set_value("a3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements:
        EQ.1:	no change (default)
        EQ.2:	switch material axes a and b
        EQ.3:	switch material axes a and c
        EQ.4:	switch material axes b and c.
        """  # nopep8
        return self._cards[17].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4}.""")
        self._cards[17].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4."""  # nopep8
        return self._cards[18].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[18].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4."""  # nopep8
        return self._cards[18].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[18].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4."""  # nopep8
        return self._cards[18].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[18].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[18].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[18].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[18].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[18].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2."""  # nopep8
        return self._cards[18].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[18].set_value("d3", value)

    @property
    def material_angle_beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells and thick shells only) and AOPT = 3 (all element types).
        This angle may be overridden on the element card;
        see *ELEMENT_SHELL_BETA, *ELEMENT_TSHELL_BETA, and *ELEMENT_SOLID_ORTHO.
        """  # nopep8
        return self._cards[18].get_value("beta")

    @material_angle_beta.setter
    def material_angle_beta(self, value: float) -> None:
        """Set the material_angle_beta property."""
        self._cards[18].set_value("beta", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword:
        *INITIAL_FOAM_REFERENCE_GEOMETRY.
        EQ.0.0:	off
        EQ.1.0:	on.
        """  # nopep8
        return self._cards[18].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[18].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line"""  # nopep8
        return self._cards[19].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[19].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")


class MatAnisotropicHyperelastic(Mat295):
    """Alias for MAT keyword."""

    subkeyword = "ANISOTROPIC_HYPERELASTIC"
