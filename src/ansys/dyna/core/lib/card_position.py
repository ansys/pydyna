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

"""Card position type for option card ordering."""

from dataclasses import dataclass
from enum import Enum


class CardPlacement(Enum):
    """Placement of an option card set relative to the keyword's main cards."""

    PRE = "pre"
    MAIN = "main"
    POST = "post"

    @property
    def _sort_key(self) -> int:
        """Numeric key for ordering: PRE < MAIN < POST."""
        return {"pre": 0, "main": 1, "post": 2}[self.value]

    @property
    def _index_ascending(self) -> bool:
        """Whether index sorts ascending (True) or descending (False) within this placement.

        For ``post`` and ``main``, higher index means later → ascending order.
        For ``pre``,  higher index means earlier (further before main cards) → descending order.
        """
        return self.value in ("post", "main")


@dataclass(frozen=True)
class CardPosition:
    """
    Position of an option card set within a keyword's card sequence.

    Specified as a URI string of the form ``"pre/N"`` or ``"post/N"``
    where N is a positive integer used to order multiple option sets
    within the same placement group.

    Ordering rules
    --------------
    - ``pre`` options sort by **descending** index: ``pre/2`` comes before ``pre/1``,
      because a higher N means the card set sits further before the main cards.
      This matches the legacy convention where ``-2`` sorted before ``-1``.
    - ``main/N`` options are interleaved *within* the main cards: the option card set
      is inserted immediately after the main card at 0-based position N.
      Multiple ``main`` options at the same index are sorted by ascending N (i.e.
      insertion order follows ascending index within the same slot).
    - ``post`` options sort by ascending index: ``post/1`` comes before ``post/2``.

    Examples
    --------
    - ``"pre/2"``   -- placed before ``"pre/1"`` (further before main cards)
    - ``"pre/1"``   -- placed immediately before main cards
    - ``"main/2"``  -- inserted after the 3rd main card (0-based index 2)
    - ``"post/1"``  -- placed immediately after all main cards
    - ``"post/3"``  -- placed after ``"post/1"`` and ``"post/2"``

    Legacy integer values are accepted for backward compatibility:
    negative integers map to ``pre`` (e.g. ``-2`` → ``"pre/2"``),
    positive integers map to ``post`` (e.g. ``1`` → ``"post/1"``).
    There is no legacy integer form for ``main``.
    """

    placement: CardPlacement
    index: int

    # ------------------------------------------------------------------
    # Construction helpers
    # ------------------------------------------------------------------

    @classmethod
    def from_str(cls, s: str) -> "CardPosition":
        """Parse a URI string such as ``"pre/1"`` or ``"post/2"``."""
        try:
            placement_str, index_str = s.split("/")
            return cls(CardPlacement(placement_str), int(index_str))
        except (ValueError, KeyError) as exc:
            raise ValueError(
                f"Invalid card position URI '{s}'. Expected 'pre/<n>', 'main/<n>', or 'post/<n>'."
            ) from exc

    @classmethod
    def from_int(cls, n: int) -> "CardPosition":
        """Create from a legacy integer value (negative → pre, positive → post)."""
        if n < 0:
            return cls(CardPlacement.PRE, abs(n))
        if n > 0:
            return cls(CardPlacement.POST, n)
        raise ValueError("Card position integer must be non-zero.")

    @classmethod
    def parse(cls, value: "str | int") -> "CardPosition":
        """Accept either a URI string or a legacy integer."""
        if isinstance(value, str):
            return cls.from_str(value)
        return cls.from_int(value)

    # ------------------------------------------------------------------
    # Ordering
    # ------------------------------------------------------------------

    def __lt__(self, other: "CardPosition") -> bool:
        if self.placement._sort_key != other.placement._sort_key:
            return self.placement._sort_key < other.placement._sort_key
        # Within the same placement, respect per-placement index direction
        if self.placement._index_ascending:
            return self.index < other.index
        else:
            # pre: higher index = earlier in output → invert
            return self.index > other.index

    def __le__(self, other: "CardPosition") -> bool:
        return self == other or self < other

    def __gt__(self, other: "CardPosition") -> bool:
        return other < self

    def __ge__(self, other: "CardPosition") -> bool:
        return other <= self

    # ------------------------------------------------------------------
    # String representation
    # ------------------------------------------------------------------

    def __repr__(self) -> str:
        return f'"{self.placement.value}/{self.index}"'

    def __str__(self) -> str:
        return f"{self.placement.value}/{self.index}"
