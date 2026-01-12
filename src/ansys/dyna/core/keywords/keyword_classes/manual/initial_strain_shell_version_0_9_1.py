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

"""Legacy INITIAL_STRAIN_SHELL implementation (version 0.9.1)."""
import typing
import warnings

import pandas as pd

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.field import Field
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.table_card import TableCard

# Schema definitions for optimized Card creation
_INITIALSTRAINSHELLLEGACYCARDSET_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("nplane", int, 10, 10, None),
    FieldSchema("nthick", int, 20, 10, None),
    FieldSchema("large", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("ilocal", int, 70, 10, 0),
)


class InitialStrainShellLegacyCardSet(Cards):
    """CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellLegacyCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLLEGACYCARDSET_CARD0,
                **kwargs,
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
                lambda: self.nplane * self.nthick if (self.nplane and self.nthick) else 2,
                lambda: self.large == None or self.large == 0,
                name="strains",
                **kwargs,
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Shell element ID."""  # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the Number of in#plane integration points being output."""  # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        self._cards[0].set_value("nplane", value)

    @property
    def nthick(self) -> typing.Optional[int]:
        """Get or set the Number of integration points through the thickness."""  # nopep8
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
        """  # nopep8
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
        """  # nopep8
        return self._cards[0].get_value("ilocal")

    @ilocal.setter
    def ilocal(self, value: int) -> None:
        """Set the ilocal property."""
        if value not in [0, 1, None]:
            raise Exception("""ilocal must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ilocal", value)

    @property
    def strains(self) -> pd.DataFrame:
        """Get the table of strains."""
        return self._cards[1].table

    @strains.setter
    def strains(self, df: pd.DataFrame):
        """Set strains from the dataframe df"""
        self._cards[1].table = df

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent


class InitialStrainShellLegacy(KeywordBase):
    """DYNA INITIAL_STRAIN_SHELL keyword (legacy version 0.9.1).

    .. deprecated:: 0.10.0
        Use :class:`~ansys.dyna.core.keywords.InitialStrainShell` instead.
        This legacy version uses TableCard/DataFrame API which does not support LARGE format.
        The new version uses CardSet API and supports both standard and LARGE formats.

    To use this legacy class when loading decks:

    .. code-block:: python

        from ansys.dyna.core.lib.deck import Deck
        from ansys.dyna.core.lib.import_handler import ImportContext
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_strain_shell_version_0_9_1 import (
            InitialStrainShellLegacy,
        )

        deck = Deck()
        context = ImportContext(
            deck=deck,
            keyword_overrides={"*INITIAL_STRAIN_SHELL": InitialStrainShellLegacy},
        )
        deck.loads(data, context=context)
    """

    keyword = "INITIAL"
    subkeyword = "STRAIN_SHELL"

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellLegacy class."""
        warnings.warn(
            "InitialStrainShellLegacy is deprecated and will be removed in a future version. "
            "This legacy class uses TableCard/DataFrame API which does not support LARGE format. "
            "Use InitialStrainShell instead, which uses CardSet API and supports both standard and LARGE formats.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        self._cards = [
            CardSet(InitialStrainShellLegacyCardSet, **kwargs),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the eid"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].eid

    @eid.setter
    def eid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].eid = value

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the nplane"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nplane

    @nplane.setter
    def nplane(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nplane = value

    @property
    def nthick(self) -> typing.Optional[int]:
        """Get or set the nthick"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nthick

    @nthick.setter
    def nthick(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nthick = value

    @property
    def large(self) -> int:
        """Get or set the large"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].large

    @large.setter
    def large(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].large = value

    @property
    def ilocal(self) -> int:
        """Get or set the ilocal"""  # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].ilocal

    @ilocal.setter
    def ilocal(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].ilocal = value

    @property
    def strains(self) -> pd.DataFrame:
        """Get the strains."""
        ensure_card_set_properties(self, False)
        return self.sets[0].strains

    @strains.setter
    def strains(self, df: pd.DataFrame):
        ensure_card_set_properties(self, True)
        self.sets[0].strains = df

    @property
    def sets(self) -> typing.List[InitialStrainShellLegacyCardSet]:
        """Gets the list of sets."""
        return self._cards[0].items()

    def add_set(self, **kwargs):
        """Adds a set."""
        self._cards[0].add_item(**kwargs)
