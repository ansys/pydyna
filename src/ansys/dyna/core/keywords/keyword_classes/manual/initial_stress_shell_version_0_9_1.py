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

"""Legacy INITIAL_STRESS_SHELL implementation (version 0.9.1)."""
# flake8: noqa: E501
import typing
import warnings

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.series_card import SeriesCard  # noqa: F401

class InitialStressShellLegacyThicknessLargeCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStressShellLegacyThicknessLargeCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "t",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigxx",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigyy",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigzz",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigxy",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigyz",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "sigzx",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "eps",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            SeriesCard(
                "hisv",
                8,
                10,
                float,
                lambda: self.parent.nhisv,
                data = kwargs.get("hisv")),
        ]

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate of through thickness integration point. Between -1 and 1 inclusive.
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[0].set_value("t", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Define the xx stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        """Set the sigxx property."""
        self._cards[0].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Define the yy stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        """Set the sigyy property."""
        self._cards[0].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Define the zz stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        """Set the sigzz property."""
        self._cards[0].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Define the xy stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        """Set the sigxy property."""
        self._cards[0].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Define the yz stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        """Set the sigyz property."""
        self._cards[0].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Define the zx stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        """Set the sigzx property."""
        self._cards[0].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        """Set the eps property."""
        self._cards[0].set_value("eps", value)

    @property
    def hisv(self) -> SeriesCard:
        """dynamic array of history variables."""
        return self._cards[1]

    @hisv.setter
    def hisv(self, value: typing.List) -> None:
        self._cards[1].data = value

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

class InitialStressShellLegacyCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the InitialStressShellLegacyCardSet CardSet."""
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nthick",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nhisv",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ntensr",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "large",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nthint",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nthhsv",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            CardSet(
                InitialStressShellLegacyThicknessLargeCardSet,
                length_func = lambda: self.nplane * self.nthick if (self.nplane and self.nthick) else 2,
                active_func = lambda: self.large == None or self.large == 0,
                **kwargs
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
    def nplane(self) -> int:
        """Get or set the Number of in plane integration points being output.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        self._cards[0].set_value("nplane", value)

    @property
    def nthick(self) -> int:
        """Get or set the Number of through thickness integration points.
        """ # nopep8
        return self._cards[0].get_value("nthick")

    @nthick.setter
    def nthick(self, value: int) -> None:
        """Set the nthick property."""
        self._cards[0].set_value("nthick", value)

    @property
    def nhisv(self) -> int:
        """Get or set the Number of additional history variables.
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        """Set the nhisv property."""
        self._cards[0].set_value("nhisv", value)

    @property
    def ntensr(self) -> int:
        """Get or set the Number of components of tensor data taken from the element history variables.
        """ # nopep8
        return self._cards[0].get_value("ntensr")

    @ntensr.setter
    def ntensr(self, value: int) -> None:
        """Set the ntensr property."""
        self._cards[0].set_value("ntensr", value)

    @property
    def large(self) -> int:
        """Get or set the Format size (0:off or 1:on).
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        """Set the large property."""
        if value not in [0, 1, None]:
            raise Exception("""large must be `None` or one of {0,1}.""")
        self._cards[0].set_value("large", value)

    @property
    def nthint(self) -> int:
        """Get or set the Number of thermal integration points.
        """ # nopep8
        return self._cards[0].get_value("nthint")

    @nthint.setter
    def nthint(self, value: int) -> None:
        """Set the nthint property."""
        self._cards[0].set_value("nthint", value)

    @property
    def nthhsv(self) -> int:
        """Get or set the Number of thermal history variables per thermal integration point..
        """ # nopep8
        return self._cards[0].get_value("nthhsv")

    @nthhsv.setter
    def nthhsv(self, value: int) -> None:
        """Set the nthhsv property."""
        self._cards[0].set_value("nthhsv", value)

    @property
    def sets(self) -> typing.List[InitialStressShellLegacyThicknessLargeCardSet]:
        """Gets the list of sets."""
        return self._cards[1].items()

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

class InitialStressShellLegacy(KeywordBase):
    """DYNA INITIAL_STRESS_SHELL keyword (legacy version 0.9.1).

    .. deprecated:: 0.10.0
        Use :class:`~ansys.dyna.core.keywords.InitialStressShell` instead.
        This legacy version uses TableCard/DataFrame API which does not support LARGE format.
        The new version uses CardSet API and supports both standard and LARGE formats.

    To use this legacy class when loading decks:

    .. code-block:: python

        from ansys.dyna.core.lib.deck import Deck
        from ansys.dyna.core.lib.import_handler import ImportContext
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_stress_shell_version_0_9_1 import InitialStressShellLegacy

        deck = Deck()
        context = ImportContext(
            deck=deck,
            keyword_overrides={"*INITIAL_STRESS_SHELL": InitialStressShellLegacy},
        )
        deck.loads(data, context=context)
    """

    keyword = "INITIAL"
    subkeyword = "STRESS_SHELL"

    def __init__(self, **kwargs):
        """Initialize the InitialStressShellLegacy class."""
        warnings.warn(
            "InitialStressShellLegacy is deprecated and will be removed in a future version. "
            "This legacy class uses TableCard/DataFrame API which does not support LARGE format. "
            "Use InitialStressShell instead, which uses CardSet API and supports both standard and LARGE formats.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        self._cards = [
            CardSet(
                InitialStressShellLegacyCardSet,
                **kwargs
            ),
        ]

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
    def nplane(self) -> int:
        """Get or set the nplane
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nplane

    @nplane.setter
    def nplane(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nplane = value

    @property
    def nthick(self) -> int:
        """Get or set the nthick
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nthick

    @nthick.setter
    def nthick(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nthick = value

    @property
    def nhisv(self) -> int:
        """Get or set the nhisv
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nhisv

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nhisv = value

    @property
    def ntensr(self) -> int:
        """Get or set the ntensr
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].ntensr

    @ntensr.setter
    def ntensr(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].ntensr = value

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
    def nthint(self) -> int:
        """Get or set the nthint
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nthint

    @nthint.setter
    def nthint(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nthint = value

    @property
    def nthhsv(self) -> int:
        """Get or set the nthhsv
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].nthhsv

    @nthhsv.setter
    def nthhsv(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].nthhsv = value

    @property
    def sets(self) -> typing.List[InitialStressShellLegacyCardSet]:
        """Gets the list of sets."""
        return self._cards[0].items()

    def add_set(self, **kwargs):
        """Adds a set."""
        self._cards[0].add_item(**kwargs)

