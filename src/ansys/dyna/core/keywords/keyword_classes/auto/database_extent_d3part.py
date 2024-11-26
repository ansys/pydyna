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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseExtentD3Part(KeywordBase):
    """DYNA DATABASE_EXTENT_D3PART keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_D3PART"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "neiph",
                        int,
                        0,
                        10,
                        kwargs.get("neiph", 0)
                    ),
                    Field(
                        "neips",
                        int,
                        10,
                        10,
                        kwargs.get("neips", 0)
                    ),
                    Field(
                        "maxint",
                        int,
                        20,
                        10,
                        kwargs.get("maxint", 3)
                    ),
                    Field(
                        "strflg",
                        int,
                        30,
                        10,
                        kwargs.get("strflg", 0)
                    ),
                    Field(
                        "sigflg",
                        int,
                        40,
                        10,
                        kwargs.get("sigflg", 1)
                    ),
                    Field(
                        "epsflg",
                        int,
                        50,
                        10,
                        kwargs.get("epsflg", 1)
                    ),
                    Field(
                        "rltflg",
                        int,
                        60,
                        10,
                        kwargs.get("rltflg", 1)
                    ),
                    Field(
                        "engflg",
                        int,
                        70,
                        10,
                        kwargs.get("engflg", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ieverp",
                        int,
                        10,
                        10,
                        kwargs.get("ieverp", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "shge",
                        int,
                        40,
                        10,
                        kwargs.get("shge", 1)
                    ),
                    Field(
                        "stssz",
                        int,
                        50,
                        10,
                        kwargs.get("stssz", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nintsld",
                        int,
                        0,
                        10,
                        kwargs.get("nintsld", 1)
                    ),
                ],
            ),
        ]

    @property
    def neiph(self) -> int:
        """Get or set the Number of additional integration point history variables written to the
        binary database for solid elements. The integration point data is written
        in the same order that it is stored in memory-each material model has its
        own history variables that are stored. For user defined materials it is
        important to store the history data that is needed for plotting before the
        data which is not of interest.
        """ # nopep8
        return self._cards[0].get_value("neiph")

    @neiph.setter
    def neiph(self, value: int) -> None:
        self._cards[0].set_value("neiph", value)

    @property
    def neips(self) -> int:
        """Get or set the Number of additional integration point history variables written to the
        binary database for both shell and thick shell elements for each
        integration point, see NEIPH above.
        """ # nopep8
        return self._cards[0].get_value("neips")

    @neips.setter
    def neips(self, value: int) -> None:
        self._cards[0].set_value("neips", value)

    @property
    def maxint(self) -> int:
        """Get or set the Number of shell integration points written to the binary database, see
        also *INTEGRATION_SHELL. If the default value of 3 is used then
        results are output for the outermost (top) and innermost (bottom)
        integration points together with results for the neutral axis. If MAXINT
        is set to 3 and the element has 1 integration point then all three results
        will be the same. If a value other than 3 is used then results for the first
        MAXINT integration points in the element will be output. Note: If the
        element has an even number of integration points and MAXINT is not
        set to 3 then you will not get mid-surface results. See Remarks below.
        If MAXINT is set to a negative number, MAXINT integration points are
        output for each in plane integration point location and no averaging is
        used. This can greatly increase the size of the binary databases
        D3PLOT, D3THDT, and D3PART.
        """ # nopep8
        return self._cards[0].get_value("maxint")

    @maxint.setter
    def maxint(self, value: int) -> None:
        self._cards[0].set_value("maxint", value)

    @property
    def strflg(self) -> int:
        """Get or set the Set to 1 to dump strain tensors for solid, shell and thick shell elements
        for plotting by LS-PREPOST and ASCII file ELOUT. For shell and
        thick shell elements two tensors are written, one at the innermost and
        one at the outermost integration point. For solid elements a single strain
        tensor is written.
        """ # nopep8
        return self._cards[0].get_value("strflg")

    @strflg.setter
    def strflg(self, value: int) -> None:
        self._cards[0].set_value("strflg", value)

    @property
    def sigflg(self) -> int:
        """Get or set the Flag for including stress tensor in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("sigflg")

    @sigflg.setter
    def sigflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""sigflg must be one of {1,2}""")
        self._cards[0].set_value("sigflg", value)

    @property
    def epsflg(self) -> int:
        """Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("epsflg")

    @epsflg.setter
    def epsflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""epsflg must be one of {1,2}""")
        self._cards[0].set_value("epsflg", value)

    @property
    def rltflg(self) -> int:
        """Get or set the Flag for including stress resultants in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("rltflg")

    @rltflg.setter
    def rltflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""rltflg must be one of {1,2}""")
        self._cards[0].set_value("rltflg", value)

    @property
    def engflg(self) -> int:
        """Get or set the Flag for including internal energy density and thickness in the LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("engflg")

    @engflg.setter
    def engflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""engflg must be one of {1,2}""")
        self._cards[0].set_value("engflg", value)

    @property
    def ieverp(self) -> int:
        """Get or set the Every plot state for D3PART database is written to a separate file. This option will limit the database to 100 states:
        EQ.0: more than one state can be on each plotfile,
        EQ.1: one state only on each plotfile.
        """ # nopep8
        return self._cards[1].get_value("ieverp")

    @ieverp.setter
    def ieverp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ieverp must be one of {0,1}""")
        self._cards[1].set_value("ieverp", value)

    @property
    def shge(self) -> int:
        """Get or set the Output shell hourglass energy density:
        EQ.1: off (default), no hourglass energy written,
        EQ.2: on.
        """ # nopep8
        return self._cards[1].get_value("shge")

    @shge.setter
    def shge(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""shge must be one of {1,2}""")
        self._cards[1].set_value("shge", value)

    @property
    def stssz(self) -> int:
        """Get or set the Output shell element time step, mass or added mass:
        EQ.1: off (default),
        EQ.2: out time step size,
        EQ.3: output mass, added mass, or time step size.
        (See Remark 3 in user's manual).
        """ # nopep8
        return self._cards[1].get_value("stssz")

    @stssz.setter
    def stssz(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""stssz must be one of {1,2,3}""")
        self._cards[1].set_value("stssz", value)

    @property
    def nintsld(self) -> int:
        """Get or set the Number of solid element integration points written to the LS-DYNA
        database. The default value is 1. For solids with multiple integration
        points NINTSLD may be set to 8. Currently, no other values for
        NINTSLD are allowed. For solids with multiple integration points, an
        average value is output if NINTSLD is set to 1.
        """ # nopep8
        return self._cards[2].get_value("nintsld")

    @nintsld.setter
    def nintsld(self, value: int) -> None:
        self._cards[2].set_value("nintsld", value)

