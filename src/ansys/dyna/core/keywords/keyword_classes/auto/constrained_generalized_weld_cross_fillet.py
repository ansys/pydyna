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

class ConstrainedGeneralizedWeldCrossFillet(KeywordBase):
    """DYNA CONSTRAINED_GENERALIZED_WELD_CROSS_FILLET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "GENERALIZED_WELD_CROSS_FILLET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "wid",
                        int,
                        0,
                        10,
                        kwargs.get("wid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "filter",
                        int,
                        20,
                        10,
                        kwargs.get("filter")
                    ),
                    Field(
                        "window",
                        float,
                        30,
                        10,
                        kwargs.get("window", 0)
                    ),
                    Field(
                        "npr",
                        int,
                        40,
                        10,
                        kwargs.get("npr")
                    ),
                    Field(
                        "nprt",
                        int,
                        50,
                        10,
                        kwargs.get("nprt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tfail",
                        float,
                        0,
                        10,
                        kwargs.get("tfail", 1.0E+20)
                    ),
                    Field(
                        "epsf",
                        float,
                        10,
                        10,
                        kwargs.get("epsf")
                    ),
                    Field(
                        "sigy",
                        float,
                        20,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "beta",
                        float,
                        30,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "l",
                        float,
                        40,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "w",
                        float,
                        50,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "a",
                        float,
                        60,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "alpha",
                        float,
                        70,
                        10,
                        kwargs.get("alpha")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodea",
                        int,
                        0,
                        10,
                        kwargs.get("nodea")
                    ),
                    Field(
                        "nodeb",
                        int,
                        10,
                        10,
                        kwargs.get("nodeb")
                    ),
                    Field(
                        "ncid",
                        int,
                        20,
                        10,
                        kwargs.get("ncid", 0)
                    ),
                ],
            ),
        ]

    @property
    def wid(self) -> typing.Optional[int]:
        """Get or set the Optional weld ID
        """ # nopep8
        return self._cards[0].get_value("wid")

    @wid.setter
    def wid(self, value: int) -> None:
        self._cards[0].set_value("wid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[1].set_value("nsid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. CID is not required for spotwelds if the nodes are not coincident.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def filter(self) -> typing.Optional[int]:
        """Get or set the Number of force vectors saved for filtering. This option can eliminate spurious failures due to numerical force spikes; however, memory requirements are significant since 6 force components are stored with each vector.
        LE.1: no filtering,
        EQ.n: simple average of force components divided by n or the maximum number of force vectors that are stored for the time window option below.
        """ # nopep8
        return self._cards[1].get_value("filter")

    @filter.setter
    def filter(self, value: int) -> None:
        self._cards[1].set_value("filter", value)

    @property
    def window(self) -> float:
        """Get or set the Time window for filtering. This option requires the specification of the maximum number of steps which can occur within the filtering time window. If the time step decreases too far, then the filtering time window will be ignored and the simple average is used.
        EQ.0: time window is not used.
        """ # nopep8
        return self._cards[1].get_value("window")

    @window.setter
    def window(self, value: float) -> None:
        self._cards[1].set_value("window", value)

    @property
    def npr(self) -> typing.Optional[int]:
        """Get or set the NFW, number of individual nodal pairs (only cross fillet or combined general weld).
        """ # nopep8
        return self._cards[1].get_value("npr")

    @npr.setter
    def npr(self, value: int) -> None:
        self._cards[1].set_value("npr", value)

    @property
    def nprt(self) -> int:
        """Get or set the Print option in file RBDOUT.
        EQ.0: default from control card is used (default),
        EQ.1: data is printed,
        EQ.2: data is not printed.
        """ # nopep8
        return self._cards[1].get_value("nprt")

    @nprt.setter
    def nprt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""nprt must be one of {0,1,2}""")
        self._cards[1].set_value("nprt", value)

    @property
    def tfail(self) -> float:
        """Get or set the Failure time for constraint set, tf (default=1.0E+20).
        """ # nopep8
        return self._cards[2].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        self._cards[2].set_value("tfail", value)

    @property
    def epsf(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain at failure.
        """ # nopep8
        return self._cards[2].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        self._cards[2].set_value("epsf", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the sigma-f, stress at failure for brittle failure.
        """ # nopep8
        return self._cards[2].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[2].set_value("sigy", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the beta, failure parameter for brittle failure.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[2].set_value("beta", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the L, length of fillet/butt weld.
        """ # nopep8
        return self._cards[2].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[2].set_value("l", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the w, width of flange.
        """ # nopep8
        return self._cards[2].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[2].set_value("w", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the a, width of fillet weld.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[2].set_value("a", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the alpha, weld angle in degrees.
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[2].set_value("alpha", value)

    @property
    def nodea(self) -> typing.Optional[int]:
        """Get or set the Node ID, A, in weld pair.
        """ # nopep8
        return self._cards[3].get_value("nodea")

    @nodea.setter
    def nodea(self, value: int) -> None:
        self._cards[3].set_value("nodea", value)

    @property
    def nodeb(self) -> typing.Optional[int]:
        """Get or set the Node ID, B, in weld pair.
        """ # nopep8
        return self._cards[3].get_value("nodeb")

    @nodeb.setter
    def nodeb(self, value: int) -> None:
        self._cards[3].set_value("nodeb", value)

    @property
    def ncid(self) -> int:
        """Get or set the Local coordinate system ID.
        """ # nopep8
        return self._cards[3].get_value("ncid")

    @ncid.setter
    def ncid(self, value: int) -> None:
        self._cards[3].set_value("ncid", value)

