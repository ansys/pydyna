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

class EmContactSubdom(KeywordBase):
    """DYNA EM_CONTACT_SUBDOM keyword"""

    keyword = "EM"
    subkeyword = "CONTACT_SUBDOM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sdtype",
                        int,
                        0,
                        10,
                        kwargs.get("sdtype", 1)
                    ),
                    Field(
                        "mvtype",
                        int,
                        10,
                        10,
                        kwargs.get("mvtype", 0)
                    ),
                    Field(
                        "lcidx/nid",
                        int,
                        20,
                        10,
                        kwargs.get("lcidx/nid")
                    ),
                    Field(
                        "lcidy",
                        int,
                        30,
                        10,
                        kwargs.get("lcidy")
                    ),
                    Field(
                        "lcidz",
                        int,
                        40,
                        10,
                        kwargs.get("lcidz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r",
                        float,
                        0,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "pminx",
                        float,
                        10,
                        10,
                        kwargs.get("pminx")
                    ),
                    Field(
                        "pminy",
                        float,
                        20,
                        10,
                        kwargs.get("pminy")
                    ),
                    Field(
                        "pminz",
                        float,
                        30,
                        10,
                        kwargs.get("pminz")
                    ),
                    Field(
                        "pmaxx",
                        float,
                        40,
                        10,
                        kwargs.get("pmaxx")
                    ),
                    Field(
                        "pmaxy",
                        float,
                        50,
                        10,
                        kwargs.get("pmaxy")
                    ),
                    Field(
                        "pmaxz",
                        float,
                        60,
                        10,
                        kwargs.get("pmaxz")
                    ),
                ],
            ),
        ]

    @property
    def sdtype(self) -> int:
        """Get or set the Subdomain definition type:
        EQ.1: Defined by box
        EQ.2: Defined by cylinder.
        EQ.3: Defined by sphere.
        .
        """ # nopep8
        return self._cards[0].get_value("sdtype")

    @sdtype.setter
    def sdtype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""sdtype must be one of {1,2,3}""")
        self._cards[0].set_value("sdtype", value)

    @property
    def mvtype(self) -> int:
        """Get or set the Movement type of subdomain:
        EQ.0: Static subdomain (Default).
        EQ.1: Domain translates in the three directions by the velocities given by LCIDX,LCIDY,LCIDZ.
        EQ.2: Domain follows the displacements of the node ID given by NID.

        """ # nopep8
        return self._cards[0].get_value("mvtype")

    @mvtype.setter
    def mvtype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""mvtype must be one of {0,1,2}""")
        self._cards[0].set_value("mvtype", value)

    @property
    def lcidx_nid(self) -> typing.Optional[int]:
        """Get or set the Time dependent load curve ID for the translational velocity in the X direction for MVTYPE = 1, Node ID for MVTYPE = 2.

        """ # nopep8
        return self._cards[0].get_value("lcidx/nid")

    @lcidx_nid.setter
    def lcidx_nid(self, value: int) -> None:
        self._cards[0].set_value("lcidx/nid", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
        .
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
        .
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[0].set_value("lcidz", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere if SDTYPE = 3 or the cylinder if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def pminx(self) -> typing.Optional[float]:
        """Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pminx")

    @pminx.setter
    def pminx(self, value: float) -> None:
        self._cards[1].set_value("pminx", value)

    @property
    def pminy(self) -> typing.Optional[float]:
        """Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pminy")

    @pminy.setter
    def pminy(self, value: float) -> None:
        self._cards[1].set_value("pminy", value)

    @property
    def pminz(self) -> typing.Optional[float]:
        """Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pminz")

    @pminz.setter
    def pminz(self, value: float) -> None:
        self._cards[1].set_value("pminz", value)

    @property
    def pmaxx(self) -> typing.Optional[float]:
        """Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pmaxx")

    @pmaxx.setter
    def pmaxx(self, value: float) -> None:
        self._cards[1].set_value("pmaxx", value)

    @property
    def pmaxy(self) -> typing.Optional[float]:
        """Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pmaxy")

    @pmaxy.setter
    def pmaxy(self, value: float) -> None:
        self._cards[1].set_value("pmaxy", value)

    @property
    def pmaxz(self) -> typing.Optional[float]:
        """Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.

        """ # nopep8
        return self._cards[1].get_value("pmaxz")

    @pmaxz.setter
    def pmaxz(self, value: float) -> None:
        self._cards[1].set_value("pmaxz", value)

