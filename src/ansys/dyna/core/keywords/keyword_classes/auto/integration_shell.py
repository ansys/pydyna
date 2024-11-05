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

class IntegrationShell(KeywordBase):
    """DYNA INTEGRATION_SHELL keyword"""

    keyword = "INTEGRATION"
    subkeyword = "SHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "irid",
                        int,
                        0,
                        10,
                        kwargs.get("irid")
                    ),
                    Field(
                        "nip",
                        int,
                        10,
                        10,
                        kwargs.get("nip")
                    ),
                    Field(
                        "esop",
                        int,
                        20,
                        10,
                        kwargs.get("esop", 0)
                    ),
                    Field(
                        "failopt",
                        int,
                        30,
                        10,
                        kwargs.get("failopt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s",
                        float,
                        0,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "wf",
                        float,
                        10,
                        10,
                        kwargs.get("wf")
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid")
                    ),
                ],
            ),
        ]

    @property
    def irid(self) -> typing.Optional[int]:
        """Get or set the Integration rule ID (IRID refers to IRID on *SECTION_SHELL card).
        """ # nopep8
        return self._cards[0].get_value("irid")

    @irid.setter
    def irid(self, value: int) -> None:
        self._cards[0].set_value("irid", value)

    @property
    def nip(self) -> typing.Optional[int]:
        """Get or set the Number of integration points.
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[0].set_value("nip", value)

    @property
    def esop(self) -> int:
        """Get or set the Equal spacing of integration points option:
        EQ.0: integration points are defined below (default),
        EQ.1: integration points are equally spaced through thickness such that the shell is subdivided into NIP layers of equal thickness.
        """ # nopep8
        return self._cards[0].get_value("esop")

    @esop.setter
    def esop(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""esop must be one of {0,1}""")
        self._cards[0].set_value("esop", value)

    @property
    def failopt(self) -> int:
        """Get or set the Treatment of failure when mixing different constitutive types, which do and do not include failure models, through the shell thickness.
        EQ.0: element is deleted when layers which include failure, fail
        EQ.1: element failure cannot occur since some layers do not have a failure option
        """ # nopep8
        return self._cards[0].get_value("failopt")

    @failopt.setter
    def failopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""failopt must be one of {0,1}""")
        self._cards[0].set_value("failopt", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Coordinate of integration point in range -1.0 to 1.0.
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[1].set_value("s", value)

    @property
    def wf(self) -> typing.Optional[float]:
        """Get or set the Weighting factor. This is typically the thickness associated with the integration point divided by actual shell thickness, i.e., the weighting factor for the ith integration point = dti/t.
        """ # nopep8
        return self._cards[1].get_value("wf")

    @wf.setter
    def wf(self, value: float) -> None:
        self._cards[1].set_value("wf", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Optional part ID if different from the PID specified on the element card.  The material type is not allowed to change, see *PART. The average mass density for the shell element is based on a weighted average of the density of each layer that is used through the thickness. When modifying the constitutive constants throuigh the thickness, it is often necessary to defined unique part IDs without elements that are referenced only by the user integration rule. These additional part IDs only provide a density and constitutive constants with local material axes (if used) and orientation angles taken from the PID referenced on the element card. In defining a PID for an integration point, it is okay to reference a solid element PID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

