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

class InitialStressSolidSet(KeywordBase):
    """DYNA INITIAL_STRESS_SOLID_SET keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_SOLID_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "nint",
                        int,
                        10,
                        10,
                        kwargs.get("nint")
                    ),
                    Field(
                        "nhisv",
                        int,
                        20,
                        10,
                        kwargs.get("nhisv")
                    ),
                    Field(
                        "large",
                        int,
                        30,
                        10,
                        kwargs.get("large")
                    ),
                    Field(
                        "iveflg",
                        int,
                        40,
                        10,
                        kwargs.get("iveflg", 0)
                    ),
                    Field(
                        "ialegp",
                        int,
                        50,
                        10,
                        kwargs.get("ialegp")
                    ),
                    Field(
                        "nthint",
                        int,
                        60,
                        10,
                        kwargs.get("nthint")
                    ),
                    Field(
                        "nthhsv",
                        int,
                        70,
                        10,
                        kwargs.get("nthhsv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigxx",
                        float,
                        0,
                        10,
                        kwargs.get("sigxx", 0.0)
                    ),
                    Field(
                        "sigyy",
                        float,
                        10,
                        10,
                        kwargs.get("sigyy", 0.0)
                    ),
                    Field(
                        "sigzz",
                        float,
                        20,
                        10,
                        kwargs.get("sigzz", 0.0)
                    ),
                    Field(
                        "sigxy",
                        float,
                        30,
                        10,
                        kwargs.get("sigxy", 0.0)
                    ),
                    Field(
                        "sigyz",
                        float,
                        40,
                        10,
                        kwargs.get("sigyz", 0.0)
                    ),
                    Field(
                        "sigzx",
                        float,
                        50,
                        10,
                        kwargs.get("sigzx", 0.0)
                    ),
                    Field(
                        "eps",
                        float,
                        60,
                        10,
                        kwargs.get("eps", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigxx",
                        float,
                        0,
                        16,
                        kwargs.get("sigxx", 0.0)
                    ),
                    Field(
                        "sigyy",
                        float,
                        16,
                        16,
                        kwargs.get("sigyy", 0.0)
                    ),
                    Field(
                        "sigzz",
                        float,
                        32,
                        16,
                        kwargs.get("sigzz", 0.0)
                    ),
                    Field(
                        "sigxy",
                        float,
                        48,
                        16,
                        kwargs.get("sigxy", 0.0)
                    ),
                    Field(
                        "sigyz",
                        float,
                        64,
                        16,
                        kwargs.get("sigyz", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the solid set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def nint(self) -> typing.Optional[int]:
        """Get or set the Number of integration points (should correspond to the solid element formulation).
        """ # nopep8
        return self._cards[0].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        self._cards[0].set_value("nint", value)

    @property
    def nhisv(self) -> typing.Optional[int]:
        """Get or set the Number of additional history variables, which is typically equal to the number of history variables stored at the integration point + IVEFLG. If NHISV exceeds the number of integration point history variables required by the constitutive model, only the number required is output; therefore, if in doubt, set NHISV to a large number
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def large(self) -> typing.Optional[int]:
        """Get or set the Format size, if zero, NHISV must also set to zero, and, if 1, a larger format is used and NHISV is used. This is the format used by LS-DYNA version 970 and earlier.
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        self._cards[0].set_value("large", value)

    @property
    def iveflg(self) -> int:
        """Get or set the Initial Volume/energy flag (only used in large format)
        EQ.0:last history variable is used as normal,
        EQ.1:last history variable is used as the initial volume of the element.
        One additional history variable is required if IVFLG=1
        EQ.2:last two history variables are used to define the initial volume
        and the internal energy per unit initial volume. Two additional
        history variables are must be allocated, see NHISV above, if
        IVFLG=2. If the initial volume is set to zero, the actual element volume is used
        """ # nopep8
        return self._cards[0].get_value("iveflg")

    @iveflg.setter
    def iveflg(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""iveflg must be one of {0,1,2}""")
        self._cards[0].set_value("iveflg", value)

    @property
    def ialegp(self) -> typing.Optional[int]:
        """Get or set the The ALE multi-material group (AMMG) ID; only if the element is of
        ALE multi-material formulation (ELEFORM = 11). In this case, each AMMG has its own sets of stress and history variables so we must
        specify to which AMMG the stress data are assigned. For mixed elements, multiple cards are needed to complete the stress
        initialization in this element as each AMMG needs to have its own set of stress data.
        EQ.0: Assuming the element is fully filled by the AMMG that the
        element part belongs to. Please refer to *ALE_MULTI-MATERIAL_GROUP card.
        EQ.n: Assigning the stress to nth AMMG in that element.
        """ # nopep8
        return self._cards[0].get_value("ialegp")

    @ialegp.setter
    def ialegp(self, value: int) -> None:
        self._cards[0].set_value("ialegp", value)

    @property
    def nthint(self) -> typing.Optional[int]:
        """Get or set the Number of thermal integration points
        """ # nopep8
        return self._cards[0].get_value("nthint")

    @nthint.setter
    def nthint(self, value: int) -> None:
        self._cards[0].set_value("nthint", value)

    @property
    def nthhsv(self) -> typing.Optional[int]:
        """Get or set the Number of thermal history variables per thermal integration point
        """ # nopep8
        return self._cards[0].get_value("nthhsv")

    @nthhsv.setter
    def nthhsv(self, value: int) -> None:
        self._cards[0].set_value("nthhsv", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Defines the 11 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        self._cards[1].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Defines the 22 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        self._cards[1].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Defines the 33 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        self._cards[1].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Defines the 12 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        self._cards[1].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Defines the 23 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        self._cards[1].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Defines the 31 stress component.
        """ # nopep8
        return self._cards[1].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        self._cards[1].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[1].set_value("eps", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Defines the 11 stress component.
        """ # nopep8
        return self._cards[2].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        self._cards[2].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Defines the 22 stress component.
        """ # nopep8
        return self._cards[2].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        self._cards[2].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Defines the 33 stress component.
        """ # nopep8
        return self._cards[2].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        self._cards[2].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Defines the 12 stress component.
        """ # nopep8
        return self._cards[2].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        self._cards[2].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Defines the 23 stress component.
        """ # nopep8
        return self._cards[2].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        self._cards[2].set_value("sigyz", value)

