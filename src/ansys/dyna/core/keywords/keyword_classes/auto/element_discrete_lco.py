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

class ElementDiscreteLco(KeywordBase):
    """DYNA ELEMENT_DISCRETE_LCO keyword"""

    keyword = "ELEMENT"
    subkeyword = "DISCRETE_LCO"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "n1",
                        int,
                        16,
                        8,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        24,
                        8,
                        kwargs.get("n2")
                    ),
                    Field(
                        "vid",
                        int,
                        32,
                        8,
                        kwargs.get("vid", 0)
                    ),
                    Field(
                        "s",
                        float,
                        40,
                        16,
                        kwargs.get("s", 1.0)
                    ),
                    Field(
                        "pf",
                        int,
                        56,
                        8,
                        kwargs.get("pf", 0)
                    ),
                    Field(
                        "offset",
                        float,
                        64,
                        16,
                        kwargs.get("offset", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "lciddr",
                        int,
                        10,
                        10,
                        kwargs.get("lciddr")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2. If zero, the spring/damper connects node N1 to ground.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def vid(self) -> int:
        """Get or set the Orientation option: The orientation option should be used cautiously since forces, which are generated as the nodal points displace, are not orthogonal to rigid body rotation unless the nodes are coincident.. The type 6, 3D beam element, is recommended when orientation is required with the absolute value of the parameter SCOOR set to 2 or 3, since this option avoids rotational constraints.
        EQ.0: the spring/damper acts along the axis from node N1 to N2,
        NE.0: the spring/damper acts along the axis defined by the orientation vector, VID defined in the *DEFINE_SD_ORIENTATION section.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def s(self) -> float:
        """Get or set the Scale factor on forces.
        """ # nopep8
        return self._cards[0].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[0].set_value("s", value)

    @property
    def pf(self) -> int:
        """Get or set the Print flag:
        EQ.0: forces are printed in DEFORC file, see *DATABASE_OPTION,
        EQ.1: forces are not printed in DEFORC file.
        """ # nopep8
        return self._cards[0].get_value("pf")

    @pf.setter
    def pf(self, value: int) -> None:
        self._cards[0].set_value("pf", value)

    @property
    def offset(self) -> float:
        """Get or set the Initial offset. The initial offset is a displacement or rotation at time zero.
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[0].set_value("offset", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the initial OFFSET as a function of time.  Positive offsets correspond to tensile forces, and, likewise negative offset result incompressive forces.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def lciddr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining OFFSET as a function of time during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("lciddr")

    @lciddr.setter
    def lciddr(self, value: int) -> None:
        self._cards[1].set_value("lciddr", value)

