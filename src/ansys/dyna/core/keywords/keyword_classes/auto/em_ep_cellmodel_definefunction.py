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

class EmEpCellmodelDefinefunction(KeywordBase):
    """DYNA EM_EP_CELLMODEL_DEFINEFUNCTION keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_DEFINEFUNCTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "nstate",
                        int,
                        10,
                        10,
                        kwargs.get("nstate")
                    ),
                    Field(
                        "fswitch",
                        int,
                        20,
                        10,
                        kwargs.get("fswitch", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dvdt",
                        int,
                        0,
                        10,
                        kwargs.get("dvdt")
                    ),
                    Field(
                        "du1dt",
                        int,
                        10,
                        10,
                        kwargs.get("du1dt")
                    ),
                    Field(
                        "du2dt",
                        int,
                        20,
                        10,
                        kwargs.get("du2dt")
                    ),
                    Field(
                        "du3dt",
                        int,
                        30,
                        10,
                        kwargs.get("du3dt")
                    ),
                    Field(
                        "du4dt",
                        int,
                        40,
                        10,
                        kwargs.get("du4dt")
                    ),
                    Field(
                        "du5dt",
                        int,
                        50,
                        10,
                        kwargs.get("du5dt")
                    ),
                    Field(
                        "du6dt",
                        int,
                        60,
                        10,
                        kwargs.get("du6dt")
                    ),
                    Field(
                        "du7dt",
                        int,
                        70,
                        10,
                        kwargs.get("du7dt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v0",
                        int,
                        0,
                        10,
                        kwargs.get("v0")
                    ),
                    Field(
                        "u1",
                        int,
                        10,
                        10,
                        kwargs.get("u1")
                    ),
                    Field(
                        "u2",
                        int,
                        20,
                        10,
                        kwargs.get("u2")
                    ),
                    Field(
                        "u3",
                        int,
                        30,
                        10,
                        kwargs.get("u3")
                    ),
                    Field(
                        "u4",
                        int,
                        40,
                        10,
                        kwargs.get("u4")
                    ),
                    Field(
                        "u5",
                        int,
                        50,
                        10,
                        kwargs.get("u5")
                    ),
                    Field(
                        "u6",
                        int,
                        60,
                        10,
                        kwargs.get("u6")
                    ),
                    Field(
                        "u7",
                        int,
                        70,
                        10,
                        kwargs.get("u7")
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT_.

        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def nstate(self) -> typing.Optional[int]:
        """Get or set the Number of state variables u1,u2,...un.The maximum value is 7.

        """ # nopep8
        return self._cards[0].get_value("nstate")

    @nstate.setter
    def nstate(self, value: int) -> None:
        self._cards[0].set_value("nstate", value)

    @property
    def fswitch(self) -> int:
        """Get or set the Switch for the ODE definition (see Remark 1):
        EQ.0:	functions
        EQ.1 : derivatives
        """ # nopep8
        return self._cards[0].get_value("fswitch")

    @fswitch.setter
    def fswitch(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""fswitch must be one of {0,1}""")
        self._cards[0].set_value("fswitch", value)

    @property
    def dvdt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of V (function g in the equations).
        """ # nopep8
        return self._cards[1].get_value("dvdt")

    @dvdt.setter
    def dvdt(self, value: int) -> None:
        self._cards[1].set_value("dvdt", value)

    @property
    def du1dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations)
        """ # nopep8
        return self._cards[1].get_value("du1dt")

    @du1dt.setter
    def du1dt(self, value: int) -> None:
        self._cards[1].set_value("du1dt", value)

    @property
    def du2dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du2dt")

    @du2dt.setter
    def du2dt(self, value: int) -> None:
        self._cards[1].set_value("du2dt", value)

    @property
    def du3dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du3dt")

    @du3dt.setter
    def du3dt(self, value: int) -> None:
        self._cards[1].set_value("du3dt", value)

    @property
    def du4dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du4dt")

    @du4dt.setter
    def du4dt(self, value: int) -> None:
        self._cards[1].set_value("du4dt", value)

    @property
    def du5dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du5dt")

    @du5dt.setter
    def du5dt(self, value: int) -> None:
        self._cards[1].set_value("du5dt", value)

    @property
    def du6dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du6dt")

    @du6dt.setter
    def du6dt(self, value: int) -> None:
        self._cards[1].set_value("du6dt", value)

    @property
    def du7dt(self) -> typing.Optional[int]:
        """Get or set the Function ID (see *DEFINE_FUNCTION) for evolution of u_i (function f_i in the equations).
        """ # nopep8
        return self._cards[1].get_value("du7dt")

    @du7dt.setter
    def du7dt(self, value: int) -> None:
        self._cards[1].set_value("du7dt", value)

    @property
    def v0(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of V.
        """ # nopep8
        return self._cards[2].get_value("v0")

    @v0.setter
    def v0(self, value: int) -> None:
        self._cards[2].set_value("v0", value)

    @property
    def u1(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u1")

    @u1.setter
    def u1(self, value: int) -> None:
        self._cards[2].set_value("u1", value)

    @property
    def u2(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u2")

    @u2.setter
    def u2(self, value: int) -> None:
        self._cards[2].set_value("u2", value)

    @property
    def u3(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u3")

    @u3.setter
    def u3(self, value: int) -> None:
        self._cards[2].set_value("u3", value)

    @property
    def u4(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u4")

    @u4.setter
    def u4(self, value: int) -> None:
        self._cards[2].set_value("u4", value)

    @property
    def u5(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u5")

    @u5.setter
    def u5(self, value: int) -> None:
        self._cards[2].set_value("u5", value)

    @property
    def u6(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u6")

    @u6.setter
    def u6(self, value: int) -> None:
        self._cards[2].set_value("u6", value)

    @property
    def u7(self) -> typing.Optional[int]:
        """Get or set the Define Function ID for initial values of u.
        """ # nopep8
        return self._cards[2].get_value("u7")

    @u7.setter
    def u7(self, value: int) -> None:
        self._cards[2].set_value("u7", value)

