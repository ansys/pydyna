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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatAddPzelectric(KeywordBase):
    """DYNA MAT_ADD_PZELECTRIC keyword"""

    keyword = "MAT"
    subkeyword = "ADD_PZELECTRIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        str,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "dtype",
                        str,
                        10,
                        10,
                        kwargs.get("dtype", "S")
                    ),
                    Field(
                        "gpt",
                        int,
                        20,
                        10,
                        kwargs.get("gpt", 8)
                    ),
                    Field(
                        "aopt",
                        int,
                        30,
                        10,
                        kwargs.get("aopt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dxx",
                        float,
                        0,
                        10,
                        kwargs.get("dxx")
                    ),
                    Field(
                        "dyy",
                        float,
                        10,
                        10,
                        kwargs.get("dyy")
                    ),
                    Field(
                        "dzz",
                        float,
                        20,
                        10,
                        kwargs.get("dzz")
                    ),
                    Field(
                        "dxy",
                        float,
                        30,
                        10,
                        kwargs.get("dxy")
                    ),
                    Field(
                        "dxz",
                        float,
                        40,
                        10,
                        kwargs.get("dxz")
                    ),
                    Field(
                        "dyz",
                        float,
                        50,
                        10,
                        kwargs.get("dyz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "px11",
                        float,
                        0,
                        10,
                        kwargs.get("px11")
                    ),
                    Field(
                        "px22",
                        float,
                        10,
                        10,
                        kwargs.get("px22")
                    ),
                    Field(
                        "px33",
                        float,
                        20,
                        10,
                        kwargs.get("px33")
                    ),
                    Field(
                        "px12",
                        float,
                        30,
                        10,
                        kwargs.get("px12")
                    ),
                    Field(
                        "px13",
                        float,
                        40,
                        10,
                        kwargs.get("px13")
                    ),
                    Field(
                        "px23",
                        float,
                        50,
                        10,
                        kwargs.get("px23")
                    ),
                    Field(
                        "py11",
                        float,
                        60,
                        10,
                        kwargs.get("py11")
                    ),
                    Field(
                        "py22",
                        float,
                        70,
                        10,
                        kwargs.get("py22")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "py33",
                        float,
                        0,
                        10,
                        kwargs.get("py33")
                    ),
                    Field(
                        "py12",
                        float,
                        10,
                        10,
                        kwargs.get("py12")
                    ),
                    Field(
                        "px13",
                        float,
                        20,
                        10,
                        kwargs.get("px13")
                    ),
                    Field(
                        "py23",
                        float,
                        30,
                        10,
                        kwargs.get("py23")
                    ),
                    Field(
                        "pz11",
                        float,
                        40,
                        10,
                        kwargs.get("pz11")
                    ),
                    Field(
                        "pz22",
                        float,
                        50,
                        10,
                        kwargs.get("pz22")
                    ),
                    Field(
                        "pz33",
                        float,
                        60,
                        10,
                        kwargs.get("pz33")
                    ),
                    Field(
                        "pz12",
                        float,
                        70,
                        10,
                        kwargs.get("pz12")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pz13",
                        float,
                        0,
                        10,
                        kwargs.get("pz13")
                    ),
                    Field(
                        "pz23",
                        float,
                        10,
                        10,
                        kwargs.get("pz23")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddPzelectric.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[str]:
        """Get or set the Material ID for which the piezoelectric properties apply
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: str) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def dtype(self) -> str:
        """Get or set the Type of piezoelectric property definition (see remarks below)
        EQ.S:	stress based definition
        EQ.E : strain based definition
        """ # nopep8
        return self._cards[0].get_value("dtype")

    @dtype.setter
    def dtype(self, value: str) -> None:
        if value not in ["S", "E"]:
            raise Exception("""dtype must be one of {"S","E"}""")
        self._cards[0].set_value("dtype", value)

    @property
    def gpt(self) -> int:
        """Get or set the Number of Gauss points used for integration:
        EQ.0: Default value 8.full integration
        EQ.1: 	reduced integration
        """ # nopep8
        return self._cards[0].get_value("gpt")

    @gpt.setter
    def gpt(self, value: int) -> None:
        self._cards[0].set_value("gpt", value)

    @property
    def aopt(self) -> int:
        """Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
        EQ.0.0:	locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : locally orthotropic with material axes determined by a point in space and the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""aopt must be one of {0,1,2}""")
        self._cards[0].set_value("aopt", value)

    @property
    def dxx(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dxx")

    @dxx.setter
    def dxx(self, value: float) -> None:
        self._cards[1].set_value("dxx", value)

    @property
    def dyy(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dyy")

    @dyy.setter
    def dyy(self, value: float) -> None:
        self._cards[1].set_value("dyy", value)

    @property
    def dzz(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dzz")

    @dzz.setter
    def dzz(self, value: float) -> None:
        self._cards[1].set_value("dzz", value)

    @property
    def dxy(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dxy")

    @dxy.setter
    def dxy(self, value: float) -> None:
        self._cards[1].set_value("dxy", value)

    @property
    def dxz(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dxz")

    @dxz.setter
    def dxz(self, value: float) -> None:
        self._cards[1].set_value("dxz", value)

    @property
    def dyz(self) -> typing.Optional[float]:
        """Get or set the Dielectric permittivity matrix, d_αβ.  α, β = x, y, z
        """ # nopep8
        return self._cards[1].get_value("dyz")

    @dyz.setter
    def dyz(self, value: float) -> None:
        self._cards[1].set_value("dyz", value)

    @property
    def px11(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px11")

    @px11.setter
    def px11(self, value: float) -> None:
        self._cards[2].set_value("px11", value)

    @property
    def px22(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px22")

    @px22.setter
    def px22(self, value: float) -> None:
        self._cards[2].set_value("px22", value)

    @property
    def px33(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px33")

    @px33.setter
    def px33(self, value: float) -> None:
        self._cards[2].set_value("px33", value)

    @property
    def px12(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px12")

    @px12.setter
    def px12(self, value: float) -> None:
        self._cards[2].set_value("px12", value)

    @property
    def px13(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px13")

    @px13.setter
    def px13(self, value: float) -> None:
        self._cards[2].set_value("px13", value)

    @property
    def px23(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("px23")

    @px23.setter
    def px23(self, value: float) -> None:
        self._cards[2].set_value("px23", value)

    @property
    def py11(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("py11")

    @py11.setter
    def py11(self, value: float) -> None:
        self._cards[2].set_value("py11", value)

    @property
    def py22(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[2].get_value("py22")

    @py22.setter
    def py22(self, value: float) -> None:
        self._cards[2].set_value("py22", value)

    @property
    def py33(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("py33")

    @py33.setter
    def py33(self, value: float) -> None:
        self._cards[3].set_value("py33", value)

    @property
    def py12(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("py12")

    @py12.setter
    def py12(self, value: float) -> None:
        self._cards[3].set_value("py12", value)

    @property
    def px13(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("px13")

    @px13.setter
    def px13(self, value: float) -> None:
        self._cards[3].set_value("px13", value)

    @property
    def py23(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("py23")

    @py23.setter
    def py23(self, value: float) -> None:
        self._cards[3].set_value("py23", value)

    @property
    def pz11(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("pz11")

    @pz11.setter
    def pz11(self, value: float) -> None:
        self._cards[3].set_value("pz11", value)

    @property
    def pz22(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("pz22")

    @pz22.setter
    def pz22(self, value: float) -> None:
        self._cards[3].set_value("pz22", value)

    @property
    def pz33(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("pz33")

    @pz33.setter
    def pz33(self, value: float) -> None:
        self._cards[3].set_value("pz33", value)

    @property
    def pz12(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[3].get_value("pz12")

    @pz12.setter
    def pz12(self, value: float) -> None:
        self._cards[3].set_value("pz12", value)

    @property
    def pz13(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[4].get_value("pz13")

    @pz13.setter
    def pz13(self, value: float) -> None:
        self._cards[4].set_value("pz13", value)

    @property
    def pz23(self) -> typing.Optional[float]:
        """Get or set the Piezoelectric matrix which depends on DTYPE.   α = x, y, z and i,j = 1, 2, 3.
        """ # nopep8
        return self._cards[4].get_value("pz23")

    @pz23.setter
    def pz23(self, value: float) -> None:
        self._cards[4].set_value("pz23", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[6].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

