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

"""Module providing the ElementShellNurbsPatchV3 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ElementShellNurbsPatchV3(KeywordBase):
    """DYNA ELEMENT_SHELL_NURBS_PATCH_V3 keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_NURBS_PATCH_V3"

    def __init__(self, **kwargs):
        """Initialize the ElementShellNurbsPatchV3 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "npeid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "npr",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nps",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ps",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "wfl",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "form",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "int",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nisr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "niss",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "imass",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nl",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rk1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rk8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sk1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sk8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "w1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "w8",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nel",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def npeid(self) -> typing.Optional[int]:
        """Get or set the Nurbs-Patch Element ID.  A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("npeid")

    @npeid.setter
    def npeid(self, value: int) -> None:
        """Set the npeid property."""
        self._cards[0].set_value("npeid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def npr(self) -> typing.Optional[int]:
        """Get or set the Number of control points in r-direction
        """ # nopep8
        return self._cards[0].get_value("npr")

    @npr.setter
    def npr(self, value: int) -> None:
        """Set the npr property."""
        self._cards[0].set_value("npr", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the Polynomial order of univariate shape functions in r-direction
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def nps(self) -> typing.Optional[int]:
        """Get or set the Number of control points in s-direction
        """ # nopep8
        return self._cards[0].get_value("nps")

    @nps.setter
    def nps(self, value: int) -> None:
        """Set the nps property."""
        self._cards[0].set_value("nps", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the Polynomial order of univariate shape functions in s-direction
        """ # nopep8
        return self._cards[0].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        """Set the ps property."""
        self._cards[0].set_value("ps", value)

    @property
    def wfl(self) -> typing.Optional[int]:
        """Get or set the Flag for weighting factors. If WFL=0, all weights will be set to 1.0 otherwise the weights of the control points have to be defined in the optional cards D.
        """ # nopep8
        return self._cards[1].get_value("wfl")

    @wfl.setter
    def wfl(self, value: int) -> None:
        """Set the wfl property."""
        self._cards[1].set_value("wfl", value)

    @property
    def form(self) -> int:
        """Get or set the Shell formulation to be used:
        EQ.0: Hughes-Liu with rotational DOFs
        EQ.1: Hughes-Liu without rotational DOFs
        EQ.2: Kirchhoff-Theory without rotational DOFs
        EQ.3: Kirchhoff-Theory with rotational DOFs
        EQ.4: combination of FORM=0 and FORM=1
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""form must be `None` or one of {0,1,2,3,4}.""")
        self._cards[1].set_value("form", value)

    @property
    def int_(self) -> int:
        """Get or set the In-plane numerical integration rule:
        EQ.0: reduced Gauss integration (NIP=PR*PS)
        EQ.1: full Gauss integration (NIP=(PR+1)*(PS+1))
        """ # nopep8
        return self._cards[1].get_value("int")

    @int_.setter
    def int_(self, value: int) -> None:
        """Set the int_ property."""
        if value not in [0, 1, None]:
            raise Exception("""int_ must be `None` or one of {0,1}.""")
        self._cards[1].set_value("int", value)

    @property
    def nisr(self) -> typing.Optional[int]:
        """Get or set the Number of Interpolation Shells in r-direction per created NURBS-Element for visualization (postprocessing) and contact
        """ # nopep8
        return self._cards[1].get_value("nisr")

    @nisr.setter
    def nisr(self, value: int) -> None:
        """Set the nisr property."""
        self._cards[1].set_value("nisr", value)

    @property
    def niss(self) -> typing.Optional[int]:
        """Get or set the Number of Interpolation Shells in s-direction per created NURBS-Element for visualization (postprocessing) and contact
        """ # nopep8
        return self._cards[1].get_value("niss")

    @niss.setter
    def niss(self, value: int) -> None:
        """Set the niss property."""
        self._cards[1].set_value("niss", value)

    @property
    def imass(self) -> int:
        """Get or set the Mass matric option:
        EQ.0: row sum
        EQ.1: diagonal weighting
        """ # nopep8
        return self._cards[1].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        """Set the imass property."""
        if value not in [0, 1, None]:
            raise Exception("""imass must be `None` or one of {0,1}.""")
        self._cards[1].set_value("imass", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of trimming loops.
        """ # nopep8
        return self._cards[1].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        """Set the nl property."""
        self._cards[1].set_value("nl", value)

    @property
    def rk1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk1")

    @rk1.setter
    def rk1(self, value: float) -> None:
        """Set the rk1 property."""
        self._cards[2].set_value("rk1", value)

    @property
    def rk2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk2")

    @rk2.setter
    def rk2(self, value: float) -> None:
        """Set the rk2 property."""
        self._cards[2].set_value("rk2", value)

    @property
    def rk3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk3")

    @rk3.setter
    def rk3(self, value: float) -> None:
        """Set the rk3 property."""
        self._cards[2].set_value("rk3", value)

    @property
    def rk4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk4")

    @rk4.setter
    def rk4(self, value: float) -> None:
        """Set the rk4 property."""
        self._cards[2].set_value("rk4", value)

    @property
    def rk5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk5")

    @rk5.setter
    def rk5(self, value: float) -> None:
        """Set the rk5 property."""
        self._cards[2].set_value("rk5", value)

    @property
    def rk6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk6")

    @rk6.setter
    def rk6(self, value: float) -> None:
        """Set the rk6 property."""
        self._cards[2].set_value("rk6", value)

    @property
    def rk7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk7")

    @rk7.setter
    def rk7(self, value: float) -> None:
        """Set the rk7 property."""
        self._cards[2].set_value("rk7", value)

    @property
    def rk8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk8")

    @rk8.setter
    def rk8(self, value: float) -> None:
        """Set the rk8 property."""
        self._cards[2].set_value("rk8", value)

    @property
    def sk1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk1")

    @sk1.setter
    def sk1(self, value: float) -> None:
        """Set the sk1 property."""
        self._cards[3].set_value("sk1", value)

    @property
    def sk2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk2")

    @sk2.setter
    def sk2(self, value: float) -> None:
        """Set the sk2 property."""
        self._cards[3].set_value("sk2", value)

    @property
    def sk3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk3")

    @sk3.setter
    def sk3(self, value: float) -> None:
        """Set the sk3 property."""
        self._cards[3].set_value("sk3", value)

    @property
    def sk4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk4")

    @sk4.setter
    def sk4(self, value: float) -> None:
        """Set the sk4 property."""
        self._cards[3].set_value("sk4", value)

    @property
    def sk5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk5")

    @sk5.setter
    def sk5(self, value: float) -> None:
        """Set the sk5 property."""
        self._cards[3].set_value("sk5", value)

    @property
    def sk6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk6")

    @sk6.setter
    def sk6(self, value: float) -> None:
        """Set the sk6 property."""
        self._cards[3].set_value("sk6", value)

    @property
    def sk7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk7")

    @sk7.setter
    def sk7(self, value: float) -> None:
        """Set the sk7 property."""
        self._cards[3].set_value("sk7", value)

    @property
    def sk8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk8")

    @sk8.setter
    def sk8(self, value: float) -> None:
        """Set the sk8 property."""
        self._cards[3].set_value("sk8", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[4].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[4].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[4].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[4].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[4].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[4].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[4].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[4].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[4].set_value("n8", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[5].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[5].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[5].set_value("w3", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[5].set_value("w4", value)

    @property
    def w5(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[5].set_value("w5", value)

    @property
    def w6(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[5].set_value("w6", value)

    @property
    def w7(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[5].set_value("w7", value)

    @property
    def w8(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[5].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[5].set_value("w8", value)

    @property
    def nel(self) -> typing.Optional[int]:
        """Get or set the Number of trimming loops.
        """ # nopep8
        return self._cards[6].get_value("nel")

    @nel.setter
    def nel(self, value: int) -> None:
        """Set the nel property."""
        self._cards[6].set_value("nel", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[7].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[7].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[7].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[7].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[7].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[7].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[7].set_value("e7", value)

    @property
    def e8(self) -> typing.Optional[int]:
        """Get or set the ID of the k-th, with k = 1,... , NEL, edge/segment defining the loop.
        """ # nopep8
        return self._cards[7].get_value("e8")

    @e8.setter
    def e8(self, value: int) -> None:
        """Set the e8 property."""
        self._cards[7].set_value("e8", value)

