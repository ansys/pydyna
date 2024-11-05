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

class ElementShellBextPatch(KeywordBase):
    """DYNA ELEMENT_SHELL_BEXT_PATCH keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_BEXT_PATCH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "epid",
                        int,
                        0,
                        10,
                        kwargs.get("epid")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nel",
                        int,
                        20,
                        10,
                        kwargs.get("nel")
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
                        kwargs.get("wfl")
                    ),
                    Field(
                        "form",
                        int,
                        10,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "int",
                        int,
                        20,
                        10,
                        kwargs.get("int", 0)
                    ),
                    Field(
                        "nisr",
                        int,
                        30,
                        10,
                        kwargs.get("nisr")
                    ),
                    Field(
                        "niss",
                        int,
                        40,
                        10,
                        kwargs.get("niss")
                    ),
                    Field(
                        "imass",
                        int,
                        50,
                        10,
                        kwargs.get("imass", 0)
                    ),
                    Field(
                        "nl",
                        int,
                        60,
                        10,
                        kwargs.get("nl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "shpe",
                        int,
                        0,
                        10,
                        kwargs.get("shpe", 0)
                    ),
                    Field(
                        "pr",
                        int,
                        10,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "ps",
                        int,
                        20,
                        10,
                        kwargs.get("ps")
                    ),
                    Field(
                        "bdry",
                        int,
                        30,
                        10,
                        kwargs.get("bdry", 0)
                    ),
                    Field(
                        "trm",
                        int,
                        40,
                        10,
                        kwargs.get("trm", 0)
                    ),
                    Field(
                        "smth",
                        int,
                        50,
                        10,
                        kwargs.get("smth", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "b1",
                        int,
                        0,
                        10,
                        kwargs.get("b1", 0)
                    ),
                    Field(
                        "b2",
                        int,
                        10,
                        10,
                        kwargs.get("b2", 0)
                    ),
                    Field(
                        "b3",
                        int,
                        20,
                        10,
                        kwargs.get("b3", 0)
                    ),
                    Field(
                        "b4",
                        int,
                        30,
                        10,
                        kwargs.get("b4", 0)
                    ),
                    Field(
                        "b5",
                        int,
                        40,
                        10,
                        kwargs.get("b5", 0)
                    ),
                    Field(
                        "b6",
                        int,
                        50,
                        10,
                        kwargs.get("b6", 0)
                    ),
                    Field(
                        "b7",
                        int,
                        60,
                        10,
                        kwargs.get("b7", 0)
                    ),
                    Field(
                        "b8",
                        int,
                        70,
                        10,
                        kwargs.get("b8", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s1",
                        int,
                        0,
                        10,
                        kwargs.get("s1", 0)
                    ),
                    Field(
                        "s2",
                        int,
                        10,
                        10,
                        kwargs.get("s2", 0)
                    ),
                    Field(
                        "s3",
                        int,
                        20,
                        10,
                        kwargs.get("s3", 0)
                    ),
                    Field(
                        "s4",
                        int,
                        30,
                        10,
                        kwargs.get("s4", 0)
                    ),
                    Field(
                        "s5",
                        int,
                        40,
                        10,
                        kwargs.get("s5", 0)
                    ),
                    Field(
                        "s6",
                        int,
                        50,
                        10,
                        kwargs.get("s6", 0)
                    ),
                    Field(
                        "s7",
                        int,
                        60,
                        10,
                        kwargs.get("s7", 0)
                    ),
                    Field(
                        "s8",
                        int,
                        70,
                        10,
                        kwargs.get("s8", 0)
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
                        kwargs.get("n1", 0)
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2", 0)
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        kwargs.get("n3", 0)
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        kwargs.get("n4", 0)
                    ),
                    Field(
                        "n5",
                        int,
                        40,
                        10,
                        kwargs.get("n5", 0)
                    ),
                    Field(
                        "n6",
                        int,
                        50,
                        10,
                        kwargs.get("n6", 0)
                    ),
                    Field(
                        "n7",
                        int,
                        60,
                        10,
                        kwargs.get("n7", 0)
                    ),
                    Field(
                        "n8",
                        int,
                        70,
                        10,
                        kwargs.get("n8", 0)
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
                        kwargs.get("w1", 0)
                    ),
                    Field(
                        "w2",
                        float,
                        10,
                        10,
                        kwargs.get("w2", 0)
                    ),
                    Field(
                        "w3",
                        float,
                        20,
                        10,
                        kwargs.get("w3", 0)
                    ),
                    Field(
                        "w4",
                        float,
                        30,
                        10,
                        kwargs.get("w4", 0)
                    ),
                    Field(
                        "w5",
                        float,
                        40,
                        10,
                        kwargs.get("w5", 0)
                    ),
                    Field(
                        "w6",
                        float,
                        50,
                        10,
                        kwargs.get("w6", 0)
                    ),
                    Field(
                        "w7",
                        float,
                        60,
                        10,
                        kwargs.get("w7", 0)
                    ),
                    Field(
                        "w8",
                        float,
                        70,
                        10,
                        kwargs.get("w8", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "o1",
                        float,
                        0,
                        10,
                        kwargs.get("o1", 0)
                    ),
                    Field(
                        "o2",
                        float,
                        10,
                        10,
                        kwargs.get("o2", 0)
                    ),
                    Field(
                        "o3",
                        float,
                        20,
                        10,
                        kwargs.get("o3", 0)
                    ),
                    Field(
                        "o4",
                        float,
                        30,
                        10,
                        kwargs.get("o4", 0)
                    ),
                    Field(
                        "o5",
                        float,
                        40,
                        10,
                        kwargs.get("o5", 0)
                    ),
                    Field(
                        "o6",
                        float,
                        50,
                        10,
                        kwargs.get("o6", 0)
                    ),
                    Field(
                        "o7",
                        float,
                        60,
                        10,
                        kwargs.get("o7", 0)
                    ),
                    Field(
                        "o8",
                        float,
                        70,
                        10,
                        kwargs.get("o8", 0)
                    ),
                ],
            ),
        ]

    @property
    def epid(self) -> typing.Optional[int]:
        """Get or set the Extraction patch element ID. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("epid")

    @epid.setter
    def epid(self, value: int) -> None:
        self._cards[0].set_value("epid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. See *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nel(self) -> typing.Optional[int]:
        """Get or set the Number of Bezier elements in the patch.
        """ # nopep8
        return self._cards[0].get_value("nel")

    @nel.setter
    def nel(self, value: int) -> None:
        self._cards[0].set_value("nel", value)

    @property
    def wfl(self) -> typing.Optional[int]:
        """Get or set the Flag for weighting factors of the control points.
        WFL = 0 : All weights at the control points are set to 1 and no optional	cards D are allowed.
        WFL != 0 : The weights at the control points are de
        ned in optional cards
        D which must be de
        ned after cards C..
        """ # nopep8
        return self._cards[1].get_value("wfl")

    @wfl.setter
    def wfl(self, value: int) -> None:
        self._cards[1].set_value("wfl", value)

    @property
    def form(self) -> int:
        """Get or set the Shell formulation to be used.
        FORM = 0 : Shear deformable shell theory with rotational DOFs.
        FORM = 1 : Shear deformable shell theory without rotational DOFs.
        FORM = 2 : Thin shell theory without rotational DOFs.
        FORM = 4 : Combination of FORM = 0 and FORM = 1.
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""form must be one of {0,1,2,4}""")
        self._cards[1].set_value("form", value)

    @property
    def int_(self) -> int:
        """Get or set the In-plane numerical integration rule.
        INT = 0 : Uniformly reduced Gauss integration, NIP = PR  PS. Note
        that the number of integration points may change from element
        to element depending on local element degree.
        INT = 1 : Full Gauss integration, NIP = (PR + 1)  (PS + 1). Note that
        the number of integration points may change from element to
        element depending on local element degree.
        """ # nopep8
        return self._cards[1].get_value("int")

    @int_.setter
    def int_(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""int_ must be one of {0,1}""")
        self._cards[1].set_value("int", value)

    @property
    def nisr(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) interpolation shell elements in the local r-direction of each Bezier element for visualization (post processing) and contact.
        """ # nopep8
        return self._cards[1].get_value("nisr")

    @nisr.setter
    def nisr(self, value: int) -> None:
        self._cards[1].set_value("nisr", value)

    @property
    def niss(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) interpolation shell elements in the local s-direction of each Bezier element for visualization (post processing) and contact.
        """ # nopep8
        return self._cards[1].get_value("niss")

    @niss.setter
    def niss(self, value: int) -> None:
        self._cards[1].set_value("niss", value)

    @property
    def imass(self) -> int:
        """Get or set the Option for lumping of mass matrix.
        IMASS = 0 : Row sum.
        IMASS = 1 : Diagonal weighting.
        """ # nopep8
        return self._cards[1].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""imass must be one of {0,1}""")
        self._cards[1].set_value("imass", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of trimming loops
        NL = 0 : No trimming loops (i.e., untrimmed U-spline).
        NL > 0 : Trimmed U-spline with NL trimming loops.
        """ # nopep8
        return self._cards[1].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        self._cards[1].set_value("nl", value)

    @property
    def shpe(self) -> int:
        """Get or set the The Bezier element shape
        SHPE = 0 : Quadrilateral Bezier element.
        SHPE = 1 : Triangular Bezier element.
        """ # nopep8
        return self._cards[2].get_value("shpe")

    @shpe.setter
    def shpe(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""shpe must be one of {0,1}""")
        self._cards[2].set_value("shpe", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the The Bezier element degree in the local r-direction.
        """ # nopep8
        return self._cards[2].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        self._cards[2].set_value("pr", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the The Bezier element degree in the local s-direction.
        """ # nopep8
        return self._cards[2].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        self._cards[2].set_value("ps", value)

    @property
    def bdry(self) -> int:
        """Get or set the A boolean indicating if the Bezier is on the patch boundary.
        """ # nopep8
        return self._cards[2].get_value("bdry")

    @bdry.setter
    def bdry(self, value: int) -> None:
        self._cards[2].set_value("bdry", value)

    @property
    def trm(self) -> int:
        """Get or set the A boolean indicating if the Bezier element is trimmed.
        """ # nopep8
        return self._cards[2].get_value("trm")

    @trm.setter
    def trm(self, value: int) -> None:
        self._cards[2].set_value("trm", value)

    @property
    def smth(self) -> int:
        """Get or set the A boolean indicating whether smoothness information will be specifed for the element.
        """ # nopep8
        return self._cards[2].get_value("smth")

    @smth.setter
    def smth(self, value: int) -> None:
        self._cards[2].set_value("smth", value)

    @property
    def b1(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: int) -> None:
        self._cards[3].set_value("b1", value)

    @property
    def b2(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b2")

    @b2.setter
    def b2(self, value: int) -> None:
        self._cards[3].set_value("b2", value)

    @property
    def b3(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b3")

    @b3.setter
    def b3(self, value: int) -> None:
        self._cards[3].set_value("b3", value)

    @property
    def b4(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b4")

    @b4.setter
    def b4(self, value: int) -> None:
        self._cards[3].set_value("b4", value)

    @property
    def b5(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b5")

    @b5.setter
    def b5(self, value: int) -> None:
        self._cards[3].set_value("b5", value)

    @property
    def b6(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b6")

    @b6.setter
    def b6(self, value: int) -> None:
        self._cards[3].set_value("b6", value)

    @property
    def b7(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b7")

    @b7.setter
    def b7(self, value: int) -> None:
        self._cards[3].set_value("b7", value)

    @property
    def b8(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b8")

    @b8.setter
    def b8(self, value: int) -> None:
        self._cards[3].set_value("b8", value)

    @property
    def s1(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s1")

    @s1.setter
    def s1(self, value: int) -> None:
        self._cards[4].set_value("s1", value)

    @property
    def s2(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s2")

    @s2.setter
    def s2(self, value: int) -> None:
        self._cards[4].set_value("s2", value)

    @property
    def s3(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s3")

    @s3.setter
    def s3(self, value: int) -> None:
        self._cards[4].set_value("s3", value)

    @property
    def s4(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s4")

    @s4.setter
    def s4(self, value: int) -> None:
        self._cards[4].set_value("s4", value)

    @property
    def s5(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s5")

    @s5.setter
    def s5(self, value: int) -> None:
        self._cards[4].set_value("s5", value)

    @property
    def s6(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s6")

    @s6.setter
    def s6(self, value: int) -> None:
        self._cards[4].set_value("s6", value)

    @property
    def s7(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s7")

    @s7.setter
    def s7(self, value: int) -> None:
        self._cards[4].set_value("s7", value)

    @property
    def s8(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s8")

    @s8.setter
    def s8(self, value: int) -> None:
        self._cards[4].set_value("s8", value)

    @property
    def n1(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)..
        """ # nopep8
        return self._cards[5].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[5].set_value("n1", value)

    @property
    def n2(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[5].set_value("n2", value)

    @property
    def n3(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[5].set_value("n3", value)

    @property
    def n4(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[5].set_value("n4", value)

    @property
    def n5(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[5].set_value("n5", value)

    @property
    def n6(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[5].set_value("n6", value)

    @property
    def n7(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[5].set_value("n7", value)

    @property
    def n8(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[5].set_value("n8", value)

    @property
    def w1(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        self._cards[6].set_value("w1", value)

    @property
    def w2(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        self._cards[6].set_value("w2", value)

    @property
    def w3(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        self._cards[6].set_value("w3", value)

    @property
    def w4(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        self._cards[6].set_value("w4", value)

    @property
    def w5(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        self._cards[6].set_value("w5", value)

    @property
    def w6(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        self._cards[6].set_value("w6", value)

    @property
    def w7(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        self._cards[6].set_value("w7", value)

    @property
    def w8(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        self._cards[6].set_value("w8", value)

    @property
    def o1(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        self._cards[7].set_value("o1", value)

    @property
    def o2(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o2")

    @o2.setter
    def o2(self, value: float) -> None:
        self._cards[7].set_value("o2", value)

    @property
    def o3(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o3")

    @o3.setter
    def o3(self, value: float) -> None:
        self._cards[7].set_value("o3", value)

    @property
    def o4(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o4")

    @o4.setter
    def o4(self, value: float) -> None:
        self._cards[7].set_value("o4", value)

    @property
    def o5(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o5")

    @o5.setter
    def o5(self, value: float) -> None:
        self._cards[7].set_value("o5", value)

    @property
    def o6(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o6")

    @o6.setter
    def o6(self, value: float) -> None:
        self._cards[7].set_value("o6", value)

    @property
    def o7(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o7")

    @o7.setter
    def o7(self, value: float) -> None:
        self._cards[7].set_value("o7", value)

    @property
    def o8(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o8")

    @o8.setter
    def o8(self, value: float) -> None:
        self._cards[7].set_value("o8", value)

