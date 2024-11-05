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

class Mat041050(KeywordBase):
    """DYNA MAT_041_050 keyword"""

    keyword = "MAT"
    subkeyword = "041_050"
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
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "mt",
                        int,
                        20,
                        10,
                        kwargs.get("mt")
                    ),
                    Field(
                        "lmc",
                        int,
                        30,
                        10,
                        kwargs.get("lmc")
                    ),
                    Field(
                        "nhv",
                        int,
                        40,
                        10,
                        kwargs.get("nhv")
                    ),
                    Field(
                        "iortho",
                        int,
                        50,
                        10,
                        kwargs.get("iortho", 0)
                    ),
                    Field(
                        "ibulk",
                        int,
                        60,
                        10,
                        kwargs.get("ibulk")
                    ),
                    Field(
                        "ig",
                        int,
                        70,
                        10,
                        kwargs.get("ig")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ivect",
                        int,
                        0,
                        10,
                        kwargs.get("ivect", 0)
                    ),
                    Field(
                        "ifail",
                        int,
                        10,
                        10,
                        kwargs.get("ifail")
                    ),
                    Field(
                        "itherm",
                        int,
                        20,
                        10,
                        kwargs.get("itherm", 0)
                    ),
                    Field(
                        "ihyper",
                        int,
                        30,
                        10,
                        kwargs.get("ihyper", 0)
                    ),
                    Field(
                        "ieos",
                        int,
                        40,
                        10,
                        kwargs.get("ieos", 0)
                    ),
                    Field(
                        "lmca",
                        int,
                        50,
                        10,
                        kwargs.get("lmca")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        10,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
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
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "ievts",
                        int,
                        70,
                        10,
                        kwargs.get("ievts")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        kwargs.get("p6")
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        kwargs.get("p7")
                    ),
                    Field(
                        "p8",
                        float,
                        70,
                        10,
                        kwargs.get("p8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat041050.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def mt(self) -> typing.Optional[int]:
        """Get or set the User material type (41-50 inclusive). A number between 41 and 50 has to be chosen.
        If MT < 0, subroutine rwumat in dyn21.f is called, where the material parameter reading can be modified
        WARNING: If two or more materials in an input deck
        share the same MT value, those materials also share values of other variables on Cards 1 and 2 excluding
        MID and RO. Those shared values are taken from the first material where the common MT is encountered.
        """ # nopep8
        return self._cards[0].get_value("mt")

    @mt.setter
    def mt(self, value: int) -> None:
        self._cards[0].set_value("mt", value)

    @property
    def lmc(self) -> typing.Optional[int]:
        """Get or set the Length of material constant array which is equal to the number of material constants to be input.
        """ # nopep8
        return self._cards[0].get_value("lmc")

    @lmc.setter
    def lmc(self, value: int) -> None:
        self._cards[0].set_value("lmc", value)

    @property
    def nhv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables to be stored, see also Appendix A of users manual.
        """ # nopep8
        return self._cards[0].get_value("nhv")

    @nhv.setter
    def nhv(self, value: int) -> None:
        self._cards[0].set_value("nhv", value)

    @property
    def iortho(self) -> int:
        """Get or set the Orthotropic/spot weld thinning flag:
        EQ.0:	if the material is not orthotropic and is not used with spot weld thinning
        EQ.1:	if the material is orthotropic
        EQ.2:	if material is used with spot weld thinning
        EQ.3:	if material is orthotropic and used with spot weld thinning.
        """ # nopep8
        return self._cards[0].get_value("iortho")

    @iortho.setter
    def iortho(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""iortho must be one of {0,1,2,3}""")
        self._cards[0].set_value("iortho", value)

    @property
    def ibulk(self) -> typing.Optional[int]:
        """Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
        """ # nopep8
        return self._cards[0].get_value("ibulk")

    @ibulk.setter
    def ibulk(self, value: int) -> None:
        self._cards[0].set_value("ibulk", value)

    @property
    def ig(self) -> typing.Optional[int]:
        """Get or set the Address of shear modulus in material constants array, see also Appendix A of users manual..
        """ # nopep8
        return self._cards[0].get_value("ig")

    @ig.setter
    def ig(self, value: int) -> None:
        self._cards[0].set_value("ig", value)

    @property
    def ivect(self) -> int:
        """Get or set the Vectorization flag:EQ.0: off (default),
        EQ.1 on.
        A vectorized user subroutine must be supplied.
        """ # nopep8
        return self._cards[1].get_value("ivect")

    @ivect.setter
    def ivect(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ivect must be one of {0,1}""")
        self._cards[1].set_value("ivect", value)

    @property
    def ifail(self) -> typing.Optional[int]:
        """Get or set the Failure flag.
        EQ.0:  No failure;
        EQ.1:  Allows failure of shell and solid elements;
        LT.0:  |IFAIL| is the address of NUMINT in the material constants array.  NUMINT is defined as the number of failed integration points that will trigger element deletion.  This option applies only to shell and solid elements (release 5 of v.971).
        """ # nopep8
        return self._cards[1].get_value("ifail")

    @ifail.setter
    def ifail(self, value: int) -> None:
        self._cards[1].set_value("ifail", value)

    @property
    def itherm(self) -> int:
        """Get or set the Failure flag (on=1). Compute element temperature.
        """ # nopep8
        return self._cards[1].get_value("itherm")

    @itherm.setter
    def itherm(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itherm must be one of {0,1}""")
        self._cards[1].set_value("itherm", value)

    @property
    def ihyper(self) -> int:
        """Get or set the IHYPER
        """ # nopep8
        return self._cards[1].get_value("ihyper")

    @ihyper.setter
    def ihyper(self, value: int) -> None:
        if value not in [0, 1, -1, -2]:
            raise Exception("""ihyper must be one of {0,1,-1,-2}""")
        self._cards[1].set_value("ihyper", value)

    @property
    def ieos(self) -> int:
        """Get or set the IEOS
        """ # nopep8
        return self._cards[1].get_value("ieos")

    @ieos.setter
    def ieos(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ieos must be one of {0,1}""")
        self._cards[1].set_value("ieos", value)

    @property
    def lmca(self) -> typing.Optional[int]:
        """Get or set the Length of additional material constant array
        """ # nopep8
        return self._cards[1].get_value("lmca")

    @lmca.setter
    def lmca(self, value: int) -> None:
        self._cards[1].set_value("lmca", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES,
        and then, for shells only, rotated about	the shell element normal by an angle BETA,
        EQ.1.0: locally orthotropic with material axes determined by a point in space and the global location of the element center, this is the a-direction.
        This option is for solid elements only.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR,
        EQ.3.0:  locally orthotropic material axes determined by rotating
        the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        EQ.4.0: locally orthotropic in cylindrical coordinate system with
        the material axes determined by a vector v, and an originating point, p, which define  the centerline axis. This option is for solid elements only
        LT.0.0: the absolute value of AOPT is the coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, _SYSTEM or _VECTOR). Available in R3 version of 971 and later
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements for quick changes: EQ.1: default,
        EQ.2: switch material axes a and b,
        EQ.3: switch material axes a and c.
        """ # nopep8
        return self._cards[2].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""macf must be one of {1,2,3}""")
        self._cards[2].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def ievts(self) -> typing.Optional[int]:
        """Get or set the Address of E(a) for ortho tropic material in thick shell formulation 5 (see remark 6).
        """ # nopep8
        return self._cards[3].get_value("ievts")

    @ievts.setter
    def ievts(self, value: int) -> None:
        self._cards[3].set_value("ievts", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the First material parameter.
        """ # nopep8
        return self._cards[4].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[4].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Second material parameter.
        """ # nopep8
        return self._cards[4].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[4].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Third material parameter.
        """ # nopep8
        return self._cards[4].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[4].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Fourth material parameter.
        """ # nopep8
        return self._cards[4].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[4].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Fifth material parameter.
        """ # nopep8
        return self._cards[4].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[4].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Sixth material parameter.
        """ # nopep8
        return self._cards[4].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[4].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Seventh material parameter.
        """ # nopep8
        return self._cards[4].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[4].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Eighth material parameter.
        """ # nopep8
        return self._cards[4].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[4].set_value("p8", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the First material parameter.
        """ # nopep8
        return self._cards[5].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[5].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Second material parameter.
        """ # nopep8
        return self._cards[5].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[5].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Third material parameter.
        """ # nopep8
        return self._cards[5].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[5].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Fourth material parameter.
        """ # nopep8
        return self._cards[5].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[5].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Fifth material parameter.
        """ # nopep8
        return self._cards[5].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[5].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Sixth material parameter.
        """ # nopep8
        return self._cards[5].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[5].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Seventh material parameter.
        """ # nopep8
        return self._cards[5].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        self._cards[5].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Eighth material parameter.
        """ # nopep8
        return self._cards[5].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        self._cards[5].set_value("p8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

