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

"""Module providing the Mat002Sunil class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat002Sunil(KeywordBase):
    """DYNA MAT_002_SUNIL keyword"""

    keyword = "MAT"
    subkeyword = "002_SUNIL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat002Sunil class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ea",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ec",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prca",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prcb",
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
                        "gab",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gbc",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gca",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "aopt",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigf",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mfparm",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "macf",
                        int,
                        60,
                        10,
                        1,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ref",
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
                        "t1fail",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c1fail",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "t2fail",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c2fail",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "t3fail",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c3fail",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "s12fail",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "s23fail",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "s31fail",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat002Sunil.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus in a-direction(1-1 direction).
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus in b-direction(2-2 direction).
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        """Set the eb property."""
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Ec, Young's modulus in c-direction(3-3 direction).
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        """Set the prba property."""
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        """Set the prca property."""
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        """Set the prcb property."""
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        """Set the gab property."""
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        """Set the gbc property."""
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        """Set the gca property."""
        self._cards[1].set_value("gca", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[1].set_value("aopt", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[1].set_value("g", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[1].set_value("sigf", value)

    @property
    def mfparm(self) -> int:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("mfparm")

    @mfparm.setter
    def mfparm(self, value: int) -> None:
        """Set the mfparm property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, None]:
            raise Exception("""mfparm must be `None` or one of {0,1,2,3,4,5,6}.""")
        self._cards[1].set_value("mfparm", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements:
        EQ.1:  No change, default,
        EQ.2:  switch material axes a and b,
        EQ.3:  switch material axes a and c,
        EQ.4:  switch material axes b and c.
        """ # nopep8
        return self._cards[2].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4}.""")
        self._cards[2].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[3].set_value("beta", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[3].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[3].set_value("ref", value)

    @property
    def t1fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in tension (a-a, or 1-1 direction).
        """ # nopep8
        return self._cards[4].get_value("t1fail")

    @t1fail.setter
    def t1fail(self, value: float) -> None:
        """Set the t1fail property."""
        self._cards[4].set_value("t1fail", value)

    @property
    def c1fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in compression (a-a, or 1-1 direction).
        """ # nopep8
        return self._cards[4].get_value("c1fail")

    @c1fail.setter
    def c1fail(self, value: float) -> None:
        """Set the c1fail property."""
        self._cards[4].set_value("c1fail", value)

    @property
    def t2fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in tension (b-b, or 2-2 direction).
        """ # nopep8
        return self._cards[4].get_value("t2fail")

    @t2fail.setter
    def t2fail(self, value: float) -> None:
        """Set the t2fail property."""
        self._cards[4].set_value("t2fail", value)

    @property
    def c2fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in compression (b-b, or 2-2 direction).
        """ # nopep8
        return self._cards[4].get_value("c2fail")

    @c2fail.setter
    def c2fail(self, value: float) -> None:
        """Set the c2fail property."""
        self._cards[4].set_value("c2fail", value)

    @property
    def t3fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in tension (c-c, or 3-3 direction).
        """ # nopep8
        return self._cards[4].get_value("t3fail")

    @t3fail.setter
    def t3fail(self, value: float) -> None:
        """Set the t3fail property."""
        self._cards[4].set_value("t3fail", value)

    @property
    def c3fail(self) -> typing.Optional[float]:
        """Get or set the Fiber failure limit in compression (c-c or, 3-3 direction).
        """ # nopep8
        return self._cards[4].get_value("c3fail")

    @c3fail.setter
    def c3fail(self, value: float) -> None:
        """Set the c3fail property."""
        self._cards[4].set_value("c3fail", value)

    @property
    def s12fail(self) -> typing.Optional[float]:
        """Get or set the In-plane shear failure limit (a-b or, 1-2 direction).
        """ # nopep8
        return self._cards[5].get_value("s12fail")

    @s12fail.setter
    def s12fail(self, value: float) -> None:
        """Set the s12fail property."""
        self._cards[5].set_value("s12fail", value)

    @property
    def s23fail(self) -> typing.Optional[float]:
        """Get or set the Transverse shear failure limit (b-c or, 2-3 direction).
        """ # nopep8
        return self._cards[5].get_value("s23fail")

    @s23fail.setter
    def s23fail(self, value: float) -> None:
        """Set the s23fail property."""
        self._cards[5].set_value("s23fail", value)

    @property
    def s31fail(self) -> typing.Optional[float]:
        """Get or set the Transverse shear failure limit (c-a or, 3-1 direction).
        """ # nopep8
        return self._cards[5].get_value("s31fail")

    @s31fail.setter
    def s31fail(self, value: float) -> None:
        """Set the s31fail property."""
        self._cards[5].set_value("s31fail", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

