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

class Mat271(KeywordBase):
    """DYNA MAT_271 keyword"""

    keyword = "MAT"
    subkeyword = "271"
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
                        "p11",
                        float,
                        20,
                        10,
                        kwargs.get("p11")
                    ),
                    Field(
                        "p22",
                        float,
                        30,
                        10,
                        kwargs.get("p22")
                    ),
                    Field(
                        "p33",
                        float,
                        40,
                        10,
                        kwargs.get("p33")
                    ),
                    Field(
                        "p12",
                        float,
                        50,
                        10,
                        kwargs.get("p12")
                    ),
                    Field(
                        "p23",
                        float,
                        50,
                        10,
                        kwargs.get("p23")
                    ),
                    Field(
                        "p13",
                        float,
                        70,
                        10,
                        kwargs.get("p13")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e0",
                        float,
                        0,
                        10,
                        kwargs.get("e0")
                    ),
                    Field(
                        "lck",
                        int,
                        10,
                        10,
                        kwargs.get("lck")
                    ),
                    Field(
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "lcx",
                        int,
                        30,
                        10,
                        kwargs.get("lcx")
                    ),
                    Field(
                        "lcy",
                        int,
                        40,
                        10,
                        kwargs.get("lcy")
                    ),
                    Field(
                        "lcc",
                        int,
                        50,
                        10,
                        kwargs.get("lcc")
                    ),
                    Field(
                        "l",
                        float,
                        60,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "r",
                        float,
                        70,
                        10,
                        kwargs.get("r")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ca",
                        float,
                        0,
                        10,
                        kwargs.get("ca")
                    ),
                    Field(
                        "cd",
                        float,
                        10,
                        10,
                        kwargs.get("cd")
                    ),
                    Field(
                        "cv",
                        float,
                        20,
                        10,
                        kwargs.get("cv")
                    ),
                    Field(
                        "p",
                        float,
                        30,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "lch",
                        int,
                        40,
                        10,
                        kwargs.get("lch")
                    ),
                    Field(
                        "lcfi",
                        int,
                        50,
                        10,
                        kwargs.get("lcfi")
                    ),
                    Field(
                        "sint",
                        float,
                        60,
                        10,
                        kwargs.get("sint", 0.0)
                    ),
                    Field(
                        "tzro",
                        float,
                        70,
                        10,
                        kwargs.get("tzro")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcfk",
                        int,
                        0,
                        10,
                        kwargs.get("lcfk")
                    ),
                    Field(
                        "lcfs2",
                        int,
                        10,
                        10,
                        kwargs.get("lcfs2")
                    ),
                    Field(
                        "dv1",
                        float,
                        20,
                        10,
                        kwargs.get("dv1")
                    ),
                    Field(
                        "dv2",
                        float,
                        30,
                        10,
                        kwargs.get("dv2")
                    ),
                    Field(
                        "ds1",
                        float,
                        40,
                        10,
                        kwargs.get("ds1")
                    ),
                    Field(
                        "ds2",
                        float,
                        50,
                        10,
                        kwargs.get("ds2")
                    ),
                    Field(
                        "omega",
                        float,
                        60,
                        10,
                        kwargs.get("omega")
                    ),
                    Field(
                        "rgas",
                        float,
                        70,
                        10,
                        kwargs.get("rgas")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcpr",
                        int,
                        0,
                        10,
                        kwargs.get("lcpr")
                    ),
                    Field(
                        "lcfs3",
                        int,
                        10,
                        10,
                        kwargs.get("lcfs3")
                    ),
                    Field(
                        "lctau",
                        int,
                        20,
                        10,
                        kwargs.get("lctau")
                    ),
                    Field(
                        "alpha",
                        float,
                        30,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "lcfs1",
                        int,
                        40,
                        10,
                        kwargs.get("lcfs1")
                    ),
                    Field(
                        "gamma",
                        float,
                        50,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "l0",
                        float,
                        60,
                        10,
                        kwargs.get("l0")
                    ),
                    Field(
                        "lcfks",
                        int,
                        70,
                        10,
                        kwargs.get("lcfks")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat271.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified
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
    def p11(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p11")

    @p11.setter
    def p11(self, value: float) -> None:
        self._cards[0].set_value("p11", value)

    @property
    def p22(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p22")

    @p22.setter
    def p22(self, value: float) -> None:
        self._cards[0].set_value("p22", value)

    @property
    def p33(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p33")

    @p33.setter
    def p33(self, value: float) -> None:
        self._cards[0].set_value("p33", value)

    @property
    def p12(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p12")

    @p12.setter
    def p12(self, value: float) -> None:
        self._cards[0].set_value("p12", value)

    @property
    def p23(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p23")

    @p23.setter
    def p23(self, value: float) -> None:
        self._cards[0].set_value("p23", value)

    @property
    def p13(self) -> typing.Optional[float]:
        """Get or set the Initial compactness tensor Pij.
        """ # nopep8
        return self._cards[0].get_value("p13")

    @p13.setter
    def p13(self, value: float) -> None:
        self._cards[0].set_value("p13", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial anisotropy variable e (value between 1 and 2).
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[1].set_value("e0", value)

    @property
    def lck(self) -> typing.Optional[int]:
        """Get or set the Load curve for bulk modulus K as function of relative density d.
        """ # nopep8
        return self._cards[1].get_value("lck")

    @lck.setter
    def lck(self, value: int) -> None:
        self._cards[1].set_value("lck", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio v.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def lcx(self) -> typing.Optional[int]:
        """Get or set the Load curve for hydrostatic compressive yield X as function of relative density d.
        """ # nopep8
        return self._cards[1].get_value("lcx")

    @lcx.setter
    def lcx(self, value: int) -> None:
        self._cards[1].set_value("lcx", value)

    @property
    def lcy(self) -> typing.Optional[int]:
        """Get or set the Load curve for uniaxial compressive yield Y as function of relative density d.
        """ # nopep8
        return self._cards[1].get_value("lcy")

    @lcy.setter
    def lcy(self, value: int) -> None:
        self._cards[1].set_value("lcy", value)

    @property
    def lcc(self) -> typing.Optional[int]:
        """Get or set the Load curve for shear yield C0 as function of relative density d.
        """ # nopep8
        return self._cards[1].get_value("lcc")

    @lcc.setter
    def lcc(self, value: int) -> None:
        self._cards[1].set_value("lcc", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Yield surface parameter L relating hydrostatic compressive yield to point on hydrostatic axis with maximum strength.
        """ # nopep8
        return self._cards[1].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[1].set_value("l", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Yield surface parameter R governing the shape of the yield surface.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def ca(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter Ca.
        """ # nopep8
        return self._cards[2].get_value("ca")

    @ca.setter
    def ca(self, value: float) -> None:
        self._cards[2].set_value("ca", value)

    @property
    def cd(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter Cd.
        """ # nopep8
        return self._cards[2].get_value("cd")

    @cd.setter
    def cd(self, value: float) -> None:
        self._cards[2].set_value("cd", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter Cv.
        """ # nopep8
        return self._cards[2].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        self._cards[2].set_value("cv", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Hardening exponent p.
        """ # nopep8
        return self._cards[2].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        self._cards[2].set_value("p", value)

    @property
    def lch(self) -> typing.Optional[int]:
        """Get or set the Load curve giving back stress parameter H as function of hardening	parameter e.
        """ # nopep8
        return self._cards[2].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        self._cards[2].set_value("lch", value)

    @property
    def lcfi(self) -> typing.Optional[int]:
        """Get or set the Load curve giving plastic strain evolution angle  as function of relative volumetric stress.
        """ # nopep8
        return self._cards[2].get_value("lcfi")

    @lcfi.setter
    def lcfi(self, value: int) -> None:
        self._cards[2].set_value("lcfi", value)

    @property
    def sint(self) -> float:
        """Get or set the Activate sintering
        EQ.0.0: Sintering off
        EQ.1.0: Sintering on.
        """ # nopep8
        return self._cards[2].get_value("sint")

    @sint.setter
    def sint(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""sint must be one of {0.0,1.0}""")
        self._cards[2].set_value("sint", value)

    @property
    def tzro(self) -> typing.Optional[float]:
        """Get or set the Absolute zero temperature T0.
        """ # nopep8
        return self._cards[2].get_value("tzro")

    @tzro.setter
    def tzro(self, value: float) -> None:
        self._cards[2].set_value("tzro", value)

    @property
    def lcfk(self) -> typing.Optional[int]:
        """Get or set the Load curve fk for viscous compliance as function of relative density d.
        """ # nopep8
        return self._cards[3].get_value("lcfk")

    @lcfk.setter
    def lcfk(self, value: int) -> None:
        self._cards[3].set_value("lcfk", value)

    @property
    def lcfs2(self) -> typing.Optional[int]:
        """Get or set the Load curve fs2 for viscous compliance as function of temperature T.
        """ # nopep8
        return self._cards[3].get_value("lcfs2")

    @lcfs2.setter
    def lcfs2(self, value: int) -> None:
        self._cards[3].set_value("lcfs2", value)

    @property
    def dv1(self) -> typing.Optional[float]:
        """Get or set the Volume diffusion coefficient dv1.
        """ # nopep8
        return self._cards[3].get_value("dv1")

    @dv1.setter
    def dv1(self, value: float) -> None:
        self._cards[3].set_value("dv1", value)

    @property
    def dv2(self) -> typing.Optional[float]:
        """Get or set the Volume diffusion coefficient dv2.
        """ # nopep8
        return self._cards[3].get_value("dv2")

    @dv2.setter
    def dv2(self, value: float) -> None:
        self._cards[3].set_value("dv2", value)

    @property
    def ds1(self) -> typing.Optional[float]:
        """Get or set the Surface diffusion coefficient ds1.
        """ # nopep8
        return self._cards[3].get_value("ds1")

    @ds1.setter
    def ds1(self, value: float) -> None:
        self._cards[3].set_value("ds1", value)

    @property
    def ds2(self) -> typing.Optional[float]:
        """Get or set the Surface diffusion coefficient ds1.
        """ # nopep8
        return self._cards[3].get_value("ds2")

    @ds2.setter
    def ds2(self, value: float) -> None:
        self._cards[3].set_value("ds2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Blending parameter w.
        """ # nopep8
        return self._cards[3].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[3].set_value("omega", value)

    @property
    def rgas(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant Rgas.
        """ # nopep8
        return self._cards[3].get_value("rgas")

    @rgas.setter
    def rgas(self, value: float) -> None:
        self._cards[3].set_value("rgas", value)

    @property
    def lcpr(self) -> typing.Optional[int]:
        """Get or set the Load curve for viscous Poisson's ratio vy as function of relative density d.
        """ # nopep8
        return self._cards[4].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: int) -> None:
        self._cards[4].set_value("lcpr", value)

    @property
    def lcfs3(self) -> typing.Optional[int]:
        """Get or set the Load curve fS3 for evolution of mobility factor as function of	temperature T.
        """ # nopep8
        return self._cards[4].get_value("lcfs3")

    @lcfs3.setter
    def lcfs3(self, value: int) -> None:
        self._cards[4].set_value("lcfs3", value)

    @property
    def lctau(self) -> typing.Optional[int]:
        """Get or set the Load curve for relaxation time t as function of temperature T.
        """ # nopep8
        return self._cards[4].get_value("lctau")

    @lctau.setter
    def lctau(self, value: int) -> None:
        self._cards[4].set_value("lctau", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient a.
        """ # nopep8
        return self._cards[4].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[4].set_value("alpha", value)

    @property
    def lcfs1(self) -> typing.Optional[int]:
        """Get or set the Load curve fs1 for sintering stress scaling as function of relative	density d.
        """ # nopep8
        return self._cards[4].get_value("lcfs1")

    @lcfs1.setter
    def lcfs1(self, value: int) -> None:
        self._cards[4].set_value("lcfs1", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Surface energy density r affecting sintering stress.
        """ # nopep8
        return self._cards[4].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[4].set_value("gamma", value)

    @property
    def l0(self) -> typing.Optional[float]:
        """Get or set the Grain size l0 affecting sintering stress.
        """ # nopep8
        return self._cards[4].get_value("l0")

    @l0.setter
    def l0(self, value: float) -> None:
        self._cards[4].set_value("l0", value)

    @property
    def lcfks(self) -> typing.Optional[int]:
        """Get or set the Load curve fks scaling bulk modulus as function of temperature T.
        """ # nopep8
        return self._cards[4].get_value("lcfks")

    @lcfks.setter
    def lcfks(self, value: int) -> None:
        self._cards[4].set_value("lcfks", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

