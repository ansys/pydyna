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

class MatLaminatedFractureDaimlerPinho(KeywordBase):
    """DYNA MAT_LAMINATED_FRACTURE_DAIMLER_PINHO keyword"""

    keyword = "MAT"
    subkeyword = "LAMINATED_FRACTURE_DAIMLER_PINHO"
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
                        "ea",
                        float,
                        20,
                        10,
                        kwargs.get("ea")
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        kwargs.get("eb")
                    ),
                    Field(
                        "ec",
                        float,
                        40,
                        10,
                        kwargs.get("ec")
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        kwargs.get("prba")
                    ),
                    Field(
                        "prca",
                        float,
                        60,
                        10,
                        kwargs.get("prca")
                    ),
                    Field(
                        "prcb",
                        float,
                        70,
                        10,
                        kwargs.get("prcb")
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
                        kwargs.get("gab")
                    ),
                    Field(
                        "gbc",
                        float,
                        10,
                        10,
                        kwargs.get("gbc")
                    ),
                    Field(
                        "gca",
                        float,
                        20,
                        10,
                        kwargs.get("gca")
                    ),
                    Field(
                        "aopt",
                        float,
                        30,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "daf",
                        float,
                        40,
                        10,
                        kwargs.get("daf", 0.0)
                    ),
                    Field(
                        "dkf",
                        float,
                        50,
                        10,
                        kwargs.get("dkf", 0.0)
                    ),
                    Field(
                        "dmf",
                        float,
                        60,
                        10,
                        kwargs.get("dmf", 0.0)
                    ),
                    Field(
                        "efs",
                        float,
                        70,
                        10,
                        kwargs.get("efs")
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
                        "mangle",
                        float,
                        60,
                        10,
                        kwargs.get("mangle")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "enkink",
                        float,
                        0,
                        10,
                        kwargs.get("enkink")
                    ),
                    Field(
                        "ena",
                        float,
                        10,
                        10,
                        kwargs.get("ena")
                    ),
                    Field(
                        "enb",
                        float,
                        20,
                        10,
                        kwargs.get("enb")
                    ),
                    Field(
                        "ent",
                        float,
                        30,
                        10,
                        kwargs.get("ent")
                    ),
                    Field(
                        "enl",
                        float,
                        40,
                        10,
                        kwargs.get("enl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "yc",
                        float,
                        20,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "yt",
                        float,
                        30,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "sl",
                        float,
                        40,
                        10,
                        kwargs.get("sl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fio",
                        float,
                        0,
                        10,
                        kwargs.get("fio", 53.0)
                    ),
                    Field(
                        "sigy",
                        float,
                        10,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "lcss",
                        int,
                        20,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "beta",
                        float,
                        30,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "pfl",
                        float,
                        40,
                        10,
                        kwargs.get("pfl")
                    ),
                    Field(
                        "puck",
                        float,
                        50,
                        10,
                        kwargs.get("puck", 0.0)
                    ),
                    Field(
                        "soft",
                        float,
                        60,
                        10,
                        kwargs.get("soft", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatLaminatedFractureDaimlerPinho.option_specs[0],
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
        """Get or set the Material identification.
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
    def ea(self) -> typing.Optional[float]:
        """Get or set the Ea, Young's modulus in a-direction (longitudinal).
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Eb, Young's modulus in b-direction (transverse).
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Ec, Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Vba, Poisson's ratio ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Vca, Poisson's ratio ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Vcb, Poisson's ratio cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Gab, shear modulus ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Gbc, shear modulus bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Gca, shear modulus ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by an angle(see MANGLE on Card 4).
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle.This angle may be set in the keyword input for the element or in the input for this keyword(see MANGLE on Card 4).
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def daf(self) -> float:
        """Get or set the Flag to control failure of an integration point based on longitudinal (fiber) tensile failure:
        EQ.0.0: IP fails if any damage variable reaches 1.0.
        EQ.1.0: no failure of IP due to fiber tensile failure (da(i)=1.0).
        """ # nopep8
        return self._cards[1].get_value("daf")

    @daf.setter
    def daf(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""daf must be one of {0.0,1.0}""")
        self._cards[1].set_value("daf", value)

    @property
    def dkf(self) -> float:
        """Get or set the Flag to control failure of an integration point based on longitudinal (fiber) compressive failure:
        EQ.0.0: IP fails if any damage variable reaches 1.0.
        EQ.1.0: no failure of IP due to fiber compressive failure	(dkink(i)=1.0).
        """ # nopep8
        return self._cards[1].get_value("dkf")

    @dkf.setter
    def dkf(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""dkf must be one of {0.0,1.0}""")
        self._cards[1].set_value("dkf", value)

    @property
    def dmf(self) -> float:
        """Get or set the Flag to control failure of an integration point based on transverse (matrix) failure:
        EQ.0.0: IP fails if any damage variable reaches 1.0.
        EQ.1.0: no failure of IP due to matrix failure (dmat(i)=1.0).
        """ # nopep8
        return self._cards[1].get_value("dmf")

    @dmf.setter
    def dmf(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""dmf must be one of {0.0,1.0}""")
        self._cards[1].set_value("dmf", value)

    @property
    def efs(self) -> typing.Optional[float]:
        """Get or set the Maximum effective strain for element layer failure. A value of unity
        would equal 100% strain.
        GT.0.0: fails when effective strain calculated assuming material is volume preserving exceeds EFS.
        LT.0.0: fails when effective strain calculated from the full strain tensor exceeds |EFS|.
        """ # nopep8
        return self._cards[1].get_value("efs")

    @efs.setter
    def efs(self, value: float) -> None:
        self._cards[1].set_value("efs", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def mangle(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells only) and 3.
        MANGLE may be overridden on the element card, see
        *ELEMENT_SHELL_BETA and *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("mangle")

    @mangle.setter
    def mangle(self, value: float) -> None:
        self._cards[3].set_value("mangle", value)

    @property
    def enkink(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness for longitudinal (fiber) compressive failure mode.
        GT.0.0: The given value will be regularized with the characteristic element length.
        LT.0.0: Load curve ID=(-ENKINK) which defines the fracture
        toughness for fiber compressive failure mode as a
        function of characteristic element length. No further regularization.
        """ # nopep8
        return self._cards[4].get_value("enkink")

    @enkink.setter
    def enkink(self, value: float) -> None:
        self._cards[4].set_value("enkink", value)

    @property
    def ena(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness for longitudinal (fiber) tensile failure mode.
        GT.0.0: The given value will be regularized with the characteristic element length.
        LT.0.0: Load curve ID=(-ENA) which defines the fracture
        toughness for fiber tensile failure mode as a function of
        characteristic element length. No further regularization.
        """ # nopep8
        return self._cards[4].get_value("ena")

    @ena.setter
    def ena(self, value: float) -> None:
        self._cards[4].set_value("ena", value)

    @property
    def enb(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness for intralaminar matrix tensile failure.
        GT.0.0: The given value will be regularized with the characteristic element length.
        LT.0.0: Load curve ID=(-ENB) which defines the fracture
        toughness for intralaminar matrix tensile failure as a
        function of characteristic element length. No further regularization.
        """ # nopep8
        return self._cards[4].get_value("enb")

    @enb.setter
    def enb(self, value: float) -> None:
        self._cards[4].set_value("enb", value)

    @property
    def ent(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness for intralaminar matrix transverse shear failure.
        GT.0.0: The given value will be regularized with the characteristic element length.
        LT.0.0: Load curve ID=(-ENT) which defines the fracture
        toughness for intralaminar matrix transverse shear failure
        as a function of characteristic element length. No further	regularization.
        """ # nopep8
        return self._cards[4].get_value("ent")

    @ent.setter
    def ent(self, value: float) -> None:
        self._cards[4].set_value("ent", value)

    @property
    def enl(self) -> typing.Optional[float]:
        """Get or set the Fracture toughness for intralaminar matrix longitudinal shear failure.
        GT.0.0: The given value will be regularized with the characteristic element length.
        LT.0.0: Load curve ID=(-ENL) which defines the fracture
        toughness for intralaminar matrix longitudinal shear
        failure as a function of characteristic element length. No further regularization.
        """ # nopep8
        return self._cards[4].get_value("enl")

    @enl.setter
    def enl(self, value: float) -> None:
        self._cards[4].set_value("enl", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Longitudinal compressive strength, a-axis (positive value).
        """ # nopep8
        return self._cards[5].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[5].set_value("xc", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Longitudinal tensile strength, a-axis.
        """ # nopep8
        return self._cards[5].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[5].set_value("xt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, b-axis (positive value).
        """ # nopep8
        return self._cards[5].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[5].set_value("yc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, b-axis.
        """ # nopep8
        return self._cards[5].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[5].set_value("yt", value)

    @property
    def sl(self) -> typing.Optional[float]:
        """Get or set the Longitudinal shear strength.
        """ # nopep8
        return self._cards[5].get_value("sl")

    @sl.setter
    def sl(self, value: float) -> None:
        self._cards[5].set_value("sl", value)

    @property
    def fio(self) -> float:
        """Get or set the Fracture angle in pure transverse compression (in degrees, default = 53.0).
        """ # nopep8
        return self._cards[6].get_value("fio")

    @fio.setter
    def fio(self, value: float) -> None:
        self._cards[6].set_value("fio", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the In-plane shear yield stress(only used when BETA < 1.0)..
        """ # nopep8
        return self._cards[6].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[6].set_value("sigy", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines the non-linear in-plane shear-stress as a function of in-plane shear-strain.
        """ # nopep8
        return self._cards[6].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[6].set_value("lcss", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter for in-plane shear plasticity (0.0 <= BETA <=	1.0).
        EQ.0.0: Pure kinematic hardening
        EQ.1.0: Pure isotropic hardening	0.0<BETA<1.0: mixed hardening.
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def pfl(self) -> typing.Optional[float]:
        """Get or set the Percentage of layers which must fail until crashfront is initiated. E.g.
        |PFL|=80.0, then 80 % of layers must fail until strengths are reduced
        in neighboring elements. Default: all layers must fail. A single layer
        fails if 1 in-plane IP fails (PFL>0) or if 4 in-plane IPs fail (PFL<0).
        """ # nopep8
        return self._cards[6].get_value("pfl")

    @pfl.setter
    def pfl(self, value: float) -> None:
        self._cards[6].set_value("pfl", value)

    @property
    def puck(self) -> float:
        """Get or set the Flag for evaluation and post-processing of Puck's inter-fiber-failure
        criterion (IFF, see Puck, Kopp and Knops [2002]).
        EQ.0.0: no evaluation of Puck's IFF-criterion.
        EQ.1.0: Puck's IFF-criterion will be evaluated.
        """ # nopep8
        return self._cards[6].get_value("puck")

    @puck.setter
    def puck(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""puck must be one of {0.0,1.0}""")
        self._cards[6].set_value("puck", value)

    @property
    def soft(self) -> float:
        """Get or set the Softening reduction factor for material strength in crashfront	elements (default = 1.0).
        """ # nopep8
        return self._cards[6].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        self._cards[6].set_value("soft", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

