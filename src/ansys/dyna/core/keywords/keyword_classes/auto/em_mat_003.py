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

class EmMat003(KeywordBase):
    """DYNA EM_MAT_003 keyword"""

    keyword = "EM"
    subkeyword = "MAT_003"

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
                        "mtype",
                        int,
                        10,
                        10,
                        kwargs.get("mtype", 0)
                    ),
                    Field(
                        "sigma11",
                        float,
                        20,
                        10,
                        kwargs.get("sigma11")
                    ),
                    Field(
                        "sigma22",
                        float,
                        30,
                        10,
                        kwargs.get("sigma22")
                    ),
                    Field(
                        "sigma33",
                        float,
                        40,
                        10,
                        kwargs.get("sigma33")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigma12",
                        int,
                        0,
                        10,
                        kwargs.get("sigma12")
                    ),
                    Field(
                        "sigma13",
                        int,
                        10,
                        10,
                        kwargs.get("sigma13")
                    ),
                    Field(
                        "sigma21",
                        float,
                        20,
                        10,
                        kwargs.get("sigma21")
                    ),
                    Field(
                        "sigma23",
                        float,
                        30,
                        10,
                        kwargs.get("sigma23")
                    ),
                    Field(
                        "sigma31",
                        float,
                        40,
                        10,
                        kwargs.get("sigma31")
                    ),
                    Field(
                        "sigma32",
                        float,
                        50,
                        10,
                        kwargs.get("sigma32")
                    ),
                    Field(
                        "aopt",
                        int,
                        60,
                        10,
                        kwargs.get("aopt", 0)
                    ),
                    Field(
                        "lambda",
                        float,
                        70,
                        10,
                        kwargs.get("lambda")
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
                    Field(
                        "macf",
                        int,
                        60,
                        10,
                        kwargs.get("macf", 1)
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
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Defines the electromagnetism type of the material:
        EQ.0:	Air or vacuum.
        EQ.1 : Insulator material : These materials have the same electromagnetism behavior as EQ.0.
        EQ.2 : Conductor carrying a source.In these conductors, the eddy current problem is solved, which gives the actual current density.Typically, this would correspond to the coil.In Electrophysiology, it corresponds to the tissue where the monodomain equations are solved for EMSOL = 11 or EMSOL = 13. An * EM_EP_CELLMODEL must be associated to this * EM_MAT_003.
        EQ.4 : Conductor not connected to any current or voltage source, where the Eddy current problem is solved.Typically, this would correspond to the workpiece.In Electrophysiology(EP), for EMSOL = 11, 12 or 13, it corresponds to the batsurrounding the tissue, where only the external potential is solved for.No* EM_EP_CELLMODEL should be associated with these materials
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""mtype must be one of {0,1,2,4}""")
        self._cards[0].set_value("mtype", value)

    @property
    def sigma11(self) -> typing.Optional[float]:
        """Get or set the The 1,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[0].get_value("sigma11")

    @sigma11.setter
    def sigma11(self, value: float) -> None:
        self._cards[0].set_value("sigma11", value)

    @property
    def sigma22(self) -> typing.Optional[float]:
        """Get or set the The 2,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[0].get_value("sigma22")

    @sigma22.setter
    def sigma22(self, value: float) -> None:
        self._cards[0].set_value("sigma22", value)

    @property
    def sigma33(self) -> typing.Optional[float]:
        """Get or set the The 3,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[0].get_value("sigma33")

    @sigma33.setter
    def sigma33(self, value: float) -> None:
        self._cards[0].set_value("sigma33", value)

    @property
    def sigma12(self) -> typing.Optional[int]:
        """Get or set the The 1,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.Note that 2 corresponds to the b material direction.. If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma12")

    @sigma12.setter
    def sigma12(self, value: int) -> None:
        self._cards[1].set_value("sigma12", value)

    @property
    def sigma13(self) -> typing.Optional[int]:
        """Get or set the The 1,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma13")

    @sigma13.setter
    def sigma13(self, value: int) -> None:
        self._cards[1].set_value("sigma13", value)

    @property
    def sigma21(self) -> typing.Optional[float]:
        """Get or set the The 2,1 term in the 3 x 3 electromagnetic conductivity tensor matrix. Note that 1 corresponds to the a material direction.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma21")

    @sigma21.setter
    def sigma21(self, value: float) -> None:
        self._cards[1].set_value("sigma21", value)

    @property
    def sigma23(self) -> typing.Optional[float]:
        """Get or set the The 2,3 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma23")

    @sigma23.setter
    def sigma23(self, value: float) -> None:
        self._cards[1].set_value("sigma23", value)

    @property
    def sigma31(self) -> typing.Optional[float]:
        """Get or set the The 3,1 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma31")

    @sigma31.setter
    def sigma31(self, value: float) -> None:
        self._cards[1].set_value("sigma31", value)

    @property
    def sigma32(self) -> typing.Optional[float]:
        """Get or set the The 3,2 term in the 3 x 3 electromagnetic conductivity tensor matrix.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters.
        """ # nopep8
        return self._cards[1].get_value("sigma32")

    @sigma32.setter
    def sigma32(self, value: float) -> None:
        self._cards[1].set_value("sigma32", value)

    @property
    def aopt(self) -> int:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by element nodes
        EQ.1.0:locally orthotropic with material axes determined by a point in space and the global location of the element center this is the a-direction.
        EQ.2.0:globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0:locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal. The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.
        EQ.4.0:locally orthotropic in cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis. This option is for solid elements only.
        EQ.5.0:globally defined reference frame with (a,b,c)=(X0,Y0,Z0).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""aopt must be one of {0,1,2,3,4,5}""")
        self._cards[1].set_value("aopt", value)

    @property
    def lambda_(self) -> typing.Optional[float]:
        """Get or set the Intra- to extracellular conductivity ratio. When non-empty, the elliptic equation is solved to compute extracellular potentials
        """ # nopep8
        return self._cards[1].get_value("lambda")

    @lambda_.setter
    def lambda_(self, value: float) -> None:
        self._cards[1].set_value("lambda", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
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
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1: No change, default
        """ # nopep8
        return self._cards[2].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        self._cards[2].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
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

