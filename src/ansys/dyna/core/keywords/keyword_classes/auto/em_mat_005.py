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

class EmMat005(KeywordBase):
    """DYNA EM_MAT_005 keyword"""

    keyword = "EM"
    subkeyword = "MAT_005"

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
                        "sigmaxxa",
                        float,
                        20,
                        10,
                        kwargs.get("sigmaxxa")
                    ),
                    Field(
                        "sigmayya",
                        float,
                        30,
                        10,
                        kwargs.get("sigmayya")
                    ),
                    Field(
                        "sigmazza",
                        float,
                        40,
                        10,
                        kwargs.get("sigmazza")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigmaxya",
                        float,
                        0,
                        10,
                        kwargs.get("sigmaxya")
                    ),
                    Field(
                        "sigmaxza",
                        float,
                        10,
                        10,
                        kwargs.get("sigmaxza")
                    ),
                    Field(
                        "sigmayxa",
                        float,
                        20,
                        10,
                        kwargs.get("sigmayxa")
                    ),
                    Field(
                        "sigmayza",
                        float,
                        30,
                        10,
                        kwargs.get("sigmayza")
                    ),
                    Field(
                        "sigmazxa",
                        float,
                        40,
                        10,
                        kwargs.get("sigmazxa")
                    ),
                    Field(
                        "sigmazya",
                        float,
                        50,
                        10,
                        kwargs.get("sigmazya")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "sigmaxxb",
                        float,
                        20,
                        10,
                        kwargs.get("sigmaxxb")
                    ),
                    Field(
                        "sigmayyb",
                        float,
                        30,
                        10,
                        kwargs.get("sigmayyb")
                    ),
                    Field(
                        "sigmazzb",
                        float,
                        40,
                        10,
                        kwargs.get("sigmazzb")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigmaxyb",
                        float,
                        0,
                        10,
                        kwargs.get("sigmaxyb")
                    ),
                    Field(
                        "sigmaxzb",
                        float,
                        10,
                        10,
                        kwargs.get("sigmaxzb")
                    ),
                    Field(
                        "sigmayxb",
                        float,
                        20,
                        10,
                        kwargs.get("sigmayxb")
                    ),
                    Field(
                        "sigmayzb",
                        float,
                        30,
                        10,
                        kwargs.get("sigmayzb")
                    ),
                    Field(
                        "sigmazxb",
                        float,
                        40,
                        10,
                        kwargs.get("sigmazxb")
                    ),
                    Field(
                        "sigmazyb",
                        float,
                        50,
                        10,
                        kwargs.get("sigmazyb")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        int,
                        0,
                        10,
                        kwargs.get("aopt", 0)
                    ),
                    Field(
                        "xp",
                        float,
                        10,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        20,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        30,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        40,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        50,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        60,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "macf",
                        int,
                        70,
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

        EQ.0:	Air or vacuum
        EQ.1 : Insulator material : these materials have the same electromagnetism behavior as EQ.0.
        EQ.2 : In EP, it corresponds to the tissue, where the bidomain equations will be solved for EMSOL = 12 or EMSOL = 13. An * EM_EP_CELLMODEL must be associated to this * EM_MAT_005
        EQ.4 : In EP, it corresponds to the bath where only the external potential is solved for.No* EM_EP_CELLMODEL should be associated with these materials.
        EQ.5 : Material associated to * EM_RANDLES_BATMAC
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        if value not in [0, 1, 2, 4, 5]:
            raise Exception("""mtype must be one of {0,1,2,4,5}""")
        self._cards[0].set_value("mtype", value)

    @property
    def sigmaxxa(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
        """ # nopep8
        return self._cards[0].get_value("sigmaxxa")

    @sigmaxxa.setter
    def sigmaxxa(self, value: float) -> None:
        self._cards[0].set_value("sigmaxxa", value)

    @property
    def sigmayya(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
        """ # nopep8
        return self._cards[0].get_value("sigmayya")

    @sigmayya.setter
    def sigmayya(self, value: float) -> None:
        self._cards[0].set_value("sigmayya", value)

    @property
    def sigmazza(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.If a negative value is entered, a *DEFINE_FUNCTION will be expected. See remark 3- for available parameters
        """ # nopep8
        return self._cards[0].get_value("sigmazza")

    @sigmazza.setter
    def sigmazza(self, value: float) -> None:
        self._cards[0].set_value("sigmazza", value)

    @property
    def sigmaxya(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmaxya")

    @sigmaxya.setter
    def sigmaxya(self, value: float) -> None:
        self._cards[1].set_value("sigmaxya", value)

    @property
    def sigmaxza(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmaxza")

    @sigmaxza.setter
    def sigmaxza(self, value: float) -> None:
        self._cards[1].set_value("sigmaxza", value)

    @property
    def sigmayxa(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmayxa")

    @sigmayxa.setter
    def sigmayxa(self, value: float) -> None:
        self._cards[1].set_value("sigmayxa", value)

    @property
    def sigmayza(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmayza")

    @sigmayza.setter
    def sigmayza(self, value: float) -> None:
        self._cards[1].set_value("sigmayza", value)

    @property
    def sigmazxa(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmazxa")

    @sigmazxa.setter
    def sigmazxa(self, value: float) -> None:
        self._cards[1].set_value("sigmazxa", value)

    @property
    def sigmazya(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[1].get_value("sigmazya")

    @sigmazya.setter
    def sigmazya(self, value: float) -> None:
        self._cards[1].set_value("sigmazya", value)

    @property
    def sigmaxxb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[2].get_value("sigmaxxb")

    @sigmaxxb.setter
    def sigmaxxb(self, value: float) -> None:
        self._cards[2].set_value("sigmaxxb", value)

    @property
    def sigmayyb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[2].get_value("sigmayyb")

    @sigmayyb.setter
    def sigmayyb(self, value: float) -> None:
        self._cards[2].set_value("sigmayyb", value)

    @property
    def sigmazzb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[2].get_value("sigmazzb")

    @sigmazzb.setter
    def sigmazzb(self, value: float) -> None:
        self._cards[2].set_value("sigmazzb", value)

    @property
    def sigmaxyb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmaxyb")

    @sigmaxyb.setter
    def sigmaxyb(self, value: float) -> None:
        self._cards[3].set_value("sigmaxyb", value)

    @property
    def sigmaxzb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmaxzb")

    @sigmaxzb.setter
    def sigmaxzb(self, value: float) -> None:
        self._cards[3].set_value("sigmaxzb", value)

    @property
    def sigmayxb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmayxb")

    @sigmayxb.setter
    def sigmayxb(self, value: float) -> None:
        self._cards[3].set_value("sigmayxb", value)

    @property
    def sigmayzb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmayzb")

    @sigmayzb.setter
    def sigmayzb(self, value: float) -> None:
        self._cards[3].set_value("sigmayzb", value)

    @property
    def sigmazxb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmazxb")

    @sigmazxb.setter
    def sigmazxb(self, value: float) -> None:
        self._cards[3].set_value("sigmazxb", value)

    @property
    def sigmazyb(self) -> typing.Optional[float]:
        """Get or set the The term in the 3 x 3 electromagnetic conductivity tensor matrix for the two conductivities.
        """ # nopep8
        return self._cards[3].get_value("sigmazyb")

    @sigmazyb.setter
    def sigmazyb(self, value: float) -> None:
        self._cards[3].set_value("sigmazyb", value)

    @property
    def aopt(self) -> int:
        """Get or set the Material axes option:
        EQ.0.0:locally orthotropic with material axes determined by element nodes as shown in part (a) the figure in *MAT_002.The a-direction is from node 1 to node 2 of the element.The b-direction is orthogonal to the adirection and is in the plane formed by nodes 1, 2,and 4.
        EQ.1.0: locally orthotropic with material axes determined by a point in space and the global location of the element center; this is the a-direction.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.
        EQ.4.0: locally orthotropic in cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis.This option is for solid elements only.
        EQ.5.0: globally defined reference frame with(a,b,c)=(X0,Y0,Z0).
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""aopt must be one of {0,1,2,3,4,5}""")
        self._cards[4].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1: No change, default
        """ # nopep8
        return self._cards[4].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        self._cards[4].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

