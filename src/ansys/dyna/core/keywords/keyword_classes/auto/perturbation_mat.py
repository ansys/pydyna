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

class PerturbationMat(KeywordBase):
    """DYNA PERTURBATION_MAT keyword"""

    keyword = "PERTURBATION"
    subkeyword = "MAT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "type",
                        int,
                        0,
                        10,
                        kwargs.get("type", 1)
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "scl",
                        float,
                        20,
                        10,
                        kwargs.get("scl", 1.0)
                    ),
                    Field(
                        "cmp",
                        int,
                        30,
                        10,
                        kwargs.get("cmp")
                    ),
                    Field(
                        "icoord",
                        int,
                        40,
                        10,
                        kwargs.get("icoord", 0)
                    ),
                    Field(
                        "cid",
                        int,
                        50,
                        10,
                        kwargs.get("cid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ampl",
                        float,
                        0,
                        10,
                        kwargs.get("ampl", 1.0)
                    ),
                    Field(
                        "xwl",
                        float,
                        10,
                        10,
                        kwargs.get("xwl", 0.0)
                    ),
                    Field(
                        "xoff",
                        float,
                        20,
                        10,
                        kwargs.get("xoff", 0.0)
                    ),
                    Field(
                        "ywl",
                        float,
                        30,
                        10,
                        kwargs.get("ywl", 0.0)
                    ),
                    Field(
                        "yoff",
                        float,
                        40,
                        10,
                        kwargs.get("yoff", 0.0)
                    ),
                    Field(
                        "zwl",
                        float,
                        50,
                        10,
                        kwargs.get("zwl", 0.0)
                    ),
                    Field(
                        "zoff",
                        float,
                        60,
                        10,
                        kwargs.get("zoff", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fade",
                        float,
                        0,
                        10,
                        kwargs.get("fade", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fname",
                        str,
                        0,
                        80,
                        kwargs.get("fname")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cstype",
                        int,
                        0,
                        10,
                        kwargs.get("cstype", 1)
                    ),
                    Field(
                        "ellip1",
                        float,
                        10,
                        10,
                        kwargs.get("ellip1", 0.0)
                    ),
                    Field(
                        "ellip2",
                        float,
                        20,
                        10,
                        kwargs.get("ellip2", 0.0)
                    ),
                    Field(
                        "rnd",
                        int,
                        30,
                        10,
                        kwargs.get("rnd", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cftype",
                        int,
                        0,
                        10,
                        kwargs.get("cftype", 1)
                    ),
                    Field(
                        "cfc1",
                        float,
                        10,
                        10,
                        kwargs.get("cfc1", 1.0)
                    ),
                    Field(
                        "cfc2",
                        float,
                        20,
                        10,
                        kwargs.get("cfc2", 1.0)
                    ),
                    Field(
                        "cfc3",
                        float,
                        30,
                        10,
                        kwargs.get("cfc3", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ampl",
                        int,
                        0,
                        10,
                        kwargs.get("ampl", 1)
                    ),
                    Field(
                        "dtype",
                        float,
                        10,
                        10,
                        kwargs.get("dtype", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def type(self) -> int:
        """Get or set the Type of perturbation.
        EQ.1:	Harmonic Field (see Remark 3)
        EQ.2:unused
        EQ.3:	Read perturbations from a file
        EQ.4 : Spectral field
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""type must be one of {1,2,3,4}""")
        self._cards[0].set_value("type", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def scl(self) -> float:
        """Get or set the Scale factor.
        """ # nopep8
        return self._cards[0].get_value("scl")

    @scl.setter
    def scl(self, value: float) -> None:
        self._cards[0].set_value("scl", value)

    @property
    def cmp(self) -> typing.Optional[int]:
        """Get or set the Component.
        """ # nopep8
        return self._cards[0].get_value("cmp")

    @cmp.setter
    def cmp(self, value: int) -> None:
        self._cards[0].set_value("cmp", value)

    @property
    def icoord(self) -> int:
        """Get or set the Coordinate system to use;
        EQ.0: Global Cartesian
        EQ.1: Cartesian
        EQ.2: Cylindrical (computed and applied)
        EQ.3: Spherical (computed and applied)
        EQ.-2: Computed in cartesian but applied in cylindrical
        EQ.-3  Computed in cartesian but applied in spherical.
        """ # nopep8
        return self._cards[0].get_value("icoord")

    @icoord.setter
    def icoord(self, value: int) -> None:
        if value not in [0, 1, 2, 3, -2, -3]:
            raise Exception("""icoord must be one of {0,1,2,3,-2,-3}""")
        self._cards[0].set_value("icoord", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID,see *DEFINE_‌COORDINATE_‌NODES.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def ampl(self) -> float:
        """Get or set the Amplitude of the harmonic perturbation.
        """ # nopep8
        return self._cards[1].get_value("ampl")

    @ampl.setter
    def ampl(self, value: float) -> None:
        self._cards[1].set_value("ampl", value)

    @property
    def xwl(self) -> float:
        """Get or set the x wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("xwl")

    @xwl.setter
    def xwl(self, value: float) -> None:
        self._cards[1].set_value("xwl", value)

    @property
    def xoff(self) -> float:
        """Get or set the x offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("xoff")

    @xoff.setter
    def xoff(self, value: float) -> None:
        self._cards[1].set_value("xoff", value)

    @property
    def ywl(self) -> float:
        """Get or set the y wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("ywl")

    @ywl.setter
    def ywl(self, value: float) -> None:
        self._cards[1].set_value("ywl", value)

    @property
    def yoff(self) -> float:
        """Get or set the y offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("yoff")

    @yoff.setter
    def yoff(self, value: float) -> None:
        self._cards[1].set_value("yoff", value)

    @property
    def zwl(self) -> float:
        """Get or set the z wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("zwl")

    @zwl.setter
    def zwl(self, value: float) -> None:
        self._cards[1].set_value("zwl", value)

    @property
    def zoff(self) -> float:
        """Get or set the z offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("zoff")

    @zoff.setter
    def zoff(self, value: float) -> None:
        self._cards[1].set_value("zoff", value)

    @property
    def fade(self) -> float:
        """Get or set the Distance over which all *PERTURBATION_NODE are faded to zero.
        """ # nopep8
        return self._cards[2].get_value("fade")

    @fade.setter
    def fade(self, value: float) -> None:
        self._cards[2].set_value("fade", value)

    @property
    def fname(self) -> typing.Optional[str]:
        """Get or set the Name of file containing the perturbation definitions.
        """ # nopep8
        return self._cards[3].get_value("fname")

    @fname.setter
    def fname(self, value: str) -> None:
        self._cards[3].set_value("fname", value)

    @property
    def cstype(self) -> int:
        """Get or set the Correlation structure:
        EQ.1: 3D isotropic. The X, Y and Z correlations are described using one correlation function.
        EQ.2: 3D product. The X, Y and Z correlations are described using a correlation function each.
        EQ.3: 2D isotropic. A correlation function describes the X correlation while the YZ isotropic relationship is described using another correlation function.
        EQ.4: 2D isotropic. A correlation function describes the Y correlation while the XZ isotropic relationship is described using another correlation function.
        EQ.5: 2D isotropic. A correlation function describes the Z correlation while the XY isotropic relationship is described using another correlation function.
        EQ.6: 3D elliptic. Define CSE1 and CSE2.
        EQ.7: 2D elliptic. A correlation function describes the X correlation while the YZ elliptic relationship is described using another correlation function.
        EQ.8: 2D elliptic. A correlation function describes the Y correlation while the ZX elliptic relationship is described using another correlation function.
        EQ.9: 2D elliptic. A correlation function describes the Z correlation while the XY elliptic relationship is described using another correlation function.
        """ # nopep8
        return self._cards[4].get_value("cstype")

    @cstype.setter
    def cstype(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""cstype must be one of {1,2,3,4,5,6,7,8,9}""")
        self._cards[4].set_value("cstype", value)

    @property
    def ellip1(self) -> float:
        """Get or set the Elliptic constant for 2D and 3D elliptic fields.
        """ # nopep8
        return self._cards[4].get_value("ellip1")

    @ellip1.setter
    def ellip1(self, value: float) -> None:
        self._cards[4].set_value("ellip1", value)

    @property
    def ellip2(self) -> float:
        """Get or set the Elliptic constant for 3D elliptic field.
        """ # nopep8
        return self._cards[4].get_value("ellip2")

    @ellip2.setter
    def ellip2(self, value: float) -> None:
        self._cards[4].set_value("ellip2", value)

    @property
    def rnd(self) -> int:
        """Get or set the Seed for random number generator.
        EQ.0: LS-DYNA will generate a random seed
        GT.0: Value to be used as seed.
        """ # nopep8
        return self._cards[4].get_value("rnd")

    @rnd.setter
    def rnd(self, value: int) -> None:
        self._cards[4].set_value("rnd", value)

    @property
    def cftype(self) -> int:
        """Get or set the Correlation function
        EQ.1: Gaussian
        EQ.2: Exponential
        EQ.3: Exponential Cosine
        EQ.4: Rational
        EQ.5: Linear.
        """ # nopep8
        return self._cards[5].get_value("cftype")

    @cftype.setter
    def cftype(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""cftype must be one of {1,2,3,4,5}""")
        self._cards[5].set_value("cftype", value)

    @property
    def cfc1(self) -> float:
        """Get or set the Correlation function constant 1.
        """ # nopep8
        return self._cards[5].get_value("cfc1")

    @cfc1.setter
    def cfc1(self, value: float) -> None:
        self._cards[5].set_value("cfc1", value)

    @property
    def cfc2(self) -> float:
        """Get or set the Correlation function constant 2.
        """ # nopep8
        return self._cards[5].get_value("cfc2")

    @cfc2.setter
    def cfc2(self, value: float) -> None:
        self._cards[5].set_value("cfc2", value)

    @property
    def cfc3(self) -> float:
        """Get or set the Correlation function constant 3.
        """ # nopep8
        return self._cards[5].get_value("cfc3")

    @cfc3.setter
    def cfc3(self, value: float) -> None:
        self._cards[5].set_value("cfc3", value)

    @property
    def ampl(self) -> int:
        """Get or set the Amplitude of the random perturbation.
        """ # nopep8
        return self._cards[6].get_value("ampl")

    @ampl.setter
    def ampl(self, value: int) -> None:
        self._cards[6].set_value("ampl", value)

    @property
    def dtype(self) -> float:
        """Get or set the Distribution type:
        EQ.0.0:	Uniform distribution between SCL×[0,AMPL]
        EQ.1.0 : Uniform distribution between SCL×[-AMPL ,AMPL]
        """ # nopep8
        return self._cards[6].get_value("dtype")

    @dtype.setter
    def dtype(self, value: float) -> None:
        self._cards[6].set_value("dtype", value)

