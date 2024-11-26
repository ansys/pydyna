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

class MatSamp1(KeywordBase):
    """DYNA MAT_SAMP_1 keyword"""

    keyword = "MAT"
    subkeyword = "SAMP_1"
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
                        "bulk",
                        float,
                        20,
                        10,
                        kwargs.get("bulk")
                    ),
                    Field(
                        "gmod",
                        float,
                        30,
                        10,
                        kwargs.get("gmod")
                    ),
                    Field(
                        "emod",
                        float,
                        40,
                        10,
                        kwargs.get("emod")
                    ),
                    Field(
                        "nue",
                        float,
                        50,
                        10,
                        kwargs.get("nue")
                    ),
                    Field(
                        "rbcfac",
                        float,
                        60,
                        10,
                        kwargs.get("rbcfac")
                    ),
                    Field(
                        "numint",
                        int,
                        70,
                        10,
                        kwargs.get("numint")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid-t",
                        int,
                        0,
                        10,
                        kwargs.get("lcid-t")
                    ),
                    Field(
                        "lcid-c",
                        int,
                        10,
                        10,
                        kwargs.get("lcid-c")
                    ),
                    Field(
                        "lcid-s",
                        int,
                        20,
                        10,
                        kwargs.get("lcid-s")
                    ),
                    Field(
                        "lcid-b",
                        int,
                        30,
                        10,
                        kwargs.get("lcid-b")
                    ),
                    Field(
                        "nuep",
                        float,
                        40,
                        10,
                        kwargs.get("nuep")
                    ),
                    Field(
                        "lcid-p",
                        int,
                        50,
                        10,
                        kwargs.get("lcid-p")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "incdam",
                        int,
                        70,
                        10,
                        kwargs.get("incdam", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid_d",
                        int,
                        0,
                        10,
                        kwargs.get("lcid_d")
                    ),
                    Field(
                        "epfail",
                        float,
                        10,
                        10,
                        kwargs.get("epfail", 1.0E+5)
                    ),
                    Field(
                        "deprpt",
                        float,
                        20,
                        10,
                        kwargs.get("deprpt")
                    ),
                    Field(
                        "lcid-tri",
                        int,
                        30,
                        10,
                        kwargs.get("lcid-tri")
                    ),
                    Field(
                        "lcid_lc",
                        int,
                        40,
                        10,
                        kwargs.get("lcid_lc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "miter",
                        int,
                        0,
                        10,
                        kwargs.get("miter")
                    ),
                    Field(
                        "mipds",
                        int,
                        10,
                        10,
                        kwargs.get("mipds")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "incfail",
                        int,
                        30,
                        10,
                        kwargs.get("incfail", 0)
                    ),
                    Field(
                        "iconv",
                        int,
                        40,
                        10,
                        kwargs.get("iconv", 0)
                    ),
                    Field(
                        "asaf",
                        int,
                        50,
                        10,
                        kwargs.get("asaf")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nhsv",
                        int,
                        70,
                        10,
                        kwargs.get("nhsv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcemod",
                        int,
                        0,
                        10,
                        kwargs.get("lcemod")
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "filt",
                        float,
                        20,
                        10,
                        kwargs.get("filt")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSamp1.option_specs[0],
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus, used by LS-DYNA in the time step calculation
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[0].set_value("bulk", value)

    @property
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, used by LS-DYNA in the time step calculation.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        self._cards[0].set_value("gmod", value)

    @property
    def emod(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("emod")

    @emod.setter
    def emod(self, value: float) -> None:
        self._cards[0].set_value("emod", value)

    @property
    def nue(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("nue")

    @nue.setter
    def nue(self, value: float) -> None:
        self._cards[0].set_value("nue", value)

    @property
    def rbcfac(self) -> typing.Optional[float]:
        """Get or set the Ratio of yield in biaxial compression vs. yield in uniaxial compression. If nonzero this will activate the use of a multi-linear yield surface. Default is 0.
        """ # nopep8
        return self._cards[0].get_value("rbcfac")

    @rbcfac.setter
    def rbcfac(self, value: float) -> None:
        self._cards[0].set_value("rbcfac", value)

    @property
    def numint(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: int) -> None:
        self._cards[0].set_value("numint", value)

    @property
    def lcid_t(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving the yield stress as a function of plastic strain, these curves should be obtained from quasi-static and (optionally) dynamic uniaxial tensile tests, this input is mandatory and the material model will not work unless at least one tensile stress-strain curve is given.
        """ # nopep8
        return self._cards[1].get_value("lcid-t")

    @lcid_t.setter
    def lcid_t(self, value: int) -> None:
        self._cards[1].set_value("lcid-t", value)

    @property
    def lcid_c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static uniaxial compression test, this input is optional.
        """ # nopep8
        return self._cards[1].get_value("lcid-c")

    @lcid_c.setter
    def lcid_c(self, value: int) -> None:
        self._cards[1].set_value("lcid-c", value)

    @property
    def lcid_s(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static shear test, this input is optional
        """ # nopep8
        return self._cards[1].get_value("lcid-s")

    @lcid_s.setter
    def lcid_s(self, value: int) -> None:
        self._cards[1].set_value("lcid-s", value)

    @property
    def lcid_b(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static biaxial tensile test, this input is optional.
        """ # nopep8
        return self._cards[1].get_value("lcid-b")

    @lcid_b.setter
    def lcid_b(self, value: int) -> None:
        self._cards[1].set_value("lcid-b", value)

    @property
    def nuep(self) -> typing.Optional[float]:
        """Get or set the plastic Poisson's ratio : an estimated ratio of transversal to longitudinal plastic rate of deformation should be given, a value <0 will result in associated plasticity to the yield surface (the associated plasticity option is implemented only for IQUAD=1).
        """ # nopep8
        return self._cards[1].get_value("nuep")

    @nuep.setter
    def nuep(self, value: float) -> None:
        self._cards[1].set_value("nuep", value)

    @property
    def lcid_p(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the plastic Poisson's ratio as a function of equivalent plastic deformation during uniaxial tensile testing, if the (optional) load curve is given, the constant value in the previous field will be ignored.
        """ # nopep8
        return self._cards[1].get_value("lcid-p")

    @lcid_p.setter
    def lcid_p(self, value: int) -> None:
        self._cards[1].set_value("lcid-p", value)

    @property
    def incdam(self) -> int:
        """Get or set the Flag to control the damage evolution as a function of triaxiality. If INCDAM=0 damage evolution is independent of the triaxialty. If INCDAM=1 an incremental formulation is used to compute the damage.
        """ # nopep8
        return self._cards[1].get_value("incdam")

    @incdam.setter
    def incdam(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""incdam must be one of {0,1}""")
        self._cards[1].set_value("incdam", value)

    @property
    def lcid_d(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the damage parameter as a function of equivalent plastic deformation during uniaxial tensile testing, by default this option assumes that effective yield values are used in the load curves LCID-T, LCID-C, LCID-S and LCID-B, if LCID-D is given a negative value, true yield stress values can be used.
        """ # nopep8
        return self._cards[2].get_value("lcid_d")

    @lcid_d.setter
    def lcid_d(self, value: int) -> None:
        self._cards[2].set_value("lcid_d", value)

    @property
    def epfail(self) -> float:
        """Get or set the This parameter is the equivalent plastic strain at failure. If EPFAIL is given as a negative integer, a load curve is expected that defines EPFAIL as a function of the plastic strain rate. Default value is 1.0e+5
        """ # nopep8
        return self._cards[2].get_value("epfail")

    @epfail.setter
    def epfail(self, value: float) -> None:
        self._cards[2].set_value("epfail", value)

    @property
    def deprpt(self) -> typing.Optional[float]:
        """Get or set the Increment of equivalent plastic strain between failure point and rupture point, stresses will fade out to zero between EPFAIL and EPFAIL+DEPRUPT
        """ # nopep8
        return self._cards[2].get_value("deprpt")

    @deprpt.setter
    def deprpt(self, value: float) -> None:
        self._cards[2].set_value("deprpt", value)

    @property
    def lcid_tri(self) -> typing.Optional[int]:
        """Get or set the Load curve that specifies a factor that works multiplicatively on the value of DC depending on the triaxiality pressue/sigma_vm.. This option is active only if DC is given as a negative value (see above)..
        """ # nopep8
        return self._cards[2].get_value("lcid-tri")

    @lcid_tri.setter
    def lcid_tri(self, value: int) -> None:
        self._cards[2].set_value("lcid-tri", value)

    @property
    def lcid_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve that specifies a factor that works multiplicatively on the value of DC depending on the linear element dimension, this option is active only if DC is given as a negative value (see above).
        """ # nopep8
        return self._cards[2].get_value("lcid_lc")

    @lcid_lc.setter
    def lcid_lc(self, value: int) -> None:
        self._cards[2].set_value("lcid_lc", value)

    @property
    def miter(self) -> typing.Optional[int]:
        """Get or set the Maximum number of iterations in the cutting plane algorithm, default is set to 400.
        """ # nopep8
        return self._cards[3].get_value("miter")

    @miter.setter
    def miter(self, value: int) -> None:
        self._cards[3].set_value("miter", value)

    @property
    def mipds(self) -> typing.Optional[int]:
        """Get or set the Maximum number of iterations in the secant iteration performed to enforce plane stress (shell elements only), default set to 10
        """ # nopep8
        return self._cards[3].get_value("mipds")

    @mipds.setter
    def mipds(self, value: int) -> None:
        self._cards[3].set_value("mipds", value)

    @property
    def incfail(self) -> int:
        """Get or set the Flag to control the failure evolution as a function of triaxiality. If INCFAIL=0 failure evolution is independent of the triaxiality. If INCFAIL=1 an incremental formulation is used to compute the failure value. If INCFAIL=-1 the failure model is deactivated.
        """ # nopep8
        return self._cards[3].get_value("incfail")

    @incfail.setter
    def incfail(self, value: int) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""incfail must be one of {0,1,-1}""")
        self._cards[3].set_value("incfail", value)

    @property
    def iconv(self) -> int:
        """Get or set the Formulation flag :
        ICONV=0 : default
        ICONV=1 : yield surface is internally modified by increasing the shear yield until a convex yield surface is achieved
        ICONV=2 : if the plastic Poisson's ratio is smaller than the elastic Poisson's ratio, both are set equal to the smaller value of the two
        """ # nopep8
        return self._cards[3].get_value("iconv")

    @iconv.setter
    def iconv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iconv must be one of {0,1}""")
        self._cards[3].set_value("iconv", value)

    @property
    def asaf(self) -> typing.Optional[int]:
        """Get or set the Safety factor, used only if ICONV=1, values between 1 and 2 can improve convergence, however the shear yield will be artificially increased if this option is used, default is set to 1.
        """ # nopep8
        return self._cards[3].get_value("asaf")

    @asaf.setter
    def asaf(self, value: int) -> None:
        self._cards[3].set_value("asaf", value)

    @property
    def nhsv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables. Default is 22. Set to 28 if the “instability criterion” should be included in the output (see Remark 5). Note that NEIPS or NEIPH must also be set on *DATABASE_EXTENT_BINARY for the history variable data to be output.
        """ # nopep8
        return self._cards[3].get_value("nhsv")

    @nhsv.setter
    def nhsv(self, value: int) -> None:
        self._cards[3].set_value("nhsv", value)

    @property
    def lcemod(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining Young's modulus as function of effective strain rate.
        """ # nopep8
        return self._cards[4].get_value("lcemod")

    @lcemod.setter
    def lcemod(self, value: int) -> None:
        self._cards[4].set_value("lcemod", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Decay constant in viscoelastic law
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def filt(self) -> typing.Optional[float]:
        """Get or set the Factor for strain rate filtering
        """ # nopep8
        return self._cards[4].get_value("filt")

    @filt.setter
    def filt(self, value: float) -> None:
        self._cards[4].set_value("filt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

