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

class MatAddErosion(KeywordBase):
    """DYNA MAT_ADD_EROSION keyword"""

    keyword = "MAT"
    subkeyword = "ADD_EROSION"
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
                        "excl",
                        float,
                        10,
                        10,
                        kwargs.get("excl")
                    ),
                    Field(
                        "mxpres",
                        float,
                        20,
                        10,
                        kwargs.get("mxpres")
                    ),
                    Field(
                        "mneps",
                        float,
                        30,
                        10,
                        kwargs.get("mneps")
                    ),
                    Field(
                        "effeps",
                        float,
                        40,
                        10,
                        kwargs.get("effeps")
                    ),
                    Field(
                        "voleps",
                        float,
                        50,
                        10,
                        kwargs.get("voleps")
                    ),
                    Field(
                        "numfip",
                        float,
                        60,
                        10,
                        kwargs.get("numfip", 1.0)
                    ),
                    Field(
                        "ncs",
                        float,
                        70,
                        10,
                        kwargs.get("ncs", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mnpres",
                        float,
                        0,
                        10,
                        kwargs.get("mnpres")
                    ),
                    Field(
                        "sigp1",
                        float,
                        10,
                        10,
                        kwargs.get("sigp1")
                    ),
                    Field(
                        "sigvm",
                        float,
                        20,
                        10,
                        kwargs.get("sigvm")
                    ),
                    Field(
                        "mxeps",
                        float,
                        30,
                        10,
                        kwargs.get("mxeps")
                    ),
                    Field(
                        "epssh",
                        float,
                        40,
                        10,
                        kwargs.get("epssh")
                    ),
                    Field(
                        "sigth",
                        float,
                        50,
                        10,
                        kwargs.get("sigth")
                    ),
                    Field(
                        "impulse",
                        float,
                        60,
                        10,
                        kwargs.get("impulse")
                    ),
                    Field(
                        "failtm",
                        float,
                        70,
                        10,
                        kwargs.get("failtm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idam",
                        int,
                        0,
                        10,
                        kwargs.get("idam")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcregd",
                        int,
                        70,
                        10,
                        kwargs.get("lcregd")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcfld",
                        int,
                        0,
                        10,
                        kwargs.get("lcfld")
                    ),
                    Field(
                        "nsff",
                        int,
                        10,
                        10,
                        kwargs.get("nsff", 10)
                    ),
                    Field(
                        "epsthin",
                        float,
                        20,
                        10,
                        kwargs.get("epsthin")
                    ),
                    Field(
                        "engcrt",
                        float,
                        30,
                        10,
                        kwargs.get("engcrt")
                    ),
                    Field(
                        "radcrt",
                        float,
                        40,
                        10,
                        kwargs.get("radcrt")
                    ),
                    Field(
                        "lceps12",
                        int,
                        50,
                        10,
                        kwargs.get("lceps12")
                    ),
                    Field(
                        "lceps13",
                        int,
                        60,
                        10,
                        kwargs.get("lceps13")
                    ),
                    Field(
                        "lcepsmx",
                        int,
                        70,
                        10,
                        kwargs.get("lcepsmx")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dteflt",
                        float,
                        0,
                        10,
                        kwargs.get("dteflt")
                    ),
                    Field(
                        "volfrac",
                        float,
                        10,
                        10,
                        kwargs.get("volfrac")
                    ),
                    Field(
                        "mxtmp",
                        float,
                        20,
                        10,
                        kwargs.get("mxtmp")
                    ),
                    Field(
                        "dtmin",
                        float,
                        30,
                        10,
                        kwargs.get("dtmin")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddErosion.option_specs[0],
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
        """Get or set the Material identification for which this erosion definition applies.
        A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def excl(self) -> typing.Optional[float]:
        """Get or set the The exclusion number, which applies to the failure values defined on Cards 1, 2, and 7.
        When any of the failure values on these cards are set to the exclusion number,
        the associated failure criterion is not invoked.  Or in other words, only the failure values not set to the exclusion number are invoked.
        The default value of EXCL is 0.0, which eliminates all failure criteria from consideration that have their constants left blank or set to 0.0.  As an example,
        to prevent a material from developing tensile pressure, the user could specify an unusual value for
        the exclusion number, e.g., 1234, set MNPRES to 0.0, and set all the remaining failure values to 1234.
        However, use of an exclusion number may be considered nonessential since the same effect
        could be achieved without use of the exclusion number by setting MNPRES to a very small negative value.
        """ # nopep8
        return self._cards[0].get_value("excl")

    @excl.setter
    def excl(self, value: float) -> None:
        self._cards[0].set_value("excl", value)

    @property
    def mxpres(self) -> typing.Optional[float]:
        """Get or set the Maximum pressure at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("mxpres")

    @mxpres.setter
    def mxpres(self, value: float) -> None:
        self._cards[0].set_value("mxpres", value)

    @property
    def mneps(self) -> typing.Optional[float]:
        """Get or set the Minimum principal strain at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("mneps")

    @mneps.setter
    def mneps(self, value: float) -> None:
        self._cards[0].set_value("mneps", value)

    @property
    def effeps(self) -> typing.Optional[float]:
        """Get or set the Maximum effective strain at failure, . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("effeps")

    @effeps.setter
    def effeps(self, value: float) -> None:
        self._cards[0].set_value("effeps", value)

    @property
    def voleps(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain at failure,  .  VOLEPS can be a positive or negative number depending on whether the failure is in tension or compression, respectively.  If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("voleps")

    @voleps.setter
    def voleps(self, value: float) -> None:
        self._cards[0].set_value("voleps", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number or percentage of failed integration points prior to element deletion (default is 1).  See Remark 2.
        NUMFIP does not apply to higher order solid element types 24, 25, 26, 27, 28, and 29, rather see the variable VOLFRAC.
        GT.0.0:	Number of integration points which must fail before element is deleted.
        LT.0.0:	Applies only to shells. "|NUMFIP|" is the percentage of integration points which must exceed the failure criterion before the element fails.
        If NUMFIP < -100, then "|NUMFIP|-100"  is the number of failed integration points prior to element deletion.
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        self._cards[0].set_value("numfip", value)

    @property
    def ncs(self) -> float:
        """Get or set the Number of failure conditions to satisfy before failure occurs.  For example, if SIGP1 and SIGVM are defined and if NCS=2, both failure criteria must be met before element deletion can occur.  The default is set to unity.
        """ # nopep8
        return self._cards[0].get_value("ncs")

    @ncs.setter
    def ncs(self, value: float) -> None:
        self._cards[0].set_value("ncs", value)

    @property
    def mnpres(self) -> typing.Optional[float]:
        """Get or set the Minimum pressure at failure,
        """ # nopep8
        return self._cards[1].get_value("mnpres")

    @mnpres.setter
    def mnpres(self, value: float) -> None:
        self._cards[1].set_value("mnpres", value)

    @property
    def sigp1(self) -> typing.Optional[float]:
        """Get or set the Maximum principal stress at failur, Sigma max .
        """ # nopep8
        return self._cards[1].get_value("sigp1")

    @sigp1.setter
    def sigp1(self, value: float) -> None:
        self._cards[1].set_value("sigp1", value)

    @property
    def sigvm(self) -> typing.Optional[float]:
        """Get or set the Equivalent stress at failure, The equivalent stress at failure is made a function of the effective strain rate by setting SIGVM to the negative of the appropriate load curve ID, Sigma max
        """ # nopep8
        return self._cards[1].get_value("sigvm")

    @sigvm.setter
    def sigvm(self, value: float) -> None:
        self._cards[1].set_value("sigvm", value)

    @property
    def mxeps(self) -> typing.Optional[float]:
        """Get or set the Variable to invoke a failure criterion based on maximum principal strain.
        GT.0:    Maximum principal strain at failure, ε_max.
        LT.0:    -MXEPS is the ID of a curve giving maximum principal strain at failure as a function of effective strain rate.
        A filter is applied to the effective strain rate according to DTEFLT; see Card 8.
        """ # nopep8
        return self._cards[1].get_value("mxeps")

    @mxeps.setter
    def mxeps(self, value: float) -> None:
        self._cards[1].set_value("mxeps", value)

    @property
    def epssh(self) -> typing.Optional[float]:
        """Get or set the Tensorial shear strain at failure, r max .
        """ # nopep8
        return self._cards[1].get_value("epssh")

    @epssh.setter
    def epssh(self, value: float) -> None:
        self._cards[1].set_value("epssh", value)

    @property
    def sigth(self) -> typing.Optional[float]:
        """Get or set the Threshold stress, Sigma 0 .
        """ # nopep8
        return self._cards[1].get_value("sigth")

    @sigth.setter
    def sigth(self, value: float) -> None:
        self._cards[1].set_value("sigth", value)

    @property
    def impulse(self) -> typing.Optional[float]:
        """Get or set the Stress impulse for failure, K f.
        """ # nopep8
        return self._cards[1].get_value("impulse")

    @impulse.setter
    def impulse(self, value: float) -> None:
        self._cards[1].set_value("impulse", value)

    @property
    def failtm(self) -> typing.Optional[float]:
        """Get or set the Failure time. When the problem time exceeds the failure time, the material is removed.
        GT.0:	Failure time is active during any phase of the analysis.
        LT.0:	Failure time is set to |FAILTM| but this criterion in inactive during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("failtm")

    @failtm.setter
    def failtm(self, value: float) -> None:
        self._cards[1].set_value("failtm", value)

    @property
    def idam(self) -> typing.Optional[int]:
        """Get or set the Flag for damage model.
        EQ.0: no damage model is used.
        NE.0:	Damage models GISSMO or DIEM, see manuals of R10 and before.
        Still available here for backward compatibility, but description actually moved to new keywords *MAT_ADD_DAMAGE_DIEM/GISSMO
        ,
        """ # nopep8
        return self._cards[2].get_value("idam")

    @idam.setter
    def idam(self, value: int) -> None:
        self._cards[2].set_value("idam", value)

    @property
    def lcregd(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining element size dependent regularization factors for equivalent plastic strain to failure.
        """ # nopep8
        return self._cards[2].get_value("lcregd")

    @lcregd.setter
    def lcregd(self, value: int) -> None:
        self._cards[2].set_value("lcregd", value)

    @property
    def lcfld(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. Load curve defines the Forming Limit Diagram, where minor engineering strains in percent are defined as abscissa values and major engineering strains in percent are defined as ordinate values. Table defines for each strain rate an associated FLD curve. The forming limit diagram is shown in Figure Error! Reference source not found.. In defining the curve, list pairs of minor and major strains starting with the left most point and ending with the right most point. This criterion is only available for shell elements.
        """ # nopep8
        return self._cards[3].get_value("lcfld")

    @lcfld.setter
    def lcfld(self, value: int) -> None:
        self._cards[3].set_value("lcfld", value)

    @property
    def nsff(self) -> int:
        """Get or set the Number of explicit time step cycles for stress fade-out used in the LCFLD criterion. Default is 10.
        """ # nopep8
        return self._cards[3].get_value("nsff")

    @nsff.setter
    def nsff(self, value: int) -> None:
        self._cards[3].set_value("nsff", value)

    @property
    def epsthin(self) -> typing.Optional[float]:
        """Get or set the Thinning strain at failure for thin and thick shells.
        GT.0.0:	individual thinning for each integration point from z-strain
        LT.0.0:	averaged thinning strain from element thickness change
        """ # nopep8
        return self._cards[3].get_value("epsthin")

    @epsthin.setter
    def epsthin(self, value: float) -> None:
        self._cards[3].set_value("epsthin", value)

    @property
    def engcrt(self) -> typing.Optional[float]:
        """Get or set the Critical energy for nonlocal failure criterion
        """ # nopep8
        return self._cards[3].get_value("engcrt")

    @engcrt.setter
    def engcrt(self, value: float) -> None:
        self._cards[3].set_value("engcrt", value)

    @property
    def radcrt(self) -> typing.Optional[float]:
        """Get or set the Critical radius for nonlocal failure criterion
        """ # nopep8
        return self._cards[3].get_value("radcrt")

    @radcrt.setter
    def radcrt(self, value: float) -> None:
        self._cards[3].set_value("radcrt", value)

    @property
    def lceps12(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining in-plane shear strain limit_12^c vs. element size
        """ # nopep8
        return self._cards[3].get_value("lceps12")

    @lceps12.setter
    def lceps12(self, value: int) -> None:
        self._cards[3].set_value("lceps12", value)

    @property
    def lceps13(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining through-thickness shear strain limit_13^c vs. element size
        """ # nopep8
        return self._cards[3].get_value("lceps13")

    @lceps13.setter
    def lceps13(self, value: int) -> None:
        self._cards[3].set_value("lceps13", value)

    @property
    def lcepsmx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining in-plane major strain limit_1^c vs. element size
        """ # nopep8
        return self._cards[3].get_value("lcepsmx")

    @lcepsmx.setter
    def lcepsmx(self, value: int) -> None:
        self._cards[3].set_value("lcepsmx", value)

    @property
    def dteflt(self) -> typing.Optional[float]:
        """Get or set the The time period (or inverse of the cutoff frequency) for the low-pass filter applied to the effective strain rate when MXEPS is negative.
        If DTEFLT is set to zero or left blank, no filtering of the effective strain rate is performed in the determination of the maximum principal strain to failure.
        """ # nopep8
        return self._cards[4].get_value("dteflt")

    @dteflt.setter
    def dteflt(self, value: float) -> None:
        self._cards[4].set_value("dteflt", value)

    @property
    def volfrac(self) -> typing.Optional[float]:
        """Get or set the The volume fraction required to fail before the element is deleted. The default is 0.5. It is used for higher order solid element types 24, 25, 26, 27, 28, and 29, and all isogeometric solids and shell elements.
        """ # nopep8
        return self._cards[4].get_value("volfrac")

    @volfrac.setter
    def volfrac(self, value: float) -> None:
        self._cards[4].set_value("volfrac", value)

    @property
    def mxtmp(self) -> typing.Optional[float]:
        """Get or set the Maximum temperature at failure
        """ # nopep8
        return self._cards[4].get_value("mxtmp")

    @mxtmp.setter
    def mxtmp(self, value: float) -> None:
        self._cards[4].set_value("mxtmp", value)

    @property
    def dtmin(self) -> typing.Optional[float]:
        """Get or set the -
        """ # nopep8
        return self._cards[4].get_value("dtmin")

    @dtmin.setter
    def dtmin(self, value: float) -> None:
        self._cards[4].set_value("dtmin", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

