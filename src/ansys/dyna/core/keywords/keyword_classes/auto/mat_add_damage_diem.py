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

class MatAddDamageDiem(KeywordBase):
    """DYNA MAT_ADD_DAMAGE_DIEM keyword"""

    keyword = "MAT"
    subkeyword = "ADD_DAMAGE_DIEM"
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
                        "ndiemc",
                        float,
                        10,
                        10,
                        kwargs.get("ndiemc", 0.0)
                    ),
                    Field(
                        "dinit",
                        int,
                        20,
                        10,
                        kwargs.get("dinit", 0)
                    ),
                    Field(
                        "deps",
                        float,
                        30,
                        10,
                        kwargs.get("deps", 0.0)
                    ),
                    Field(
                        "numfip",
                        float,
                        40,
                        10,
                        kwargs.get("numfip", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dityp",
                        float,
                        0,
                        10,
                        kwargs.get("dityp", 0.0)
                    ),
                    Field(
                        "p1",
                        float,
                        10,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        20,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "p3",
                        float,
                        30,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "p4",
                        float,
                        40,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "p5",
                        float,
                        50,
                        10,
                        kwargs.get("p5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "detyp",
                        float,
                        0,
                        10,
                        kwargs.get("detyp", 0.0)
                    ),
                    Field(
                        "dctyp",
                        float,
                        10,
                        10,
                        kwargs.get("dctyp", 0.0)
                    ),
                    Field(
                        "q1",
                        float,
                        20,
                        10,
                        kwargs.get("q1")
                    ),
                    Field(
                        "q2",
                        float,
                        30,
                        10,
                        kwargs.get("q2")
                    ),
                    Field(
                        "q3",
                        float,
                        40,
                        10,
                        kwargs.get("q3")
                    ),
                    Field(
                        "q4",
                        float,
                        50,
                        10,
                        kwargs.get("q4")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddDamageDiem.option_specs[0],
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
        """Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ndiemc(self) -> float:
        """Get or set the Number of damage initiation and evolution model (DIEM) criteria to be applied.
        """ # nopep8
        return self._cards[0].get_value("ndiemc")

    @ndiemc.setter
    def ndiemc(self, value: float) -> None:
        self._cards[0].set_value("ndiemc", value)

    @property
    def dinit(self) -> int:
        """Get or set the Damage initialization option.
        EQ.0:	No action is taken
        EQ.1:	Damage history is initiated based on values of initial plastic strains and initial strain tensor, this is to be used in multistage analyses.
        """ # nopep8
        return self._cards[0].get_value("dinit")

    @dinit.setter
    def dinit(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dinit must be one of {0,1}""")
        self._cards[0].set_value("dinit", value)

    @property
    def deps(self) -> float:
        """Get or set the Plastic strain increment between evaluation of damage instability and evolution criteria. See remarks, the default is zero.
        """ # nopep8
        return self._cards[0].get_value("deps")

    @deps.setter
    def deps(self, value: float) -> None:
        self._cards[0].set_value("deps", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
        GT.0.0:	Number of integration points which must fail before element is deleted.
        LT.0.0:	Applies only to shells. |NUMFIP| is the percentage of layers which must fail before element fails.
        For shell formulations with 4 integration points per layer, the layer is considered failed if any of the integration points in the layer fails
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        self._cards[0].set_value("numfip", value)

    @property
    def dityp(self) -> float:
        """Get or set the Damage initiation type
        EQ.0.0:	Ductile based on stress triaxiality
        EQ.1.0:	Shear
        EQ.2.0:	MSFLD
        EQ.3.0:	FLD
        EQ.4.0:	Ductile based on normalized principal stress.
        """ # nopep8
        return self._cards[1].get_value("dityp")

    @dityp.setter
    def dityp(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0]:
            raise Exception("""dityp must be one of {0.0,1.0,2.0,3.0,4.0}""")
        self._cards[1].set_value("dityp", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0:	Load curve/table ID representing plastic strain at onset of damage as function of stress triaxiality and optionally plastic strain rate.
        DITYP.EQ.1.0:	Load curve/table ID representing plastic strain at onset of damage as function of shear influence and optionally plastic strain rate.
        DITYP.EQ.2.0:	Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate.
        DITYP.EQ.3.0:	Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate.
        DITYP.EQ.4.0:	Load curve/table ID representing plastic strain at onset of damage as function of stress state parameter and optionally plastic strain rate..
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0:	Not used
        DITYP.EQ.1.0:	Pressure influence parameter k_s
        DITYP.EQ.2.0:	Layer specification
        EQ.0:	Mid layer
        EQ.1:	Outer layer
        DITYP.EQ.3.0:	Layer specification
        EQ.0:	Mid layer
        EQ.1:	Outer layer
        DITYP.EQ.4.0:	Triaxiality influence parameter k_d.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0:	Not used
        DITYP.EQ.1.0:	Not used
        DITYP.EQ.2.0:	Initiation formulation
        EQ.0: Direct
        EQ.1: Incremental
        DITYP.EQ.3.0:	Initiation formulation
        EQ.0:	Direct
        EQ.1:	Incremental
        DITYP.EQ.4.0:	Not used.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Plane stress option for shell elements:
        EQ.0.0:	transverse shear stresses σ_yz and σ_zx are included in the computation of stress invariants, such as the triaxiality.
        EQ.1.0 : transverse shear stresses σ_yz and σ_zx are not included in the computation of stress invariants, such as the triaxiality.Useful in combination with “plane stress” material models, where the transverse shear stresses are also excluded from the yield condition, e.g.,* MAT_024_2D or *MAT_036.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and abscissa value of the criterion used (table).. This factor scales the plastic strain at the onset of damage defined with P1.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[1].set_value("p5", value)

    @property
    def detyp(self) -> float:
        """Get or set the Damage evolution type
        EQ.0.0:	Linear softening, evolution of damage is a function of the plastic displacement after the initiation of damage.
        EQ.1.0:	Linear softening, evolution of damage is a function of the fracture energy after the initiation of damage.
        """ # nopep8
        return self._cards[2].get_value("detyp")

    @detyp.setter
    def detyp(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""detyp must be one of {0.0,1.0}""")
        self._cards[2].set_value("detyp", value)

    @property
    def dctyp(self) -> float:
        """Get or set the Damage composition option for multiple criteria
        EQ.-1.0:	Damage not coupled to stress
        EQ.0.0:	Maximum
        EQ.1.0:	Multiplicative.
        """ # nopep8
        return self._cards[2].get_value("dctyp")

    @dctyp.setter
    def dctyp(self, value: float) -> None:
        if value not in [0.0, -1.0, 1.0]:
            raise Exception("""dctyp must be one of {0.0,-1.0,1.0}""")
        self._cards[2].set_value("dctyp", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter
        DETYP.EQ.0.0:	Plastic displacement at failure,u_f^p, a negative value corresponds to a table ID for u_f^p as a function of triaxiality and damage.
        DETYP.EQ.1.0:	Fracture energy at failure,G_f.
        """ # nopep8
        return self._cards[2].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[2].set_value("q1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Set to 1.0 to output information to log files (messag and d3hsp) when an integration point fails.
        """ # nopep8
        return self._cards[2].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[2].set_value("q2", value)

    @property
    def q3(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter:
        DETYP.EQ.0.0:	Exponent, α, in nonlinear damage evolution law, activated when u_f^ p > 0 and α > 0.
        DETYP.EQ.1.0:	Not used.
        """ # nopep8
        return self._cards[2].get_value("q3")

    @q3.setter
    def q3(self, value: float) -> None:
        self._cards[2].set_value("q3", value)

    @property
    def q4(self) -> typing.Optional[float]:
        """Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and plastic strain rate (table). This factor scales the damage evolution parameter Q1
        """ # nopep8
        return self._cards[2].get_value("q4")

    @q4.setter
    def q4(self, value: float) -> None:
        self._cards[2].set_value("q4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

