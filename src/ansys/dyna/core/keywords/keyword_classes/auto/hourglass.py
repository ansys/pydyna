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
from ansys.dyna.core.lib.card_set import CardSet
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class HourglassCardSet(Cards):

    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "hgid",
                        int,
                        0,
                        10,
                        kwargs.get("hgid", 0)
                    ),
                    Field(
                        "ihq",
                        int,
                        10,
                        10,
                        kwargs.get("ihq", 0)
                    ),
                    Field(
                        "qm",
                        float,
                        20,
                        10,
                        kwargs.get("qm", 0.1)
                    ),
                    Field(
                        "ibq",
                        int,
                        30,
                        10,
                        kwargs.get("ibq")
                    ),
                    Field(
                        "q1",
                        float,
                        40,
                        10,
                        kwargs.get("q1", 1.5)
                    ),
                    Field(
                        "q2",
                        float,
                        50,
                        10,
                        kwargs.get("q2", 6.0E-02)
                    ),
                    Field(
                        "qb/vdc",
                        float,
                        60,
                        10,
                        kwargs.get("qb/vdc", 0.1)
                    ),
                    Field(
                        "qw",
                        float,
                        70,
                        10,
                        kwargs.get("qw", 0.1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = HourglassCardSet.option_specs[0],
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
    def hgid(self) -> int:
        """Get or set the Hourglass ID.  A unique number or label must be specified.  This ID is referenced by HGID in the *PART command.
        """ # nopep8
        return self._cards[0].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        self._cards[0].set_value("hgid", value)

    @property
    def ihq(self) -> int:
        """Get or set the Hourglass control type. For solid elements six options are available. For quadrilateral shell and membrane elements the hourglass control is based on the formulation of Belytschko and Tsay, i.e., options 1-3 are identical, and options 4-6 are identical:
        EQ.0: default=1 regardless of IHQ in *control_hourglass,
        EQ.1:standard LS-DYNA viscous form,
        EQ.2:Flanagan-Belytschko viscous form,
        EQ.3: Flanagan-Belytschko viscous form with exact volume integration for solid elements,
        EQ.4:Flanagan-Belytschko stiffness form,
        EQ.5:Flanagan-Belytschko stiffness form with exact volume integration for solid elements,
        EQ:6:Belytschko-Bindeman [1993] assumed strain co-rotational stiffness form for 2D and 3D solid elements only. This form is available for explicit and IMPLICIT solution medhods. Type 6 is mandatory for the implicit options,
        EQ.7:  Linear total strain form of type 6 hourglass control.  This form is available for explicit and implicit solution method (See remark 6 below).
        EQ:8:Applicable to the type 16 fully integrated shell element.
        IHQ=8 EQ.8:Activates the full projection warping stiffness for shell formulations 16 and -16, and is the default for these formulations.  A speed penalty of 25% is common for this option.
        EQ.9:	Puso [2000] enhanced assumed strain stiffness form for 3D hexahedral elements.
        EQ.10:	Cosserat Point Element (CPE) developed by Jabareen and Rubin [2008] and Jabareen et.al. [2013], see *CONTROL_HOURGLASS
        """ # nopep8
        return self._cards[0].get_value("ihq")

    @ihq.setter
    def ihq(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
            raise Exception("""ihq must be one of {0,1,2,3,4,5,6,7,8,9,10}""")
        self._cards[0].set_value("ihq", value)

    @property
    def qm(self) -> float:
        """Get or set the Hourglass coefficient. Values of QM that exceed 0.15 (for IHQ<6) may cause instabilities. Values of QM that exceed .15 (for IHQ<6) may cause instabilities. The recommended default applies to all options. The stiffness forms, however, can stiffen the response especially if deformations are large and therefore should be used with care. For the shell and membrane elements QM is taken as the membrane hourglass coefficient, the bending as QB, and warping as QW. These coefficients can be specified independently, but generally, QM=QB=QW, is adequate. For type 6 solid element hourglass control, QM=1.0 gives an accurate coarse mesh bending stiffness that does not lock in the incompressible limit. For type 6 values such as 0.001-0.01 will avoid an overly stiff response.
        """ # nopep8
        return self._cards[0].get_value("qm")

    @qm.setter
    def qm(self, value: float) -> None:
        self._cards[0].set_value("qm", value)

    @property
    def ibq(self) -> typing.Optional[int]:
        """Get or set the Not used.  Bulk viscosity is always on for solids.  Bulk viscosity for beams and shells can only be turned on using the variable TYPE in *CONTROL_‌BULK_‌VISCOSITY; however, the coefficients can be set using Q1 and Q2 below.
        """ # nopep8
        return self._cards[0].get_value("ibq")

    @ibq.setter
    def ibq(self, value: int) -> None:
        self._cards[0].set_value("ibq", value)

    @property
    def q1(self) -> float:
        """Get or set the Quadratic bulk viscosity coefficient.
        """ # nopep8
        return self._cards[0].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[0].set_value("q1", value)

    @property
    def q2(self) -> float:
        """Get or set the Linear bulk viscosity coefficient.
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[0].set_value("q2", value)

    @property
    def qb_vdc(self) -> float:
        """Get or set the Hourglass coefficient for shell bending. The default is QB=QM.
        """ # nopep8
        return self._cards[0].get_value("qb/vdc")

    @qb_vdc.setter
    def qb_vdc(self, value: float) -> None:
        self._cards[0].set_value("qb/vdc", value)

    @property
    def qw(self) -> float:
        """Get or set the Hourglass coefficient for shell warping. The default is QB=QW.
        """ # nopep8
        return self._cards[0].get_value("qw")

    @qw.setter
    def qw(self, value: float) -> None:
        self._cards[0].set_value("qw", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

    @property
    def parent(self) -> KeywordBase:
        return self._parent


class Hourglass(KeywordBase):
    """DYNA HOURGLASS keyword"""

    keyword = "HOURGLASS"
    subkeyword = "HOURGLASS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        self._cards = [
            CardSet(
                HourglassCardSet,
                option_specs = HourglassCardSet.option_specs,
                **kwargs
            ),
        ]

    @property
    def sets(self) -> typing.List[HourglassCardSet]:
        return self._cards[0].items()

    def add_set(self, **kwargs):
        self._cards[0].add_item(**kwargs)

