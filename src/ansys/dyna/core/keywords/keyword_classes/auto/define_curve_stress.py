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

class DefineCurveStress(KeywordBase):
    """DYNA DEFINE_CURVE_STRESS keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_STRESS"
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
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        kwargs.get("itype", 1)
                    ),
                    Field(
                        "p1",
                        float,
                        20,
                        10,
                        kwargs.get("p1", 0.0)
                    ),
                    Field(
                        "p2",
                        float,
                        30,
                        10,
                        kwargs.get("p2", 0.0)
                    ),
                    Field(
                        "p3",
                        float,
                        40,
                        10,
                        kwargs.get("p3", 0.0)
                    ),
                    Field(
                        "p4",
                        float,
                        50,
                        10,
                        kwargs.get("p4", 0.0)
                    ),
                    Field(
                        "p5",
                        float,
                        60,
                        10,
                        kwargs.get("p5", 0.0)
                    ),
                    Field(
                        "p6",
                        float,
                        70,
                        10,
                        kwargs.get("p6", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        kwargs.get("itype", 1)
                    ),
                    Field(
                        "p1",
                        float,
                        20,
                        10,
                        kwargs.get("p1", 0.0)
                    ),
                    Field(
                        "p2",
                        float,
                        30,
                        10,
                        kwargs.get("p2", 0.0)
                    ),
                    Field(
                        "p3",
                        float,
                        40,
                        10,
                        kwargs.get("p3", 0.0)
                    ),
                    Field(
                        "p4",
                        float,
                        50,
                        10,
                        kwargs.get("p4", 0.0)
                    ),
                    Field(
                        "p5",
                        float,
                        60,
                        10,
                        kwargs.get("p5", 0.0)
                    ),
                    Field(
                        "p6",
                        float,
                        70,
                        10,
                        kwargs.get("p6", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveStress.option_specs[0],
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
    def lcid(self) -> int:
        """Get or set the Load curve ID for the stress-strain curve to be generated.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def itype(self) -> int:
        """Get or set the Type of hardening law:
        ITYPE.EQ.1: Swift power law
        ITYPE.EQ.2: Voce law, please take a look at the manual
        ITYPE.EQ.3: Voce law in different forms, please take a look at the manual.
        ITYPE.EQ.4: Voce law, please take a look at the manual.
        ITYPE.EQ.5: Stoughton-Yoon hardening law.
        ITYPE.EQ.11: A weighted combination of ITYPE = 1 and any of the other ITYPEs.
        Check the manual for detail.
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11]:
            raise Exception("""itype must be one of {1,2,3,4,5,11}""")
        self._cards[0].set_value("itype", value)

    @property
    def p1(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[0].set_value("p1", value)

    @property
    def p2(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[0].set_value("p2", value)

    @property
    def p3(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[0].set_value("p3", value)

    @property
    def p4(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[0].set_value("p4", value)

    @property
    def p5(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[0].set_value("p5", value)

    @property
    def p6(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[0].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[0].set_value("p6", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID for the stress-strain curve to be generated.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def itype(self) -> int:
        """Get or set the Type of hardening law:
        ITYPE.EQ.1: Swift power law
        ITYPE.EQ.2: Voce law, please take a look at the manual
        ITYPE.EQ.3: Voce law in different forms, please take a look at the manual.
        ITYPE.EQ.4: Voce law, please take a look at the manual.
        ITYPE.EQ.5: Stoughton-Yoon hardening law.
        ITYPE.EQ.11:	A weighted combination of ITYPE = 1 and any of the other ITYPEs.
        Check the manual for detail.
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11]:
            raise Exception("""itype must be one of {1,2,3,4,5,11}""")
        self._cards[1].set_value("itype", value)

    @property
    def p1(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> float:
        """Get or set the ITYPE.EQ.1:	P1=K,P2=n,P3=e_0.
        ITYPE.EQ.2:	P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
        ITYPE.EQ.3:	P1=A,P2=B,P3=C,
        P4: hardening curve contributing weighting factor.
        ITYPE.EQ.4:	P1=A,P2=B,P3=C,P4=H.
        P5: hardening curve contributing weighting factor.
        ITYPE.EQ.11:	P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        self._cards[1].set_value("p6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

