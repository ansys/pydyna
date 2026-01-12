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

"""Module providing the DefineCurveStress class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECURVESTRESS_CARD0 = (
    FieldSchema("lcid", int, 0, 10, 0),
    FieldSchema("itype", int, 10, 10, 1),
    FieldSchema("p1", float, 20, 10, 0.0),
    FieldSchema("p2", float, 30, 10, 0.0),
    FieldSchema("p3", float, 40, 10, 0.0),
    FieldSchema("p4", float, 50, 10, 0.0),
    FieldSchema("p5", float, 60, 10, 0.0),
    FieldSchema("p6", float, 70, 10, 0.0),
)

_DEFINECURVESTRESS_CARD1 = (
    FieldSchema("lcid", int, 0, 10, 0),
    FieldSchema("itype", int, 10, 10, 1),
    FieldSchema("p1", float, 20, 10, 0.0),
    FieldSchema("p2", float, 30, 10, 0.0),
    FieldSchema("p3", float, 40, 10, 0.0),
    FieldSchema("p4", float, 50, 10, 0.0),
    FieldSchema("p5", float, 60, 10, 0.0),
    FieldSchema("p6", float, 70, 10, 0.0),
)

class DefineCurveStress(KeywordBase):
    """DYNA DEFINE_CURVE_STRESS keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_STRESS"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCurveStress class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECURVESTRESS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVESTRESS_CARD1,
                **kwargs,
            ),            OptionCardSet(
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
        """Set the lcid property."""
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
        """Set the itype property."""
        if value not in [1, 2, 3, 4, 5, 11, None]:
            raise Exception("""itype must be `None` or one of {1,2,3,4,5,11}.""")
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
        """Set the p1 property."""
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
        """Set the p2 property."""
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
        """Set the p3 property."""
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
        """Set the p4 property."""
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
        """Set the p5 property."""
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
        """Set the p6 property."""
        self._cards[0].set_value("p6", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID for the stress-strain curve to be generated.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
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
        """Set the itype property."""
        if value not in [1, 2, 3, 4, 5, 11, None]:
            raise Exception("""itype must be `None` or one of {1,2,3,4,5,11}.""")
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
        """Set the p1 property."""
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
        """Set the p2 property."""
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
        """Set the p3 property."""
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
        """Set the p4 property."""
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
        """Set the p5 property."""
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
        """Set the p6 property."""
        self._cards[1].set_value("p6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

