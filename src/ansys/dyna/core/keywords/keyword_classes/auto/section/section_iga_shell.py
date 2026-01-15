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

"""Module providing the SectionIgaShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONIGASHELL_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 0),
    FieldSchema("shrf", float, 20, 10, 1.0),
    FieldSchema("nip", int, 30, 10, 2),
    FieldSchema("irl", int, 40, 10, 0),
    FieldSchema("qr_irid", float, 50, 10, 0.0, "qr/irid"),
    FieldSchema("icomp", int, 60, 10, 0),
)

_SECTIONIGASHELL_CARD1 = (
    FieldSchema("t", float, 0, 10, 0.0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("nloc", float, 40, 10, 0.0),
)

_SECTIONIGASHELL_CARD2 = (
    FieldSchema("b1", float, 0, 10, None),
    FieldSchema("b2", float, 10, 10, None),
    FieldSchema("b3", float, 20, 10, None),
    FieldSchema("b4", float, 30, 10, None),
    FieldSchema("b5", float, 40, 10, None),
    FieldSchema("b6", float, 50, 10, None),
    FieldSchema("b7", float, 60, 10, None),
    FieldSchema("b8", float, 70, 10, None),
)

_SECTIONIGASHELL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionIgaShell(KeywordBase):
    """DYNA SECTION_IGA_SHELL keyword"""

    keyword = "SECTION"
    subkeyword = "IGA_SHELL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionIgaShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONIGASHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONIGASHELL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONIGASHELL_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionIgaShell.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONIGASHELL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation
        EQ.0: Reissner - Mindlin with fibers at the control points
        EQ.1 : Kirchhoff - Love with fibers at the control points
        EQ.2 : Kirchhoff - Love with fibers at the integration points
        EQ.3 : Reissner - Mindlin with fibers at the integration points.
        EQ.5:	Thick shell with thickness stretch based on the ELFORM = 0. See Remark 1.
        EQ.6:	Thick shell with thickness stretch based on ELFORM = 3. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [0, 1, 2, 3, 5, 6, None]:
            raise Exception("""elform must be `None` or one of {0,1,2,3,5,6}.""")
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear correction factor which scales the transverse shear stress, see Remark 1.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[0].set_value("shrf", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through thickness integration points, see Remark 2.
        GT.0.0: Number of quadrature points(up to 10 points).
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[0].set_value("nip", value)

    @property
    def irl(self) -> int:
        """Get or set the Lamina integration rule
        EQ.0: Reduced Gauss - Legendre
        EQ.1 : Gauss - Legendre
        EQ.2 : Patchwise reduced Gauss - Legendre(for biquadratic NURBS only).
        """ # nopep8
        return self._cards[0].get_value("irl")

    @irl.setter
    def irl(self, value: int) -> None:
        """Set the irl property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""irl must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("irl", value)

    @property
    def qr_irid(self) -> float:
        """Get or set the Fiber quadrature rule or fiber integration rule ID, see *INTEGRATION_SHELL.
        LT.0.0: Absolute value is specified rule number.
        EQ.0.0 : Gauss - Legendre / Gauss - Lobatto(up to 10 points)
        EQ.1.0 : Trapezoidal, not recommended for accuracy reasons.
        """ # nopep8
        return self._cards[0].get_value("qr_irid")

    @qr_irid.setter
    def qr_irid(self, value: float) -> None:
        """Set the qr_irid property."""
        self._cards[0].set_value("qr_irid", value)

    @property
    def icomp(self) -> int:
        """Get or set the Flag for anisotropic layered composite material model, see Remark 3.
        EQ.1: A material angle in degrees is defined for each through
        thickness integration point.Thus, each layer has one integration point.
        """ # nopep8
        return self._cards[0].get_value("icomp")

    @icomp.setter
    def icomp(self, value: int) -> None:
        """Set the icomp property."""
        self._cards[0].set_value("icomp", value)

    @property
    def t(self) -> float:
        """Get or set the Shell thickness.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def nloc(self) -> float:
        """Get or set the Location of reference surface, see Remark 4.
        """ # nopep8
        return self._cards[1].get_value("nloc")

    @nloc.setter
    def nloc(self, value: float) -> None:
        """Set the nloc property."""
        self._cards[1].set_value("nloc", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[2].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[2].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        """Set the b3 property."""
        self._cards[2].set_value("b3", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        """Set the b4 property."""
        self._cards[2].set_value("b4", value)

    @property
    def b5(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b5")

    @b5.setter
    def b5(self, value: float) -> None:
        """Set the b5 property."""
        self._cards[2].set_value("b5", value)

    @property
    def b6(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b6")

    @b6.setter
    def b6(self, value: float) -> None:
        """Set the b6 property."""
        self._cards[2].set_value("b6", value)

    @property
    def b7(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b7")

    @b7.setter
    def b7(self, value: float) -> None:
        """Set the b7 property."""
        self._cards[2].set_value("b7", value)

    @property
    def b8(self) -> typing.Optional[float]:
        """Get or set the Material angle at the ith fiber integration point.
        """ # nopep8
        return self._cards[2].get_value("b8")

    @b8.setter
    def b8(self, value: float) -> None:
        """Set the b8 property."""
        self._cards[2].set_value("b8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

