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

"""Module providing the SectionTShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONTSHELL_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 1),
    FieldSchema("shrf", float, 20, 10, 1.0),
    FieldSchema("nip", int, 30, 10, 2),
    FieldSchema("propt", float, 40, 10, 1.0),
    FieldSchema("qr", int, 50, 10, 0),
    FieldSchema("icomp", int, 60, 10, 0),
    FieldSchema("tshear", int, 70, 10, 0),
)

_SECTIONTSHELL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionTShell(KeywordBase):
    """DYNA SECTION_TSHELL keyword"""

    keyword = "SECTION"
    subkeyword = "TSHELL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionTShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONTSHELL_CARD0,
                **kwargs,
            ),            SeriesCard(
                "bi",
                8,
                10,
                float,
                lambda: self.nip,
                lambda: self.icomp == 1,
                data = kwargs.get("bi")),            OptionCardSet(
                option_spec = SectionTShell.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONTSHELL_OPTION0_CARD0,
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
        """Get or set the Element formulation:
        EQ.1: one point reduced integration (default),
        EQ.2: selective reduced 2x2 in plane integration.
        EQ.3: assumed strain 2x2 in plane integration.
        EQ.5:  assumed strain reduced integration.
        EQ.6: assumed strain reduced integration with shell materials
        EQ.7:	assumed strain  2×2 in plane integration
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 5, 6, 7, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,5,6,7}.""")
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor (default =1.0). A value of 5/6 is recommended.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[0].set_value("shrf", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through shell thickness integration points (default = 2).
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[0].set_value("nip", value)

    @property
    def propt(self) -> float:
        """Get or set the Printout option:
        EQ.1.0: average resultants and fiber lengths (default),
        EQ.2.0: resultants at plan points and fiber lengths,
        EQ.3.0: resultants, stresses at all points, fiber lengths.
        """ # nopep8
        return self._cards[0].get_value("propt")

    @propt.setter
    def propt(self, value: float) -> None:
        """Set the propt property."""
        if value not in [1.0, 2.0, 3.0, None]:
            raise Exception("""propt must be `None` or one of {1.0,2.0,3.0}.""")
        self._cards[0].set_value("propt", value)

    @property
    def qr(self) -> int:
        """Get or set the Quadrature rule:
        LT.0: absolute value is specified rule number,
        EQ.0: Gauss (up to five points are permitted),
        EQ.1: trapezoidal, not recommended for accuracy reasons.
        """ # nopep8
        return self._cards[0].get_value("qr")

    @qr.setter
    def qr(self, value: int) -> None:
        """Set the qr property."""
        self._cards[0].set_value("qr", value)

    @property
    def icomp(self) -> int:
        """Get or set the Flag for layered composite material mode:
        EQ.0: Flag turned off (default),
        EQ.1: a material angle is defined for each through thickness integration point . For each layer one integration point is used.
        """ # nopep8
        return self._cards[0].get_value("icomp")

    @icomp.setter
    def icomp(self, value: int) -> None:
        """Set the icomp property."""
        if value not in [0, 1, None]:
            raise Exception("""icomp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("icomp", value)

    @property
    def tshear(self) -> int:
        """Get or set the Flag for transverse shear strain or stress distribution (see Remarks 4 and 5):
        EQ.0.0: Parabolic,
        EQ.1.0: Constant through thickness.
        """ # nopep8
        return self._cards[0].get_value("tshear")

    @tshear.setter
    def tshear(self, value: int) -> None:
        """Set the tshear property."""
        if value not in [0, 1, None]:
            raise Exception("""tshear must be `None` or one of {0,1}.""")
        self._cards[0].set_value("tshear", value)

    @property
    def bi(self) -> SeriesCard:
        """dynamic array of beta-i: material angle at ith-integration point.."""
        return self._cards[1]

    @bi.setter
    def bi(self, value: typing.List) -> None:
        self._cards[1].data = value

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

