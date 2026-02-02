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

"""Module providing the SectionAle2D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONALE2D_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("aleform", int, 10, 10, 6),
    FieldSchema("aet", int, 20, 10, None),
    FieldSchema("elform", int, 30, 10, 13),
)

_SECTIONALE2D_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionAle2D(KeywordBase):
    """DYNA SECTION_ALE2D keyword"""

    keyword = "SECTION"
    subkeyword = "ALE2D"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionAle2D class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONALE2D_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionAle2D.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONALE2D_OPTION0_CARD0,
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
    def aleform(self) -> int:
        """Get or set the ALE formulation:
        EQ.6  : Single material Eulerian formulation
        EQ.7  : Single material Ambient Eulerian formulation
        EQ.11: Multi-Material ALE formulation

        """ # nopep8
        return self._cards[0].get_value("aleform")

    @aleform.setter
    def aleform(self, value: int) -> None:
        """Set the aleform property."""
        if value not in [6, 7, 11, None]:
            raise Exception("""aleform must be `None` or one of {6,7,11}.""")
        self._cards[0].set_value("aleform", value)

    @property
    def aet(self) -> typing.Optional[int]:
        """Get or set the Ambient Element Type: can be defined for ALEFORM 7 and 11.
        EQ.4: pressure inflow
        .
        """ # nopep8
        return self._cards[0].get_value("aet")

    @aet.setter
    def aet(self, value: int) -> None:
        """Set the aet property."""
        self._cards[0].set_value("aet", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation:
        EQ.13: plane strain (x-y plane)
        EQ.14: axisymmetric solid (y-axis of symmetry) - area weighted

        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [13, 14, None]:
            raise Exception("""elform must be `None` or one of {13,14}.""")
        self._cards[0].set_value("elform", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

