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

"""Module providing the SectionBeamAisc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONBEAMAISC_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("label", str, 10, 70, None),
)

_SECTIONBEAMAISC_CARD1 = (
    FieldSchema("elform", int, 0, 10, 1),
    FieldSchema("shrf", float, 10, 10, 0.0),
    FieldSchema("nsm", float, 20, 10, 0.0),
    FieldSchema("lfac", float, 30, 10, 0.0),
    FieldSchema("nsloc", float, 40, 10, 0.0),
    FieldSchema("ntloc", float, 50, 10, 0.0),
    FieldSchema("k", int, 60, 10, 0),
)

_SECTIONBEAMAISC_CARD2 = (
    FieldSchema("elform", int, 0, 10, 1),
    FieldSchema("shrf", float, 10, 10, 0.0),
    FieldSchema("nsm", float, 20, 10, 0.0),
    FieldSchema("lfac", float, 30, 10, 0.0),
)

_SECTIONBEAMAISC_CARD3 = (
    FieldSchema("elform", int, 0, 10, 1),
    FieldSchema("lfac", float, 10, 10, 0.0),
    FieldSchema("rampt", float, 20, 10, 0.0),
    FieldSchema("stress", float, 30, 10, 0.0),
)

_SECTIONBEAMAISC_CARD4 = (
    FieldSchema("elform", int, 0, 10, 1),
    FieldSchema("shrf", float, 10, 10, 0.0),
    FieldSchema("nsm", float, 20, 10, 0.0),
    FieldSchema("lfac", float, 30, 10, 0.0),
    FieldSchema("k", int, 40, 10, None),
)

_SECTIONBEAMAISC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionBeamAisc(KeywordBase):
    """DYNA SECTION_BEAM_AISC keyword"""

    keyword = "SECTION"
    subkeyword = "BEAM_AISC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionBeamAisc class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONBEAMAISC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAMAISC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAMAISC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAMAISC_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAMAISC_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionBeamAisc.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONBEAMAISC_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID.  SECID is referenced on the *PART card.  A unique number or label not exceeding 8 characters must be specified.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def label(self) -> typing.Optional[str]:
        """Get or set the AISC section label.
        """ # nopep8
        return self._cards[0].get_value("label")

    @label.setter
    def label(self, value: str) -> None:
        """Set the label property."""
        self._cards[0].set_value("label", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[1].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 4, 5, 11, 12, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,4,5,11,12}.""")
        self._cards[1].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[1].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[1].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[1].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        """Set the nsm property."""
        self._cards[1].set_value("nsm", value)

    @property
    def lfac(self) -> float:
        """Get or set the GT.0.0: Length scale factor to convert dimensions from standard units
        If LFAC < 0, then a predefined length factor for specific model units is used:
        EQ.-1.0: ft
        EQ.-2.0: m
        EQ.-3.0: in
        EQ.-4.0: mm
        EQ.-5.0: cm.
        """ # nopep8
        return self._cards[1].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        """Set the lfac property."""
        self._cards[1].set_value("lfac", value)

    @property
    def nsloc(self) -> float:
        """Get or set the Location of reference surface (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("nsloc")

    @nsloc.setter
    def nsloc(self, value: float) -> None:
        """Set the nsloc property."""
        self._cards[1].set_value("nsloc", value)

    @property
    def ntloc(self) -> float:
        """Get or set the Location of reference surface (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("ntloc")

    @ntloc.setter
    def ntloc(self, value: float) -> None:
        """Set the ntloc property."""
        self._cards[1].set_value("ntloc", value)

    @property
    def k(self) -> int:
        """Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[2].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 4, 5, 11, 12, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,4,5,11,12}.""")
        self._cards[2].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[2].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[2].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[2].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        """Set the nsm property."""
        self._cards[2].set_value("nsm", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[2].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        """Set the lfac property."""
        self._cards[2].set_value("lfac", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[3].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 4, 5, 11, 12, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,4,5,11,12}.""")
        self._cards[3].set_value("elform", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[3].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        """Set the lfac property."""
        self._cards[3].set_value("lfac", value)

    @property
    def rampt(self) -> float:
        """Get or set the Optional ramp-up time (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[3].get_value("rampt")

    @rampt.setter
    def rampt(self, value: float) -> None:
        """Set the rampt property."""
        self._cards[3].set_value("rampt", value)

    @property
    def stress(self) -> float:
        """Get or set the Optional initial stress (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[3].get_value("stress")

    @stress.setter
    def stress(self, value: float) -> None:
        """Set the stress property."""
        self._cards[3].set_value("stress", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[4].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 4, 5, 11, 12, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,4,5,11,12}.""")
        self._cards[4].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[4].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[4].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[4].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        """Set the nsm property."""
        self._cards[4].set_value("nsm", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[4].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        """Set the lfac property."""
        self._cards[4].set_value("lfac", value)

    @property
    def k(self) -> typing.Optional[int]:
        """Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
        """ # nopep8
        return self._cards[4].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        """Set the k property."""
        self._cards[4].set_value("k", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

