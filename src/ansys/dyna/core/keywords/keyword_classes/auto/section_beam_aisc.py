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

class SectionBeamAisc(KeywordBase):
    """DYNA SECTION_BEAM_AISC keyword"""

    keyword = "SECTION"
    subkeyword = "BEAM_AISC"
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
                        "secid",
                        int,
                        0,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "label",
                        str,
                        10,
                        70,
                        kwargs.get("label")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "elform",
                        int,
                        0,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "shrf",
                        float,
                        10,
                        10,
                        kwargs.get("shrf", 0.0)
                    ),
                    Field(
                        "nsm",
                        float,
                        20,
                        10,
                        kwargs.get("nsm", 0.0)
                    ),
                    Field(
                        "lfac",
                        float,
                        30,
                        10,
                        kwargs.get("lfac", 0.0)
                    ),
                    Field(
                        "nsloc",
                        float,
                        40,
                        10,
                        kwargs.get("nsloc", 0.0)
                    ),
                    Field(
                        "ntloc",
                        float,
                        50,
                        10,
                        kwargs.get("ntloc", 0.0)
                    ),
                    Field(
                        "k",
                        int,
                        60,
                        10,
                        kwargs.get("k", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "elform",
                        int,
                        0,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "shrf",
                        float,
                        10,
                        10,
                        kwargs.get("shrf", 0.0)
                    ),
                    Field(
                        "nsm",
                        float,
                        20,
                        10,
                        kwargs.get("nsm", 0.0)
                    ),
                    Field(
                        "lfac",
                        float,
                        30,
                        10,
                        kwargs.get("lfac", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "elform",
                        int,
                        0,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "lfac",
                        float,
                        10,
                        10,
                        kwargs.get("lfac", 0.0)
                    ),
                    Field(
                        "rampt",
                        float,
                        20,
                        10,
                        kwargs.get("rampt", 0.0)
                    ),
                    Field(
                        "stress",
                        float,
                        30,
                        10,
                        kwargs.get("stress", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "elform",
                        int,
                        0,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "shrf",
                        float,
                        10,
                        10,
                        kwargs.get("shrf", 0.0)
                    ),
                    Field(
                        "nsm",
                        float,
                        20,
                        10,
                        kwargs.get("nsm", 0.0)
                    ),
                    Field(
                        "lfac",
                        float,
                        30,
                        10,
                        kwargs.get("lfac", 0.0)
                    ),
                    Field(
                        "k",
                        int,
                        40,
                        10,
                        kwargs.get("k")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionBeamAisc.option_specs[0],
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
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID.  SECID is referenced on the *PART card.  A unique number or label not exceeding 8 characters must be specified.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[0].set_value("secid", value)

    @property
    def label(self) -> typing.Optional[str]:
        """Get or set the AISC section label.
        """ # nopep8
        return self._cards[0].get_value("label")

    @label.setter
    def label(self, value: str) -> None:
        self._cards[0].set_value("label", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[1].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11, 12]:
            raise Exception("""elform must be one of {1,2,3,4,5,11,12}""")
        self._cards[1].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[1].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[1].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[1].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
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
        self._cards[1].set_value("lfac", value)

    @property
    def nsloc(self) -> float:
        """Get or set the Location of reference surface (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("nsloc")

    @nsloc.setter
    def nsloc(self, value: float) -> None:
        self._cards[1].set_value("nsloc", value)

    @property
    def ntloc(self) -> float:
        """Get or set the Location of reference surface (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("ntloc")

    @ntloc.setter
    def ntloc(self, value: float) -> None:
        self._cards[1].set_value("ntloc", value)

    @property
    def k(self) -> int:
        """Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        self._cards[1].set_value("k", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[2].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11, 12]:
            raise Exception("""elform must be one of {1,2,3,4,5,11,12}""")
        self._cards[2].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[2].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[2].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[2].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        self._cards[2].set_value("nsm", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[2].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        self._cards[2].set_value("lfac", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[3].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11, 12]:
            raise Exception("""elform must be one of {1,2,3,4,5,11,12}""")
        self._cards[3].set_value("elform", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[3].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        self._cards[3].set_value("lfac", value)

    @property
    def rampt(self) -> float:
        """Get or set the Optional ramp-up time (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[3].get_value("rampt")

    @rampt.setter
    def rampt(self, value: float) -> None:
        self._cards[3].set_value("rampt", value)

    @property
    def stress(self) -> float:
        """Get or set the Optional initial stress (see *SECTION_BEAM).
        """ # nopep8
        return self._cards[3].get_value("stress")

    @stress.setter
    def stress(self, value: float) -> None:
        self._cards[3].set_value("stress", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation.
        """ # nopep8
        return self._cards[4].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 11, 12]:
            raise Exception("""elform must be one of {1,2,3,4,5,11,12}""")
        self._cards[4].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor.
        """ # nopep8
        return self._cards[4].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[4].set_value("shrf", value)

    @property
    def nsm(self) -> float:
        """Get or set the Non-structural mass per unit length.
        """ # nopep8
        return self._cards[4].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        self._cards[4].set_value("nsm", value)

    @property
    def lfac(self) -> float:
        """Get or set the Length scale factor to convert dimensions from standard units.
        """ # nopep8
        return self._cards[4].get_value("lfac")

    @lfac.setter
    def lfac(self, value: float) -> None:
        self._cards[4].set_value("lfac", value)

    @property
    def k(self) -> typing.Optional[int]:
        """Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
        """ # nopep8
        return self._cards[4].get_value("k")

    @k.setter
    def k(self, value: int) -> None:
        self._cards[4].set_value("k", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

