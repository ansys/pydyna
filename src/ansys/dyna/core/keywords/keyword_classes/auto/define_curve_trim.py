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

"""Module providing the DefineCurveTrim class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineCurveTrim(KeywordBase):
    """DYNA DEFINE_CURVE_TRIM keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_TRIM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCurveTrim class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "tcid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tctype",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tctol",
                        float,
                        40,
                        10,
                        0.25,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cx",
                        float,
                        0,
                        20,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cy",
                        float,
                        20,
                        20,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveTrim.option_specs[0],
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
    def tcid(self) -> typing.Optional[int]:
        """Get or set the ID number for trim curve. A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("tcid")

    @tcid.setter
    def tcid(self, value: int) -> None:
        """Set the tcid property."""
        self._cards[0].set_value("tcid", value)

    @property
    def tctype(self) -> int:
        """Get or set the Trim curve type:
        EQ.1: digitized curve provided,
        EQ.2: IGES trim curve.
        """ # nopep8
        return self._cards[0].get_value("tctype")

    @tctype.setter
    def tctype(self, value: int) -> None:
        """Set the tctype property."""
        if value not in [1, 2, None]:
            raise Exception("""tctype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("tctype", value)

    @property
    def tctol(self) -> float:
        """Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
        """ # nopep8
        return self._cards[0].get_value("tctol")

    @tctol.setter
    def tctol(self, value: float) -> None:
        """Set the tctol property."""
        self._cards[0].set_value("tctol", value)

    @property
    def cx(self) -> float:
        """Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        """Set the cx property."""
        self._cards[1].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        """Set the cy property."""
        self._cards[1].set_value("cy", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
        """ # nopep8
        return self._cards[2].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[2].set_value("filename", value)

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

