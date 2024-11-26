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

class DefineCurveFeedback(KeywordBase):
    """DYNA DEFINE_CURVE_FEEDBACK keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_FEEDBACK"
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
                        kwargs.get("lcid")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "fldid",
                        int,
                        30,
                        10,
                        kwargs.get("fldid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fsl",
                        float,
                        0,
                        10,
                        kwargs.get("fsl")
                    ),
                    Field(
                        "tsl",
                        float,
                        10,
                        10,
                        kwargs.get("tsl")
                    ),
                    Field(
                        "sff",
                        float,
                        20,
                        10,
                        kwargs.get("sff", 1.0)
                    ),
                    Field(
                        "sft",
                        float,
                        30,
                        10,
                        kwargs.get("sft", 1.0)
                    ),
                    Field(
                        "bias",
                        float,
                        40,
                        10,
                        kwargs.get("bias", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveFeedback.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the ID number for load curve to be scaled.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Active part ID for load curve control
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Box ID. Elements of specified part ID contained in box are checked. If the box ID is set to zero then all elements of the active part are checked.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def fldid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines the flow limit. If the product of FSL and the ordinate value of the maximum principal strain is exceeded the scale factor for flow, SF, is active.
        """ # nopep8
        return self._cards[0].get_value("fldid")

    @fldid.setter
    def fldid(self, value: int) -> None:
        self._cards[0].set_value("fldid", value)

    @property
    def fsl(self) -> typing.Optional[float]:
        """Get or set the If the strain ratio, epsilon-major-workpiece to epsilon-major-fld, is exceeded the scale factor for flow, SF, is active.
        """ # nopep8
        return self._cards[1].get_value("fsl")

    @fsl.setter
    def fsl(self, value: float) -> None:
        self._cards[1].set_value("fsl", value)

    @property
    def tsl(self) -> typing.Optional[float]:
        """Get or set the Thickness strain limit. If the through thickness strain is exceeded the scale factor for thickening, ST, is active.
        """ # nopep8
        return self._cards[1].get_value("tsl")

    @tsl.setter
    def tsl(self, value: float) -> None:
        self._cards[1].set_value("tsl", value)

    @property
    def sff(self) -> float:
        """Get or set the Scale factor for the flow limit diagram, SF (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("sff")

    @sff.setter
    def sff(self, value: float) -> None:
        self._cards[1].set_value("sff", value)

    @property
    def sft(self) -> float:
        """Get or set the Scale factor for thickening, ST (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        self._cards[1].set_value("sft", value)

    @property
    def bias(self) -> float:
        """Get or set the Bias for combined flow and thickening, S, -1.0 <= S <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("bias")

    @bias.setter
    def bias(self, value: float) -> None:
        self._cards[1].set_value("bias", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

