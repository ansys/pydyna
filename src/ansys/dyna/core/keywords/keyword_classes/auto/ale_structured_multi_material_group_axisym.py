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

"""Module providing the AleStructuredMultiMaterialGroupAxisym class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class AleStructuredMultiMaterialGroupAxisym(KeywordBase):
    """DYNA ALE_STRUCTURED_MULTI-MATERIAL_GROUP_AXISYM keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MULTI-MATERIAL_GROUP_AXISYM"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMultiMaterialGroupAxisym class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ammg_name",
                        str,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eosid",
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
                        "unused",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pref",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def ammg_name(self) -> typing.Optional[str]:
        """Get or set the AMMG name. Required to identify the AMMG (S-ALE fluid); Not case sensitive and need to be unique; See remark 2.
        """ # nopep8
        return self._cards[0].get_value("ammg_name")

    @ammg_name.setter
    def ammg_name(self, value: str) -> None:
        """Set the ammg_name property."""
        self._cards[0].set_value("ammg_name", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation-of-state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def pref(self) -> float:
        """Get or set the Defines reference pressure of this AMMG; See remark 3
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        """Set the pref property."""
        self._cards[0].set_value("pref", value)

