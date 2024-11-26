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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceBlanksizeInitialAdaptive(KeywordBase):
    """DYNA INTERFACE_BLANKSIZE_INITIAL_ADAPTIVE keyword"""

    keyword = "INTERFACE"
    subkeyword = "BLANKSIZE_INITIAL_ADAPTIVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename9",
                        str,
                        0,
                        80,
                        kwargs.get("filename9")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename10",
                        str,
                        0,
                        80,
                        kwargs.get("filename10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename11",
                        str,
                        0,
                        80,
                        kwargs.get("filename11")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename12",
                        str,
                        0,
                        80,
                        kwargs.get("filename12")
                    ),
                ],
            ),
        ]

    @property
    def filename9(self) -> typing.Optional[str]:
        """Get or set the The following file names, FILENAME8~11 are for the option INITIAL_ADAPTIVE:
        Same file name as FILENAME7, calculated.
        """ # nopep8
        return self._cards[0].get_value("filename9")

    @filename9.setter
    def filename9(self, value: str) -> None:
        self._cards[0].set_value("filename9", value)

    @property
    def filename10(self) -> typing.Optional[str]:
        """Get or set the OP30 initial blank file name, from last state of OP30 D3PLOTS with adaptive
        constraints, or otherwise the same as OP20 dynain. To get the file from
        D3PLOTS, access POST/OUTPUT/Dynain ASCII and Exclude strain and
        stress in LS-PrePost4.0. Typically, OP30 will be a flanging operation.
        """ # nopep8
        return self._cards[1].get_value("filename10")

    @filename10.setter
    def filename10(self, value: str) -> None:
        self._cards[1].set_value("filename10", value)

    @property
    def filename11(self) -> typing.Optional[str]:
        """Get or set the OP30 adapted initial blank file name. For now, this can be extracted from "adapt.msh" file of OP30 simulation.
        """ # nopep8
        return self._cards[2].get_value("filename11")

    @filename11.setter
    def filename11(self, value: str) -> None:
        self._cards[2].set_value("filename11", value)

    @property
    def filename12(self) -> typing.Optional[str]:
        """Get or set the OP30 flat blank (calculated) file name. For example, it can be "op30_flat_new".
        """ # nopep8
        return self._cards[3].get_value("filename12")

    @filename12.setter
    def filename12(self, value: str) -> None:
        self._cards[3].set_value("filename12", value)

