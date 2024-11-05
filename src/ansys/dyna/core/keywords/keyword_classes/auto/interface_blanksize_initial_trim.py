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

class InterfaceBlanksizeInitialTrim(KeywordBase):
    """DYNA INTERFACE_BLANKSIZE_INITIAL_TRIM keyword"""

    keyword = "INTERFACE"
    subkeyword = "BLANKSIZE_INITIAL_TRIM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename5",
                        str,
                        0,
                        80,
                        kwargs.get("filename5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename6",
                        str,
                        0,
                        80,
                        kwargs.get("filename6")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename7",
                        str,
                        0,
                        80,
                        kwargs.get("filename7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename8",
                        str,
                        0,
                        80,
                        kwargs.get("filename8")
                    ),
                ],
            ),
        ]

    @property
    def filename5(self) -> typing.Optional[str]:
        """Get or set the The following file names, FILENAME4~7 are for the option INITIAL_TRIM:
        OP10 adapted initial flat blank in keyword format. For now, this can be extracted
        from adapt.msh  file. Typically, OP10 can be a draw forming operation..
        """ # nopep8
        return self._cards[0].get_value("filename5")

    @filename5.setter
    def filename5(self, value: str) -> None:
        self._cards[0].set_value("filename5", value)

    @property
    def filename6(self) -> typing.Optional[str]:
        """Get or set the OP10 final blank in keyword format. �Dynain file from this OP10 simulation can be used.
        """ # nopep8
        return self._cards[1].get_value("filename6")

    @filename6.setter
    def filename6(self, value: str) -> None:
        self._cards[1].set_value("filename6", value)

    @property
    def filename7(self) -> typing.Optional[str]:
        """Get or set the OP20 blank in keyword format. �Dynain file from this OP20 simulation can be used. Typically, OP20 is a trimming operation.
        """ # nopep8
        return self._cards[2].get_value("filename7")

    @filename7.setter
    def filename7(self, value: str) -> None:
        self._cards[2].set_value("filename7", value)

    @property
    def filename8(self) -> typing.Optional[str]:
        """Get or set the OP20 flat blank (calculated) file name. For example, it can be "op20_flat_new".
        """ # nopep8
        return self._cards[3].get_value("filename8")

    @filename8.setter
    def filename8(self, value: str) -> None:
        self._cards[3].set_value("filename8", value)

