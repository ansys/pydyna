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

class DualceseModel(KeywordBase):
    """DYNA DUALCESE_MODEL keyword"""

    keyword = "DUALCESE"
    subkeyword = "MODEL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "unitsys",
                        str,
                        0,
                        10,
                        kwargs.get("unitsys")
                    ),
                    Field(
                        "filename",
                        str,
                        10,
                        70,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

    @property
    def unitsys(self) -> typing.Optional[str]:
        """Get or set the Name of the unit system of this dual CESE model (defined with *UNIT_SYSTEM).
        EQ.<BLANK>:	Use same units as the presumed units of the entire problem
        """ # nopep8
        return self._cards[0].get_value("unitsys")

    @unitsys.setter
    def unitsys(self, value: str) -> None:
        self._cards[0].set_value("unitsys", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Filename of the keyword file containing the dual CESE model. Note that only *DUALCESE_... keyword cards are allowed in this file
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

