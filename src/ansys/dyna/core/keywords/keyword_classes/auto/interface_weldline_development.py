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

"""Module providing the InterfaceWeldlineDevelopment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceWeldlineDevelopment(KeywordBase):
    """DYNA INTERFACE_WELDLINE_DEVELOPMENT keyword"""

    keyword = "INTERFACE"
    subkeyword = "WELDLINE_DEVELOPMENT"

    def __init__(self, **kwargs):
        """Initialize the InterfaceWeldlineDevelopment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ioption",
                        int,
                        0,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def ioption(self) -> int:
        """Get or set the Welding curve development options:
        EQ.1:	Calculate initial weld curve from final(given) weld curve, with output file name weldline.ibo, which will be on the initial blank mesh.
        EQ. - 1 : Calculate final weld curve from initial weld curve, with output file name weldline_f.ibo, which will be on the formed blank mesh.
        """ # nopep8
        return self._cards[0].get_value("ioption")

    @ioption.setter
    def ioption(self, value: int) -> None:
        """Set the ioption property."""
        if value not in [1, -1, None]:
            raise Exception("""ioption must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("ioption", value)

