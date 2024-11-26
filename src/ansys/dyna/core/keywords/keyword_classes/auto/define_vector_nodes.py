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

class DefineVectorNodes(KeywordBase):
    """DYNA DEFINE_VECTOR_NODES keyword"""

    keyword = "DEFINE"
    subkeyword = "VECTOR_NODES"
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
                        "vid",
                        int,
                        0,
                        10,
                        kwargs.get("vid", 0)
                    ),
                    Field(
                        "nodet",
                        int,
                        10,
                        10,
                        kwargs.get("nodet", 0)
                    ),
                    Field(
                        "nodeh",
                        int,
                        20,
                        10,
                        kwargs.get("nodeh", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineVectorNodes.option_specs[0],
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
    def vid(self) -> int:
        """Get or set the Vector ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def nodet(self) -> int:
        """Get or set the Nodal point to define tail of vector.
        """ # nopep8
        return self._cards[0].get_value("nodet")

    @nodet.setter
    def nodet(self, value: int) -> None:
        self._cards[0].set_value("nodet", value)

    @property
    def nodeh(self) -> int:
        """Get or set the Nodal point to define head of vector.
        """ # nopep8
        return self._cards[0].get_value("nodeh")

    @nodeh.setter
    def nodeh(self, value: int) -> None:
        self._cards[0].set_value("nodeh", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

