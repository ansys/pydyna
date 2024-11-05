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

class DefinePartFromLayer(KeywordBase):
    """DYNA DEFINE_PART_FROM_LAYER keyword"""

    keyword = "DEFINE"
    subkeyword = "PART_FROM_LAYER"
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
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "layer",
                        int,
                        10,
                        10,
                        kwargs.get("layer")
                    ),
                    Field(
                        "pidsrc",
                        int,
                        20,
                        10,
                        kwargs.get("pidsrc")
                    ),
                    Field(
                        "layold",
                        int,
                        30,
                        10,
                        kwargs.get("layold")
                    ),
                    Field(
                        "mid",
                        int,
                        40,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "thick",
                        float,
                        50,
                        10,
                        kwargs.get("thick")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefinePartFromLayer.option_specs[0],
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
    def pid(self) -> typing.Optional[int]:
        """Get or set the The part ID to be created
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def layer(self) -> typing.Optional[int]:
        """Get or set the The layer ID of the PID,
        """ # nopep8
        return self._cards[0].get_value("layer")

    @layer.setter
    def layer(self, value: int) -> None:
        self._cards[0].set_value("layer", value)

    @property
    def pidsrc(self) -> typing.Optional[int]:
        """Get or set the The part ID of the existing blank to be copied from
        """ # nopep8
        return self._cards[0].get_value("pidsrc")

    @pidsrc.setter
    def pidsrc(self, value: int) -> None:
        self._cards[0].set_value("pidsrc", value)

    @property
    def layold(self) -> typing.Optional[int]:
        """Get or set the The layer ID of the existing blank
        """ # nopep8
        return self._cards[0].get_value("layold")

    @layold.setter
    def layold(self, value: int) -> None:
        self._cards[0].set_value("layold", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the The material ID of the PID
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the The thickness of the PID
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

