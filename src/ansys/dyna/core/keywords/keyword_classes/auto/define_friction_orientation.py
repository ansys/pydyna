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

class DefineFrictionOrientation(KeywordBase):
    """DYNA DEFINE_FRICTION_ORIENTATION keyword"""

    keyword = "DEFINE"
    subkeyword = "FRICTION_ORIENTATION"
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
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "lcidp",
                        int,
                        20,
                        10,
                        kwargs.get("lcidp", 0)
                    ),
                    Field(
                        "v1",
                        float,
                        30,
                        10,
                        kwargs.get("v1", 0.0)
                    ),
                    Field(
                        "v2",
                        float,
                        40,
                        10,
                        kwargs.get("v2", 0.0)
                    ),
                    Field(
                        "v3",
                        float,
                        50,
                        10,
                        kwargs.get("v3", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFrictionOrientation.option_specs[0],
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
        """Get or set the Part ID to which directional and pressure-sensitive COF is to be applied. See *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> int:
        """Get or set the ID of the load curve defining COF vs. orientation in degree.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def lcidp(self) -> int:
        """Get or set the ID of the load curve defining COF scale factor vs. pressure.
        """ # nopep8
        return self._cards[0].get_value("lcidp")

    @lcidp.setter
    def lcidp(self, value: int) -> None:
        self._cards[0].set_value("lcidp", value)

    @property
    def v1(self) -> float:
        """Get or set the Vector components of vector V defining zero-degree (rolling) direction.
        """ # nopep8
        return self._cards[0].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[0].set_value("v1", value)

    @property
    def v2(self) -> float:
        """Get or set the Vector components of vector V defining zero-degree (rolling) direction.
        """ # nopep8
        return self._cards[0].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[0].set_value("v2", value)

    @property
    def v3(self) -> float:
        """Get or set the Vector components of vector V defining zero-degree (rolling) direction.
        """ # nopep8
        return self._cards[0].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[0].set_value("v3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

