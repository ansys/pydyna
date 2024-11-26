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

class DefineFormingContact(KeywordBase):
    """DYNA DEFINE_FORMING_CONTACT keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_CONTACT"
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
                        "ips",
                        int,
                        0,
                        10,
                        kwargs.get("ips")
                    ),
                    Field(
                        "ipm",
                        int,
                        10,
                        10,
                        kwargs.get("ipm")
                    ),
                    Field(
                        "fs",
                        float,
                        20,
                        10,
                        kwargs.get("fs")
                    ),
                    Field(
                        "oneway",
                        int,
                        30,
                        10,
                        kwargs.get("oneway", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFormingContact.option_specs[0],
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
    def ips(self) -> typing.Optional[int]:
        """Get or set the Part ID of a slave sliding member, typically a deformable sheet metal blank.
        """ # nopep8
        return self._cards[0].get_value("ips")

    @ips.setter
    def ips(self, value: int) -> None:
        self._cards[0].set_value("ips", value)

    @property
    def ipm(self) -> typing.Optional[int]:
        """Get or set the Part ID of a master sliding member, typically a tool or die defined as a rigid body.
        """ # nopep8
        return self._cards[0].get_value("ipm")

    @ipm.setter
    def ipm(self, value: int) -> None:
        self._cards[0].set_value("ipm", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Coulomb friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[0].set_value("fs", value)

    @property
    def oneway(self) -> int:
        """Get or set the Define FORMING contact type:
        EQ.0:	The contact is FORMING_ONE_WAY_SURFACE_TO_ SURFACE.
        EQ.1:	The contact is FORMING_ SURFACE_TO_ SURFACE.
        """ # nopep8
        return self._cards[0].get_value("oneway")

    @oneway.setter
    def oneway(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""oneway must be one of {0,1}""")
        self._cards[0].set_value("oneway", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

