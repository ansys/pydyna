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

class DefineCoordinateNodes(KeywordBase):
    """DYNA DEFINE_COORDINATE_NODES keyword"""

    keyword = "DEFINE"
    subkeyword = "COORDINATE_NODES"
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
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "n1",
                        int,
                        10,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        20,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        30,
                        10,
                        kwargs.get("n3")
                    ),
                    Field(
                        "flag",
                        int,
                        40,
                        10,
                        kwargs.get("flag")
                    ),
                    Field(
                        "dir",
                        str,
                        50,
                        10,
                        kwargs.get("dir", "X")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCoordinateNodes.option_specs[0],
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
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID. A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the ID of node located at local origin.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the ID of node located along local x-axis if DIR=X, the y-axis if DIR=Y, and along the z-axis if DIR=Z
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the ID of node located in local xy plane if DIR=X, the local yz plane if DIR=Y, and the local zx plane if DIR=Z
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def flag(self) -> typing.Optional[int]:
        """Get or set the Set to unity, 1, if the local system is to be updated each time step for the BOUNDARY_SPC nodal constraints and ELEMENT_BEAM type 6, the discrete beam element. Generally, this option when used with nodal SPC's is not recommended since it can cause excursions in the energy balance because the constraint forces at the node may go through a displacement if the node is partially constrained
        """ # nopep8
        return self._cards[0].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        self._cards[0].set_value("flag", value)

    @property
    def dir(self) -> str:
        """Get or set the Axis defined by node N2 moving from the origin node N1. The default direction is the x-axis.
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: str) -> None:
        if value not in ["X", "Y", "Z"]:
            raise Exception("""dir must be one of {"X","Y","Z"}""")
        self._cards[0].set_value("dir", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

