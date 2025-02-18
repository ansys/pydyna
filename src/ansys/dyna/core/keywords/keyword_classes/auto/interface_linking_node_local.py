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

class InterfaceLinkingNodeLocal(KeywordBase):
    """DYNA INTERFACE_LINKING_NODE_LOCAL keyword"""

    keyword = "INTERFACE"
    subkeyword = "LINKING_NODE_LOCAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ifid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fx",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fy",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fz",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lnid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "usec",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "usen",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID to be moved by interface file, see *NODE.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def ifid(self) -> typing.Optional[int]:
        """Get or set the Interface ID in interface file.
        """ # nopep8
        return self._cards[0].get_value("ifid")

    @ifid.setter
    def ifid(self, value: int) -> None:
        self._cards[0].set_value("ifid", value)

    @property
    def fx(self) -> typing.Optional[int]:
        """Get or set the The ID of a *DEFINE_FUNCTION which determines the x direction displacement scale factor. See Remarks.
        """ # nopep8
        return self._cards[0].get_value("fx")

    @fx.setter
    def fx(self, value: int) -> None:
        self._cards[0].set_value("fx", value)

    @property
    def fy(self) -> typing.Optional[int]:
        """Get or set the The ID of a *DEFINE_FUNCTION which determines the y direction displacement scale factor. See Remarks.
        """ # nopep8
        return self._cards[0].get_value("fy")

    @fy.setter
    def fy(self, value: int) -> None:
        self._cards[0].set_value("fy", value)

    @property
    def fz(self) -> typing.Optional[int]:
        """Get or set the The ID of a *DEFINE_FUNCTION which determines the z direction displacement scale factor. See Remarks.
        """ # nopep8
        return self._cards[0].get_value("fz")

    @fz.setter
    def fz(self, value: int) -> None:
        self._cards[0].set_value("fz", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID for transforming displacements.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def lnid(self) -> typing.Optional[int]:
        """Get or set the Local node ID for transforming displacements.
        """ # nopep8
        return self._cards[1].get_value("lnid")

    @lnid.setter
    def lnid(self, value: int) -> None:
        self._cards[1].set_value("lnid", value)

    @property
    def usec(self) -> int:
        """Get or set the Flag to indicate the use of the coordinate system in the linking file
        during displacement transformation. See Remarks.
        EQ.0: Linking file coordinate system is ignored.
        EQ.1: Linking file coordinate system is used.
        """ # nopep8
        return self._cards[1].get_value("usec")

    @usec.setter
    def usec(self, value: int) -> None:
        if value not in [0, 1, None]:
            raise Exception("""usec must be `None` or one of {0,1}""")
        self._cards[1].set_value("usec", value)

    @property
    def usen(self) -> int:
        """Get or set the Flag to indicate the use of the node displacement in the linking
        file during displacement transformation. See Remarks.
        EQ.0: Node displacement is not used.
        EQ.1: Node displacement is used.
        """ # nopep8
        return self._cards[1].get_value("usen")

    @usen.setter
    def usen(self, value: int) -> None:
        if value not in [0, 1, None]:
            raise Exception("""usen must be `None` or one of {0,1}""")
        self._cards[1].set_value("usen", value)

