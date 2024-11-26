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

class DefineSphMassflowPlane(KeywordBase):
    """DYNA DEFINE_SPH_MASSFLOW_PLANE keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_MASSFLOW_PLANE"
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
                        "prtclsid",
                        int,
                        0,
                        10,
                        kwargs.get("prtclsid", 0)
                    ),
                    Field(
                        "surfsid",
                        int,
                        10,
                        10,
                        kwargs.get("surfsid", 0)
                    ),
                    Field(
                        "ptype",
                        int,
                        20,
                        10,
                        kwargs.get("ptype", 0)
                    ),
                    Field(
                        "stype",
                        int,
                        30,
                        10,
                        kwargs.get("stype", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphMassflowPlane.option_specs[0],
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
    def prtclsid(self) -> int:
        """Get or set the Node set ID, node ID, part set ID or part ID specifying SPH particles to be measured.  PTYPE below indicates the ID type specified by PRTCLSID
        """ # nopep8
        return self._cards[0].get_value("prtclsid")

    @prtclsid.setter
    def prtclsid(self, value: int) -> None:
        self._cards[0].set_value("prtclsid", value)

    @property
    def surfsid(self) -> int:
        """Get or set the Part set ID or part ID defining the surface across which the flow rate is measured.  STYPE below indicates the ID type specified by SURFSID
        """ # nopep8
        return self._cards[0].get_value("surfsid")

    @surfsid.setter
    def surfsid(self, value: int) -> None:
        self._cards[0].set_value("surfsid", value)

    @property
    def ptype(self) -> int:
        """Get or set the PRTCLSID type:
        EQ.0:	Node set
        EQ.1 : Node
        EQ.2 : Part set
        EQ.3 : Part
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""ptype must be one of {0,1,2,3}""")
        self._cards[0].set_value("ptype", value)

    @property
    def stype(self) -> int:
        """Get or set the SURFSID type:
        EQ.0:	Part set
        EQ.1 : Part
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""stype must be one of {0,1}""")
        self._cards[0].set_value("stype", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

