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

"""Module providing the DefineDeMassflowPlane class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineDeMassflowPlane(KeywordBase):
    """DYNA DEFINE_DE_MASSFLOW_PLANE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_MASSFLOW_PLANE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeMassflowPlane class."""
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "surfsid",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ptype",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "stype",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeMassflowPlane.option_specs[0],
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
        """Get or set the Node set ID, node ID, part set ID or part ID specifying DES to be measured.  PTYPE below indicates the ID type specified by PRTCLSID
        """ # nopep8
        return self._cards[0].get_value("prtclsid")

    @prtclsid.setter
    def prtclsid(self, value: int) -> None:
        """Set the prtclsid property."""
        self._cards[0].set_value("prtclsid", value)

    @property
    def surfsid(self) -> int:
        """Get or set the Part set ID or part ID defining the surface.  STYPE below indicates the ID type specified by SURFSID
        """ # nopep8
        return self._cards[0].get_value("surfsid")

    @surfsid.setter
    def surfsid(self, value: int) -> None:
        """Set the surfsid property."""
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
        """Set the ptype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ptype must be `None` or one of {0,1,2,3}.""")
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
        """Set the stype property."""
        if value not in [0, 1, None]:
            raise Exception("""stype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

