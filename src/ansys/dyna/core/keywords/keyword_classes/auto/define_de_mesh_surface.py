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

class DefineDeMeshSurface(KeywordBase):
    """DYNA DEFINE_DE_MESH_SURFACE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_MESH_SURFACE"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid", 0)
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "nquad",
                        int,
                        20,
                        10,
                        kwargs.get("nquad", 1)
                    ),
                    Field(
                        "despid",
                        int,
                        30,
                        10,
                        kwargs.get("despid", 0)
                    ),
                    Field(
                        "descid",
                        int,
                        40,
                        10,
                        kwargs.get("descid", 0)
                    ),
                    Field(
                        "nsid",
                        int,
                        50,
                        10,
                        kwargs.get("nsid", 0)
                    ),
                    Field(
                        "rsf",
                        float,
                        60,
                        10,
                        kwargs.get("rsf", 1.0)
                    ),
                    Field(
                        "iactive",
                        int,
                        70,
                        10,
                        kwargs.get("iactive", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeMeshSurface.option_specs[0],
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
    def sid(self) -> int:
        """Get or set the Part or part set ID for the region of the mesh upon which the DES elements will be placed
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def type(self) -> int:
        """Get or set the EQ.0:	Part set
        EQ.1:	Part
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def nquad(self) -> int:
        """Get or set the Number of equally spaced DES elements created on a shell element in each local shell direction. For instance, NQUAD × NQUAD DES elements will be created on the surface a quad shell.
        """ # nopep8
        return self._cards[0].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[0].set_value("nquad", value)

    @property
    def despid(self) -> int:
        """Get or set the Part ID for generated DES elements
        """ # nopep8
        return self._cards[0].get_value("despid")

    @despid.setter
    def despid(self, value: int) -> None:
        self._cards[0].set_value("despid", value)

    @property
    def descid(self) -> int:
        """Get or set the Section ID for generated DES elements
        """ # nopep8
        return self._cards[0].get_value("descid")

    @descid.setter
    def descid(self, value: int) -> None:
        self._cards[0].set_value("descid", value)

    @property
    def nsid(self) -> int:
        """Get or set the If defined, this card creates a node set with ID NSID (see *SET_NODE) for the nodes generated by this card ( this nodeset will be created by DYNA )
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def rsf(self) -> float:
        """Get or set the GE. 0: Scale factor of DES radius. DES radius is based on shell thickness. (Default: RSF = 1.0).
        LT. 0: DES radius is the 0.5*abs(RSF)*(max diagonal length )/ NQUAD for rectangular segment or the 0.5*abs(RSF)*(max side length) / NQUAD for triangular segment.
        """ # nopep8
        return self._cards[0].get_value("rsf")

    @rsf.setter
    def rsf(self, value: float) -> None:
        self._cards[0].set_value("rsf", value)

    @property
    def iactive(self) -> int:
        """Get or set the Activate DES:
        EQ.0: DES is inactive and used as a shadow (default)
        EQ.1: DES is active
        """ # nopep8
        return self._cards[0].get_value("iactive")

    @iactive.setter
    def iactive(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iactive must be one of {0,1}""")
        self._cards[0].set_value("iactive", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

