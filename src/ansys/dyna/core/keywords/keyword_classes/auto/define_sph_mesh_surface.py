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

class DefineSphMeshSurface(KeywordBase):
    """DYNA DEFINE_SPH_MESH_SURFACE keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_MESH_SURFACE"
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
                        kwargs.get("sid")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "sphpid",
                        int,
                        20,
                        10,
                        kwargs.get("sphpid")
                    ),
                    Field(
                        "sphxid",
                        int,
                        30,
                        10,
                        kwargs.get("sphxid")
                    ),
                    Field(
                        "nsid",
                        int,
                        40,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "space",
                        float,
                        50,
                        10,
                        kwargs.get("space", 0.0)
                    ),
                    Field(
                        "iout",
                        int,
                        60,
                        10,
                        kwargs.get("iout", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphMeshSurface.option_specs[0],
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
    def sid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID for the region of the mesh upon which the SPH elements will be placed.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def type(self) -> int:
        """Get or set the SID type:
        EQ.0:	part set ID
        EQ.1 : part ID.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def sphpid(self) -> typing.Optional[int]:
        """Get or set the Part ID for generated SPH elements.
        """ # nopep8
        return self._cards[0].get_value("sphpid")

    @sphpid.setter
    def sphpid(self, value: int) -> None:
        self._cards[0].set_value("sphpid", value)

    @property
    def sphxid(self) -> typing.Optional[int]:
        """Get or set the Section ID for generated SPH elements
        """ # nopep8
        return self._cards[0].get_value("sphxid")

    @sphxid.setter
    def sphxid(self, value: int) -> None:
        self._cards[0].set_value("sphxid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the If defined, this card creates a node set with ID NSID (see *SET_NODE) for the nodes generated by this card.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def space(self) -> float:
        """Get or set the Maximum space between SPH elements.
        """ # nopep8
        return self._cards[0].get_value("space")

    @space.setter
    def space(self, value: float) -> None:
        self._cards[0].set_value("space", value)

    @property
    def iout(self) -> int:
        """Get or set the Keyword file output:
        EQ.0:	no output(default)
        EQ.1 : output generated nodes, SPH elementsand node set to a keyword file with SPH_surface_ prefix.
        """ # nopep8
        return self._cards[0].get_value("iout")

    @iout.setter
    def iout(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iout must be one of {0,1}""")
        self._cards[0].set_value("iout", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

