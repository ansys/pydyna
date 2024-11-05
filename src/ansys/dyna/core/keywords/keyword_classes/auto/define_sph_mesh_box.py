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

class DefineSphMeshBox(KeywordBase):
    """DYNA DEFINE_SPH_MESH_BOX keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_MESH_BOX"
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
                        "xmin",
                        float,
                        0,
                        10,
                        kwargs.get("xmin")
                    ),
                    Field(
                        "ymin",
                        float,
                        10,
                        10,
                        kwargs.get("ymin")
                    ),
                    Field(
                        "zmin",
                        float,
                        20,
                        10,
                        kwargs.get("zmin")
                    ),
                    Field(
                        "xlen",
                        float,
                        30,
                        10,
                        kwargs.get("xlen")
                    ),
                    Field(
                        "ylen",
                        float,
                        40,
                        10,
                        kwargs.get("ylen")
                    ),
                    Field(
                        "zlen",
                        float,
                        50,
                        10,
                        kwargs.get("zlen")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ipid",
                        int,
                        0,
                        10,
                        kwargs.get("ipid")
                    ),
                    Field(
                        "nx",
                        int,
                        10,
                        10,
                        kwargs.get("nx")
                    ),
                    Field(
                        "ny",
                        int,
                        20,
                        10,
                        kwargs.get("ny")
                    ),
                    Field(
                        "nz",
                        int,
                        30,
                        10,
                        kwargs.get("nz")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "idseg",
                        int,
                        50,
                        10,
                        kwargs.get("idseg", 0)
                    ),
                    Field(
                        "sfsp",
                        float,
                        60,
                        10,
                        kwargs.get("sfsp")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSphMeshBox.option_specs[0],
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
    def xmin(self) -> typing.Optional[float]:
        """Get or set the Minimum x-coordinate
        """ # nopep8
        return self._cards[0].get_value("xmin")

    @xmin.setter
    def xmin(self, value: float) -> None:
        self._cards[0].set_value("xmin", value)

    @property
    def ymin(self) -> typing.Optional[float]:
        """Get or set the Minimum y-coordinate
        """ # nopep8
        return self._cards[0].get_value("ymin")

    @ymin.setter
    def ymin(self, value: float) -> None:
        self._cards[0].set_value("ymin", value)

    @property
    def zmin(self) -> typing.Optional[float]:
        """Get or set the Minimum z-coordinate
        """ # nopep8
        return self._cards[0].get_value("zmin")

    @zmin.setter
    def zmin(self, value: float) -> None:
        self._cards[0].set_value("zmin", value)

    @property
    def xlen(self) -> typing.Optional[float]:
        """Get or set the Box length in the x-direction.
        """ # nopep8
        return self._cards[0].get_value("xlen")

    @xlen.setter
    def xlen(self, value: float) -> None:
        self._cards[0].set_value("xlen", value)

    @property
    def ylen(self) -> typing.Optional[float]:
        """Get or set the Box length in the y-direction.
        """ # nopep8
        return self._cards[0].get_value("ylen")

    @ylen.setter
    def ylen(self, value: float) -> None:
        self._cards[0].set_value("ylen", value)

    @property
    def zlen(self) -> typing.Optional[float]:
        """Get or set the Box length in the z-direction.
        """ # nopep8
        return self._cards[0].get_value("zlen")

    @zlen.setter
    def zlen(self, value: float) -> None:
        self._cards[0].set_value("zlen", value)

    @property
    def ipid(self) -> typing.Optional[int]:
        """Get or set the Part ID for generated SPH elements
        """ # nopep8
        return self._cards[1].get_value("ipid")

    @ipid.setter
    def ipid(self, value: int) -> None:
        self._cards[1].set_value("ipid", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the x-direction.
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the y-direction.
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: int) -> None:
        self._cards[1].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the z-direction.
        """ # nopep8
        return self._cards[1].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        self._cards[1].set_value("nz", value)

    @property
    def idseg(self) -> int:
        """Get or set the Segment set ID that can be used to removed generated SPH elements. segment set is used to split the box into two regions, one that has SPH elements and one without SPH (see Remark 2). The sign of IDSEG determines which region keeps the SPH elements. Also, to avoid sudden movement, elements that are "too close" to the segment set will be removed, regardless of the sign of IDSEG. Too close means the normal distance from the center of the SPH element to the nearest segment is smaller than the SPH smoothing length scaled by SFSP.
        EQ.0 : No generated elements are removed.
        GT.0 : Keep the SPH element if it lies nominally in the normal direction of the segments in the segment set.
        LT.0 : Keep the SPH element if it lies nominally in the reverse normal direction of segments in the segment set.
        """ # nopep8
        return self._cards[1].get_value("idseg")

    @idseg.setter
    def idseg(self, value: int) -> None:
        self._cards[1].set_value("idseg", value)

    @property
    def sfsp(self) -> typing.Optional[float]:
        """Get or set the Scale factor for interparticle distance and only active when IDSEG.ne.0.
        If the distance between SPH particle and nearest segment is smaller than this distance, SPH element is removed.
        """ # nopep8
        return self._cards[1].get_value("sfsp")

    @sfsp.setter
    def sfsp(self, value: float) -> None:
        self._cards[1].set_value("sfsp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

