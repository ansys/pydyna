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

"""Module providing the DefineSphMeshBox class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPHMESHBOX_CARD0 = (
    FieldSchema("xmin", float, 0, 10, None),
    FieldSchema("ymin", float, 10, 10, None),
    FieldSchema("zmin", float, 20, 10, None),
    FieldSchema("xlen", float, 30, 10, None),
    FieldSchema("ylen", float, 40, 10, None),
    FieldSchema("zlen", float, 50, 10, None),
)

_DEFINESPHMESHBOX_CARD1 = (
    FieldSchema("ipid", int, 0, 10, None),
    FieldSchema("nx", int, 10, 10, None),
    FieldSchema("ny", int, 20, 10, None),
    FieldSchema("nz", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("idseg", int, 50, 10, 0),
    FieldSchema("sfsp", float, 60, 10, None),
)

_DEFINESPHMESHBOX_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphMeshBox(KeywordBase):
    """DYNA DEFINE_SPH_MESH_BOX keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_MESH_BOX"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "idseg": LinkType.SET_SEGMENT,
        "ipid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSphMeshBox class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHMESHBOX_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPHMESHBOX_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSphMeshBox.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHMESHBOX_OPTION0_CARD0,
                        **kwargs,
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
        """Set the xmin property."""
        self._cards[0].set_value("xmin", value)

    @property
    def ymin(self) -> typing.Optional[float]:
        """Get or set the Minimum y-coordinate
        """ # nopep8
        return self._cards[0].get_value("ymin")

    @ymin.setter
    def ymin(self, value: float) -> None:
        """Set the ymin property."""
        self._cards[0].set_value("ymin", value)

    @property
    def zmin(self) -> typing.Optional[float]:
        """Get or set the Minimum z-coordinate
        """ # nopep8
        return self._cards[0].get_value("zmin")

    @zmin.setter
    def zmin(self, value: float) -> None:
        """Set the zmin property."""
        self._cards[0].set_value("zmin", value)

    @property
    def xlen(self) -> typing.Optional[float]:
        """Get or set the Box length in the x-direction.
        """ # nopep8
        return self._cards[0].get_value("xlen")

    @xlen.setter
    def xlen(self, value: float) -> None:
        """Set the xlen property."""
        self._cards[0].set_value("xlen", value)

    @property
    def ylen(self) -> typing.Optional[float]:
        """Get or set the Box length in the y-direction.
        """ # nopep8
        return self._cards[0].get_value("ylen")

    @ylen.setter
    def ylen(self, value: float) -> None:
        """Set the ylen property."""
        self._cards[0].set_value("ylen", value)

    @property
    def zlen(self) -> typing.Optional[float]:
        """Get or set the Box length in the z-direction.
        """ # nopep8
        return self._cards[0].get_value("zlen")

    @zlen.setter
    def zlen(self, value: float) -> None:
        """Set the zlen property."""
        self._cards[0].set_value("zlen", value)

    @property
    def ipid(self) -> typing.Optional[int]:
        """Get or set the Part ID for generated SPH elements
        """ # nopep8
        return self._cards[1].get_value("ipid")

    @ipid.setter
    def ipid(self, value: int) -> None:
        """Set the ipid property."""
        self._cards[1].set_value("ipid", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the x-direction.
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        """Set the nx property."""
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the y-direction.
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: int) -> None:
        """Set the ny property."""
        self._cards[1].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[int]:
        """Get or set the Number of SPH particles in the z-direction.
        """ # nopep8
        return self._cards[1].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        """Set the nz property."""
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
        """Set the idseg property."""
        self._cards[1].set_value("idseg", value)

    @property
    def sfsp(self) -> typing.Optional[float]:
        """Get or set the Scale factor for interparticle distance and only active when IDSEG.ne.0.
        If the distance between SPH particle and nearest segment is smaller than this distance, SPH element is removed.
        """ # nopep8
        return self._cards[1].get_value("sfsp")

    @sfsp.setter
    def sfsp(self, value: float) -> None:
        """Set the sfsp property."""
        self._cards[1].set_value("sfsp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def idseg_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for idseg."""
        return self._get_set_link("SEGMENT", self.idseg)

    @idseg_link.setter
    def idseg_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for idseg."""
        self.idseg = value.sid

    @property
    def ipid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given ipid."""
        return self._get_link_by_attr("PART", "pid", self.ipid, "parts")

