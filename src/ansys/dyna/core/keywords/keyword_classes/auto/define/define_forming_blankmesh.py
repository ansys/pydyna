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

"""Module providing the DefineFormingBlankmesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFORMINGBLANKMESH_CARD0 = (
    FieldSchema("idmsh", int, 0, 10, None),
    FieldSchema("eleng", float, 10, 10, 0.0),
    FieldSchema("xleng", float, 20, 10, 0.0),
    FieldSchema("yleng", float, 30, 10, 0.0),
    FieldSchema("anglex", float, 40, 10, 0.0),
    FieldSchema("nplane", int, 50, 10, 1),
    FieldSchema("cid", int, 60, 10, None),
)

_DEFINEFORMINGBLANKMESH_CARD1 = (
    FieldSchema("pidbk", int, 0, 10, None),
    FieldSchema("nid", int, 10, 10, None),
    FieldSchema("eid", int, 20, 10, None),
    FieldSchema("xcent", float, 30, 10, 0.0),
    FieldSchema("ycent", float, 40, 10, 0.0),
    FieldSchema("zcent", float, 50, 10, 0.0),
    FieldSchema("xshift", float, 60, 10, 0.0),
    FieldSchema("yshift", float, 70, 10, 0.0),
)

class DefineFormingBlankmesh(KeywordBase):
    """DYNA DEFINE_FORMING_BLANKMESH keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_BLANKMESH"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFormingBlankmesh class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFORMINGBLANKMESH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFORMINGBLANKMESH_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFormingBlankmesh.option_specs[0],
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
    def idmsh(self) -> typing.Optional[int]:
        """Get or set the ID of the blankmesh (not the blank PID); must be unique.
        """ # nopep8
        return self._cards[0].get_value("idmsh")

    @idmsh.setter
    def idmsh(self, value: int) -> None:
        """Set the idmsh property."""
        self._cards[0].set_value("idmsh", value)

    @property
    def eleng(self) -> float:
        """Get or set the Element edge length.
        """ # nopep8
        return self._cards[0].get_value("eleng")

    @eleng.setter
    def eleng(self, value: float) -> None:
        """Set the eleng property."""
        self._cards[0].set_value("eleng", value)

    @property
    def xleng(self) -> float:
        """Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined.
        """ # nopep8
        return self._cards[0].get_value("xleng")

    @xleng.setter
    def xleng(self, value: float) -> None:
        """Set the xleng property."""
        self._cards[0].set_value("xleng", value)

    @property
    def yleng(self) -> float:
        """Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined.
        """ # nopep8
        return self._cards[0].get_value("yleng")

    @yleng.setter
    def yleng(self, value: float) -> None:
        """Set the yleng property."""
        self._cards[0].set_value("yleng", value)

    @property
    def anglex(self) -> float:
        """Get or set the An angle defined about Z-axis of the CID specified, starting from the X-axis as the zero degree,
        to rotate the blank and the orientation of the mesh to be generated. The sign of the rotation angle follows the right-hand rule.
        """ # nopep8
        return self._cards[0].get_value("anglex")

    @anglex.setter
    def anglex(self, value: float) -> None:
        """Set the anglex property."""
        self._cards[0].set_value("anglex", value)

    @property
    def nplane(self) -> int:
        """Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
        EQ.0 or 1:	XY-plane (default)
        EQ.2:	XZ-plane
        EQ.3:	YZ-plan.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        self._cards[0].set_value("nplane", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID of a local coordinate system defined by *DEFINE_COORDINATE_SYSTEM. Default is 0 representing the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def pidbk(self) -> typing.Optional[int]:
        """Get or set the Part ID of the blank, as defined by *PART.
        """ # nopep8
        return self._cards[1].get_value("pidbk")

    @pidbk.setter
    def pidbk(self, value: int) -> None:
        """Set the pidbk property."""
        self._cards[1].set_value("pidbk", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Starting node ID of the blank to be generated.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Starting element ID of the blank to be generated.
        """ # nopep8
        return self._cards[1].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[1].set_value("eid", value)

    @property
    def xcent(self) -> float:
        """Get or set the X-coordinate of the center of the blank.
        """ # nopep8
        return self._cards[1].get_value("xcent")

    @xcent.setter
    def xcent(self, value: float) -> None:
        """Set the xcent property."""
        self._cards[1].set_value("xcent", value)

    @property
    def ycent(self) -> float:
        """Get or set the Y-coordinate of the center of the blank.
        """ # nopep8
        return self._cards[1].get_value("ycent")

    @ycent.setter
    def ycent(self, value: float) -> None:
        """Set the ycent property."""
        self._cards[1].set_value("ycent", value)

    @property
    def zcent(self) -> float:
        """Get or set the Z-coordinate of the center of the blank.
        """ # nopep8
        return self._cards[1].get_value("zcent")

    @zcent.setter
    def zcent(self, value: float) -> None:
        """Set the zcent property."""
        self._cards[1].set_value("zcent", value)

    @property
    def xshift(self) -> float:
        """Get or set the Blank shifting distance in X-axis in coordinate system (CID) defined.
        """ # nopep8
        return self._cards[1].get_value("xshift")

    @xshift.setter
    def xshift(self, value: float) -> None:
        """Set the xshift property."""
        self._cards[1].set_value("xshift", value)

    @property
    def yshift(self) -> float:
        """Get or set the Blank shifting distance in Y-axis in coordinate system (CID) defined.
        """ # nopep8
        return self._cards[1].get_value("yshift")

    @yshift.setter
    def yshift(self, value: float) -> None:
        """Set the yshift property."""
        self._cards[1].set_value("yshift", value)

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

