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

"""Module providing the ControlFormingGenerateBlankmesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGGENERATEBLANKMESH_CARD0 = (
    FieldSchema("gentyp", int, 0, 10, None),
    FieldSchema("eleng", float, 10, 10, None),
    FieldSchema("center", int, 20, 10, 0),
    FieldSchema("xleng", float, 30, 10, None),
    FieldSchema("yleng", float, 40, 10, None),
    FieldSchema("align", float, 50, 10, None),
    FieldSchema("plane", int, 60, 10, 1),
    FieldSchema("cid", int, 70, 10, None),
)

_CONTROLFORMINGGENERATEBLANKMESH_CARD1 = (
    FieldSchema("bpid", int, 0, 10, None),
    FieldSchema("snid", int, 10, 10, None),
    FieldSchema("seid", int, 20, 10, None),
    FieldSchema("xcent", float, 30, 10, None),
    FieldSchema("ycent", float, 40, 10, None),
    FieldSchema("zcent", float, 50, 10, None),
    FieldSchema("xshift", float, 60, 10, None),
    FieldSchema("yshift", float, 70, 10, None),
)

_CONTROLFORMINGGENERATEBLANKMESH_CARD2 = (
    FieldSchema("filename", str, 0, 256, None),
)

class ControlFormingGenerateBlankmesh(KeywordBase):
    """DYNA CONTROL_FORMING_GENERATE_BLANKMESH keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_GENERATE_BLANKMESH"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingGenerateBlankmesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGGENERATEBLANKMESH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGGENERATEBLANKMESH_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGGENERATEBLANKMESH_CARD2,
                **kwargs,
            ),        ]
    @property
    def gentyp(self) -> typing.Optional[int]:
        """Get or set the EQ.1: Rectangle shape.
        Find the smallest possible untrimmed rectangle depending on the requested element length and edge size in z and y, create quad elements with the same edge length and afterwards trim the elements at the requested edge size in z and y (like *ELEMENT_BALNKING). In order to make sure that the last row of elements does not have a smaller element lenght (bad time step size) after trimming than the requested eleng the last two wlement rows will be optimized and then the element size can be even larger;
        .EQ.2: import blank outline as IGES-Curve.
        Import an iges file of the desired blank shape as a curve. Find the smallest possible untrimmed rectangle depending on the requested element lenght and the blank outline, create quad elements with the same edge elength and afterwards trim the elements at the blank outline ( like *ELEMENT_BLANKING). In order to make sure that the last row of elements geoes not have a smaller wlement lenght (bad time step size) after trimming than the requested eleng the last two wlement rows will be optimized and then the element size can be even larger
        """ # nopep8
        return self._cards[0].get_value("gentyp")

    @gentyp.setter
    def gentyp(self, value: int) -> None:
        """Set the gentyp property."""
        self._cards[0].set_value("gentyp", value)

    @property
    def eleng(self) -> typing.Optional[float]:
        """Get or set the edge-length of quad
        """ # nopep8
        return self._cards[0].get_value("eleng")

    @eleng.setter
    def eleng(self, value: float) -> None:
        """Set the eleng property."""
        self._cards[0].set_value("eleng", value)

    @property
    def center(self) -> int:
        """Get or set the center of rectangle.
        EQ.0 0,0,0 of global coordinate system or 0.0.0 of local coordinate system if cid is defined
        EQ.1  center via absolute coordinates define xcent, ycent and zcent
        """ # nopep8
        return self._cards[0].get_value("center")

    @center.setter
    def center(self, value: int) -> None:
        """Set the center property."""
        if value not in [0, 1, None]:
            raise Exception("""center must be `None` or one of {0,1}.""")
        self._cards[0].set_value("center", value)

    @property
    def xleng(self) -> typing.Optional[float]:
        """Get or set the length of rectangle blank regarding global/local x-axis
        """ # nopep8
        return self._cards[0].get_value("xleng")

    @xleng.setter
    def xleng(self, value: float) -> None:
        """Set the xleng property."""
        self._cards[0].set_value("xleng", value)

    @property
    def yleng(self) -> typing.Optional[float]:
        """Get or set the length of rectangle blank regarding global/local y-axis
        """ # nopep8
        return self._cards[0].get_value("yleng")

    @yleng.setter
    def yleng(self, value: float) -> None:
        """Set the yleng property."""
        self._cards[0].set_value("yleng", value)

    @property
    def align(self) -> typing.Optional[float]:
        """Get or set the alignment of elements refers to global/local x-axis
        """ # nopep8
        return self._cards[0].get_value("align")

    @align.setter
    def align(self, value: float) -> None:
        """Set the align property."""
        self._cards[0].set_value("align", value)

    @property
    def plane(self) -> int:
        """Get or set the principale plane in which flat blank will be generated if blank outline as iges curve are not flat then projection to principal plane.
        1: xy-plane of local or global coord system
        2: xz-plane of local or global coord system
        3: zy-plane of local or global coord system
        """ # nopep8
        return self._cards[0].get_value("plane")

    @plane.setter
    def plane(self, value: int) -> None:
        """Set the plane property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""plane must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("plane", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the local coordinate system
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the part id of generated blank
        """ # nopep8
        return self._cards[1].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        """Set the bpid property."""
        self._cards[1].set_value("bpid", value)

    @property
    def snid(self) -> typing.Optional[int]:
        """Get or set the start node id
        """ # nopep8
        return self._cards[1].get_value("snid")

    @snid.setter
    def snid(self, value: int) -> None:
        """Set the snid property."""
        self._cards[1].set_value("snid", value)

    @property
    def seid(self) -> typing.Optional[int]:
        """Get or set the start element id
        """ # nopep8
        return self._cards[1].get_value("seid")

    @seid.setter
    def seid(self, value: int) -> None:
        """Set the seid property."""
        self._cards[1].set_value("seid", value)

    @property
    def xcent(self) -> typing.Optional[float]:
        """Get or set the absolute x-coordinate for center
        """ # nopep8
        return self._cards[1].get_value("xcent")

    @xcent.setter
    def xcent(self, value: float) -> None:
        """Set the xcent property."""
        self._cards[1].set_value("xcent", value)

    @property
    def ycent(self) -> typing.Optional[float]:
        """Get or set the absolute y-coordinate for center
        """ # nopep8
        return self._cards[1].get_value("ycent")

    @ycent.setter
    def ycent(self, value: float) -> None:
        """Set the ycent property."""
        self._cards[1].set_value("ycent", value)

    @property
    def zcent(self) -> typing.Optional[float]:
        """Get or set the absolute z-coordinate for center
        """ # nopep8
        return self._cards[1].get_value("zcent")

    @zcent.setter
    def zcent(self, value: float) -> None:
        """Set the zcent property."""
        self._cards[1].set_value("zcent", value)

    @property
    def xshift(self) -> typing.Optional[float]:
        """Get or set the shift length of blank regarding global/local x-axix
        """ # nopep8
        return self._cards[1].get_value("xshift")

    @xshift.setter
    def xshift(self, value: float) -> None:
        """Set the xshift property."""
        self._cards[1].set_value("xshift", value)

    @property
    def yshift(self) -> typing.Optional[float]:
        """Get or set the shift length of blank regarding global/local y-axix
        """ # nopep8
        return self._cards[1].get_value("yshift")

    @yshift.setter
    def yshift(self, value: float) -> None:
        """Set the yshift property."""
        self._cards[1].set_value("yshift", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[2].set_value("filename", value)

