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

"""Module providing the DefineCurveDrawbead class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_DEFINECURVEDRAWBEAD_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("tcype", int, 10, 10, 1),
    FieldSchema("vid", int, 20, 10, None),
    FieldSchema("pid", int, 30, 10, None),
    FieldSchema("blkid", int, 40, 10, None),
    FieldSchema("perct", int, 50, 10, None),
)

_DEFINECURVEDRAWBEAD_CARD1 = (
    FieldSchema("cx", float, 0, 20, 0.0),
    FieldSchema("cy", float, 20, 20, 0.0),
)

_DEFINECURVEDRAWBEAD_CARD2 = (
    FieldSchema("filename", str, 0, 80, None),
)

_DEFINECURVEDRAWBEAD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCurveDrawbead(KeywordBase):
    """DYNA DEFINE_CURVE_DRAWBEAD keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_DRAWBEAD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "vid": LinkType.DEFINE_VECTOR,
        "pid": LinkType.PART,
        "blkid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCurveDrawbead class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECURVEDRAWBEAD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVEDRAWBEAD_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVEDRAWBEAD_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineCurveDrawbead.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECURVEDRAWBEAD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Curve ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def tcype(self) -> int:
        """Get or set the Bea data type.
        EQ.1:x,y,z data
        EQ. 2. IGES data
        """ # nopep8
        return self._cards[0].get_value("tcype")

    @tcype.setter
    def tcype(self, value: int) -> None:
        """Set the tcype property."""
        if value not in [1, 2, None]:
            raise Exception("""tcype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("tcype", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID, See DEFINE_VECTOR. This vector is used to project the bead to the rigid part (PID)
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID to attach the drawbead.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def blkid(self) -> typing.Optional[int]:
        """Get or set the The part id of the blank.
        """ # nopep8
        return self._cards[0].get_value("blkid")

    @blkid.setter
    def blkid(self, value: int) -> None:
        """Set the blkid property."""
        self._cards[0].set_value("blkid", value)

    @property
    def perct(self) -> typing.Optional[int]:
        """Get or set the Percentage of restraining force (the ratio of restraining force over Lock force). The value should be between 0 and 100
        """ # nopep8
        return self._cards[0].get_value("perct")

    @perct.setter
    def perct(self, value: int) -> None:
        """Set the perct property."""
        self._cards[0].set_value("perct", value)

    @property
    def cx(self) -> float:
        """Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        """Set the cx property."""
        self._cards[1].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        """Set the cy property."""
        self._cards[1].set_value("cy", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
        """ # nopep8
        return self._cards[2].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[2].set_value("filename", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def vid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

    @property
    def blkid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given blkid."""
        return self._get_link_by_attr("PART", "pid", self.blkid, "parts")

