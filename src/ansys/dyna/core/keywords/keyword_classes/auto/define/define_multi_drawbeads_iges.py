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

"""Module providing the DefineMultiDrawbeadsIges class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_DEFINEMULTIDRAWBEADSIGES_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_DEFINEMULTIDRAWBEADSIGES_CARD1 = (
    FieldSchema("dbid", int, 0, 10, None),
    FieldSchema("vid", int, 10, 10, None),
    FieldSchema("pid", int, 20, 10, None),
    FieldSchema("blkid", int, 30, 10, None),
    FieldSchema("ncur", int, 40, 10, None),
)

_DEFINEMULTIDRAWBEADSIGES_CARD2 = (
    FieldSchema("crvid", int, 0, 10, None),
    FieldSchema("bforce", float, 10, 10, 0.0),
)

_DEFINEMULTIDRAWBEADSIGES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineMultiDrawbeadsIges(KeywordBase):
    """DYNA DEFINE_MULTI_DRAWBEADS_IGES keyword"""

    keyword = "DEFINE"
    subkeyword = "MULTI_DRAWBEADS_IGES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "vid": LinkType.DEFINE_VECTOR,
        "blkid": LinkType.SET_PART,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineMultiDrawbeadsIges class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEMULTIDRAWBEADSIGES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEMULTIDRAWBEADSIGES_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEMULTIDRAWBEADSIGES_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineMultiDrawbeadsIges.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEMULTIDRAWBEADSIGES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the IGES file that has the draw bead curve segment definitions
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def dbid(self) -> typing.Optional[int]:
        """Get or set the Draw bead set ID, which may consists many draw bead segments.
        """ # nopep8
        return self._cards[1].get_value("dbid")

    @dbid.setter
    def dbid(self, value: int) -> None:
        """Set the dbid property."""
        self._cards[1].set_value("dbid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID, as defined by *DEFINE_VECTOR. This vector is used to
        project the supplied curves to the rigid tool, defined by variable PID.
        """ # nopep8
        return self._cards[1].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[1].set_value("vid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid tool to which the curves are projected and attached.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def blkid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the blank.
        """ # nopep8
        return self._cards[1].get_value("blkid")

    @blkid.setter
    def blkid(self, value: int) -> None:
        """Set the blkid property."""
        self._cards[1].set_value("blkid", value)

    @property
    def ncur(self) -> typing.Optional[int]:
        """Get or set the Number of draw bead curve segments (in the IGES file) to be defined.
        """ # nopep8
        return self._cards[1].get_value("ncur")

    @ncur.setter
    def ncur(self, value: int) -> None:
        """Set the ncur property."""
        self._cards[1].set_value("ncur", value)

    @property
    def crvid(self) -> typing.Optional[int]:
        """Get or set the IGES curve ID for each segment.
        """ # nopep8
        return self._cards[2].get_value("crvid")

    @crvid.setter
    def crvid(self, value: int) -> None:
        """Set the crvid property."""
        self._cards[2].set_value("crvid", value)

    @property
    def bforce(self) -> float:
        """Get or set the Draw bead force for each segment.
        """ # nopep8
        return self._cards[2].get_value("bforce")

    @bforce.setter
    def bforce(self, value: float) -> None:
        """Set the bforce property."""
        self._cards[2].set_value("bforce", value)

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
    def vid_link(self) -> DefineVector:
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
    def blkid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for blkid."""
        return self._get_set_link("PART", self.blkid)

    @blkid_link.setter
    def blkid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for blkid."""
        self.blkid = value.sid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

