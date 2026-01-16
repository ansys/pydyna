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

"""Module providing the DefineRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINEREGION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_DEFINEREGION_CARD1 = (
    FieldSchema("type", int, 0, 10, 0),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("move", int, 20, 10, 0),
)

_DEFINEREGION_CARD2 = (
    FieldSchema("xmn", float, 0, 10, 0.0),
    FieldSchema("xmx", float, 10, 10, 0.0),
    FieldSchema("ymn", float, 20, 10, 0.0),
    FieldSchema("ymx", float, 30, 10, 0.0),
    FieldSchema("zmn", float, 40, 10, 0.0),
    FieldSchema("zmx", float, 50, 10, 0.0),
)

_DEFINEREGION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineRegion(KeywordBase):
    """DYNA DEFINE_REGION keyword"""

    keyword = "DEFINE"
    subkeyword = "REGION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineRegion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEREGION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEREGION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEREGION_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineRegion.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEREGION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Region ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Title for this region.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def type(self) -> int:
        """Get or set the Region type:
        EQ.0: Box
        EQ.1: Sphere or spherical shell
        EQ.2: Cylinder or cylindrical shell, infinite or finite in length
        EQ.3: Ellipsoid.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""type must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("type", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Optional local coordinate system ID. If given, all the following
        input parameters will be interpreted in this coordinate system.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def move(self) -> int:
        """Get or set the Flag to specify whether the region moves:
        EQ.0:	Region is stationary.
        EQ.1 : Region moves to follow the local origin and rotates with the local coordinate system(see CID)..
        """ # nopep8
        return self._cards[1].get_value("move")

    @move.setter
    def move(self, value: int) -> None:
        """Set the move property."""
        if value not in [0, 1, None]:
            raise Exception("""move must be `None` or one of {0,1}.""")
        self._cards[1].set_value("move", value)

    @property
    def xmn(self) -> float:
        """Get or set the Lower x limit of box.
        """ # nopep8
        return self._cards[2].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        """Set the xmn property."""
        self._cards[2].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Upper x limit of box.
        """ # nopep8
        return self._cards[2].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        """Set the xmx property."""
        self._cards[2].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Lower y limit of box.
        """ # nopep8
        return self._cards[2].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        """Set the ymn property."""
        self._cards[2].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Upper y limit of box.
        """ # nopep8
        return self._cards[2].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        """Set the ymx property."""
        self._cards[2].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Lower z limit of box.
        """ # nopep8
        return self._cards[2].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        """Set the zmn property."""
        self._cards[2].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Upper z limit of box.
        """ # nopep8
        return self._cards[2].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        """Set the zmx property."""
        self._cards[2].set_value("zmx", value)

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
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

