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

"""Module providing the DefineContactVolume class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINECONTACTVOLUME_CARD0 = (
    FieldSchema("cvid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("type", int, 20, 10, 0),
    FieldSchema("xc", float, 30, 10, None),
    FieldSchema("yc", float, 40, 10, None),
    FieldSchema("zc", float, 50, 10, None),
)

_DEFINECONTACTVOLUME_CARD1 = (
    FieldSchema("xmn", float, 0, 10, 0.0),
    FieldSchema("xmx", float, 10, 10, 0.0),
    FieldSchema("ymn", float, 20, 10, 0.0),
    FieldSchema("ymx", float, 30, 10, 0.0),
    FieldSchema("zmn", float, 40, 10, 0.0),
    FieldSchema("zmx", float, 50, 10, 0.0),
)

_DEFINECONTACTVOLUME_CARD2 = (
    FieldSchema("length", float, 0, 10, 0.0),
    FieldSchema("rinner", float, 10, 10, 0.0),
    FieldSchema("router", float, 20, 10, 0.0),
    FieldSchema("d_angc", float, 30, 10, 0.0),
)

_DEFINECONTACTVOLUME_CARD3 = (
    FieldSchema("rinner", float, 0, 10, 0.0),
    FieldSchema("router", float, 10, 10, 0.0),
    FieldSchema("d_angs", float, 20, 10, 0.0),
)

_DEFINECONTACTVOLUME_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineContactVolume(KeywordBase):
    """DYNA DEFINE_CONTACT_VOLUME keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTACT_VOLUME"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineContactVolume class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONTACTVOLUME_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECONTACTVOLUME_CARD1,
                active_func=lambda: self.type==0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECONTACTVOLUME_CARD2,
                active_func=lambda: self.type==1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECONTACTVOLUME_CARD3,
                active_func=lambda: self.type==2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineContactVolume.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONTACTVOLUME_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def cvid(self) -> typing.Optional[int]:
        """Get or set the Contact volume ID.
        """ # nopep8
        return self._cards[0].get_value("cvid")

    @cvid.setter
    def cvid(self, value: int) -> None:
        """Set the cvid property."""
        self._cards[0].set_value("cvid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID. Required for rectangular and cylindrical volumes
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def type(self) -> int:
        """Get or set the Volume type. Set to 0 for rectangular, 1 for cylindrical, and 2 for spherical.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""type must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("type", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the x-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the y-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the z-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        """Set the xmn property."""
        self._cards[1].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        """Set the xmx property."""
        self._cards[1].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        """Set the ymn property."""
        self._cards[1].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        """Set the ymx property."""
        self._cards[1].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        """Set the zmn property."""
        self._cards[1].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate in local coordinate system.
        """ # nopep8
        return self._cards[1].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        """Set the zmx property."""
        self._cards[1].set_value("zmx", value)

    @property
    def length(self) -> float:
        """Get or set the Length of cylinder originating at(xc,yc,zc) and revolving around the local x-axis.
        """ # nopep8
        return self._cards[2].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[2].set_value("length", value)

    @property
    def rinner(self) -> float:
        """Get or set the Inner radius of cylinder or sphere.
        """ # nopep8
        return self._cards[2].get_value("rinner")

    @rinner.setter
    def rinner(self, value: float) -> None:
        """Set the rinner property."""
        self._cards[2].set_value("rinner", value)
        self._cards[3].set_value("rinner", value)

    @property
    def router(self) -> float:
        """Get or set the Outer radius of cylinder or sphere.
        """ # nopep8
        return self._cards[2].get_value("router")

    @router.setter
    def router(self, value: float) -> None:
        """Set the router property."""
        self._cards[2].set_value("router", value)
        self._cards[3].set_value("router", value)

    @property
    def d_angc(self) -> float:
        """Get or set the If the included angle between axis of the cylinder and the normal vector ot the contact segment is less than this angle, the segment is deleted.
        """ # nopep8
        return self._cards[2].get_value("d_angc")

    @d_angc.setter
    def d_angc(self, value: float) -> None:
        """Set the d_angc property."""
        self._cards[2].set_value("d_angc", value)

    @property
    def d_angs(self) -> float:
        """Get or set the If the included angle between a line draw from the center of the sphere to the centroid of the segment, and the normal vector to the contact segment is greater than this angle, the segment is deleted.
        """ # nopep8
        return self._cards[3].get_value("d_angs")

    @d_angs.setter
    def d_angs(self, value: float) -> None:
        """Set the d_angs property."""
        self._cards[3].set_value("d_angs", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

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

