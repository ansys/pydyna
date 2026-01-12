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

"""Module providing the LoadSegmentContactMask class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADSEGMENTCONTACTMASK_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_LOADSEGMENTCONTACTMASK_CARD1 = (
    FieldSchema("lsid", int, 0, 10, None),
    FieldSchema("p1", float, 10, 10, None),
    FieldSchema("p2", float, 20, 10, None),
    FieldSchema("cid1", int, 30, 10, None),
    FieldSchema("cid2", int, 40, 10, None),
    FieldSchema("cid3", int, 50, 10, None),
    FieldSchema("cid4", int, 60, 10, None),
    FieldSchema("cid5", int, 70, 10, None),
)

_LOADSEGMENTCONTACTMASK_CARD2 = (
    FieldSchema("cid1", int, 0, 10, None),
    FieldSchema("cid2", int, 10, 10, None),
    FieldSchema("cid3", int, 20, 10, None),
    FieldSchema("cid4", int, 30, 10, None),
    FieldSchema("cid5", int, 40, 10, None),
    FieldSchema("cid6", int, 50, 10, None),
    FieldSchema("cid7", int, 60, 10, None),
    FieldSchema("cid8", int, 70, 10, None),
)

class LoadSegmentContactMask(KeywordBase):
    """DYNA LOAD_SEGMENT_CONTACT_MASK keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT_CONTACT_MASK"

    def __init__(self, **kwargs):
        """Initialize the LoadSegmentContactMask class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTCONTACTMASK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTCONTACTMASK_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTCONTACTMASK_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def lsid(self) -> typing.Optional[int]:
        """Get or set the Load set ID to mask, which must match a *LOAD_SEGMENT_SET.	See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lsid")

    @lsid.setter
    def lsid(self, value: int) -> None:
        """Set the lsid property."""
        self._cards[1].set_value("lsid", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Lower pressure limit. When the surface pressure due to contact is
        below P1, no masking is done and the full load defined in *LOAD_SEGMENT_SET is applied.
        For pressures between P1 and P2 see Remark 1.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Upper pressure limit. When the surface pressure due to contact is
        above P2, no load is applied due to the *LOAD_SEGMENT_SET. For pressures between P1 and P2 see Remark 1.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[1].set_value("p2", value)

    @property
    def cid1(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[1].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        """Set the cid1 property."""
        self._cards[1].set_value("cid1", value)

    @property
    def cid2(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[1].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        """Set the cid2 property."""
        self._cards[1].set_value("cid2", value)

    @property
    def cid3(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[1].get_value("cid3")

    @cid3.setter
    def cid3(self, value: int) -> None:
        """Set the cid3 property."""
        self._cards[1].set_value("cid3", value)

    @property
    def cid4(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[1].get_value("cid4")

    @cid4.setter
    def cid4(self, value: int) -> None:
        """Set the cid4 property."""
        self._cards[1].set_value("cid4", value)

    @property
    def cid5(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[1].get_value("cid5")

    @cid5.setter
    def cid5(self, value: int) -> None:
        """Set the cid5 property."""
        self._cards[1].set_value("cid5", value)

    @property
    def cid1(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect..
        """ # nopep8
        return self._cards[2].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        """Set the cid1 property."""
        self._cards[2].set_value("cid1", value)

    @property
    def cid2(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        """Set the cid2 property."""
        self._cards[2].set_value("cid2", value)

    @property
    def cid3(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid3")

    @cid3.setter
    def cid3(self, value: int) -> None:
        """Set the cid3 property."""
        self._cards[2].set_value("cid3", value)

    @property
    def cid4(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid4")

    @cid4.setter
    def cid4(self, value: int) -> None:
        """Set the cid4 property."""
        self._cards[2].set_value("cid4", value)

    @property
    def cid5(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid5")

    @cid5.setter
    def cid5(self, value: int) -> None:
        """Set the cid5 property."""
        self._cards[2].set_value("cid5", value)

    @property
    def cid6(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid6")

    @cid6.setter
    def cid6(self, value: int) -> None:
        """Set the cid6 property."""
        self._cards[2].set_value("cid6", value)

    @property
    def cid7(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid7")

    @cid7.setter
    def cid7(self, value: int) -> None:
        """Set the cid7 property."""
        self._cards[2].set_value("cid7", value)

    @property
    def cid8(self) -> typing.Optional[int]:
        """Get or set the The IDs of contacts that can mask the pressure loads. The specified contact definitions must all be of the same type. Furthermore, only
        SURFACE_TO_SURFACE (two way) or AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK are supported.
        For TIEBREAK contacts, pressure is masked until the tie fails. Once	the tie fails, the full pressure will be applied for the remainder of the
        simulation. The values P1 and P2 are ignored. For other contact types, the contact forces, along with the nodal
        contact surface areas, are used to compute the contact pressure at	each node to determine any masking effect.
        """ # nopep8
        return self._cards[2].get_value("cid8")

    @cid8.setter
    def cid8(self, value: int) -> None:
        """Set the cid8 property."""
        self._cards[2].set_value("cid8", value)

