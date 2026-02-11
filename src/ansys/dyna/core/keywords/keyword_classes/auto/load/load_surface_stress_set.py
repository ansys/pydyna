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

"""Module providing the LoadSurfaceStressSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_LOADSURFACESTRESSSET_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
)

_LOADSURFACESTRESSSET_CARD1 = (
    FieldSchema("lscid1", int, 0, 10, None),
    FieldSchema("lscid2", int, 10, 10, None),
    FieldSchema("lscid3", int, 20, 10, None),
    FieldSchema("lscid4", int, 30, 10, None),
    FieldSchema("lscid5", int, 40, 10, None),
    FieldSchema("lscid6", int, 50, 10, None),
    FieldSchema("lscid7", int, 60, 10, None),
    FieldSchema("lscid8", int, 70, 10, None),
)

_LOADSURFACESTRESSSET_CARD2 = (
    FieldSchema("uscid1", int, 0, 10, None),
    FieldSchema("uscid2", int, 10, 10, None),
    FieldSchema("uscid3", int, 20, 10, None),
    FieldSchema("uscid4", int, 30, 10, None),
    FieldSchema("uscid5", int, 40, 10, None),
    FieldSchema("uscid6", int, 50, 10, None),
    FieldSchema("uscid7", int, 60, 10, None),
    FieldSchema("uscid8", int, 70, 10, None),
)

class LoadSurfaceStressSet(KeywordBase):
    """DYNA LOAD_SURFACE_STRESS_SET keyword"""

    keyword = "LOAD"
    subkeyword = "SURFACE_STRESS_SET"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadSurfaceStressSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSURFACESTRESSSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSURFACESTRESSSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSURFACESTRESSSET_CARD2,
                **kwargs,
            ),        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the part set id
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def lscid1(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.
        """ # nopep8
        return self._cards[1].get_value("lscid1")

    @lscid1.setter
    def lscid1(self, value: int) -> None:
        """Set the lscid1 property."""
        self._cards[1].set_value("lscid1", value)

    @property
    def lscid2(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.
        """ # nopep8
        return self._cards[1].get_value("lscid2")

    @lscid2.setter
    def lscid2(self, value: int) -> None:
        """Set the lscid2 property."""
        self._cards[1].set_value("lscid2", value)

    @property
    def lscid3(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.
        """ # nopep8
        return self._cards[1].get_value("lscid3")

    @lscid3.setter
    def lscid3(self, value: int) -> None:
        """Set the lscid3 property."""
        self._cards[1].set_value("lscid3", value)

    @property
    def lscid4(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.  .
        """ # nopep8
        return self._cards[1].get_value("lscid4")

    @lscid4.setter
    def lscid4(self, value: int) -> None:
        """Set the lscid4 property."""
        self._cards[1].set_value("lscid4", value)

    @property
    def lscid5(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.
        """ # nopep8
        return self._cards[1].get_value("lscid5")

    @lscid5.setter
    def lscid5(self, value: int) -> None:
        """Set the lscid5 property."""
        self._cards[1].set_value("lscid5", value)

    @property
    def lscid6(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.  .
        """ # nopep8
        return self._cards[1].get_value("lscid6")

    @lscid6.setter
    def lscid6(self, value: int) -> None:
        """Set the lscid6 property."""
        self._cards[1].set_value("lscid6", value)

    @property
    def lscid7(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.  .
        """ # nopep8
        return self._cards[1].get_value("lscid7")

    @lscid7.setter
    def lscid7(self, value: int) -> None:
        """Set the lscid7 property."""
        self._cards[1].set_value("lscid7", value)

    @property
    def lscid8(self) -> typing.Optional[int]:
        """Get or set the Lower surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the lower surface of the shell.  If the pressure on the lower surface is due to applied pressure loads, specify a -1 instead of a contact ID.  .
        """ # nopep8
        return self._cards[1].get_value("lscid8")

    @lscid8.setter
    def lscid8(self, value: int) -> None:
        """Set the lscid8 property."""
        self._cards[1].set_value("lscid8", value)

    @property
    def uscid1(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8.
        """ # nopep8
        return self._cards[2].get_value("uscid1")

    @uscid1.setter
    def uscid1(self, value: int) -> None:
        """Set the uscid1 property."""
        self._cards[2].set_value("uscid1", value)

    @property
    def uscid2(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8.
        """ # nopep8
        return self._cards[2].get_value("uscid2")

    @uscid2.setter
    def uscid2(self, value: int) -> None:
        """Set the uscid2 property."""
        self._cards[2].set_value("uscid2", value)

    @property
    def uscid3(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8.
        """ # nopep8
        return self._cards[2].get_value("uscid3")

    @uscid3.setter
    def uscid3(self, value: int) -> None:
        """Set the uscid3 property."""
        self._cards[2].set_value("uscid3", value)

    @property
    def uscid4(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8..
        """ # nopep8
        return self._cards[2].get_value("uscid4")

    @uscid4.setter
    def uscid4(self, value: int) -> None:
        """Set the uscid4 property."""
        self._cards[2].set_value("uscid4", value)

    @property
    def uscid5(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8.
        """ # nopep8
        return self._cards[2].get_value("uscid5")

    @uscid5.setter
    def uscid5(self, value: int) -> None:
        """Set the uscid5 property."""
        self._cards[2].set_value("uscid5", value)

    @property
    def uscid6(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8..
        """ # nopep8
        return self._cards[2].get_value("uscid6")

    @uscid6.setter
    def uscid6(self, value: int) -> None:
        """Set the uscid6 property."""
        self._cards[2].set_value("uscid6", value)

    @property
    def uscid7(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8..
        """ # nopep8
        return self._cards[2].get_value("uscid7")

    @uscid7.setter
    def uscid7(self, value: int) -> None:
        """Set the uscid7 property."""
        self._cards[2].set_value("uscid7", value)

    @property
    def uscid8(self) -> typing.Optional[int]:
        """Get or set the Upper surface contact ID's.  Up to eight ID's can be defined.  These contacts contribute to the pressure acting on the upper surface of the shell. .  If the pressure on the upper surface is due to applied pressure loads, specify a -1 instead of a contact ID.  Only one, -1, may exist in the set of 8..
        """ # nopep8
        return self._cards[2].get_value("uscid8")

    @uscid8.setter
    def uscid8(self, value: int) -> None:
        """Set the uscid8 property."""
        self._cards[2].set_value("uscid8", value)

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

