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

"""Module providing the DefineFormingAdaptivePartInitial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEFORMINGADAPTIVEPARTINITIAL_CARD0 = (
    FieldSchema("pstid", int, 0, 10, None),
    FieldSchema("pstbid", int, 10, 10, None),
    FieldSchema("vx", float, 20, 10, None),
    FieldSchema("vy", float, 30, 10, None),
    FieldSchema("vz", float, 40, 10, None),
    FieldSchema("smin", float, 50, 10, None),
    FieldSchema("imove", int, 60, 10, 0),
)

_DEFINEFORMINGADAPTIVEPARTINITIAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFormingAdaptivePartInitial(KeywordBase):
    """DYNA DEFINE_FORMING_ADAPTIVE_PART_INITIAL keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_ADAPTIVE_PART_INITIAL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pstid": LinkType.SET_PART,
        "pstbid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineFormingAdaptivePartInitial class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFORMINGADAPTIVEPARTINITIAL_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineFormingAdaptivePartInitial._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFORMINGADAPTIVEPARTINITIAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pstid(self) -> typing.Optional[int]:
        """Get or set the Part set of local regions that will be projected onto the parts of the blank (PSTBID) to determine where the blank will be refined
        """ # nopep8
        return self._cards[0].get_value("pstid")

    @pstid.setter
    def pstid(self, value: int) -> None:
        """Set the pstid property."""
        self._cards[0].set_value("pstid", value)

    @property
    def pstbid(self) -> typing.Optional[int]:
        """Get or set the Part set of the deformable body (blank) to be refined
        """ # nopep8
        return self._cards[0].get_value("pstbid")

    @pstbid.setter
    def pstbid(self, value: int) -> None:
        """Set the pstbid property."""
        self._cards[0].set_value("pstbid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Vector for projecting the parts of PSTID onto the parts of PSTBID
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Vector for projecting the parts of PSTID onto the parts of PSTBID
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Vector for projecting the parts of PSTID onto the parts of PSTBID
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def smin(self) -> typing.Optional[float]:
        """Get or set the Minimum element size after refinement
        """ # nopep8
        return self._cards[0].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        """Set the smin property."""
        self._cards[0].set_value("smin", value)

    @property
    def imove(self) -> int:
        """Get or set the Flag for treatment of the vector (VX, VY, VZ):
        EQ.0:	Use(VX, VY, VZ) as the projection vector.
        EQ.1 : Ignore(VX, VY, VZ) and use the movement vector defined with * PART_MOVE instead for the projection.This option may be ideal when including this keyword with* INCLUDE_TRANSFORM because the vector changes from the transformation.This option enables determining the vector only once.
        """ # nopep8
        return self._cards[0].get_value("imove")

    @imove.setter
    def imove(self, value: int) -> None:
        """Set the imove property."""
        if value not in [0, 1, None]:
            raise Exception("""imove must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imove", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pstid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for pstid."""
        return self._get_set_link("PART", self.pstid)

    @pstid_link.setter
    def pstid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pstid."""
        self.pstid = value.sid

    @property
    def pstbid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for pstbid."""
        return self._get_set_link("PART", self.pstbid)

    @pstbid_link.setter
    def pstbid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pstbid."""
        self.pstbid = value.sid

