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

"""Module providing the InterfaceCompensation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INTERFACECOMPENSATION_CARD0 = (
    FieldSchema("stage", int, 0, 10, 1),
    FieldSchema("psidt", int, 10, 10, None),
    FieldSchema("psidb", int, 20, 10, None),
    FieldSchema("smooth", int, 30, 10, 3),
    FieldSchema("scale", float, 40, 10, 1.0),
)

_INTERFACECOMPENSATION_CARD1 = (
    FieldSchema("dbname", str, 0, 80, "lscomp"),
)

_INTERFACECOMPENSATION_CARD2 = (
    FieldSchema("outname", str, 0, 80, "lstool"),
)

class InterfaceCompensation(KeywordBase):
    """DYNA INTERFACE_COMPENSATION keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION"
    _link_fields = {
        "psidt": LinkType.SET_PART,
        "psidb": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InterfaceCompensation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION_CARD2,
                **kwargs,
            ),        ]
    @property
    def stage(self) -> int:
        """Get or set the Stage of this simulation in overall process. Stamping and springback must be finished before compensation can be performed.
        EQ.1: stamping
        EQ.2: springback
        EQ.3: compensation(generate new tools).
        """ # nopep8
        return self._cards[0].get_value("stage")

    @stage.setter
    def stage(self, value: int) -> None:
        """Set the stage property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""stage must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("stage", value)

    @property
    def psidt(self) -> typing.Optional[int]:
        """Get or set the Part set ID including all tool parts(not required for STAGE=2)
        """ # nopep8
        return self._cards[0].get_value("psidt")

    @psidt.setter
    def psidt(self, value: int) -> None:
        """Set the psidt property."""
        self._cards[0].set_value("psidt", value)

    @property
    def psidb(self) -> typing.Optional[int]:
        """Get or set the Part set ID including all blank(sheet)parts.
        """ # nopep8
        return self._cards[0].get_value("psidb")

    @psidb.setter
    def psidb(self, value: int) -> None:
        """Set the psidb property."""
        self._cards[0].set_value("psidb", value)

    @property
    def smooth(self) -> int:
        """Get or set the Extrapolation method for tool surfaces outside final part (STAGE=3 only) A negative value can be used if undercutting occurs.
        EQ.1: Preserve boundary slope.
        EQ.2:Zero slope.
        EQ.3: Smoothing method A(DEFAULT).
        EQ.4: Smoothing method B.
        EQ.5: Smoothing method c.
        """ # nopep8
        return self._cards[0].get_value("smooth")

    @smooth.setter
    def smooth(self, value: int) -> None:
        """Set the smooth property."""
        if value not in [3, 1, 2, 4, 5, None]:
            raise Exception("""smooth must be `None` or one of {3,1,2,4,5}.""")
        self._cards[0].set_value("smooth", value)

    @property
    def scale(self) -> float:
        """Get or set the Compensation scale factor (STAGE=3 only).
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        """Set the scale property."""
        self._cards[0].set_value("scale", value)

    @property
    def dbname(self) -> str:
        """Get or set the File name for binary compensation database(DEFAULT=lscomp)
        """ # nopep8
        return self._cards[1].get_value("dbname")

    @dbname.setter
    def dbname(self, value: str) -> None:
        """Set the dbname property."""
        self._cards[1].set_value("dbname", value)

    @property
    def outname(self) -> str:
        """Get or set the File name for keyword output containing new tolls(STAGE=3 only,DEFAULT=lstool)
        """ # nopep8
        return self._cards[2].get_value("outname")

    @outname.setter
    def outname(self, value: str) -> None:
        """Set the outname property."""
        self._cards[2].set_value("outname", value)

    @property
    def psidt_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psidt."""
        return self._get_set_link("PART", self.psidt)

    @psidt_link.setter
    def psidt_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psidt."""
        self.psidt = value.sid

    @property
    def psidb_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psidb."""
        return self._get_set_link("PART", self.psidb)

    @psidb_link.setter
    def psidb_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psidb."""
        self.psidb = value.sid

