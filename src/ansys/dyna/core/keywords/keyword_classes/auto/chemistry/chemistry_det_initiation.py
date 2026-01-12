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

"""Module providing the ChemistryDetInitiation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CHEMISTRYDETINITIATION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("compid", int, 10, 10, None),
    FieldSchema("nmesh", int, 20, 10, None),
    FieldSchema("dlen", float, 30, 10, None),
    FieldSchema("cfl", float, 40, 10, None),
    FieldSchema("tlimit", float, 50, 10, None),
    FieldSchema("xyzd", float, 60, 10, None),
    FieldSchema("detdir", int, 70, 10, None),
)

_CHEMISTRYDETINITIATION_CARD1 = (
    FieldSchema("file", str, 0, 256, None),
)

class ChemistryDetInitiation(KeywordBase):
    """DYNA CHEMISTRY_DET_INITIATION keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "DET_INITIATION"

    def __init__(self, **kwargs):
        """Initialize the ChemistryDetInitiation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYDETINITIATION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CHEMISTRYDETINITIATION_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this one-dimensional detonation computation.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def compid(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use.
        """ # nopep8
        return self._cards[0].get_value("compid")

    @compid.setter
    def compid(self, value: int) -> None:
        """Set the compid property."""
        self._cards[0].set_value("compid", value)

    @property
    def nmesh(self) -> typing.Optional[int]:
        """Get or set the Number of equal-width elements in the one-dimensional domain.
        """ # nopep8
        return self._cards[0].get_value("nmesh")

    @nmesh.setter
    def nmesh(self, value: int) -> None:
        """Set the nmesh property."""
        self._cards[0].set_value("nmesh", value)

    @property
    def dlen(self) -> typing.Optional[float]:
        """Get or set the Length of the one-dimensional domain.
        """ # nopep8
        return self._cards[0].get_value("dlen")

    @dlen.setter
    def dlen(self, value: float) -> None:
        """Set the dlen property."""
        self._cards[0].set_value("dlen", value)

    @property
    def cfl(self) -> typing.Optional[float]:
        """Get or set the Time-step limiting factor.
        """ # nopep8
        return self._cards[0].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[0].set_value("cfl", value)

    @property
    def tlimit(self) -> typing.Optional[float]:
        """Get or set the Time limit for the simulation
        """ # nopep8
        return self._cards[0].get_value("tlimit")

    @tlimit.setter
    def tlimit(self, value: float) -> None:
        """Set the tlimit property."""
        self._cards[0].set_value("tlimit", value)

    @property
    def xyzd(self) -> typing.Optional[float]:
        """Get or set the Position of the detonation front in the DETDIR direction.
        """ # nopep8
        return self._cards[0].get_value("xyzd")

    @xyzd.setter
    def xyzd(self, value: float) -> None:
        """Set the xyzd property."""
        self._cards[0].set_value("xyzd", value)

    @property
    def detdir(self) -> typing.Optional[int]:
        """Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
        """ # nopep8
        return self._cards[0].get_value("detdir")

    @detdir.setter
    def detdir(self, value: int) -> None:
        """Set the detdir property."""
        self._cards[0].set_value("detdir", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the LSDA file in which to write the one-dimensional solution.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        """Set the file property."""
        self._cards[1].set_value("file", value)

