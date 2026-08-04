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

"""Module providing the ControlEosUserLibrary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLEOSUSERLIBRARY_CARD0 = (
    FieldSchema("path", str, 0, 80, None),
)

_CONTROLEOSUSERLIBRARY_CARD1 = (
    FieldSchema("conm", float, 0, 10, 1.0),
    FieldSchema("conl", float, 10, 10, 1.0),
    FieldSchema("cont", float, 20, 10, 1.0),
    FieldSchema("conp", float, 20, 10, 100.0),
)

_CONTROLEOSUSERLIBRARY_CARD2 = (
    FieldSchema("sclr", float, 0, 10, 1.0),
    FieldSchema("sclt", float, 10, 10, 1.0),
    FieldSchema("scle", float, 20, 10, 1.0),
    FieldSchema("sclp", float, 30, 10, 1.0),
)

class ControlEosUserLibrary(KeywordBase):
    """DYNA CONTROL_EOS_USER_LIBRARY keyword"""

    keyword = "CONTROL"
    subkeyword = "EOS_USER_LIBRARY"

    def __init__(self, **kwargs):
        """Initialize the ControlEosUserLibrary class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLEOSUSERLIBRARY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLEOSUSERLIBRARY_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLEOSUSERLIBRARY_CARD2,
                **kwargs,
            ),
        ]
    @property
    def path(self) -> typing.Optional[str]:
        """Get or set the Path to the library seslib. The default path is the current working directory.
        For the ASCII keyword option, the path is replaced by the name of an ASCII file containing data tables.The filename should include the path if the file is not in the current directory.
        """ # nopep8
        return self._cards[0].get_value("path")

    @path.setter
    def path(self, value: str) -> None:
        """Set the path property."""
        self._cards[0].set_value("path", value)

    @property
    def conm(self) -> float:
        """Get or set the Scaling factors for the mass conversion from the input deck units to the library units
        """ # nopep8
        return self._cards[1].get_value("conm")

    @conm.setter
    def conm(self, value: float) -> None:
        """Set the conm property."""
        self._cards[1].set_value("conm", value)

    @property
    def conl(self) -> float:
        """Get or set the Scaling factors for length cononversion from the input deck units to the library units
        """ # nopep8
        return self._cards[1].get_value("conl")

    @conl.setter
    def conl(self, value: float) -> None:
        """Set the conl property."""
        self._cards[1].set_value("conl", value)

    @property
    def cont(self) -> float:
        """Get or set the Scaling factors for time conversion from the input deck units to the library units
        """ # nopep8
        return self._cards[1].get_value("cont")

    @cont.setter
    def cont(self, value: float) -> None:
        """Set the cont property."""
        self._cards[1].set_value("cont", value)

    @property
    def conp(self) -> float:
        """Get or set the Scaling factors for pressure conversion from the input deck units to the library units
        """ # nopep8
        return self._cards[1].get_value("conp")

    @conp.setter
    def conp(self, value: float) -> None:
        """Set the conp property."""
        self._cards[1].set_value("conp", value)

    @property
    def sclr(self) -> float:
        """Get or set the Plot scaling factor for densities
        """ # nopep8
        return self._cards[2].get_value("sclr")

    @sclr.setter
    def sclr(self, value: float) -> None:
        """Set the sclr property."""
        self._cards[2].set_value("sclr", value)

    @property
    def sclt(self) -> float:
        """Get or set the Plot scaling factor for temperatures
        """ # nopep8
        return self._cards[2].get_value("sclt")

    @sclt.setter
    def sclt(self, value: float) -> None:
        """Set the sclt property."""
        self._cards[2].set_value("sclt", value)

    @property
    def scle(self) -> float:
        """Get or set the Plot scaling factor for energies
        """ # nopep8
        return self._cards[2].get_value("scle")

    @scle.setter
    def scle(self, value: float) -> None:
        """Set the scle property."""
        self._cards[2].set_value("scle", value)

    @property
    def sclp(self) -> float:
        """Get or set the Plot scaling factor for pressures
        """ # nopep8
        return self._cards[2].get_value("sclp")

    @sclp.setter
    def sclp(self, value: float) -> None:
        """Set the sclp property."""
        self._cards[2].set_value("sclp", value)

