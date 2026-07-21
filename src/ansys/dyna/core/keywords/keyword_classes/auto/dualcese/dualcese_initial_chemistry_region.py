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

"""Module providing the DualceseInitialChemistryRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEINITIALCHEMISTRYREGION_CARD0 = (
    FieldSchema("reg_id", int, 0, 10, None),
    FieldSchema("chemid", int, 10, 10, None),
    FieldSchema("compid", int, 20, 10, None),
)

_DUALCESEINITIALCHEMISTRYREGION_CARD1 = (
    FieldSchema("uic", float, 0, 10, None),
    FieldSchema("vic", float, 10, 10, None),
    FieldSchema("wic", float, 20, 10, None),
    FieldSchema("rhoc", float, 30, 10, None),
    FieldSchema("pic", float, 40, 10, None),
    FieldSchema("tic", float, 50, 10, None),
    FieldSchema("hic", float, 60, 10, None),
)

class DualceseInitialChemistryRegion(KeywordBase):
    """DYNA DUALCESE_INITIAL_CHEMISTRY_REGION keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_CHEMISTRY_REGION"

    def __init__(self, **kwargs):
        """Initialize the DualceseInitialChemistryRegion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALCHEMISTRYREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALCHEMISTRYREGION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def reg_id(self) -> typing.Optional[int]:
        """Get or set the Region Identifier (see *DEFINE_REGION).
        """ # nopep8
        return self._cards[0].get_value("reg_id")

    @reg_id.setter
    def reg_id(self, value: int) -> None:
        """Set the reg_id property."""
        self._cards[0].set_value("reg_id", value)

    @property
    def chemid(self) -> typing.Optional[int]:
        """Get or set the Identifier of chemistry control card to use (see *CHEMISTRY_CONTROL_FULL).
        """ # nopep8
        return self._cards[0].get_value("chemid")

    @chemid.setter
    def chemid(self, value: int) -> None:
        """Set the chemid property."""
        self._cards[0].set_value("chemid", value)

    @property
    def compid(self) -> typing.Optional[int]:
        """Get or set the Identifier of chemical composition to use (see *CHEMISTRY_COMPOSITION)
        """ # nopep8
        return self._cards[0].get_value("compid")

    @compid.setter
    def compid(self, value: int) -> None:
        """Set the compid property."""
        self._cards[0].set_value("compid", value)

    @property
    def uic(self) -> typing.Optional[float]:
        """Get or set the X-component of the fluid velocity
        """ # nopep8
        return self._cards[1].get_value("uic")

    @uic.setter
    def uic(self, value: float) -> None:
        """Set the uic property."""
        self._cards[1].set_value("uic", value)

    @property
    def vic(self) -> typing.Optional[float]:
        """Get or set the Y-component of the fluid velocity
        """ # nopep8
        return self._cards[1].get_value("vic")

    @vic.setter
    def vic(self, value: float) -> None:
        """Set the vic property."""
        self._cards[1].set_value("vic", value)

    @property
    def wic(self) -> typing.Optional[float]:
        """Get or set the Z-component of the fluid velocity
        """ # nopep8
        return self._cards[1].get_value("wic")

    @wic.setter
    def wic(self, value: float) -> None:
        """Set the wic property."""
        self._cards[1].set_value("wic", value)

    @property
    def rhoc(self) -> typing.Optional[float]:
        """Get or set the [Not used] Initial fluid density
        """ # nopep8
        return self._cards[1].get_value("rhoc")

    @rhoc.setter
    def rhoc(self, value: float) -> None:
        """Set the rhoc property."""
        self._cards[1].set_value("rhoc", value)

    @property
    def pic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid pressure
        """ # nopep8
        return self._cards[1].get_value("pic")

    @pic.setter
    def pic(self, value: float) -> None:
        """Set the pic property."""
        self._cards[1].set_value("pic", value)

    @property
    def tic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid temperature
        """ # nopep8
        return self._cards[1].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        """Set the tic property."""
        self._cards[1].set_value("tic", value)

    @property
    def hic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid enthalpy.
        """ # nopep8
        return self._cards[1].get_value("hic")

    @hic.setter
    def hic(self, value: float) -> None:
        """Set the hic property."""
        self._cards[1].set_value("hic", value)

