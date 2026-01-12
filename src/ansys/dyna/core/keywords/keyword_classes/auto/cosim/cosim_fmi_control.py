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

"""Module providing the CosimFmiControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_COSIMFMICONTROL_CARD0 = (
    FieldSchema("appid", str, 0, 20, None),
    FieldSchema("opt", str, 20, 10, "G"),
    FieldSchema("mode", str, 30, 10, "P"),
    FieldSchema("fmi", int, 40, 10, 0),
)

_COSIMFMICONTROL_CARD1 = (
    FieldSchema("setting", str, 0, 80, None),
)

class CosimFmiControl(KeywordBase):
    """DYNA COSIM_FMI_CONTROL keyword"""

    keyword = "COSIM"
    subkeyword = "FMI_CONTROL"

    def __init__(self, **kwargs):
        """Initialize the CosimFmiControl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _COSIMFMICONTROL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _COSIMFMICONTROL_CARD1,
                **kwargs,
            ),        ]
    @property
    def appid(self) -> typing.Optional[str]:
        """Get or set the FMU identification. Each FMU must have a unique APPID
        """ # nopep8
        return self._cards[0].get_value("appid")

    @appid.setter
    def appid(self, value: str) -> None:
        """Set the appid property."""
        self._cards[0].set_value("appid", value)

    @property
    def opt(self) -> str:
        """Get or set the LS-DYNA's role (see Remark 1):
        EQ.G:	Generation mode.LS - DYNA will generate a new FMU.
        EQ.C : Co - simulation mode.LS - DYNA will co - simulate with an existing FMU.
        """ # nopep8
        return self._cards[0].get_value("opt")

    @opt.setter
    def opt(self, value: str) -> None:
        """Set the opt property."""
        if value not in ["G", "C", None]:
            raise Exception("""opt must be `None` or one of {"G","C"}.""")
        self._cards[0].set_value("opt", value)

    @property
    def mode(self) -> str:
        """Get or set the LS-DYNA's mode
        EQ.P:	LS - DYNA is Primary,and another software is secondary.
        EQ.S : LS - DYNA is Secondary,and another software is primary..
        """ # nopep8
        return self._cards[0].get_value("mode")

    @mode.setter
    def mode(self, value: str) -> None:
        """Set the mode property."""
        if value not in ["P", "S", None]:
            raise Exception("""mode must be `None` or one of {"P","S"}.""")
        self._cards[0].set_value("mode", value)

    @property
    def fmi(self) -> int:
        """Get or set the FMI1.0 or FMI2.0
        EQ.1: the generated or co - simulated FMU is based on FMI1.0 standard.
        EQ.0 or 2 : the generated or co - simulated FMU is based on FMI2.0 standard.If the FMI option is left blank, the default FMU 2 will be used.
        """ # nopep8
        return self._cards[0].get_value("fmi")

    @fmi.setter
    def fmi(self, value: int) -> None:
        """Set the fmi property."""
        self._cards[0].set_value("fmi", value)

    @property
    def setting(self) -> typing.Optional[str]:
        """Get or set the Settings for the generating or using the FMU. Up to 80 characters per line. Multiple lines are allowed for each setting and multiple settings can be defined for each FMU
        """ # nopep8
        return self._cards[1].get_value("setting")

    @setting.setter
    def setting(self, value: str) -> None:
        """Set the setting property."""
        self._cards[1].set_value("setting", value)

