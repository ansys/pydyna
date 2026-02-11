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

"""Module providing the ControlFormingShellToTshell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLFORMINGSHELLTOTSHELL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("thick", float, 10, 10, None),
    FieldSchema("midsf", float, 20, 10, 0.0),
    FieldSchema("idsegb", float, 30, 10, None),
    FieldSchema("idsegt", float, 40, 10, None),
)

class ControlFormingShellToTshell(KeywordBase):
    """DYNA CONTROL_FORMING_SHELL_TO_TSHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_SHELL_TO_TSHELL"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingShellToTshell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGSHELLTOTSHELL_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the thin shell elements.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Thickness of the thick shell elements.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[0].set_value("thick", value)

    @property
    def midsf(self) -> float:
        """Get or set the TSHELL’s mid-plane position definition (see Figure 0-1 and Remark 4):
        EQ.0:	Mid - plane is at thin shell surface.
        EQ.1 : Mid - plane is at one half of THICK above thin shell surface.
        EQ. - 1 : Mid - plane is at one half of THICK below thin shell surface.
        """ # nopep8
        return self._cards[0].get_value("midsf")

    @midsf.setter
    def midsf(self, value: float) -> None:
        """Set the midsf property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""midsf must be `None` or one of {0,1,-1}.""")
        self._cards[0].set_value("midsf", value)

    @property
    def idsegb(self) -> typing.Optional[float]:
        """Get or set the Set ID of the segments to be generated at the bottom layer of the TSHELLs, which can be used for segment-based contact.  The bottom layer of the TSHELLs has an outward normal that points in the opposite direction to the positive normal side of thin shells
        """ # nopep8
        return self._cards[0].get_value("idsegb")

    @idsegb.setter
    def idsegb(self, value: float) -> None:
        """Set the idsegb property."""
        self._cards[0].set_value("idsegb", value)

    @property
    def idsegt(self) -> typing.Optional[float]:
        """Get or set the Set ID of the segments to be generated at the top layer of the TSHELLs, which can be used for segment-based contact.  The top side of a TSHELL has an outward normal that points in the same direction as the positive normal side of the thin shells
        """ # nopep8
        return self._cards[0].get_value("idsegt")

    @idsegt.setter
    def idsegt(self, value: float) -> None:
        """Set the idsegt property."""
        self._cards[0].set_value("idsegt", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

