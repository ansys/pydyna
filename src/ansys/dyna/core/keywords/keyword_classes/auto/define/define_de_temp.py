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

"""Module providing the DefineDeTemp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDETEMP_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sft", float, 30, 10, None),
    FieldSchema("alpha", float, 40, 10, None),
    FieldSchema("ini_temp", float, 50, 10, None),
)

_DEFINEDETEMP_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeTemp(KeywordBase):
    """DYNA DEFINE_DE_TEMP keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TEMP"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeTemp class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETEMP_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeTemp._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDETEMP_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set, part set, or part ID for which the temperature effect applies
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the SID type:
        EQ.0: DES node set
        EQ.2: DES part set
        EQ.3: DES part
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,2,3}.""")
        self._cards[0].set_value("stype", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the temperature as a function of time during the dynamic process
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sft(self) -> typing.Optional[float]:
        """Get or set the Scale factor on the temperature given with LCID
        """ # nopep8
        return self._cards[0].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[0].set_value("sft", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion (1/oC or 1/oF)
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def ini_temp(self) -> typing.Optional[float]:
        """Get or set the Initial temperature of the DES particles (oC or oF)
        """ # nopep8
        return self._cards[0].get_value("ini_temp")

    @ini_temp.setter
    def ini_temp(self, value: float) -> None:
        """Set the ini_temp property."""
        self._cards[0].set_value("ini_temp", value)

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

