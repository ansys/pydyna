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

"""Module providing the DefineCpgAdaptive class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECPGADAPTIVE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
)

_DEFINECPGADAPTIVE_CARD1 = (
    FieldSchema("distsw", int, 0, 10, 0),
    FieldSchema("hmin", float, 10, 10, 0.0),
    FieldSchema("hmax", float, 20, 10, 0.0),
)

_DEFINECPGADAPTIVE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpgAdaptive(KeywordBase):
    """DYNA DEFINE_CPG_ADAPTIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "CPG_ADAPTIVE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCpgAdaptive class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPGADAPTIVE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPGADAPTIVE_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpgAdaptive._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPGADAPTIVE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this adaptivity definition.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def distsw(self) -> int:
        """Get or set the Distance to the wall switch:
        EQ.0:	InactivenQ.1 : Active.The particle size will dynamically vary from HMIN at a distance away from the wall.See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("distsw")

    @distsw.setter
    def distsw(self, value: int) -> None:
        """Set the distsw property."""
        if value not in [0, 1, None]:
            raise Exception("""distsw must be `None` or one of {0,1}.""")
        self._cards[1].set_value("distsw", value)

    @property
    def hmin(self) -> float:
        """Get or set the Particle size at the wall.
        """ # nopep8
        return self._cards[1].get_value("hmin")

    @hmin.setter
    def hmin(self, value: float) -> None:
        """Set the hmin property."""
        self._cards[1].set_value("hmin", value)

    @property
    def hmax(self) -> float:
        """Get or set the Particle size away from the wall.
        """ # nopep8
        return self._cards[1].get_value("hmax")

    @hmax.setter
    def hmax(self, value: float) -> None:
        """Set the hmax property."""
        self._cards[1].set_value("hmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

