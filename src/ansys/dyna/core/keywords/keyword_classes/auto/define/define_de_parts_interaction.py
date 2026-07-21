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

"""Module providing the DefineDePartsInteraction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEDEPARTSINTERACTION_CARD0 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("ndamp", float, 20, 10, 0.0),
    FieldSchema("tdamp", float, 30, 10, 0.0),
    FieldSchema("frics", float, 40, 10, 0.0),
    FieldSchema("fricr", float, 50, 10, 0.0),
    FieldSchema("normk", float, 60, 10, 0.01),
    FieldSchema("sheark", float, 70, 10, None),
)

_DEFINEDEPARTSINTERACTION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDePartsInteraction(KeywordBase):
    """DYNA DEFINE_DE_PARTS_INTERACTION keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_PARTS_INTERACTION"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineDePartsInteraction class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEPARTSINTERACTION_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDePartsInteraction._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEPARTSINTERACTION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID of DES nodes(see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID of DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

    @property
    def ndamp(self) -> float:
        """Get or set the Normal damping coefficient
        """ # nopep8
        return self._cards[0].get_value("ndamp")

    @ndamp.setter
    def ndamp(self, value: float) -> None:
        """Set the ndamp property."""
        self._cards[0].set_value("ndamp", value)

    @property
    def tdamp(self) -> float:
        """Get or set the Tangential damping coefficient
        """ # nopep8
        return self._cards[0].get_value("tdamp")

    @tdamp.setter
    def tdamp(self, value: float) -> None:
        """Set the tdamp property."""
        self._cards[0].set_value("tdamp", value)

    @property
    def frics(self) -> float:
        """Get or set the Static coefficient of friction (see Remark 2):
        EQ.0.0: 3 DOF
        NE.0.0: 6 DOF(consider rotational DOF)
        """ # nopep8
        return self._cards[0].get_value("frics")

    @frics.setter
    def frics(self, value: float) -> None:
        """Set the frics property."""
        self._cards[0].set_value("frics", value)

    @property
    def fricr(self) -> float:
        """Get or set the Rolling friction coefficient
        """ # nopep8
        return self._cards[0].get_value("fricr")

    @fricr.setter
    def fricr(self, value: float) -> None:
        """Set the fricr property."""
        self._cards[0].set_value("fricr", value)

    @property
    def normk(self) -> float:
        """Get or set the Optional scale factor of normal spring constant (Default = 0.01)
        """ # nopep8
        return self._cards[0].get_value("normk")

    @normk.setter
    def normk(self, value: float) -> None:
        """Set the normk property."""
        self._cards[0].set_value("normk", value)

    @property
    def sheark(self) -> typing.Optional[float]:
        """Get or set the Optional ratio between SHEARK/NORMK (Default = 2/7)
        """ # nopep8
        return self._cards[0].get_value("sheark")

    @sheark.setter
    def sheark(self, value: float) -> None:
        """Set the sheark property."""
        self._cards[0].set_value("sheark", value)

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
    def pid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

