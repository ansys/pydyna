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

"""Module providing the DefineMultiSheetConnectors class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEMULTISHEETCONNECTORS_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 1),
    FieldSchema("nsheets", int, 20, 10, None),
)

_DEFINEMULTISHEETCONNECTORS_CARD1 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("jnt12", int, 10, 10, None),
    FieldSchema("pid2", int, 20, 10, None),
    FieldSchema("jnt23", int, 30, 10, None),
    FieldSchema("pid3", int, 40, 10, None),
    FieldSchema("jnt34", int, 50, 10, None),
    FieldSchema("pid5", int, 60, 10, None),
)

_DEFINEMULTISHEETCONNECTORS_CARD2 = (
    FieldSchema("parm1", float, 0, 10, None),
    FieldSchema("parm2", float, 10, 10, None),
    FieldSchema("parm3", float, 20, 10, None),
    FieldSchema("parm4", float, 30, 10, None),
    FieldSchema("parm5", float, 40, 10, None),
    FieldSchema("parm6", float, 50, 10, None),
    FieldSchema("parm7", float, 60, 10, None),
    FieldSchema("parm8", float, 70, 10, None),
)

_DEFINEMULTISHEETCONNECTORS_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineMultiSheetConnectors(KeywordBase):
    """DYNA DEFINE_MULTI_SHEET_CONNECTORS keyword"""

    keyword = "DEFINE"
    subkeyword = "MULTI_SHEET_CONNECTORS"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "jnt12": LinkType.ELEMENT_SHELL,
        "jnt23": LinkType.ELEMENT_SHELL,
        "jnt34": LinkType.ELEMENT_SHELL,
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
        "pid3": LinkType.PART,
        "pid5": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineMultiSheetConnectors class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEMULTISHEETCONNECTORS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEMULTISHEETCONNECTORS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEMULTISHEETCONNECTORS_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineMultiSheetConnectors._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEMULTISHEETCONNECTORS_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Coupled node/node set
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the Material model of joint elements:
        EQ.1: *MAT_SPOTWELD_DAIMLERCHRYSLER.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def nsheets(self) -> typing.Optional[int]:
        """Get or set the Number of sheets connected with this multi sheet connector
        """ # nopep8
        return self._cards[0].get_value("nsheets")

    @nsheets.setter
    def nsheets(self, value: int) -> None:
        """Set the nsheets property."""
        self._cards[0].set_value("nsheets", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID of sheet number 1
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[1].set_value("pid1", value)

    @property
    def jnt12(self) -> typing.Optional[int]:
        """Get or set the ID of joining element between sheet 1 and 2.
        """ # nopep8
        return self._cards[1].get_value("jnt12")

    @jnt12.setter
    def jnt12(self, value: int) -> None:
        """Set the jnt12 property."""
        self._cards[1].set_value("jnt12", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID of sheet number 2
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[1].set_value("pid2", value)

    @property
    def jnt23(self) -> typing.Optional[int]:
        """Get or set the ID of joining element between sheet 2 and 3.
        """ # nopep8
        return self._cards[1].get_value("jnt23")

    @jnt23.setter
    def jnt23(self, value: int) -> None:
        """Set the jnt23 property."""
        self._cards[1].set_value("jnt23", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part ID of sheet number 3
        """ # nopep8
        return self._cards[1].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        """Set the pid3 property."""
        self._cards[1].set_value("pid3", value)

    @property
    def jnt34(self) -> typing.Optional[int]:
        """Get or set the ID of joining element between sheet 3 and 4.
        """ # nopep8
        return self._cards[1].get_value("jnt34")

    @jnt34.setter
    def jnt34(self, value: int) -> None:
        """Set the jnt34 property."""
        self._cards[1].set_value("jnt34", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part ID of sheet number 4
        """ # nopep8
        return self._cards[1].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        """Set the pid5 property."""
        self._cards[1].set_value("pid5", value)

    @property
    def parm1(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm1")

    @parm1.setter
    def parm1(self, value: float) -> None:
        """Set the parm1 property."""
        self._cards[2].set_value("parm1", value)

    @property
    def parm2(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm2")

    @parm2.setter
    def parm2(self, value: float) -> None:
        """Set the parm2 property."""
        self._cards[2].set_value("parm2", value)

    @property
    def parm3(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm3")

    @parm3.setter
    def parm3(self, value: float) -> None:
        """Set the parm3 property."""
        self._cards[2].set_value("parm3", value)

    @property
    def parm4(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm4")

    @parm4.setter
    def parm4(self, value: float) -> None:
        """Set the parm4 property."""
        self._cards[2].set_value("parm4", value)

    @property
    def parm5(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm5")

    @parm5.setter
    def parm5(self, value: float) -> None:
        """Set the parm5 property."""
        self._cards[2].set_value("parm5", value)

    @property
    def parm6(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm6")

    @parm6.setter
    def parm6(self, value: float) -> None:
        """Set the parm6 property."""
        self._cards[2].set_value("parm6", value)

    @property
    def parm7(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm7")

    @parm7.setter
    def parm7(self, value: float) -> None:
        """Set the parm7 property."""
        self._cards[2].set_value("parm7", value)

    @property
    def parm8(self) -> typing.Optional[float]:
        """Get or set the Set of user parameters additionally available in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[2].get_value("parm8")

    @parm8.setter
    def parm8(self, value: float) -> None:
        """Set the parm8 property."""
        self._cards[2].set_value("parm8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def jnt12_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given jnt12."""
        return self._get_link_by_attr("ELEMENT", "eid", self.jnt12, "parts")

    @property
    def jnt23_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given jnt23."""
        return self._get_link_by_attr("ELEMENT", "eid", self.jnt23, "parts")

    @property
    def jnt34_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given jnt34."""
        return self._get_link_by_attr("ELEMENT", "eid", self.jnt34, "parts")

    @property
    def pid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

    @property
    def pid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid3."""
        return self._get_link_by_attr("PART", "pid", self.pid3, "parts")

    @property
    def pid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid5."""
        return self._get_link_by_attr("PART", "pid", self.pid5, "parts")

