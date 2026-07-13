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

"""Module providing the SetPartGeneralCollect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETPARTGENERALCOLLECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
)

_SETPARTGENERALCOLLECT_CARD1 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

_SETPARTGENERALCOLLECT_CARD2 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

_SETPARTGENERALCOLLECT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetPartGeneralCollect(KeywordBase):
    """DYNA SET_PART_GENERAL_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "PART_GENERAL_COLLECT"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "e1": LinkType.PART,
        "e2": LinkType.PART,
        "e3": LinkType.PART,
        "e4": LinkType.PART,
        "e5": LinkType.PART,
        "e6": LinkType.PART,
        "e7": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the SetPartGeneralCollect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETPARTGENERALCOLLECT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETPARTGENERALCOLLECT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETPARTGENERALCOLLECT_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SetPartGeneralCollect._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETPARTGENERALCOLLECT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Part set ID. All part sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanics.
        EQ.CESE: CE/SE compressible fluid flow solver.
        EQ.ICFD: Incompressible fluid flow solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        if value not in ["MECH", "CESE", "ICFD", None]:
            raise Exception("""solver must be `None` or one of {"MECH","CESE","ICFD"}.""")
        self._cards[0].set_value("solver", value)

    @property
    def option(self) -> str:
        """Get or set the 
        EQ.ALL: All parts will be included in the set.
        EQ.MATTYPE:E1, �, E7 refer to material types, such as 1 for *MAT_ELASTIC and 2 for *MAT_ORTHOTROPIC_ELASTIC. For each provided material type, all parts with the specified material type are added to the part set.
        EQ.PART:Parts E1, ..., E7 will be included.
        EQ.DPART:Parts E1, ..., E7 if previously added will be excluded.
        EQ.SET:Parts of part sets E1, ..., E7 will be included.
        EQ.DSET:Previously added parts that are members of part sets E1, ..., E7 will be excluded.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "MATTYPE", "PART", "DPART", "SET", "DSET", None]:
            raise Exception("""option must be `None` or one of {"ALL","MATTYPE","PART","DPART","SET","DSET"}.""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

    @property
    def option(self) -> str:
        """Get or set the 
        EQ.ALL: All parts will be included in the set.
        EQ.PART:Parts E1, ..., E7 will be included.
        EQ.DPART:Parts E1, ..., E7 if previously added will be excluded.
        EQ.SET:Parts of part sets E1, ..., E7 will be included.
        EQ.DSET:Previously added parts that are members of part sets E1, ..., E7 will be excluded.
        """ # nopep8
        return self._cards[2].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "PART", "DPART", "SET", "DSET", None]:
            raise Exception("""option must be `None` or one of {"ALL","PART","DPART","SET","DSET"}.""")
        self._cards[2].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[2].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[2].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[2].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[2].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[2].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[2].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the Specified entity. Each card must have an option specified.
        """ # nopep8
        return self._cards[2].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[2].set_value("e7", value)

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
    def e1_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e1."""
        return self._get_link_by_attr("PART", "pid", self.e1, "parts")

    @property
    def e2_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e2."""
        return self._get_link_by_attr("PART", "pid", self.e2, "parts")

    @property
    def e3_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e3."""
        return self._get_link_by_attr("PART", "pid", self.e3, "parts")

    @property
    def e4_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e4."""
        return self._get_link_by_attr("PART", "pid", self.e4, "parts")

    @property
    def e5_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e5."""
        return self._get_link_by_attr("PART", "pid", self.e5, "parts")

    @property
    def e6_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e6."""
        return self._get_link_by_attr("PART", "pid", self.e6, "parts")

    @property
    def e7_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given e7."""
        return self._get_link_by_attr("PART", "pid", self.e7, "parts")

