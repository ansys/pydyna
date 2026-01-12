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

"""Module providing the SetShellGeneralCollect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETSHELLGENERALCOLLECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
)

_SETSHELLGENERALCOLLECT_CARD1 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

class SetShellGeneralCollect(KeywordBase):
    """DYNA SET_SHELL_GENERAL_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "SHELL_GENERAL_COLLECT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetShellGeneralCollect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETSHELLGENERALCOLLECT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETSHELLGENERALCOLLECT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetShellGeneralCollect.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Shell element set ID. All shell sets should have a unique set ID.
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
    def option(self) -> str:
        """Get or set the OPTION.EQ.ALL: All shell elements will be included in the set,
        OPTION.EQ.ELEM: Shell elements E1...E7 will be included in the current set,
        OPTION.EQ.DELEM: Shell elements E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from parts E1...E7 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from parts E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside boxes E1...E7 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside boxes E1...E7 previously added will be excluded from the current set.
        OPTION.EQ.SALECPT:Elements inside a box for a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4,and E5 correspond to IMIN, IMAX, JMIN,and JMAX, respectively.They are the minimumand the maximum nodal indices along each direction in the S - ALE mesh.This option is only to be used for a Structured ALE mesh.It can be used with SALEFAC to generate a shell set but should not be used with other “_‌GENERAL” options.
        OPTION.EQ.SALEFAC:Elements on the face of a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, and E5 correspond to the - X, +X, -Y, and +Y faces, respectively.Assigning 1 to these 4 values would include all the boundary elements at these faces in the shell element set.This option is only to be used for a Structured ALE mesh.It can be used with SALECPT to generate a shell set but should not be used with other “_GENERAL” options.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "ELEM", "DELEM", "PART", "DPART", "BOX", "DBOX", "SALECPT", "SALEFAC", None]:
            raise Exception("""option must be `None` or one of {"ALL","ELEM","DELEM","PART","DPART","BOX","DBOX","SALECPT","SALEFAC"}.""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E1 not used,
        OPTION.EQ.ELEM: Shell element E1 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E1 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E1 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E1 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E1 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E1 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E2 not used,
        OPTION.EQ.ELEM: Shell element E2 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E2 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E2 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E2 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E2 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E2 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E3 not used,
        OPTION.EQ.ELEM: Shell element E3 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E3 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E3 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E3 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E3 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E3 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E4 not used,
        OPTION.EQ.ELEM: Shell element E4 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E4 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E4 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E4 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E4 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E4 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E5 not used,
        OPTION.EQ.ELEM: Shell element E5 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E5 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E5 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E5 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E5 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E5 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E6 not used,
        OPTION.EQ.ELEM: Shell element E6 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E6 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E6 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E6 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E6 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E6 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E7 not used,
        OPTION.EQ.ELEM: Shell element E7 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E7 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E7 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E7 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E7 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E7 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

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

