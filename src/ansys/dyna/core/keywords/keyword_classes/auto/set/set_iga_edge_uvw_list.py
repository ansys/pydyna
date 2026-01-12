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

"""Module providing the SetIgaEdgeUvwList class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETIGAEDGEUVWLIST_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
)

_SETIGAEDGEUVWLIST_CARD1 = (
    FieldSchema("eid1", int, 0, 10, None),
    FieldSchema("eid2", int, 10, 10, None),
    FieldSchema("eid3", int, 20, 10, None),
    FieldSchema("eid4", int, 30, 10, None),
    FieldSchema("eid5", int, 40, 10, None),
    FieldSchema("eid6", int, 50, 10, None),
    FieldSchema("eid7", int, 60, 10, None),
    FieldSchema("eid8", int, 70, 10, None),
)

_SETIGAEDGEUVWLIST_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetIgaEdgeUvwList(KeywordBase):
    """DYNA SET_IGA_EDGE_UVW_LIST keyword"""

    keyword = "SET"
    subkeyword = "IGA_EDGE_UVW_LIST"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetIgaEdgeUvwList class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETIGAEDGEUVWLIST_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETIGAEDGEUVWLIST_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetIgaEdgeUvwList.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETIGAEDGEUVWLIST_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First attribute default value; see Remark 1.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second attribute default value.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third attribute default value0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth attribute default value.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the Name of solver using this set (MECH, CESE, etc.). See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        if value not in ["MECH", "CESE", "ICFD", None]:
            raise Exception("""solver must be `None` or one of {"MECH","CESE","ICFD"}.""")
        self._cards[0].set_value("solver", value)

    @property
    def eid1(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid1")

    @eid1.setter
    def eid1(self, value: int) -> None:
        """Set the eid1 property."""
        self._cards[1].set_value("eid1", value)

    @property
    def eid2(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid2")

    @eid2.setter
    def eid2(self, value: int) -> None:
        """Set the eid2 property."""
        self._cards[1].set_value("eid2", value)

    @property
    def eid3(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid3")

    @eid3.setter
    def eid3(self, value: int) -> None:
        """Set the eid3 property."""
        self._cards[1].set_value("eid3", value)

    @property
    def eid4(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid4")

    @eid4.setter
    def eid4(self, value: int) -> None:
        """Set the eid4 property."""
        self._cards[1].set_value("eid4", value)

    @property
    def eid5(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid5")

    @eid5.setter
    def eid5(self, value: int) -> None:
        """Set the eid5 property."""
        self._cards[1].set_value("eid5", value)

    @property
    def eid6(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid6")

    @eid6.setter
    def eid6(self, value: int) -> None:
        """Set the eid6 property."""
        self._cards[1].set_value("eid6", value)

    @property
    def eid7(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid7")

    @eid7.setter
    def eid7(self, value: int) -> None:
        """Set the eid7 property."""
        self._cards[1].set_value("eid7", value)

    @property
    def eid8(self) -> typing.Optional[int]:
        """Get or set the ith parametric edge ID.
        """ # nopep8
        return self._cards[1].get_value("eid8")

    @eid8.setter
    def eid8(self, value: int) -> None:
        """Set the eid8 property."""
        self._cards[1].set_value("eid8", value)

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

