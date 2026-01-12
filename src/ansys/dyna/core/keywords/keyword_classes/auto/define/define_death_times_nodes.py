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

"""Module providing the DefineDeathTimesNodes class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDEATHTIMESNODES_CARD0 = (
    FieldSchema("geo", int, 0, 10, None),
    FieldSchema("n1", int, 10, 10, None),
    FieldSchema("n2", int, 20, 10, None),
    FieldSchema("n3", int, 30, 10, None),
)

_DEFINEDEATHTIMESNODES_CARD1 = (
    FieldSchema("x_t", float, 0, 10, None),
    FieldSchema("y_t", float, 10, 10, None),
    FieldSchema("z_t", float, 20, 10, None),
    FieldSchema("x_h", float, 30, 10, None),
    FieldSchema("y_h", float, 40, 10, None),
    FieldSchema("x_h", float, 50, 10, None),
    FieldSchema("r", float, 60, 10, None),
    FieldSchema("flag", int, 70, 10, None),
)

_DEFINEDEATHTIMESNODES_CARD2 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
    FieldSchema("nid4", int, 30, 10, None),
    FieldSchema("nid5", int, 40, 10, None),
    FieldSchema("nid6", int, 50, 10, None),
    FieldSchema("nid7", int, 60, 10, None),
    FieldSchema("nid8", int, 70, 10, None),
)

class DefineDeathTimesNodes(KeywordBase):
    """DYNA DEFINE_DEATH_TIMES_NODES keyword"""

    keyword = "DEFINE"
    subkeyword = "DEATH_TIMES_NODES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeathTimesNodes class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEATHTIMESNODES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEATHTIMESNODES_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEATHTIMESNODES_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeathTimesNodes.option_specs[0],
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
    def geo(self) -> typing.Optional[int]:
        """Get or set the Geometric entity type. =1 plane, =2 infinite cylinder, =3 sphere
        """ # nopep8
        return self._cards[0].get_value("geo")

    @geo.setter
    def geo(self, value: int) -> None:
        """Set the geo property."""
        self._cards[0].set_value("geo", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node defining the origin of the geometric entity (optional).
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node defining the tail of the orientation vector (optional).
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node defining the head of the orientation vector (optional).
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def x_t(self) -> typing.Optional[float]:
        """Get or set the X coordinate of the origin of the geometric entity and the tail of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("x_t")

    @x_t.setter
    def x_t(self, value: float) -> None:
        """Set the x_t property."""
        self._cards[1].set_value("x_t", value)

    @property
    def y_t(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of the origin of the geometric entity and the tail of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("y_t")

    @y_t.setter
    def y_t(self, value: float) -> None:
        """Set the y_t property."""
        self._cards[1].set_value("y_t", value)

    @property
    def z_t(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of the origin of the geometric entity and the tail of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("z_t")

    @z_t.setter
    def z_t(self, value: float) -> None:
        """Set the z_t property."""
        self._cards[1].set_value("z_t", value)

    @property
    def x_h(self) -> typing.Optional[float]:
        """Get or set the X coordinate of the head of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("x_h")

    @x_h.setter
    def x_h(self, value: float) -> None:
        """Set the x_h property."""
        self._cards[1].set_value("x_h", value)

    @property
    def y_h(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of the head of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("y_h")

    @y_h.setter
    def y_h(self, value: float) -> None:
        """Set the y_h property."""
        self._cards[1].set_value("y_h", value)

    @property
    def x_h(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of the head of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("x_h")

    @x_h.setter
    def x_h(self, value: float) -> None:
        """Set the x_h property."""
        self._cards[1].set_value("x_h", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of cylinder or sphere.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[1].set_value("r", value)

    @property
    def flag(self) -> typing.Optional[int]:
        """Get or set the +1 for killing motion when the node is outside of the geometric entity or on the positive side of the plane as defined by the normal direction, or -1 for the inside.
        """ # nopep8
        return self._cards[1].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        """Set the flag property."""
        self._cards[1].set_value("flag", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the First node ID
        """ # nopep8
        return self._cards[2].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[2].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Second node ID.
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[2].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Third node ID
        """ # nopep8
        return self._cards[2].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[2].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Fourth node ID.
        """ # nopep8
        return self._cards[2].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[2].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Fifth node ID
        """ # nopep8
        return self._cards[2].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[2].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Sixth node ID
        """ # nopep8
        return self._cards[2].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[2].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Seventh node ID.
        """ # nopep8
        return self._cards[2].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        """Set the nid7 property."""
        self._cards[2].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Eighth node ID.
        """ # nopep8
        return self._cards[2].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        """Set the nid8 property."""
        self._cards[2].set_value("nid8", value)

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

