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

"""Module providing the ElementShellComposite class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSHELLCOMPOSITE_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
    FieldSchema("n1", int, 16, 8, None),
    FieldSchema("n2", int, 24, 8, None),
    FieldSchema("n3", int, 32, 8, None),
    FieldSchema("n4", int, 40, 8, None),
    FieldSchema("n5", int, 48, 8, None),
    FieldSchema("n6", int, 56, 8, None),
    FieldSchema("n7", int, 64, 8, None),
    FieldSchema("n8", int, 72, 8, None),
)

_ELEMENTSHELLCOMPOSITE_CARD1 = (
    FieldSchema("mid1", int, 0, 10, None),
    FieldSchema("thick1", float, 10, 10, None),
    FieldSchema("b1", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("mid2", int, 40, 10, None),
    FieldSchema("thick2", float, 50, 10, None),
    FieldSchema("b2", float, 60, 10, None),
)

class ElementShellComposite(KeywordBase):
    """DYNA ELEMENT_SHELL_COMPOSITE keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_COMPOSITE"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
        "mid1": LinkType.MAT,
        "mid2": LinkType.MAT,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementShellComposite class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLCOMPOSITE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLCOMPOSITE_CARD1,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 5.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 6.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[0].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 7.
        """ # nopep8
        return self._cards[0].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[0].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 8.
        """ # nopep8
        return self._cards[0].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[0].set_value("n8", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_ . Section.
        """ # nopep8
        return self._cards[1].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        """Set the mid1 property."""
        self._cards[1].set_value("mid1", value)

    @property
    def thick1(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point i.
        """ # nopep8
        return self._cards[1].get_value("thick1")

    @thick1.setter
    def thick1(self, value: float) -> None:
        """Set the thick1 property."""
        self._cards[1].set_value("thick1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i.
        """ # nopep8
        return self._cards[1].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[1].set_value("b1", value)

    @property
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_ . Section.
        """ # nopep8
        return self._cards[1].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        """Set the mid2 property."""
        self._cards[1].set_value("mid2", value)

    @property
    def thick2(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point i.
        """ # nopep8
        return self._cards[1].get_value("thick2")

    @thick2.setter
    def thick2(self, value: float) -> None:
        """Set the thick2 property."""
        self._cards[1].set_value("thick2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i.
        """ # nopep8
        return self._cards[1].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[1].set_value("b2", value)

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def n5_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def n7_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n7."""
        return self._get_link_by_attr("NODE", "nid", self.n7, "parts")

    @property
    def n8_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n8."""
        return self._get_link_by_attr("NODE", "nid", self.n8, "parts")

    @property
    def mid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid1:
                return kwd
        return None

    @mid1_link.setter
    def mid1_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid1."""
        self.mid1 = value.mid

    @property
    def mid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid2:
                return kwd
        return None

    @mid2_link.setter
    def mid2_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid2."""
        self.mid2 = value.mid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

