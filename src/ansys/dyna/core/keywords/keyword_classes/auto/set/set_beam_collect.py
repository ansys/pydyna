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

"""Module providing the SetBeamCollect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETBEAMCOLLECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
)

_SETBEAMCOLLECT_CARD1 = (
    FieldSchema("k1", int, 0, 10, None),
    FieldSchema("k2", int, 10, 10, None),
    FieldSchema("k3", int, 20, 10, None),
    FieldSchema("k4", int, 30, 10, None),
    FieldSchema("k5", int, 40, 10, None),
    FieldSchema("k6", int, 50, 10, None),
    FieldSchema("k7", int, 60, 10, None),
    FieldSchema("k8", int, 70, 10, None),
)

_SETBEAMCOLLECT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetBeamCollect(KeywordBase):
    """DYNA SET_BEAM_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "BEAM_COLLECT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "k1": LinkType.ELEMENT_BEAM,
        "k2": LinkType.ELEMENT_BEAM,
        "k3": LinkType.ELEMENT_BEAM,
        "k4": LinkType.ELEMENT_BEAM,
        "k5": LinkType.ELEMENT_BEAM,
        "k6": LinkType.ELEMENT_BEAM,
        "k7": LinkType.ELEMENT_BEAM,
        "k8": LinkType.ELEMENT_BEAM,
    }

    def __init__(self, **kwargs):
        """Initialize the SetBeamCollect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETBEAMCOLLECT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETBEAMCOLLECT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetBeamCollect.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETBEAMCOLLECT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Beam element set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def k1(self) -> typing.Optional[int]:
        """Get or set the First beam ID.
        """ # nopep8
        return self._cards[1].get_value("k1")

    @k1.setter
    def k1(self, value: int) -> None:
        """Set the k1 property."""
        self._cards[1].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[int]:
        """Get or set the Second beam ID.
        """ # nopep8
        return self._cards[1].get_value("k2")

    @k2.setter
    def k2(self, value: int) -> None:
        """Set the k2 property."""
        self._cards[1].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[int]:
        """Get or set the Third beam ID.
        """ # nopep8
        return self._cards[1].get_value("k3")

    @k3.setter
    def k3(self, value: int) -> None:
        """Set the k3 property."""
        self._cards[1].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[int]:
        """Get or set the Fourth beam ID.
        """ # nopep8
        return self._cards[1].get_value("k4")

    @k4.setter
    def k4(self, value: int) -> None:
        """Set the k4 property."""
        self._cards[1].set_value("k4", value)

    @property
    def k5(self) -> typing.Optional[int]:
        """Get or set the Fifth beam ID.
        """ # nopep8
        return self._cards[1].get_value("k5")

    @k5.setter
    def k5(self, value: int) -> None:
        """Set the k5 property."""
        self._cards[1].set_value("k5", value)

    @property
    def k6(self) -> typing.Optional[int]:
        """Get or set the Sixth beam ID.
        """ # nopep8
        return self._cards[1].get_value("k6")

    @k6.setter
    def k6(self, value: int) -> None:
        """Set the k6 property."""
        self._cards[1].set_value("k6", value)

    @property
    def k7(self) -> typing.Optional[int]:
        """Get or set the Seventh beam ID.
        """ # nopep8
        return self._cards[1].get_value("k7")

    @k7.setter
    def k7(self, value: int) -> None:
        """Set the k7 property."""
        self._cards[1].set_value("k7", value)

    @property
    def k8(self) -> typing.Optional[int]:
        """Get or set the Eighth beam ID.
        """ # nopep8
        return self._cards[1].get_value("k8")

    @k8.setter
    def k8(self, value: int) -> None:
        """Set the k8 property."""
        self._cards[1].set_value("k8", value)

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

    @property
    def k1_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k1."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k1, "parts")

    @property
    def k2_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k2."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k2, "parts")

    @property
    def k3_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k3."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k3, "parts")

    @property
    def k4_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k4."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k4, "parts")

    @property
    def k5_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k5."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k5, "parts")

    @property
    def k6_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k6."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k6, "parts")

    @property
    def k7_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k7."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k7, "parts")

    @property
    def k8_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given k8."""
        return self._get_link_by_attr("ELEMENT", "eid", self.k8, "parts")

