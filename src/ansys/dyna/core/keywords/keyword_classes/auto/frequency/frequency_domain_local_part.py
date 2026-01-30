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

"""Module providing the FrequencyDomainLocalPart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_FREQUENCYDOMAINLOCALPART_CARD0 = (
    FieldSchema("id1", int, 0, 10, None),
    FieldSchema("id2", int, 10, 10, None),
    FieldSchema("id3", int, 20, 10, None),
    FieldSchema("id4", int, 30, 10, None),
    FieldSchema("id5", int, 40, 10, None),
    FieldSchema("id6", int, 50, 10, None),
    FieldSchema("id7", int, 60, 10, None),
    FieldSchema("id8", int, 70, 10, None),
)

class FrequencyDomainLocalPart(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_LOCAL_PART keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_LOCAL_PART"
    _link_fields = {
        "id1": LinkType.PART,
        "id2": LinkType.PART,
        "id3": LinkType.PART,
        "id4": LinkType.PART,
        "id5": LinkType.PART,
        "id6": LinkType.PART,
        "id7": LinkType.PART,
        "id8": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainLocalPart class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINLOCALPART_CARD0,
                **kwargs,
            ),        ]
    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        """Set the id1 property."""
        self._cards[0].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        """Set the id2 property."""
        self._cards[0].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        """Set the id3 property."""
        self._cards[0].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        """Set the id4 property."""
        self._cards[0].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        """Set the id5 property."""
        self._cards[0].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        """Set the id6 property."""
        self._cards[0].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        """Set the id7 property."""
        self._cards[0].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[0].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        """Set the id8 property."""
        self._cards[0].set_value("id8", value)

    @property
    def id1_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id1."""
        return self._get_link_by_attr("PART", "pid", self.id1, "parts")

    @property
    def id2_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id2."""
        return self._get_link_by_attr("PART", "pid", self.id2, "parts")

    @property
    def id3_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id3."""
        return self._get_link_by_attr("PART", "pid", self.id3, "parts")

    @property
    def id4_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id4."""
        return self._get_link_by_attr("PART", "pid", self.id4, "parts")

    @property
    def id5_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id5."""
        return self._get_link_by_attr("PART", "pid", self.id5, "parts")

    @property
    def id6_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id6."""
        return self._get_link_by_attr("PART", "pid", self.id6, "parts")

    @property
    def id7_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id7."""
        return self._get_link_by_attr("PART", "pid", self.id7, "parts")

    @property
    def id8_link(self) -> KeywordBase:
        """Get the PART keyword containing the given id8."""
        return self._get_link_by_attr("PART", "pid", self.id8, "parts")

