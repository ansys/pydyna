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

"""Module providing the InitialVolumeFractionNalegp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INITIALVOLUMEFRACTIONNALEGP_CARD0 = (
    FieldSchema("nalegp", int, 0, 10, None),
)

_INITIALVOLUMEFRACTIONNALEGP_CARD1 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("vf1", float, 10, 10, 0.0),
    FieldSchema("vf2", float, 20, 10, 0.0),
    FieldSchema("vf3", float, 30, 10, 0.0),
    FieldSchema("vf4", float, 40, 10, 0.0),
    FieldSchema("vf5", float, 50, 10, 0.0),
    FieldSchema("vf6", float, 60, 10, 0.0),
    FieldSchema("vf7", float, 70, 10, 0.0),
)

_INITIALVOLUMEFRACTIONNALEGP_CARD2 = (
    FieldSchema("vf", float, 0, 10, 0.0),
    FieldSchema("vf", float, 10, 10, 0.0),
    FieldSchema("vf", float, 20, 10, 0.0),
    FieldSchema("vf", float, 30, 10, 0.0),
    FieldSchema("vf", float, 40, 10, 0.0),
    FieldSchema("vf", float, 50, 10, 0.0),
    FieldSchema("vf", float, 60, 10, 0.0),
    FieldSchema("vf", float, 70, 10, 0.0),
)

class InitialVolumeFractionNalegp(KeywordBase):
    """DYNA INITIAL_VOLUME_FRACTION_NALEGP keyword"""

    keyword = "INITIAL"
    subkeyword = "VOLUME_FRACTION_NALEGP"
    _link_fields = {
        "eid": LinkType.ELEMENT_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialVolumeFractionNalegp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONNALEGP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONNALEGP_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALVOLUMEFRACTIONNALEGP_CARD2,
                **kwargs,
            ),        ]
    @property
    def nalegp(self) -> typing.Optional[int]:
        """Get or set the Count of volume fractions for each element.
        """ # nopep8
        return self._cards[0].get_value("nalegp")

    @nalegp.setter
    def nalegp(self, value: int) -> None:
        """Set the nalegp property."""
        self._cards[0].set_value("nalegp", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID, see also *ELEMENT_OPTION.
        """ # nopep8
        return self._cards[1].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[1].set_value("eid", value)

    @property
    def vf1(self) -> float:
        """Get or set the Volume fraction of multi-material group 1.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("vf1")

    @vf1.setter
    def vf1(self, value: float) -> None:
        """Set the vf1 property."""
        self._cards[1].set_value("vf1", value)

    @property
    def vf2(self) -> float:
        """Get or set the Volume fraction of multi-material group 2.
        Only needed in simulations with 3 material groups. Otherwise VF2=1-VF1.
        """ # nopep8
        return self._cards[1].get_value("vf2")

    @vf2.setter
    def vf2(self, value: float) -> None:
        """Set the vf2 property."""
        self._cards[1].set_value("vf2", value)

    @property
    def vf3(self) -> float:
        """Get or set the Volume fraction of multi-material group 3, AMMGID=3.
        """ # nopep8
        return self._cards[1].get_value("vf3")

    @vf3.setter
    def vf3(self, value: float) -> None:
        """Set the vf3 property."""
        self._cards[1].set_value("vf3", value)

    @property
    def vf4(self) -> float:
        """Get or set the Volume fraction of multi-material group 4, AMMGID=4.
        """ # nopep8
        return self._cards[1].get_value("vf4")

    @vf4.setter
    def vf4(self, value: float) -> None:
        """Set the vf4 property."""
        self._cards[1].set_value("vf4", value)

    @property
    def vf5(self) -> float:
        """Get or set the Volume fraction of multi-material group 5, AMMGID=5.
        """ # nopep8
        return self._cards[1].get_value("vf5")

    @vf5.setter
    def vf5(self, value: float) -> None:
        """Set the vf5 property."""
        self._cards[1].set_value("vf5", value)

    @property
    def vf6(self) -> float:
        """Get or set the Volume fraction of multi-material group 6, AMMGID=6.
        """ # nopep8
        return self._cards[1].get_value("vf6")

    @vf6.setter
    def vf6(self, value: float) -> None:
        """Set the vf6 property."""
        self._cards[1].set_value("vf6", value)

    @property
    def vf7(self) -> float:
        """Get or set the Volume fraction of multi-material group 7, AMMGID=7.
        """ # nopep8
        return self._cards[1].get_value("vf7")

    @vf7.setter
    def vf7(self, value: float) -> None:
        """Set the vf7 property."""
        self._cards[1].set_value("vf7", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def vf(self) -> float:
        """Get or set the Volume fraction of multi-material group.
        """ # nopep8
        return self._cards[2].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        """Set the vf property."""
        self._cards[2].set_value("vf", value)

    @property
    def eid_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given eid."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid, "parts")

