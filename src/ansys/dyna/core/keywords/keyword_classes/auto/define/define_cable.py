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

"""Module providing the DefineCable class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINECABLE_CARD0 = (
    FieldSchema("bbpid", int, 0, 10, None),
    FieldSchema("nlayr", int, 10, 10, None),
    FieldSchema("ncirc", int, 20, 10, None),
    FieldSchema("ncycl", int, 30, 10, None),
    FieldSchema("shlpid", int, 40, 10, None),
    FieldSchema("elnr", int, 50, 10, None),
    FieldSchema("ndnr", int, 60, 10, None),
    FieldSchema("trim", int, 70, 10, None),
)

_DEFINECABLE_CARD1 = (
    FieldSchema("sldpid", int, 0, 10, None),
    FieldSchema("sldthk", float, 0, 10, None),
)

_DEFINECABLE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCable(KeywordBase):
    """DYNA DEFINE_CABLE keyword"""

    keyword = "DEFINE"
    subkeyword = "CABLE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "bbpid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCable class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECABLE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECABLE_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCable._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECABLE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def bbpid(self) -> typing.Optional[int]:
        """Get or set the Beam part ID for the cable core. All connected beam elements in the part will model a cable. For MPP, all elements in the beam part will be put on a single processor.
        """ # nopep8
        return self._cards[0].get_value("bbpid")

    @bbpid.setter
    def bbpid(self, value: int) -> None:
        """Set the bbpid property."""
        self._cards[0].set_value("bbpid", value)

    @property
    def nlayr(self) -> typing.Optional[int]:
        """Get or set the Number of solid layers in thickness.
        """ # nopep8
        return self._cards[0].get_value("nlayr")

    @nlayr.setter
    def nlayr(self, value: int) -> None:
        """Set the nlayr property."""
        self._cards[0].set_value("nlayr", value)

    @property
    def ncirc(self) -> typing.Optional[int]:
        """Get or set the Number of solid elements on the circumference.
        """ # nopep8
        return self._cards[0].get_value("ncirc")

    @ncirc.setter
    def ncirc(self, value: int) -> None:
        """Set the ncirc property."""
        self._cards[0].set_value("ncirc", value)

    @property
    def ncycl(self) -> typing.Optional[int]:
        """Get or set the Collect data every NCYCL cycle
        """ # nopep8
        return self._cards[0].get_value("ncycl")

    @ncycl.setter
    def ncycl(self, value: int) -> None:
        """Set the ncycl property."""
        self._cards[0].set_value("ncycl", value)

    @property
    def shlpid(self) -> typing.Optional[int]:
        """Get or set the Optional PID to create shells on solid faces. This is commonly used for extra null shells that may increase contact stability. This needs to be an existing part without any elements associated with it. SHLPID on this card cannot be used by any other *DEFINE_CABLE cards
        """ # nopep8
        return self._cards[0].get_value("shlpid")

    @shlpid.setter
    def shlpid(self, value: int) -> None:
        """Set the shlpid property."""
        self._cards[0].set_value("shlpid", value)

    @property
    def elnr(self) -> typing.Optional[int]:
        """Get or set the Optional starting element number for generated solids. If several cable cards have ELNR that causes collisions, then ELNR will be automatically adjusted.
        """ # nopep8
        return self._cards[0].get_value("elnr")

    @elnr.setter
    def elnr(self, value: int) -> None:
        """Set the elnr property."""
        self._cards[0].set_value("elnr", value)

    @property
    def ndnr(self) -> typing.Optional[int]:
        """Get or set the Optional starting node number for generated nodes. If several cable cards have NDNR that causes collisions, then NDNR will be automatically adjusted.
        """ # nopep8
        return self._cards[0].get_value("ndnr")

    @ndnr.setter
    def ndnr(self, value: int) -> None:
        """Set the ndnr property."""
        self._cards[0].set_value("ndnr", value)

    @property
    def trim(self) -> typing.Optional[int]:
        """Get or set the Set to 1 if adjoint cables need to be trimmed, i.e. a cable connected to the interior of another cable will not create new solids such that there are overlapping solids at the connection.
        """ # nopep8
        return self._cards[0].get_value("trim")

    @trim.setter
    def trim(self, value: int) -> None:
        """Set the trim property."""
        self._cards[0].set_value("trim", value)

    @property
    def sldpid(self) -> typing.Optional[int]:
        """Get or set the Solid part ID for the automatically generated solids. This needs to be an existing part without any elements associated with it. SLDPID on this card cannot be used by any other *DEFINE_CABLE cards.
        """ # nopep8
        return self._cards[1].get_value("sldpid")

    @sldpid.setter
    def sldpid(self, value: int) -> None:
        """Set the sldpid property."""
        self._cards[1].set_value("sldpid", value)

    @property
    def sldthk(self) -> typing.Optional[float]:
        """Get or set the Thickness for each solid layer.
        """ # nopep8
        return self._cards[1].get_value("sldthk")

    @sldthk.setter
    def sldthk(self, value: float) -> None:
        """Set the sldthk property."""
        self._cards[1].set_value("sldthk", value)

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
    def bbpid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given bbpid."""
        return self._get_link_by_attr("PART", "pid", self.bbpid, "parts")

