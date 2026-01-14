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

"""Module providing the DefineControlVolumeFlowArea class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECONTROLVOLUMEFLOWAREA_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("stype", int, 20, 10, 1),
    FieldSchema("pid_", int, 30, 10, 0, "pid "),
    FieldSchema("area_", float, 40, 10, None, "area "),
    FieldSchema("cviid_", float, 50, 10, None, "cviid "),
)

_DEFINECONTROLVOLUMEFLOWAREA_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineControlVolumeFlowArea(KeywordBase):
    """DYNA DEFINE_CONTROL_VOLUME_FLOW_AREA keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTROL_VOLUME_FLOW_AREA"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineControlVolumeFlowArea class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONTROLVOLUMEFLOWAREA_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineControlVolumeFlowArea.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONTROLVOLUMEFLOWAREA_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Flow area ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the SET ID defining the flow area
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Type of set defining the flow area.
        A value of 1 indicates a node set for the perimeter which will be automatically mesh,
        and 2 indicates a segment set covering the flow area
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [1, 2, None]:
            raise Exception("""stype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("stype", value)

    @property
    def pid_(self) -> int:
        """Get or set the PART ID for null shells for visualizing the flow area. It defaults to 0, in which case the area will not be visualized.
        """ # nopep8
        return self._cards[0].get_value("pid_")

    @pid_.setter
    def pid_(self, value: int) -> None:
        """Set the pid_ property."""
        self._cards[0].set_value("pid_", value)

    @property
    def area_(self) -> typing.Optional[float]:
        """Get or set the This is a constant area for the case when a flow area definition is not defined
        """ # nopep8
        return self._cards[0].get_value("area_")

    @area_.setter
    def area_(self, value: float) -> None:
        """Set the area_ property."""
        self._cards[0].set_value("area_", value)

    @property
    def cviid_(self) -> typing.Optional[float]:
        """Get or set the CONTROL_VOLUME_INTERACTION ID that uses the flow area
        """ # nopep8
        return self._cards[0].get_value("cviid_")

    @cviid_.setter
    def cviid_(self, value: float) -> None:
        """Set the cviid_ property."""
        self._cards[0].set_value("cviid_", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

