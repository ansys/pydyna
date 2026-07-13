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
    FieldSchema("faid", int, 0, 10, None),
    FieldSchema("fcisid", int, 10, 10, None),
    FieldSchema("fasid", int, 10, 10, None),
    FieldSchema("stype", int, 20, 10, 1),
)

_DEFINECONTROLVOLUMEFLOWAREA_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineControlVolumeFlowArea(KeywordBase):
    """DYNA DEFINE_CONTROL_VOLUME_FLOW_AREA keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTROL_VOLUME_FLOW_AREA"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineControlVolumeFlowArea class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONTROLVOLUMEFLOWAREA_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineControlVolumeFlowArea._option_spec_list[0],
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
    def faid(self) -> typing.Optional[int]:
        """Get or set the Flow area ID.
        """ # nopep8
        return self._cards[0].get_value("faid")

    @faid.setter
    def faid(self, value: int) -> None:
        """Set the faid property."""
        self._cards[0].set_value("faid", value)

    @property
    def fcisid(self) -> typing.Optional[int]:
        """Get or set the Fluid cavity interaction ID referencing the *DEFINE_CONTROL_VOLUME_INTERACTION for which this area is used
        """ # nopep8
        return self._cards[0].get_value("fcisid")

    @fcisid.setter
    def fcisid(self, value: int) -> None:
        """Set the fcisid property."""
        self._cards[0].set_value("fcisid", value)

    @property
    def fasid(self) -> typing.Optional[int]:
        """Get or set the Set ID giving the flow area
        """ # nopep8
        return self._cards[0].get_value("fasid")

    @fasid.setter
    def fasid(self, value: int) -> None:
        """Set the fasid property."""
        self._cards[0].set_value("fasid", value)

    @property
    def stype(self) -> int:
        """Get or set the Type of set specifying flow area (see Remark 2):
        EQ.1:	Node set giving the perimeter of the area.The flow area will be automatically meshed.
        EQ.2 : Segment set covering the flow area
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [1, 2, None]:
            raise Exception("""stype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("stype", value)

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

