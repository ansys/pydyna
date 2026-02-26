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

"""Module providing the SensorDefineNodeUpdate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_SENSORDEFINENODEUPDATE_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("node1", int, 10, 10, None),
    FieldSchema("node2", int, 20, 10, None),
    FieldSchema("vid", str, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("ctype", str, 50, 10, "ACC"),
)

_SENSORDEFINENODEUPDATE_CARD1 = (
    FieldSchema("birth", float, 0, 10, None),
    FieldSchema("death", float, 10, 10, None),
    FieldSchema("dtupd", float, 20, 10, None),
)

_SENSORDEFINENODEUPDATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineNodeUpdate(KeywordBase):
    """DYNA SENSOR_DEFINE_NODE_UPDATE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_NODE_UPDATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "node1": LinkType.NODE,
        "node2": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the SensorDefineNodeUpdate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINENODEUPDATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORDEFINENODEUPDATE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorDefineNodeUpdate.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINENODEUPDATE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        """Set the sensid property."""
        self._cards[0].set_value("sensid", value)

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.  If CTYPE = TEMP, then the temperature at NODE1 will be output. If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
        When the keyword option SET is active, NODE1 is a node set ID.If NODE2 is needed, it must be a node set of the same length as NODE1 with SETOPT defined, but it can be either a node or node set without SETOPT defined.
        When the SET option is active but SETOPT is not defined, determining the status of a related* SENSOR_SWITCH depends on the sign of NODE1.See Remark 2 for details
        """ # nopep8
        return self._cards[0].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        """Set the node1 property."""
        self._cards[0].set_value("node1", value)

    @property
    def node2(self) -> typing.Optional[int]:
        """Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.If CTYPE = TEMP, then the temperature at NODE1 will be output.If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
        When the keyword option SET is active, NODE1 is a node set ID.If NODE2 is needed, it must be a node set of the same length as NODE1 with SETOPT defined, but it can be either a node or node set without SETOPT defined.
        When the SET option is active but SETOPT is not defined, determining the status of a related * SENSOR_SWITCH depends on the sign of NODE1.See Remark 2 for details
        """ # nopep8
        return self._cards[0].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        """Set the node2 property."""
        self._cards[0].set_value("node2", value)

    @property
    def vid(self) -> typing.Optional[str]:
        """Get or set the ID of vector along which the nodal values are measured, see *DEFINE_?VECTOR.  The magnitude of nodal values (coordinate, velocity, or acceleration) will be output if VID is 0 or undefined.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: str) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def ctype(self) -> str:
        """Get or set the Output component type:
        EQ.ACC:	acceleration
        EQ.VEL:	velocity
        EQ.COORD: Coordinate
        EQ.TEMP:	Temperature
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: str) -> None:
        """Set the ctype property."""
        if value not in ["ACC", "VEL", "COORD", "TEMP", None]:
            raise Exception("""ctype must be `None` or one of {"ACC","VEL","COORD","TEMP"}.""")
        self._cards[0].set_value("ctype", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Birth time of this sensor.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time of this sensor.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def dtupd(self) -> typing.Optional[float]:
        """Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
        """ # nopep8
        return self._cards[1].get_value("dtupd")

    @dtupd.setter
    def dtupd(self, value: float) -> None:
        """Set the dtupd property."""
        self._cards[1].set_value("dtupd", value)

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
    def node1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node1."""
        return self._get_link_by_attr("NODE", "nid", self.node1, "parts")

    @property
    def node2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node2."""
        return self._get_link_by_attr("NODE", "nid", self.node2, "parts")

