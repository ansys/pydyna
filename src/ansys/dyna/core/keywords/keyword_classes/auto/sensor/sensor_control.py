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

"""Module providing the SensorControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORCONTROL_CARD0 = (
    FieldSchema("cntlid", int, 0, 10, None),
    FieldSchema("type", str, 10, 10, "AIRBAG"),
    FieldSchema("typeid", int, 20, 10, None),
    FieldSchema("timeoff", int, 30, 10, 0),
    FieldSchema("nrep", int, 40, 10, 0),
    FieldSchema("estyp", str, 50, 10, "BEAM"),
)

_SENSORCONTROL_CARD1 = (
    FieldSchema("initstt", str, 0, 10, "ON"),
    FieldSchema("swit1", int, 10, 10, None),
    FieldSchema("swit2", int, 20, 10, None),
    FieldSchema("swit3", int, 30, 10, None),
    FieldSchema("swit4", int, 40, 10, None),
    FieldSchema("swit5", int, 50, 10, None),
    FieldSchema("swit6", int, 60, 10, None),
    FieldSchema("swit7", int, 70, 10, None),
)

_SENSORCONTROL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorControl(KeywordBase):
    """DYNA SENSOR_CONTROL keyword"""

    keyword = "SENSOR"
    subkeyword = "CONTROL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorControl class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORCONTROL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORCONTROL_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorControl.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORCONTROL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def cntlid(self) -> typing.Optional[int]:
        """Get or set the Control ID
        """ # nopep8
        return self._cards[0].get_value("cntlid")

    @cntlid.setter
    def cntlid(self, value: int) -> None:
        """Set the cntlid property."""
        self._cards[0].set_value("cntlid", value)

    @property
    def type(self) -> str:
        """Get or set the Entity to be controlled:
        EQ.AIRBAG:	*AIRBAG
        EQ.BAGVENTPOP:	to close/open the airbag venting holes (see remark 1)
        EQ.BELTPRET:	to fire belt pretensioner (see remark 2)
        EQ.BELTRETRA:	to lock belt retractor (see remark 2)
        EQ.BELTSLIP:	for one-way slip ring element (see remark 3)
        EQ.CONTACT:	*CONTACT
        EQ.CONTACT2D:	*CONTACT_2D
        EQ.CONSTRL: * CONSTRAINED_LOCAL
        EQ.CNRB: *CONSTRAINED_NODAL_RIGID_BODY
        EQ. CPM  *AIRBAG_PARTICLE
        EQ.DEF2RIG:	*DEFORMABLE_TO_RIGID_AUTOMATIC (see remark 4)
        EQ.ELESET: Element set, see ESTYP below.
        EQ.EM: EM solver
        EQ.FUNCTION:	*DEFINE_CURVE_FUNCTION (see remarks 5 & 6)
        EQ.JOINT:	*CONSTRAINED_JOINT
        EQ.JOINTSTIF:	*CONSTRAINED_JOINT_STIFFNESS
        EQ.M PRESSURE:	*LOAD_MOVING_PRESSURE
        EQ.POREAIR: *MAT_ADD_PORE_AIR
        EQ.PRESC-MOT:	*BOUNDARY_PRESCRIBED_MOTION
        EQ. PRESC-ORI : *BOUNDARY_PRESCRIBED_ORIENTATION_RIGID
        EQ.PRESSURE:	*LOAD_SEGMENT_SET
        EQ.PZBC: *BOUNDARY PZEPOT
        EQ.RWALL:	*RIGID_WALL
        EQ.SPC:	*BOUNDARY_SPC
        EQ.BPWPN:	*BOUNDARY_PWP_NODE/SET_ID
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        """Set the type property."""
        if value not in ["AIRBAG", "BAGVENTPOP", "BELTPRET", "BELTRETRA", "BELTSLIP", "CONTACT", "CONTACT2D", "CONSTRL", "CNRB", "CPM", "DEF2RIG", "ELESET", "EM", "FUNCTION", "JOINT", "JOINTSTIF", "MPRESSURE", "POREAIR", "PRESC-MOT", "PRESC-ORI", "PRESSURE", "PZBC", "RWALL", "SPC", "SPOTWELD", "BPWPN", None]:
            raise Exception("""type must be `None` or one of {"AIRBAG","BAGVENTPOP","BELTPRET","BELTRETRA","BELTSLIP","CONTACT","CONTACT2D","CONSTRL","CNRB","CPM","DEF2RIG","ELESET","EM","FUNCTION","JOINT","JOINTSTIF","MPRESSURE","POREAIR","PRESC-MOT","PRESC-ORI","PRESSURE","PZBC","RWALL","SPC","SPOTWELD","BPWPN"}.""")
        self._cards[0].set_value("type", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the ID of entity to be controlled if TYPE ≠ FUNCTION, LOADTHM, or POREAIR.  If TYPE = FUNCTION, see Remark 5.  For TYPE = LOADTHM, TYPEID is the node set for which the temperature boundary condition specified by either *LOAD_THERMAL_VARIABLE or *LOAD_THERMAL_VARIABLE_NODE will be controlled.  For TYPE = POREAIR, TYPEID is the ID of the part containing material with pore air.
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        """Set the typeid property."""
        self._cards[0].set_value("typeid", value)

    @property
    def timeoff(self) -> int:
        """Get or set the Flag for offset of time in curve:
        EQ.0: No offset is applied.
        EQ.1: Offset the abscissa of the time-dependent curve by the time value at which the sensor is triggered.
        The curves affected when TIMEOFF = 1 are those specified in
        *LOAD_SEGMENT and *BOUNDARY_PRESCRIBED_MOTION when TYPE is PRESSURE and PRESC-MOT, respectively
        """ # nopep8
        return self._cards[0].get_value("timeoff")

    @timeoff.setter
    def timeoff(self, value: int) -> None:
        """Set the timeoff property."""
        if value not in [0, 1, None]:
            raise Exception("""timeoff must be `None` or one of {0,1}.""")
        self._cards[0].set_value("timeoff", value)

    @property
    def nrep(self) -> int:
        """Get or set the Number of repeat of cycle of switches, SWITn, defined on the 2nd card.
        For example, a definition of SWITn like "601, 602, 601, 602, 601, 602" can be replaced by setting NREP to 2 and SWITn to "601, 602". Setting NREP = -1 repeats the cycle for infinite number of times. Default is 0.
        """ # nopep8
        return self._cards[0].get_value("nrep")

    @nrep.setter
    def nrep(self, value: int) -> None:
        """Set the nrep property."""
        self._cards[0].set_value("nrep", value)

    @property
    def estyp(self) -> str:
        """Get or set the Type of element set to be controlled. With initial status set to  ON ,
        all the elements included in set TYPEID can be eroded when the
        controller status is changed to  OFF . When TYPEID is not defined,
        all elements of type ESTYP in the whole system will be eroded.
        EQ. BEAM : Beam element set.
        EQ. DISC : Discrete element set
        EQ. SHELL : Thin shell element set
        EQ. SOLID : Solid element set
        EQ. TSHELL : Thick shell element set
        """ # nopep8
        return self._cards[0].get_value("estyp")

    @estyp.setter
    def estyp(self, value: str) -> None:
        """Set the estyp property."""
        if value not in ["BEAM", "DISC", "SHELL", "SOLID", "TSHELL", None]:
            raise Exception("""estyp must be `None` or one of {"BEAM","DISC","SHELL","SOLID","TSHELL"}.""")
        self._cards[0].set_value("estyp", value)

    @property
    def initstt(self) -> str:
        """Get or set the Initial status:
        EQ.On:
        EQ.Off.
        """ # nopep8
        return self._cards[1].get_value("initstt")

    @initstt.setter
    def initstt(self, value: str) -> None:
        """Set the initstt property."""
        if value not in ["ON", "OFF", None]:
            raise Exception("""initstt must be `None` or one of {"ON","OFF"}.""")
        self._cards[1].set_value("initstt", value)

    @property
    def swit1(self) -> typing.Optional[int]:
        """Get or set the ID of switch which will change the initial status after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit1")

    @swit1.setter
    def swit1(self, value: int) -> None:
        """Set the swit1 property."""
        self._cards[1].set_value("swit1", value)

    @property
    def swit2(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit2")

    @swit2.setter
    def swit2(self, value: int) -> None:
        """Set the swit2 property."""
        self._cards[1].set_value("swit2", value)

    @property
    def swit3(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit3")

    @swit3.setter
    def swit3(self, value: int) -> None:
        """Set the swit3 property."""
        self._cards[1].set_value("swit3", value)

    @property
    def swit4(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit4")

    @swit4.setter
    def swit4(self, value: int) -> None:
        """Set the swit4 property."""
        self._cards[1].set_value("swit4", value)

    @property
    def swit5(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit5")

    @swit5.setter
    def swit5(self, value: int) -> None:
        """Set the swit5 property."""
        self._cards[1].set_value("swit5", value)

    @property
    def swit6(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit6")

    @swit6.setter
    def swit6(self, value: int) -> None:
        """Set the swit6 property."""
        self._cards[1].set_value("swit6", value)

    @property
    def swit7(self) -> typing.Optional[int]:
        """Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
        """ # nopep8
        return self._cards[1].get_value("swit7")

    @swit7.setter
    def swit7(self, value: int) -> None:
        """Set the swit7 property."""
        self._cards[1].set_value("swit7", value)

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

