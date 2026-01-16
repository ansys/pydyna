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

"""Module providing the DeformableToRigidAutomatic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFORMABLETORIGIDAUTOMATIC_CARD0 = (
    FieldSchema("swset", int, 0, 10, None),
    FieldSchema("code", int, 10, 10, 0),
    FieldSchema("time1", float, 20, 10, 0.0),
    FieldSchema("time2", float, 30, 10, 1e+20),
    FieldSchema("time3", float, 40, 10, 0.0),
    FieldSchema("entno", int, 50, 10, 0),
    FieldSchema("relsw", int, 60, 10, 0),
    FieldSchema("paired", int, 70, 10, 0),
)

_DEFORMABLETORIGIDAUTOMATIC_CARD1 = (
    FieldSchema("nrbf", int, 0, 10, 0),
    FieldSchema("ncsf", int, 10, 10, 0),
    FieldSchema("rwf", int, 20, 10, 0),
    FieldSchema("dtmax", float, 30, 10, 0.0),
    FieldSchema("d2r", int, 40, 10, 0),
    FieldSchema("r2d", int, 50, 10, 0),
    FieldSchema("offset", int, 60, 10, 0),
)

_DEFORMABLETORIGIDAUTOMATIC_CARD2 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lrb", int, 10, 10, None),
    FieldSchema("ptype", str, 20, 10, "PART"),
)

_DEFORMABLETORIGIDAUTOMATIC_CARD3 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ptype", str, 10, 10, "PART"),
)

class DeformableToRigidAutomatic(KeywordBase):
    """DYNA DEFORMABLE_TO_RIGID_AUTOMATIC keyword"""

    keyword = "DEFORMABLE"
    subkeyword = "TO_RIGID_AUTOMATIC"
    _link_fields = {
        "lrb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DeformableToRigidAutomatic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFORMABLETORIGIDAUTOMATIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFORMABLETORIGIDAUTOMATIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFORMABLETORIGIDAUTOMATIC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFORMABLETORIGIDAUTOMATIC_CARD3,
                **kwargs,
            ),        ]
    @property
    def swset(self) -> typing.Optional[int]:
        """Get or set the Set number for this automatic switch set. Must be unique.
        """ # nopep8
        return self._cards[0].get_value("swset")

    @swset.setter
    def swset(self, value: int) -> None:
        """Set the swset property."""
        self._cards[0].set_value("swset", value)

    @property
    def code(self) -> int:
        """Get or set the Activation switch code. Defines the test to activate the automatic material switch of the part:
        EQ.0: switch takes place at time 1,
        EQ.1: switch takes place between time 1 and time 2 if rigid wall force is zero,
        EQ.2: switch takes place between time 1 and time 2 if contact surface force is zero,
        EQ.3: switch takes place between time 1 and time 2 if rigid wall force is nonzer,
        EQ.4: switch takes place between time 1 and time 2 if contact surface force is nonzer.
        EQ 5, switch is turned on/off by *SENSOR_CONTROL_DEF2RIG.  Variables other than those identified above will be ignored when CODE=5.
        """ # nopep8
        return self._cards[0].get_value("code")

    @code.setter
    def code(self, value: int) -> None:
        """Set the code property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""code must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("code", value)

    @property
    def time1(self) -> float:
        """Get or set the Switch will not take place before this time (default = 0.0).
        """ # nopep8
        return self._cards[0].get_value("time1")

    @time1.setter
    def time1(self, value: float) -> None:
        """Set the time1 property."""
        self._cards[0].set_value("time1", value)

    @property
    def time2(self) -> float:
        """Get or set the Switch will not take place after this time.
        EQ.0.0: Time 2 set to 1.0E+20.
        """ # nopep8
        return self._cards[0].get_value("time2")

    @time2.setter
    def time2(self, value: float) -> None:
        """Set the time2 property."""
        self._cards[0].set_value("time2", value)

    @property
    def time3(self) -> float:
        """Get or set the Delay period. Another TIME 3 Delay period. After this part switch has taken place, another automatic switch will not take place for the duration of the delay period. If set to zero a part switch may take place immediately after this switch.
        """ # nopep8
        return self._cards[0].get_value("time3")

    @time3.setter
    def time3(self, value: float) -> None:
        """Set the time3 property."""
        self._cards[0].set_value("time3", value)

    @property
    def entno(self) -> int:
        """Get or set the Rigid wall/contact surface number for switch codes 1, 2, 3, 4.
        """ # nopep8
        return self._cards[0].get_value("entno")

    @entno.setter
    def entno(self, value: int) -> None:
        """Set the entno property."""
        self._cards[0].set_value("entno", value)

    @property
    def relsw(self) -> int:
        """Get or set the Related switch set.  The related switch set is another automatic switch set paired to this one so the switches can be activated more than once.
        EQ.0:	No related switch set
        """ # nopep8
        return self._cards[0].get_value("relsw")

    @relsw.setter
    def relsw(self, value: int) -> None:
        """Set the relsw property."""
        self._cards[0].set_value("relsw", value)

    @property
    def paired(self) -> int:
        """Get or set the Specify how the related switch sets are paired (if there are paired switches):
        EQ.0:	SWSET is not paired to another switch set.
        EQ.1 : SWSET is paired with switch set RELSWand is the first switch set to be activated.
        EQ. - 1 : SWSET is paired with switch set RELSWand is the second switch to be activated.
        """ # nopep8
        return self._cards[0].get_value("paired")

    @paired.setter
    def paired(self, value: int) -> None:
        """Set the paired property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""paired must be `None` or one of {0,-1,1}.""")
        self._cards[0].set_value("paired", value)

    @property
    def nrbf(self) -> int:
        """Get or set the Flag to delete or activate nodal rigid bodies.
        If nodal rigid bodies or generalized, weld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instablilities:
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[1].get_value("nrbf")

    @nrbf.setter
    def nrbf(self, value: int) -> None:
        """Set the nrbf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrbf must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("nrbf", value)

    @property
    def ncsf(self) -> int:
        """Get or set the Flag to delete or activate nodal constraint set.
        If nodal constraint/spotweld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instablilities:
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[1].get_value("ncsf")

    @ncsf.setter
    def ncsf(self, value: int) -> None:
        """Set the ncsf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ncsf must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("ncsf", value)

    @property
    def rwf(self) -> int:
        """Get or set the Flag to delete or activate rigid walls.
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[1].get_value("rwf")

    @rwf.setter
    def rwf(self, value: int) -> None:
        """Set the rwf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""rwf must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("rwf", value)

    @property
    def dtmax(self) -> float:
        """Get or set the Maximum permitted time step size after switch.
        """ # nopep8
        return self._cards[1].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[1].set_value("dtmax", value)

    @property
    def d2r(self) -> int:
        """Get or set the Number of deformable parts to be switched to rigid plus number of rigid parts for which new merged (lead/constrained) rigid body combinations will be defined.
        EQ.0:	No parts defined
        """ # nopep8
        return self._cards[1].get_value("d2r")

    @d2r.setter
    def d2r(self, value: int) -> None:
        """Set the d2r property."""
        self._cards[1].set_value("d2r", value)

    @property
    def r2d(self) -> int:
        """Get or set the Number of rigid parts to be switched to deformable:
        EQ.0: no parts defined.
        """ # nopep8
        return self._cards[1].get_value("r2d")

    @r2d.setter
    def r2d(self, value: int) -> None:
        """Set the r2d property."""
        self._cards[1].set_value("r2d", value)

    @property
    def offset(self) -> int:
        """Get or set the Optional contact thickness for switch to deformable. For contact, its value should be set to a value greater than the contact thickness offsets to ensure the switching occurs prior to impact. This option applies if and only if CODE is set to 3 or 4.  For CODE=3 all rigid wall options are implemented.  For CODE=4, the implementation works for the contact type CONTACT_AUTOMATIC_ when the options: ONE_WAY_ SURFACE_TO_SURFACE,  NODES_TO_SURFACE, and SUR-FACE_ TO_ SURFACE are specified.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: int) -> None:
        """Set the offset property."""
        self._cards[1].set_value("offset", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the part which is switched to a rigid material.  When PID is merged to another rigid body by the LRB field, this part is allowed to be rigid before the switch..
        """ # nopep8
        return self._cards[2].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[2].set_value("pid", value)

    @property
    def lrb(self) -> typing.Optional[int]:
        """Get or set the Part ID of the lead rigid body to which part PID is merged.  If zero, part PID becomes either an independent or lead rigid body..
        """ # nopep8
        return self._cards[2].get_value("lrb")

    @lrb.setter
    def lrb(self, value: int) -> None:
        """Set the lrb property."""
        self._cards[2].set_value("lrb", value)

    @property
    def ptype(self) -> str:
        """Get or set the Type of PID:
        EQ."PART": PID is a part ID.
        EQ."PSET": PID is a part set ID.
        """ # nopep8
        return self._cards[2].get_value("ptype")

    @ptype.setter
    def ptype(self, value: str) -> None:
        """Set the ptype property."""
        if value not in ["PART", "PSET", None]:
            raise Exception("""ptype must be `None` or one of {"PART","PSET"}.""")
        self._cards[2].set_value("ptype", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the part which is switched to a deformable material.
        """ # nopep8
        return self._cards[3].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[3].set_value("pid", value)

    @property
    def ptype(self) -> str:
        """Get or set the Type of PID:
        EQ."PART": PID is a part ID.
        EQ."PSET": PID is a part set ID.
        """ # nopep8
        return self._cards[3].get_value("ptype")

    @ptype.setter
    def ptype(self, value: str) -> None:
        """Set the ptype property."""
        if value not in ["PART", "PSET", None]:
            raise Exception("""ptype must be `None` or one of {"PART","PSET"}.""")
        self._cards[3].set_value("ptype", value)

    @property
    def lrb_link(self) -> KeywordBase:
        """Get the PART keyword containing the given lrb."""
        return self._get_link_by_attr("PART", "pid", self.lrb, "parts")

