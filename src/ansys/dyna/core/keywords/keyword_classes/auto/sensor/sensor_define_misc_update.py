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

"""Module providing the SensorDefineMiscUpdate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORDEFINEMISCUPDATE_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("mtype", str, 10, 10, "ANGLE"),
    FieldSchema("i0", str, 20, 10, None),
    FieldSchema("i1", str, 30, 10, None),
    FieldSchema("i2", str, 40, 10, None),
    FieldSchema("i3", str, 50, 10, None),
    FieldSchema("i4", str, 60, 10, None),
    FieldSchema("i5", str, 70, 10, None),
)

_SENSORDEFINEMISCUPDATE_CARD1 = (
    FieldSchema("birth", float, 0, 10, None),
    FieldSchema("death", float, 10, 10, None),
    FieldSchema("dtupd", float, 20, 10, None),
)

_SENSORDEFINEMISCUPDATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineMiscUpdate(KeywordBase):
    """DYNA SENSOR_DEFINE_MISC_UPDATE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_MISC_UPDATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorDefineMiscUpdate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEMISCUPDATE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEMISCUPDATE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SensorDefineMiscUpdate.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINEMISCUPDATE_OPTION0_CARD0,
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
    def mtype(self) -> str:
        """Get or set the Entity to be traced:
        EQ.ANGLE:  Angular accelerometer sensor tracing the angle between two lines.
        The fields I1 and I2 are node numbers defining the 1st line, while I3
        and I4 are node numbers defining the 2nd line.
        EQ.BNDOUT:	Boundary condition energy as reported in file bndout?  I1 is the ID as defined in *BOUNDARY_RESCRIBED_MOTION
        EQ.CURVE:       The value of a time-dependent curve defined by *DEFINE_CURVE_FUNCTION or *DEFINE_CURVE.  I1 is the curve ID.
        EQ.CVBAG: Information reported in ABSTAT for control volume airbag I1, including
        I0.EQ.TEMP: airbag temperature
        I0.EQ.VOL: airbag volume
        EQ.ICVOL:	Information reported in ICVOUT for incompressible control volume I1, see *DEFINE_CONTROL_VOLUME, including
        I0.EQ.PRES:	Temperature of control volume
        I0.EQ.VOL : Volume of control volume
        EQ.MATSUM:	Information reported in MATSUM for part set I1, including.I0.eq.KINETIC: kinetic energy;I0.eq.INTERNAL: internal energy;I0.eq.ERODEKE: eroded kinetic energy;I0.eq.ERODEIE: eroded internal energy
        EQ.NFAILE: Number of failed elements of type I0 in set I1 will be traced. I0, element type, can be  SOLID
        for solid elements,  SHELL  for thin shell elements,  TSHELL  for thick shell elements,
        BEAM  for beam elements or  DISC  for discrete elements. I1 is the related element set
        number. If undefined, the failure of all elements of type I0 will be traced
        EQ.RETRACTOR: The seatbelt retractor payout rate is traced. I1 is the retractor ID.
        EQ.RIGIDBODY: Accelerometer sensor tracing the kinematics of
        a rigid body with id I1. The I2 field specifies which kinematical component is to be traced.
        It may be set to  TX ,  TY , or  TZ  for X, Y, and Z translations and to  RX ,  RY , or  RZ
        for the X, Y, and Z components of the rotation. The I3 field specifies the kinematics type:  D
        for displacement,  V  for velocity and  A  for acceleration. Output is calculated with respect
        to the global coordinate system when the I4 field is set to  0 , its default value; the local
        rigid-body coordinate system is used when I4 is set to  1 .
        EQ.TIME:  The current analysis time is traced.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: str) -> None:
        """Set the mtype property."""
        if value not in ["ANGLE", "BNDOUT", "CURVE", "CVBAG", "ICVOL", "MATSUM", "NFAILE", "RETRACTOR", "RIGIDBODY", "TIME", None]:
            raise Exception("""mtype must be `None` or one of {"ANGLE","BNDOUT","CURVE","CVBAG","ICVOL","MATSUM","NFAILE","RETRACTOR","RIGIDBODY","TIME"}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def i0(self) -> typing.Optional[str]:
        """Get or set the See MTYPE
        """ # nopep8
        return self._cards[0].get_value("i0")

    @i0.setter
    def i0(self, value: str) -> None:
        """Set the i0 property."""
        self._cards[0].set_value("i0", value)

    @property
    def i1(self) -> typing.Optional[str]:
        """Get or set the See MTYPE.
        """ # nopep8
        return self._cards[0].get_value("i1")

    @i1.setter
    def i1(self, value: str) -> None:
        """Set the i1 property."""
        self._cards[0].set_value("i1", value)

    @property
    def i2(self) -> typing.Optional[str]:
        """Get or set the See MTYPE.
        """ # nopep8
        return self._cards[0].get_value("i2")

    @i2.setter
    def i2(self, value: str) -> None:
        """Set the i2 property."""
        self._cards[0].set_value("i2", value)

    @property
    def i3(self) -> typing.Optional[str]:
        """Get or set the See MTYPE
        """ # nopep8
        return self._cards[0].get_value("i3")

    @i3.setter
    def i3(self, value: str) -> None:
        """Set the i3 property."""
        self._cards[0].set_value("i3", value)

    @property
    def i4(self) -> typing.Optional[str]:
        """Get or set the See MTYPE.
        """ # nopep8
        return self._cards[0].get_value("i4")

    @i4.setter
    def i4(self, value: str) -> None:
        """Set the i4 property."""
        self._cards[0].set_value("i4", value)

    @property
    def i5(self) -> typing.Optional[str]:
        """Get or set the See MTYPE.
        """ # nopep8
        return self._cards[0].get_value("i5")

    @i5.setter
    def i5(self, value: str) -> None:
        """Set the i5 property."""
        self._cards[0].set_value("i5", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Birth time of this sensor
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time of this sensor
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

