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

"""Module providing the SensorDefineElement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SENSORDEFINEELEMENT_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("etype", str, 10, 10, "BEAM"),
    FieldSchema("elemid", int, 20, 10, None),
    FieldSchema("comp", str, 30, 10, None),
    FieldSchema("ctype", str, 40, 10, "AREA"),
    FieldSchema("layer", str, 50, 10, "BOT"),
    FieldSchema("sf", float, 60, 10, None),
    FieldSchema("pwr", float, 70, 10, None),
)

_SENSORDEFINEELEMENT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineElement(KeywordBase):
    """DYNA SENSOR_DEFINE_ELEMENT keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_ELEMENT"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SensorDefineElement class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEELEMENT_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SensorDefineElement._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINEELEMENT_OPTION0_CARD0,
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
    def etype(self) -> str:
        """Get or set the Element type:
        EQ.BEAM: Beam element.
        EQ.DISC-ELE: Discrete element.
        EQ.SEATBELT: Seatbelt element
        EQ.SHELL: Shell element
        EQ.SOLID: Solid element
        EQ.TSHELL: Thick shell element
        """ # nopep8
        return self._cards[0].get_value("etype")

    @etype.setter
    def etype(self, value: str) -> None:
        """Set the etype property."""
        if value not in ["BEAM", "DISC-ELE", "SEATBELT", "SHELL", "SOLID", "TSHELL", None]:
            raise Exception("""etype must be `None` or one of {"BEAM","DISC-ELE","SEATBELT","SHELL","SOLID","TSHELL"}.""")
        self._cards[0].set_value("etype", value)

    @property
    def elemid(self) -> typing.Optional[int]:
        """Get or set the Element ID or element set ID when the SET keyword option is active.
        In the case of the SET keyword option with SETOPT not defined, determining the status of a related *SENSOR_SWITCH depends on the sign of ELEMID.If SETOPT is defined, then ELEMID must be greater than 0.
        """ # nopep8
        return self._cards[0].get_value("elemid")

    @elemid.setter
    def elemid(self, value: int) -> None:
        """Set the elemid property."""
        self._cards[0].set_value("elemid", value)

    @property
    def comp(self) -> typing.Optional[str]:
        """Get or set the Component type.  The definition of the component, and its related coordinate system, is consistent with that of elout.  Leave blank for discrete elements.  Available options for elements other than discrete elements include:
        EQ.F[ID]:	Value of a function, *DEFINE_FUNCTION,*DEFINE_FUNCTION, with all 6 stress/strain components as input. It is available for shells, thick shells, and solids.  The ID of the function must follow right after F. For instance, F1000 means the function with ID of 1000 defines the sensed value.
        EQ.HYDR: Hydrostatic stress or strain, the average of the three normal components, for shells, thick shells, and solids.
        EQ.MAXS:	Maximum shear stress or strain for shells, thick shells, and solids
        EQ.PRIN1:	1st principal stress or strain for shells, thick shells, and solids
        EQ.PRIN2:	2nd principal stress or strain for shells, thick shells, and solids
        EQ.PRIN3:	3rd principal stress or strain for shells, thick shells, and solids
        EQ.SHEARS:	Local s-direction
        EQ.SHEART:	Local t-direction
        EQ.VM:	von Mises stress for shells, thick shells, and solids
        EQ.XX:	x-normal component for shells, thick shells, and solids
        EQ.XY:	xy-shear component for shells, thick shells, and solids
        EQ.YY:	y-normal component for shells, thick shells, and solids
        EQ.YZ:	yz-shear component for shells, thick shells, and solids
        EQ.ZX:	zx-shear component for shells, thick shells, and solids
        """ # nopep8
        return self._cards[0].get_value("comp")

    @comp.setter
    def comp(self, value: str) -> None:
        """Set the comp property."""
        self._cards[0].set_value("comp", value)

    @property
    def ctype(self) -> str:
        """Get or set the Sensor type:
        EQ.AREA:	Area of shell element. COMP is ignored.
        EQ.DLEN:	Change in length for discrete or seat belt element. COMP is ignored.
        EQ.FAIL:	Failure of the element, sensor value = 1 when the element fails, = 0 therwise. COMP is ignored.
        EQ.FORCE:	Force resultants for beams, seat belt, or translational discrete element; oment resultant for rotational discrete element
        EQ.MOMENT:	Moment resultants for beams
        EQ.STRAIN:	Strain component for shells, thick shells, and solids
        EQ.STRESS:	Stress component for shells, thick shells, and solids
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: str) -> None:
        """Set the ctype property."""
        if value not in ["AREA", "DLEN", "FAIL", "FORCE", "MOMENT", "STRAIN", "STRESS", None]:
            raise Exception("""ctype must be `None` or one of {"AREA","DLEN","FAIL","FORCE","MOMENT","STRAIN","STRESS"}.""")
        self._cards[0].set_value("ctype", value)

    @property
    def layer(self) -> str:
        """Get or set the Layer of the integration point in a shell element
        EQ.BOT: component at lower surface
        EQ.TOP: component at upper surface.
        When CTYPE = STRESS, LAYER cancould be an integer i to monitor the stress of the ith integration point.
        """ # nopep8
        return self._cards[0].get_value("layer")

    @layer.setter
    def layer(self, value: str) -> None:
        """Set the layer property."""
        if value not in ["BOT", "TOP", None]:
            raise Exception("""layer must be `None` or one of {"BOT","TOP"}.""")
        self._cards[0].set_value("layer", value)

    @property
    def sf(self) -> typing.Optional[float]:
        """Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def pwr(self) -> typing.Optional[float]:
        """Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
        """ # nopep8
        return self._cards[0].get_value("pwr")

    @pwr.setter
    def pwr(self, value: float) -> None:
        """Set the pwr property."""
        self._cards[0].set_value("pwr", value)

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

