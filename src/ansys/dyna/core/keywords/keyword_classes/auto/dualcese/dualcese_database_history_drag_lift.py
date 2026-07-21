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

"""Module providing the DualceseDatabaseHistoryDragLift class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEDATABASEHISTORYDRAGLIFT_CARD0 = (
    FieldSchema("dt", float, 0, 10, 0.0),
    FieldSchema("lcur", int, 10, 10, 0),
    FieldSchema("ioopt", int, 20, 10, 0),
    FieldSchema("refdens", float, 30, 10, None),
    FieldSchema("refvel", float, 40, 10, None),
    FieldSchema("refpress", float, 50, 10, None),
    FieldSchema("refarea", float, 60, 10, None),
    FieldSchema("refleng", float, 70, 10, None),
)

_DUALCESEDATABASEHISTORYDRAGLIFT_CARD1 = (
    FieldSchema("dragdirx", float, 0, 10, 1.0),
    FieldSchema("dragdiry", float, 10, 10, 0.0),
    FieldSchema("dragdirz", float, 20, 10, 0.0),
    FieldSchema("liftdirx", float, 30, 10, 0.0),
    FieldSchema("liftdiry", float, 40, 10, 1.0),
    FieldSchema("liftdirz", float, 50, 10, 0.0),
)

_DUALCESEDATABASEHISTORYDRAGLIFT_CARD2 = (
    FieldSchema("drgarea", int, 0, 10, 0),
    FieldSchema("momntd", int, 10, 10, 0),
    FieldSchema("momntl", int, 20, 10, 0),
    FieldSchema("momntc", int, 30, 10, 0),
)

class DualceseDatabaseHistoryDragLift(KeywordBase):
    """DYNA DUALCESE_DATABASE_HISTORY_DRAG_LIFT keyword"""

    keyword = "DUALCESE"
    subkeyword = "DATABASE_HISTORY_DRAG_LIFT"

    def __init__(self, **kwargs):
        """Initialize the DualceseDatabaseHistoryDragLift class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYDRAGLIFT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYDRAGLIFT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYDRAGLIFT_CARD2,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs. If DT is zero, no output is generated.DT is ignored if LCUR > 0.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcur(self) -> int:
        """Get or set the Optional curve ID specifying the time interval between outputs. Use *DEFINE_CURVE to define the curve; the abscissa is time, and the ordinate is time interval between outputs.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of the output frequency load curve defined by LCUR:
        EQ.1: When output is generated at time t_n, the next output time t_(n + 1) is computed as
        ?(2 / 2) t_(n + 1) = t_n + LCUR (t_n)  .
        This is the default behavior.
        EQ.2: When output is generated at time t_n, the next output time t_(n + 1) is computed as
        ?(2 / 2) t_(n + 1) = t_n + LCUR (t_(n + 1))  .
        EQ.3: Output is generated for each abscissa point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        """Set the ioopt property."""
        self._cards[0].set_value("ioopt", value)

    @property
    def refdens(self) -> typing.Optional[float]:
        """Get or set the An appropriate reference density
        """ # nopep8
        return self._cards[0].get_value("refdens")

    @refdens.setter
    def refdens(self, value: float) -> None:
        """Set the refdens property."""
        self._cards[0].set_value("refdens", value)

    @property
    def refvel(self) -> typing.Optional[float]:
        """Get or set the An appropriate reference velocity
        """ # nopep8
        return self._cards[0].get_value("refvel")

    @refvel.setter
    def refvel(self, value: float) -> None:
        """Set the refvel property."""
        self._cards[0].set_value("refvel", value)

    @property
    def refpress(self) -> typing.Optional[float]:
        """Get or set the An appropriate reference pressure
        """ # nopep8
        return self._cards[0].get_value("refpress")

    @refpress.setter
    def refpress(self, value: float) -> None:
        """Set the refpress property."""
        self._cards[0].set_value("refpress", value)

    @property
    def refarea(self) -> typing.Optional[float]:
        """Get or set the An appropriate reference area.
        """ # nopep8
        return self._cards[0].get_value("refarea")

    @refarea.setter
    def refarea(self, value: float) -> None:
        """Set the refarea property."""
        self._cards[0].set_value("refarea", value)

    @property
    def refleng(self) -> typing.Optional[float]:
        """Get or set the An appropriate reference length
        """ # nopep8
        return self._cards[0].get_value("refleng")

    @refleng.setter
    def refleng(self, value: float) -> None:
        """Set the refleng property."""
        self._cards[0].set_value("refleng", value)

    @property
    def dragdirx(self) -> float:
        """Get or set the The x-component of the drag direction
        """ # nopep8
        return self._cards[1].get_value("dragdirx")

    @dragdirx.setter
    def dragdirx(self, value: float) -> None:
        """Set the dragdirx property."""
        self._cards[1].set_value("dragdirx", value)

    @property
    def dragdiry(self) -> float:
        """Get or set the The y-component of the drag direction
        """ # nopep8
        return self._cards[1].get_value("dragdiry")

    @dragdiry.setter
    def dragdiry(self, value: float) -> None:
        """Set the dragdiry property."""
        self._cards[1].set_value("dragdiry", value)

    @property
    def dragdirz(self) -> float:
        """Get or set the The z-component of the drag direction
        """ # nopep8
        return self._cards[1].get_value("dragdirz")

    @dragdirz.setter
    def dragdirz(self, value: float) -> None:
        """Set the dragdirz property."""
        self._cards[1].set_value("dragdirz", value)

    @property
    def liftdirx(self) -> float:
        """Get or set the The x-component of the lift direction
        """ # nopep8
        return self._cards[1].get_value("liftdirx")

    @liftdirx.setter
    def liftdirx(self, value: float) -> None:
        """Set the liftdirx property."""
        self._cards[1].set_value("liftdirx", value)

    @property
    def liftdiry(self) -> float:
        """Get or set the The y-component of the lift direction
        """ # nopep8
        return self._cards[1].get_value("liftdiry")

    @liftdiry.setter
    def liftdiry(self, value: float) -> None:
        """Set the liftdiry property."""
        self._cards[1].set_value("liftdiry", value)

    @property
    def liftdirz(self) -> float:
        """Get or set the The z-component of the lift direction
        """ # nopep8
        return self._cards[1].get_value("liftdirz")

    @liftdirz.setter
    def liftdirz(self, value: float) -> None:
        """Set the liftdirz property."""
        self._cards[1].set_value("liftdirz", value)

    @property
    def drgarea(self) -> int:
        """Get or set the Flag specifying how to obtain the drag area needed for finding the drag-related coefficients:
        EQ.0: Use REFAREA in all the calculations requiring an enclosing surface area
        NE.0: Use LS - DYNA's dynamic calculation of the surface area of each piece / part to compute the various drag - related coefficients.REFAREA scales the computed surface area before being used to calculate the drag - related coefficients.
        """ # nopep8
        return self._cards[2].get_value("drgarea")

    @drgarea.setter
    def drgarea(self, value: int) -> None:
        """Set the drgarea property."""
        self._cards[2].set_value("drgarea", value)

    @property
    def momntd(self) -> int:
        """Get or set the Flag determining whether to compute the torque in the drag direction:
        EQ.0: Do not compute the torque.
        NE.0: Compute the torque.
        """ # nopep8
        return self._cards[2].get_value("momntd")

    @momntd.setter
    def momntd(self, value: int) -> None:
        """Set the momntd property."""
        self._cards[2].set_value("momntd", value)

    @property
    def momntl(self) -> int:
        """Get or set the Flag determining whether to compute the torque in the lift direction:
        EQ.0: Do not compute the torque.
        NE.0: Compute the torque
        """ # nopep8
        return self._cards[2].get_value("momntl")

    @momntl.setter
    def momntl(self, value: int) -> None:
        """Set the momntl property."""
        self._cards[2].set_value("momntl", value)

    @property
    def momntc(self) -> int:
        """Get or set the Flag determining whether to compute the torque in the direction that is the cross product of the drag and lift directions:
        EQ.0: Do not compute the torque.
        NE.0: Compute the torque.
        """ # nopep8
        return self._cards[2].get_value("momntc")

    @momntc.setter
    def momntc(self, value: int) -> None:
        """Set the momntc property."""
        self._cards[2].set_value("momntc", value)

