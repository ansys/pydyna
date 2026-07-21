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

"""Module providing the DualcesePointSourceTwo_Phase class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEPOINTSOURCETWO_PHASE_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
)

_DUALCESEPOINTSOURCETWO_PHASE_CARD1 = (
    FieldSchema("lc_mrt", int, 0, 10, None),
    FieldSchema("lc_z1", int, 10, 10, None),
    FieldSchema("lc_v", int, 20, 10, None),
    FieldSchema("lc_d1", int, 30, 10, None),
    FieldSchema("lc_d2", int, 40, 10, None),
    FieldSchema("lc_p", int, 50, 10, None),
    FieldSchema("lc_t", int, 60, 10, None),
)

_DUALCESEPOINTSOURCETWO_PHASE_CARD2 = (
    FieldSchema("sf_mrt", float, 0, 10, 1.0),
    FieldSchema("sf_z1", float, 10, 10, 1.0),
    FieldSchema("sf_v", float, 20, 10, 1.0),
    FieldSchema("sf_d1", float, 30, 10, 1.0),
    FieldSchema("sf_d2", float, 40, 10, 1.0),
    FieldSchema("sf_p", float, 50, 10, 1.0),
    FieldSchema("sf_t", float, 60, 10, 1.0),
)

class DualcesePointSourceTwo_Phase(KeywordBase):
    """DYNA DUALCESE_POINT_SOURCE_TWO-PHASE keyword"""

    keyword = "DUALCESE"
    subkeyword = "POINT_SOURCE_TWO-PHASE"

    def __init__(self, **kwargs):
        """Initialize the DualcesePointSourceTwo_Phase class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSOURCETWO_PHASE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSOURCETWO_PHASE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOINTSOURCETWO_PHASE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the The ID of a *DUALCESE_POINT_SOURCE_STRUCTNODE_SET card where the point source values in this card should be set.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def lc_mrt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) to describe the mass flow rate as a function of time.
        EQ.0: Constant mass flow rate with value SF_MRT.
        EQ. - 1: The solver computers the mass flow rate.
        """ # nopep8
        return self._cards[1].get_value("lc_mrt")

    @lc_mrt.setter
    def lc_mrt(self, value: int) -> None:
        """Set the lc_mrt property."""
        self._cards[1].set_value("lc_mrt", value)

    @property
    def lc_z1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the magnitude of the volume fraction of material 1 as a function of time.
        EQ.0: Constant volume fraction for material 1 with value SF_Z1.
        EQ. - 1: The solver computes the volume fraction of material.
        """ # nopep8
        return self._cards[1].get_value("lc_z1")

    @lc_z1.setter
    def lc_z1(self, value: int) -> None:
        """Set the lc_z1 property."""
        self._cards[1].set_value("lc_z1", value)

    @property
    def lc_v(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the magnitude of the velocity as a function of time.
        EQ.0: Constant magnitude of velocity with value SF_V.
        EQ. - 1: The solver computes the magnitude of velocity.
        """ # nopep8
        return self._cards[1].get_value("lc_v")

    @lc_v.setter
    def lc_v(self, value: int) -> None:
        """Set the lc_v property."""
        self._cards[1].set_value("lc_v", value)

    @property
    def lc_d1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density of material 1 as a function of time.
        EQ.0: Constant density of material 1 with value SF_D1.
        EQ. - 1: The solver computes the density of material 1
        """ # nopep8
        return self._cards[1].get_value("lc_d1")

    @lc_d1.setter
    def lc_d1(self, value: int) -> None:
        """Set the lc_d1 property."""
        self._cards[1].set_value("lc_d1", value)

    @property
    def lc_d2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density of material 2 as a function of time.
        EQ.0: Constant density of material 2 with value SF_D2.
        EQ. - 1: The solver computes the density of material 2.
        """ # nopep8
        return self._cards[1].get_value("lc_d2")

    @lc_d2.setter
    def lc_d2(self, value: int) -> None:
        """Set the lc_d2 property."""
        self._cards[1].set_value("lc_d2", value)

    @property
    def lc_p(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the pressure as a function of time.
        EQ.0: Constant pressure with value SF_P.
        EQ. - 1: the solver computes the pressure.
        """ # nopep8
        return self._cards[1].get_value("lc_p")

    @lc_p.setter
    def lc_p(self, value: int) -> None:
        """Set the lc_p property."""
        self._cards[1].set_value("lc_p", value)

    @property
    def lc_t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the temperature as a function of time.
        EQ.0: Constant temperature with value SF_T.
        EQ. - 1: The solver computes the temperature.
        """ # nopep8
        return self._cards[1].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: int) -> None:
        """Set the lc_t property."""
        self._cards[1].set_value("lc_t", value)

    @property
    def sf_mrt(self) -> float:
        """Get or set the Scale factor for LC_MRT
        """ # nopep8
        return self._cards[2].get_value("sf_mrt")

    @sf_mrt.setter
    def sf_mrt(self, value: float) -> None:
        """Set the sf_mrt property."""
        self._cards[2].set_value("sf_mrt", value)

    @property
    def sf_z1(self) -> float:
        """Get or set the Scale factor for LC_Z1
        """ # nopep8
        return self._cards[2].get_value("sf_z1")

    @sf_z1.setter
    def sf_z1(self, value: float) -> None:
        """Set the sf_z1 property."""
        self._cards[2].set_value("sf_z1", value)

    @property
    def sf_v(self) -> float:
        """Get or set the Scale factor for LC_V
        """ # nopep8
        return self._cards[2].get_value("sf_v")

    @sf_v.setter
    def sf_v(self, value: float) -> None:
        """Set the sf_v property."""
        self._cards[2].set_value("sf_v", value)

    @property
    def sf_d1(self) -> float:
        """Get or set the Scale factor for LC_D1
        """ # nopep8
        return self._cards[2].get_value("sf_d1")

    @sf_d1.setter
    def sf_d1(self, value: float) -> None:
        """Set the sf_d1 property."""
        self._cards[2].set_value("sf_d1", value)

    @property
    def sf_d2(self) -> float:
        """Get or set the Scale factor for LC_D2
        """ # nopep8
        return self._cards[2].get_value("sf_d2")

    @sf_d2.setter
    def sf_d2(self, value: float) -> None:
        """Set the sf_d2 property."""
        self._cards[2].set_value("sf_d2", value)

    @property
    def sf_p(self) -> float:
        """Get or set the Scale factor for LC_P
        """ # nopep8
        return self._cards[2].get_value("sf_p")

    @sf_p.setter
    def sf_p(self, value: float) -> None:
        """Set the sf_p property."""
        self._cards[2].set_value("sf_p", value)

    @property
    def sf_t(self) -> float:
        """Get or set the Scale factor for LC_T
        """ # nopep8
        return self._cards[2].get_value("sf_t")

    @sf_t.setter
    def sf_t(self, value: float) -> None:
        """Set the sf_t property."""
        self._cards[2].set_value("sf_t", value)

