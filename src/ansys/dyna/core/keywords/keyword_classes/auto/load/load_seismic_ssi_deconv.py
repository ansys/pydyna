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

"""Module providing the LoadSeismicSsiDeconv class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADSEISMICSSIDECONV_CARD0 = (
    FieldSchema("ssid", int, 0, 8, None),
    FieldSchema("xp", float, 8, 16, 0.0),
    FieldSchema("yp", float, 24, 16, 0.0),
    FieldSchema("zp", float, 40, 16, 0.0),
    FieldSchema("gmx", int, 56, 8, None),
    FieldSchema("gmy", int, 64, 8, None),
    FieldSchema("gmz", int, 72, 8, None),
)

_LOADSEISMICSSIDECONV_CARD1 = (
    FieldSchema("sf", float, 0, 10, 1.0),
    FieldSchema("cid", int, 10, 10, 0),
    FieldSchema("birth", float, 20, 10, 0.0),
    FieldSchema("death", float, 30, 10, 1e+28),
    FieldSchema("isg", int, 40, 10, 0),
    FieldSchema("igm", int, 50, 10, 0),
    FieldSchema("pset", int, 60, 10, None),
    FieldSchema("vdir", int, 70, 10, 3),
)

class LoadSeismicSsiDeconv(KeywordBase):
    """DYNA LOAD_SEISMIC_SSI_DECONV keyword"""

    keyword = "LOAD"
    subkeyword = "SEISMIC_SSI_DECONV"

    def __init__(self, **kwargs):
        """Initialize the LoadSeismicSsiDeconv class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSIDECONV_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSEISMICSSIDECONV_CARD1,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def xp(self) -> float:
        """Get or set the Curve multiplier at node N1.
        """ # nopep8
        return self._cards[0].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[0].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the Curve multiplier at node N2.
        """ # nopep8
        return self._cards[0].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[0].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the Curve multiplier at node N3.
        """ # nopep8
        return self._cards[0].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[0].set_value("zp", value)

    @property
    def gmx(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) x-direction.
        """ # nopep8
        return self._cards[0].get_value("gmx")

    @gmx.setter
    def gmx(self, value: int) -> None:
        """Set the gmx property."""
        self._cards[0].set_value("gmx", value)

    @property
    def gmy(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) y-direction.
        """ # nopep8
        return self._cards[0].get_value("gmy")

    @gmy.setter
    def gmy(self, value: int) -> None:
        """Set the gmy property."""
        self._cards[0].set_value("gmy", value)

    @property
    def gmz(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) z-direction.
        """ # nopep8
        return self._cards[0].get_value("gmz")

    @gmz.setter
    def gmz(self, value: int) -> None:
        """Set the gmz property."""
        self._cards[0].set_value("gmz", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[1].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
        """ # nopep8
        return self._cards[1].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        """Set the isg property."""
        if value not in [0, 1, None]:
            raise Exception("""isg must be `None` or one of {0,1}.""")
        self._cards[1].set_value("isg", value)

    @property
    def igm(self) -> int:
        """Get or set the Specification of ground motions GMX, GMY, GMZ:
        EQ.0: ground motions are specified as acceleration load curves. See *DEFINE_CURVE
        EQ.1: Both ground accelerations and velocities specified using *DEFINE_GROUND_MOTION
        .
        """ # nopep8
        return self._cards[1].get_value("igm")

    @igm.setter
    def igm(self, value: int) -> None:
        """Set the igm property."""
        if value not in [0, 1, None]:
            raise Exception("""igm must be `None` or one of {0,1}.""")
        self._cards[1].set_value("igm", value)

    @property
    def pset(self) -> typing.Optional[int]:
        """Get or set the Soil part set through which ground motion travels (DECONV option only)
        """ # nopep8
        return self._cards[1].get_value("pset")

    @pset.setter
    def pset(self, value: int) -> None:
        """Set the pset property."""
        self._cards[1].set_value("pset", value)

    @property
    def vdir(self) -> int:
        """Get or set the Vertical direction (local if CID ≠ 0) for ground motion propagation (DECONV option only):
        EQ. - 1:	-x - direction
        EQ. - 2 : -y - direction
        EQ. - 3 : -z - direction
        EQ.1 : x - direction
        EQ.2 : y - direction
        EQ.3 : z - direction
        """ # nopep8
        return self._cards[1].get_value("vdir")

    @vdir.setter
    def vdir(self, value: int) -> None:
        """Set the vdir property."""
        if value not in [3, -1, -2, -3, 1, 2, None]:
            raise Exception("""vdir must be `None` or one of {3,-1,-2,-3,1,2}.""")
        self._cards[1].set_value("vdir", value)

