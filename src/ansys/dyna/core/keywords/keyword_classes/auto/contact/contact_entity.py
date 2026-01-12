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

"""Module providing the ContactEntity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTENTITY_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("geotyp", int, 10, 10, 1),
    FieldSchema("ssid", int, 20, 10, None),
    FieldSchema("sstyp", int, 30, 10, 0),
    FieldSchema("sf", float, 40, 10, 1.0),
    FieldSchema("df", float, 50, 10, 0.0),
    FieldSchema("cf", float, 60, 10, 0.0),
    FieldSchema("intord", int, 70, 10, 0),
)

_CONTACTENTITY_CARD1 = (
    FieldSchema("bt", float, 0, 10, 0.0),
    FieldSchema("dt", float, 10, 10, 1e+20),
    FieldSchema("so", int, 20, 10, 0),
    FieldSchema("go", int, 30, 10, 0),
    FieldSchema("ithk", int, 40, 10, 0),
    FieldSchema("spr", int, 50, 10, 0),
)

_CONTACTENTITY_CARD2 = (
    FieldSchema("xc", float, 0, 10, 0.0),
    FieldSchema("yc", float, 10, 10, 0.0),
    FieldSchema("zc", float, 20, 10, 0.0),
    FieldSchema("ax", float, 30, 10, 0.0),
    FieldSchema("ay", float, 40, 10, 0.0),
    FieldSchema("az", float, 50, 10, 0.0),
)

_CONTACTENTITY_CARD3 = (
    FieldSchema("bx", float, 0, 10, 0.0),
    FieldSchema("by", float, 10, 10, 0.0),
    FieldSchema("bz", float, 20, 10, 0.0),
)

_CONTACTENTITY_CARD4 = (
    FieldSchema("inout", int, 0, 10, 0),
    FieldSchema("g1", float, 10, 10, 0.0),
    FieldSchema("g2", float, 20, 10, 0.0),
    FieldSchema("g3", float, 30, 10, 0.0),
    FieldSchema("g4", float, 40, 10, 0.0),
    FieldSchema("g5", float, 50, 10, 0.0),
    FieldSchema("g6", float, 60, 10, 0.0),
    FieldSchema("g7", float, 70, 10, 0.0),
)

class ContactEntity(KeywordBase):
    """DYNA CONTACT_ENTITY keyword"""

    keyword = "CONTACT"
    subkeyword = "ENTITY"

    def __init__(self, **kwargs):
        """Initialize the ContactEntity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTENTITY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTENTITY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTENTITY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTENTITY_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTENTITY_CARD4,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid body to which the geometric entity is attached, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def geotyp(self) -> int:
        """Get or set the Type of geometric entity:
        EQ.1: plane (default),
        EQ.2: sphere,
        EQ.3: cylinder,
        EQ.4: ellipsoid,
        EQ.5: torus,
        EQ.6: CAL3D/MADYMO plane, see Appendix F of USER MANUAL,
        EQ.7: CAL3D/MADYMO ellipsoid, see Appendix F of USER MANUAL,
        EQ.8: VDA surface, see Appendix I of USER MANUAL,
        EQ.9: rigid body finite element mesh (shells only),
        EQ.10: finite plane,
        EQ.11: load curve defining line as surface profile of axisymmetric rigid bodies.
        """ # nopep8
        return self._cards[0].get_value("geotyp")

    @geotyp.setter
    def geotyp(self, value: int) -> None:
        """Set the geotyp property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, None]:
            raise Exception("""geotyp must be `None` or one of {1,2,3,4,5,6,7,8,9,10,11}.""")
        self._cards[0].set_value("geotyp", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Slave set type:
        EQ.0: node set (default),
        EQ.1: part ID,
        EQ.2: part set ID.
        """ # nopep8
        return self._cards[0].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        """Set the sstyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sstyp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sstyp", value)

    @property
    def sf(self) -> float:
        """Get or set the Penalty scale factor. Useful to scale maximized penalty.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def df(self) -> float:
        """Get or set the Damping option, see description for *CONTACT_OPTION:
        EQ.0.0: no damping (default),
        GT.0.0: viscous damping in percent of critical, e.g., 20.0 for 20% damping,
        EQ.-n: |n| is the load curve ID giving the damping force versus relative normal velocity.
        """ # nopep8
        return self._cards[0].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        """Set the df property."""
        self._cards[0].set_value("df", value)

    @property
    def cf(self) -> float:
        """Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("cf")

    @cf.setter
    def cf(self, value: float) -> None:
        """Set the cf property."""
        self._cards[0].set_value("cf", value)

    @property
    def intord(self) -> int:
        """Get or set the Integration order (slaved materials only). This option is not available with entity types 8 and 9 where only nodes are checked:
        EQ.0: check nodes only (default),
        EQ.1: 1-point integration over segments,
        EQ.2: 2x2 integration,
        EQ.3: 3x3 integration,
        EQ.4: 4x4 integration,
        EQ.5: 5x5 integration.
        This option allows a check of the penetration of the rigid body into the deformable (slaved) material. Then virtual nodes at the location of the integration points are checked.
        """ # nopep8
        return self._cards[0].get_value("intord")

    @intord.setter
    def intord(self, value: int) -> None:
        """Set the intord property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""intord must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("intord", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[1].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[1].set_value("dt", value)

    @property
    def so(self) -> int:
        """Get or set the Flag to use penalty stiffness as in surface to surface contact:
        EQ.0: contact entity stiffness formulation (default),
        EQ.1: surface to surface contact method,
        EQ.-n: |n| is the load curve ID giving the force versus the normal penetration.
        """ # nopep8
        return self._cards[1].get_value("so")

    @so.setter
    def so(self, value: int) -> None:
        """Set the so property."""
        self._cards[1].set_value("so", value)

    @property
    def go(self) -> int:
        """Get or set the Flag for mesh generation of the contact entity for entity types 1-5 and 10-11. This is used for visualization in post-processing only:
        EQ.0: mesh is not generated (default),
        EQ.1: mesh is generated.
        """ # nopep8
        return self._cards[1].get_value("go")

    @go.setter
    def go(self, value: int) -> None:
        """Set the go property."""
        if value not in [0, 1, None]:
            raise Exception("""go must be `None` or one of {0,1}.""")
        self._cards[1].set_value("go", value)

    @property
    def ithk(self) -> int:
        """Get or set the Flag for considering thickness for shell slave nodes (applies only
        to entity types 1, 2, 3; SSTYP must be set to zero).
        EQ.0: shell thickness is not considered,
        EQ.1: shell thickness is considered.
        """ # nopep8
        return self._cards[1].get_value("ithk")

    @ithk.setter
    def ithk(self, value: int) -> None:
        """Set the ithk property."""
        if value not in [0, 1, None]:
            raise Exception("""ithk must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ithk", value)

    @property
    def spr(self) -> int:
        """Get or set the Include the slave side in *DATABASE_BINARY_INTFOR
        interface force files; valid only when SSTYP > 0:
        EQ.1: slave side forces included..
        """ # nopep8
        return self._cards[1].get_value("spr")

    @spr.setter
    def spr(self, value: int) -> None:
        """Set the spr property."""
        self._cards[1].set_value("spr", value)

    @property
    def xc(self) -> float:
        """Get or set the xc, x-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the yc, y-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[2].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the zc, z-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[2].set_value("zc", value)

    @property
    def ax(self) -> float:
        """Get or set the Ax, x-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        """Set the ax property."""
        self._cards[2].set_value("ax", value)

    @property
    def ay(self) -> float:
        """Get or set the Ay, y-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        """Set the ay property."""
        self._cards[2].set_value("ay", value)

    @property
    def az(self) -> float:
        """Get or set the Az, z-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        """Set the az property."""
        self._cards[2].set_value("az", value)

    @property
    def bx(self) -> float:
        """Get or set the Bx, x-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("bx")

    @bx.setter
    def bx(self, value: float) -> None:
        """Set the bx property."""
        self._cards[3].set_value("bx", value)

    @property
    def by(self) -> float:
        """Get or set the By, y-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("by")

    @by.setter
    def by(self, value: float) -> None:
        """Set the by property."""
        self._cards[3].set_value("by", value)

    @property
    def bz(self) -> float:
        """Get or set the Bz, z-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("bz")

    @bz.setter
    def bz(self, value: float) -> None:
        """Set the bz property."""
        self._cards[3].set_value("bz", value)

    @property
    def inout(self) -> int:
        """Get or set the In-out flag. Allows contact from the inside or the outside of the entity:
        EQ.0: slave nodes exist outside of the entity (default),
        EQ.1: slave nodes exist inside the entity.
        """ # nopep8
        return self._cards[4].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        """Set the inout property."""
        if value not in [0, 1, None]:
            raise Exception("""inout must be `None` or one of {0,1}.""")
        self._cards[4].set_value("inout", value)

    @property
    def g1(self) -> float:
        """Get or set the Entity coefficient g1 (CAL3D/MADYMO plane or ellipse number) for coupled analysis.
        For further information please see USER MANUAL section 6.33 and Appendix F.
        """ # nopep8
        return self._cards[4].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        """Set the g1 property."""
        self._cards[4].set_value("g1", value)

    @property
    def g2(self) -> float:
        """Get or set the Entity coefficient g2.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        """Set the g2 property."""
        self._cards[4].set_value("g2", value)

    @property
    def g3(self) -> float:
        """Get or set the Entity coefficient g3.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        """Set the g3 property."""
        self._cards[4].set_value("g3", value)

    @property
    def g4(self) -> float:
        """Get or set the Entity coefficient g4.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        """Set the g4 property."""
        self._cards[4].set_value("g4", value)

    @property
    def g5(self) -> float:
        """Get or set the Entity coefficient g5.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        """Set the g5 property."""
        self._cards[4].set_value("g5", value)

    @property
    def g6(self) -> float:
        """Get or set the Entity coefficient g6.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        """Set the g6 property."""
        self._cards[4].set_value("g6", value)

    @property
    def g7(self) -> float:
        """Get or set the Entity coefficient g7.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g7")

    @g7.setter
    def g7(self, value: float) -> None:
        """Set the g7 property."""
        self._cards[4].set_value("g7", value)

