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

"""Module providing the ConstrainedJointStiffnessGeneralized class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD0 = (
    FieldSchema("jsid", int, 0, 10, None),
    FieldSchema("pida", int, 10, 10, None),
    FieldSchema("pidb", int, 20, 10, None),
    FieldSchema("cida", int, 30, 10, None),
    FieldSchema("cidb", int, 40, 10, 0),
    FieldSchema("jid", int, 50, 10, None),
)

_CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD1 = (
    FieldSchema("lcidph", int, 0, 10, 0),
    FieldSchema("lcidt", int, 10, 10, 0),
    FieldSchema("lcidps", int, 20, 10, 0),
    FieldSchema("dlcidph", int, 30, 10, 0),
    FieldSchema("dlcidt", int, 40, 10, 0),
    FieldSchema("dlcidps", int, 50, 10, 0),
)

_CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD2 = (
    FieldSchema("esph", float, 0, 10, 0.0),
    FieldSchema("fmph", float, 10, 10, 0.0),
    FieldSchema("est", float, 20, 10, 0.0),
    FieldSchema("fmt", float, 30, 10, 0.0),
    FieldSchema("esps", float, 40, 10, 0.0),
    FieldSchema("fmps", float, 50, 10, 0.0),
)

_CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD3 = (
    FieldSchema("nsaph", float, 0, 10, 0.0),
    FieldSchema("psaph", float, 10, 10, 0.0),
    FieldSchema("nsat", float, 20, 10, 0.0),
    FieldSchema("psat", float, 30, 10, 0.0),
    FieldSchema("nsaps", float, 40, 10, 0.0),
    FieldSchema("psaps", float, 50, 10, 0.0),
)

class ConstrainedJointStiffnessGeneralized(KeywordBase):
    """DYNA CONSTRAINED_JOINT_STIFFNESS_GENERALIZED keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_STIFFNESS_GENERALIZED"
    _link_fields = {
        "lcidph": LinkType.DEFINE_CURVE,
        "lcidt": LinkType.DEFINE_CURVE,
        "lcidps": LinkType.DEFINE_CURVE,
        "dlcidph": LinkType.DEFINE_CURVE,
        "dlcidt": LinkType.DEFINE_CURVE,
        "dlcidps": LinkType.DEFINE_CURVE,
        "cida": LinkType.DEFINE_COORDINATE_SYSTEM,
        "cidb": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pida": LinkType.PART,
        "pidb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedJointStiffnessGeneralized class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSGENERALIZED_CARD3,
                **kwargs,
            ),        ]
    @property
    def jsid(self) -> typing.Optional[int]:
        """Get or set the Joint stiffness ID.
        """ # nopep8
        return self._cards[0].get_value("jsid")

    @jsid.setter
    def jsid(self, value: int) -> None:
        """Set the jsid property."""
        self._cards[0].set_value("jsid", value)

    @property
    def pida(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body A, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pida")

    @pida.setter
    def pida(self, value: int) -> None:
        """Set the pida property."""
        self._cards[0].set_value("pida", value)

    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body B, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        """Set the pidb property."""
        self._cards[0].set_value("pidb", value)

    @property
    def cida(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("cida")

    @cida.setter
    def cida(self, value: int) -> None:
        """Set the cida property."""
        self._cards[0].set_value("cida", value)

    @property
    def cidb(self) -> int:
        """Get or set the Coordinate ID for rigid body B.
        If zero, the coordinate ID for rigid body A is used (default).See *DEFINE_COORDINATE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("cidb")

    @cidb.setter
    def cidb(self, value: int) -> None:
        """Set the cidb property."""
        self._cards[0].set_value("cidb", value)

    @property
    def jid(self) -> typing.Optional[int]:
        """Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
        """ # nopep8
        return self._cards[0].get_value("jid")

    @jid.setter
    def jid(self, value: int) -> None:
        """Set the jid property."""
        self._cards[0].set_value("jid", value)

    @property
    def lcidph(self) -> int:
        """Get or set the Load curve ID for x-moment versus rotation in radians.
        If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidph")

    @lcidph.setter
    def lcidph(self, value: int) -> None:
        """Set the lcidph property."""
        self._cards[1].set_value("lcidph", value)

    @property
    def lcidt(self) -> int:
        """Get or set the Load curve ID for y-moment versus rotation in radians.
        If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[1].set_value("lcidt", value)

    @property
    def lcidps(self) -> int:
        """Get or set the Load curve ID for z-moment versus rotation in radians.
        If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidps")

    @lcidps.setter
    def lcidps(self, value: int) -> None:
        """Set the lcidps property."""
        self._cards[1].set_value("lcidps", value)

    @property
    def dlcidph(self) -> int:
        """Get or set the Load curve ID for x-damping moment versus rate of rotation in radians per unit time.
        If zero, damping is not considered (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidph")

    @dlcidph.setter
    def dlcidph(self, value: int) -> None:
        """Set the dlcidph property."""
        self._cards[1].set_value("dlcidph", value)

    @property
    def dlcidt(self) -> int:
        """Get or set the Load curve ID for y-damping moment versus rate of rotation in radians per unit time.
        If zero, damping is not considered (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidt")

    @dlcidt.setter
    def dlcidt(self, value: int) -> None:
        """Set the dlcidt property."""
        self._cards[1].set_value("dlcidt", value)

    @property
    def dlcidps(self) -> int:
        """Get or set the Load curve ID for z-damping torque versus rate of rotation in radians per unit time.
        If zero, damping is not considered (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidps")

    @dlcidps.setter
    def dlcidps(self, value: int) -> None:
        """Set the dlcidps property."""
        self._cards[1].set_value("dlcidps", value)

    @property
    def esph(self) -> float:
        """Get or set the Elastic stiffness per unit radian for friction and stop angles for x-rotation.
        If zero, friction and stop angles are inactive for x-rotation (default).
        """ # nopep8
        return self._cards[2].get_value("esph")

    @esph.setter
    def esph(self, value: float) -> None:
        """Set the esph property."""
        self._cards[2].set_value("esph", value)

    @property
    def fmph(self) -> float:
        """Get or set the Frictional moment limiting value for x-rotation. If zero, friction is inactive for x-rotation. This option may also be thought of as an elastic-plastic spring.
        """ # nopep8
        return self._cards[2].get_value("fmph")

    @fmph.setter
    def fmph(self, value: float) -> None:
        """Set the fmph property."""
        self._cards[2].set_value("fmph", value)

    @property
    def est(self) -> float:
        """Get or set the Elastic stiffness per unit radian for friction and stop angles for y-rotation.
        If zero, friction and stop angles are inactive for y-rotation (default).
        """ # nopep8
        return self._cards[2].get_value("est")

    @est.setter
    def est(self, value: float) -> None:
        """Set the est property."""
        self._cards[2].set_value("est", value)

    @property
    def fmt(self) -> float:
        """Get or set the Frictional moment limiting value for y-rotation. If zero, friction is inactive for y-rotation. This option may also be thought of as an elastic-plastic spring.
        """ # nopep8
        return self._cards[2].get_value("fmt")

    @fmt.setter
    def fmt(self, value: float) -> None:
        """Set the fmt property."""
        self._cards[2].set_value("fmt", value)

    @property
    def esps(self) -> float:
        """Get or set the ESPS Elastic stiffness per unit radian for friction and stop angles for z-rotation.
        If zero, friction and stop angles are inactive for z-rotation (default).
        """ # nopep8
        return self._cards[2].get_value("esps")

    @esps.setter
    def esps(self, value: float) -> None:
        """Set the esps property."""
        self._cards[2].set_value("esps", value)

    @property
    def fmps(self) -> float:
        """Get or set the Frictional moment limiting value for z-rotation.
        If zero, friction is inactive for z-rotation (default).
        """ # nopep8
        return self._cards[2].get_value("fmps")

    @fmps.setter
    def fmps(self, value: float) -> None:
        """Set the fmps property."""
        self._cards[2].set_value("fmps", value)

    @property
    def nsaph(self) -> float:
        """Get or set the Stop angle in degrees for negative x-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("nsaph")

    @nsaph.setter
    def nsaph(self, value: float) -> None:
        """Set the nsaph property."""
        self._cards[3].set_value("nsaph", value)

    @property
    def psaph(self) -> float:
        """Get or set the Stop angle in degrees for positive x-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("psaph")

    @psaph.setter
    def psaph(self, value: float) -> None:
        """Set the psaph property."""
        self._cards[3].set_value("psaph", value)

    @property
    def nsat(self) -> float:
        """Get or set the Stop angle in degrees for negative y-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("nsat")

    @nsat.setter
    def nsat(self, value: float) -> None:
        """Set the nsat property."""
        self._cards[3].set_value("nsat", value)

    @property
    def psat(self) -> float:
        """Get or set the Stop angle in degrees for positive y-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("psat")

    @psat.setter
    def psat(self, value: float) -> None:
        """Set the psat property."""
        self._cards[3].set_value("psat", value)

    @property
    def nsaps(self) -> float:
        """Get or set the Stop angle in degrees for negative z-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("nsaps")

    @nsaps.setter
    def nsaps(self, value: float) -> None:
        """Set the nsaps property."""
        self._cards[3].set_value("nsaps", value)

    @property
    def psaps(self) -> float:
        """Get or set the Stop angle in degrees for positive z-rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("psaps")

    @psaps.setter
    def psaps(self, value: float) -> None:
        """Set the psaps property."""
        self._cards[3].set_value("psaps", value)

    @property
    def lcidph_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidph."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidph:
                return kwd
        return None

    @lcidph_link.setter
    def lcidph_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidph."""
        self.lcidph = value.lcid

    @property
    def lcidt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidt:
                return kwd
        return None

    @lcidt_link.setter
    def lcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidt."""
        self.lcidt = value.lcid

    @property
    def lcidps_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidps."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidps:
                return kwd
        return None

    @lcidps_link.setter
    def lcidps_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidps."""
        self.lcidps = value.lcid

    @property
    def dlcidph_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for dlcidph."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidph:
                return kwd
        return None

    @dlcidph_link.setter
    def dlcidph_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidph."""
        self.dlcidph = value.lcid

    @property
    def dlcidt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for dlcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidt:
                return kwd
        return None

    @dlcidt_link.setter
    def dlcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidt."""
        self.dlcidt = value.lcid

    @property
    def dlcidps_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for dlcidps."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidps:
                return kwd
        return None

    @dlcidps_link.setter
    def dlcidps_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidps."""
        self.dlcidps = value.lcid

    @property
    def cida_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cida."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cida:
                return kwd
        return None

    @cida_link.setter
    def cida_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cida."""
        self.cida = value.cid

    @property
    def cidb_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cidb."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cidb:
                return kwd
        return None

    @cidb_link.setter
    def cidb_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cidb."""
        self.cidb = value.cid

    @property
    def pida_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pida."""
        return self._get_link_by_attr("PART", "pid", self.pida, "parts")

    @property
    def pidb_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pidb."""
        return self._get_link_by_attr("PART", "pid", self.pidb, "parts")

