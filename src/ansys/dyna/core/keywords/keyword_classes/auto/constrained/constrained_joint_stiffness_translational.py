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

"""Module providing the ConstrainedJointStiffnessTranslational class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD0 = (
    FieldSchema("jsid", int, 0, 10, None),
    FieldSchema("pida", int, 10, 10, None),
    FieldSchema("pidb", int, 20, 10, None),
    FieldSchema("cida", int, 30, 10, None),
    FieldSchema("cidb", int, 40, 10, 0),
    FieldSchema("jid", int, 50, 10, None),
)

_CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD1 = (
    FieldSchema("lcidx", int, 0, 10, None),
    FieldSchema("lcidy", int, 10, 10, None),
    FieldSchema("lcidz", int, 20, 10, None),
    FieldSchema("dlcidx", int, 30, 10, None),
    FieldSchema("dlcidy", int, 40, 10, None),
    FieldSchema("dlcidz", int, 50, 10, None),
)

_CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD2 = (
    FieldSchema("esx", float, 0, 10, 0.0),
    FieldSchema("ffx", float, 10, 10, 0.0),
    FieldSchema("esy", float, 20, 10, 0.0),
    FieldSchema("ffy", float, 30, 10, 0.0),
    FieldSchema("esz", float, 40, 10, 0.0),
    FieldSchema("ffz", float, 50, 10, 0.0),
)

_CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD3 = (
    FieldSchema("nsdx", float, 0, 10, None),
    FieldSchema("psdx", float, 10, 10, None),
    FieldSchema("nsdy", float, 20, 10, None),
    FieldSchema("psdy", float, 30, 10, None),
    FieldSchema("nsdz", float, 40, 10, None),
    FieldSchema("psdz", float, 50, 10, None),
)

class ConstrainedJointStiffnessTranslational(KeywordBase):
    """DYNA CONSTRAINED_JOINT_STIFFNESS_TRANSLATIONAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_STIFFNESS_TRANSLATIONAL"
    _link_fields = {
        "lcidx": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
        "dlcidx": LinkType.DEFINE_CURVE,
        "dlcidy": LinkType.DEFINE_CURVE,
        "dlcidz": LinkType.DEFINE_CURVE,
        "cida": LinkType.DEFINE_COORDINATE_SYSTEM,
        "cidb": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedJointStiffnessTranslational class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTSTIFFNESSTRANSLATIONAL_CARD3,
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
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for x force versus x-translational relative displacement between the origins of CIDA and CIDB based on the x-direction of CIDB. If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for y force versus y-translational relative displacement between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for z force versus z-translational relative displacement between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[1].set_value("lcidz", value)

    @property
    def dlcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for x damping force versus rate of x-translational displacement per unit time between the origins of CIDA and CIDB based on the x-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidx")

    @dlcidx.setter
    def dlcidx(self, value: int) -> None:
        """Set the dlcidx property."""
        self._cards[1].set_value("dlcidx", value)

    @property
    def dlcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for y damping force versus rate of y-translational displacement per unit time between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidy")

    @dlcidy.setter
    def dlcidy(self, value: int) -> None:
        """Set the dlcidy property."""
        self._cards[1].set_value("dlcidy", value)

    @property
    def dlcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for z damping force versus rate of z-translational displacement per unit time between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidz")

    @dlcidz.setter
    def dlcidz(self, value: int) -> None:
        """Set the dlcidz property."""
        self._cards[1].set_value("dlcidz", value)

    @property
    def esx(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for x-translation.  If zero, friction and stop angles are inactive for x-translation.
        """ # nopep8
        return self._cards[2].get_value("esx")

    @esx.setter
    def esx(self, value: float) -> None:
        """Set the esx property."""
        self._cards[2].set_value("esx", value)

    @property
    def ffx(self) -> float:
        """Get or set the Frictional force limiting value for x-translation.  If zero, friction is inactive for x-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus x-translation.
        """ # nopep8
        return self._cards[2].get_value("ffx")

    @ffx.setter
    def ffx(self, value: float) -> None:
        """Set the ffx property."""
        self._cards[2].set_value("ffx", value)

    @property
    def esy(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for y-translation.   If zero, friction and stop angles are inactive for y-translation.
        """ # nopep8
        return self._cards[2].get_value("esy")

    @esy.setter
    def esy(self, value: float) -> None:
        """Set the esy property."""
        self._cards[2].set_value("esy", value)

    @property
    def ffy(self) -> float:
        """Get or set the Frictional force limiting value for y-translation.  If zero, friction is inactive for y-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus y-translation.
        """ # nopep8
        return self._cards[2].get_value("ffy")

    @ffy.setter
    def ffy(self, value: float) -> None:
        """Set the ffy property."""
        self._cards[2].set_value("ffy", value)

    @property
    def esz(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for z-translation.  If zero, friction and stop angles are inactive for z-translation.
        """ # nopep8
        return self._cards[2].get_value("esz")

    @esz.setter
    def esz(self, value: float) -> None:
        """Set the esz property."""
        self._cards[2].set_value("esz", value)

    @property
    def ffz(self) -> float:
        """Get or set the Frictional force limiting value for z-translation.  If zero, friction is inactive for z-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus z-translation.
        """ # nopep8
        return self._cards[2].get_value("ffz")

    @ffz.setter
    def ffz(self, value: float) -> None:
        """Set the ffz property."""
        self._cards[2].set_value("ffz", value)

    @property
    def nsdx(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative x-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdx")

    @nsdx.setter
    def nsdx(self, value: float) -> None:
        """Set the nsdx property."""
        self._cards[3].set_value("nsdx", value)

    @property
    def psdx(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive x-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdx")

    @psdx.setter
    def psdx(self, value: float) -> None:
        """Set the psdx property."""
        self._cards[3].set_value("psdx", value)

    @property
    def nsdy(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative y-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdy")

    @nsdy.setter
    def nsdy(self, value: float) -> None:
        """Set the nsdy property."""
        self._cards[3].set_value("nsdy", value)

    @property
    def psdy(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive y-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdy")

    @psdy.setter
    def psdy(self, value: float) -> None:
        """Set the psdy property."""
        self._cards[3].set_value("psdy", value)

    @property
    def nsdz(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative z-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdz")

    @nsdz.setter
    def nsdz(self, value: float) -> None:
        """Set the nsdz property."""
        self._cards[3].set_value("nsdz", value)

    @property
    def psdz(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive z-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdz")

    @psdz.setter
    def psdz(self, value: float) -> None:
        """Set the psdz property."""
        self._cards[3].set_value("psdz", value)

    @property
    def lcidx_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidx:
                return kwd
        return None

    @lcidx_link.setter
    def lcidx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidx."""
        self.lcidx = value.lcid

    @property
    def lcidy_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidy:
                return kwd
        return None

    @lcidy_link.setter
    def lcidy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidy."""
        self.lcidy = value.lcid

    @property
    def lcidz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidz:
                return kwd
        return None

    @lcidz_link.setter
    def lcidz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidz."""
        self.lcidz = value.lcid

    @property
    def dlcidx_link(self) -> DefineCurve:
        """Get the DefineCurve object for dlcidx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidx:
                return kwd
        return None

    @dlcidx_link.setter
    def dlcidx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidx."""
        self.dlcidx = value.lcid

    @property
    def dlcidy_link(self) -> DefineCurve:
        """Get the DefineCurve object for dlcidy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidy:
                return kwd
        return None

    @dlcidy_link.setter
    def dlcidy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidy."""
        self.dlcidy = value.lcid

    @property
    def dlcidz_link(self) -> DefineCurve:
        """Get the DefineCurve object for dlcidz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.dlcidz:
                return kwd
        return None

    @dlcidz_link.setter
    def dlcidz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for dlcidz."""
        self.dlcidz = value.lcid

    @property
    def cida_link(self) -> DefineCoordinateSystem:
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
    def cidb_link(self) -> DefineCoordinateSystem:
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

