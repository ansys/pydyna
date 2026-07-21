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

"""Module providing the Mat020 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT020_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("n", float, 40, 10, 0.0),
    FieldSchema("couple", float, 50, 10, 0.0),
    FieldSchema("m", float, 60, 10, 0.0),
    FieldSchema("alias", str, 70, 10, None),
)

_MAT020_CARD1 = (
    FieldSchema("cmo", float, 0, 10, 0.0),
    FieldSchema("con1", float, 10, 10, None),
    FieldSchema("con2", float, 20, 10, None),
    FieldSchema("spcnid", int, 30, 10, None),
    FieldSchema("xspc", float, 40, 10, None),
    FieldSchema("yspc", float, 50, 10, None),
    FieldSchema("zspc", float, 60, 10, None),
)

_MAT020_CARD2 = (
    FieldSchema("lco_or_a1", float, 0, 10, None, "lco or a1"),
    FieldSchema("a2", float, 10, 10, None),
    FieldSchema("a3", float, 20, 10, None),
    FieldSchema("v1", float, 30, 10, None),
    FieldSchema("v2", float, 40, 10, None),
    FieldSchema("v3", float, 50, 10, None),
    FieldSchema("bndlco", int, 60, 10, 0),
)

_MAT020_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat020(KeywordBase):
    """DYNA MAT_020 keyword"""

    keyword = "MAT"
    subkeyword = "020"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat020 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT020_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT020_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT020_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat020._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT020_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus. Reasonable values have to be chosen for contact analysis (choice of penalty).
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio. Reasonable values have to be chosen for contact analysis (choice of penalty).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> float:
        """Get or set the MADYMO3D (not CAL3D) coupling flag, n:
        EQ.0: use normal LS-DYNA rigid body updates,
        GT.0: the rigid body is coupled to MADYMO ellipsoid number n,
        LT.0: the rigid body is coupled to MADYMO plane number |n|.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def couple(self) -> float:
        """Get or set the Coupling option if applicable:
        EQ.-1: attach VDA surface in ALIAS (defined in the eighth field) and automatically generate a mesh for viewing the surface in LS-TAURUS.

        MADYMO3D/CAL3D coupling option:
        EQ.0: the undeformed geometry input to LS-DYNA corresponds to the local system for MADYMO/CAL3D. The finite element mesh is input,
        EQ.1: the undeformed geometry input to LS-DYNA corresponds to the global system for MADYMO/CAL3D,
        EQ.2: generate a mesh for the ellipsoids and planes internally in LS-DYNA3D.
        """ # nopep8
        return self._cards[0].get_value("couple")

    @couple.setter
    def couple(self, value: float) -> None:
        """Set the couple property."""
        if value not in [0.0, -1.0, 1.0, 2.0, None]:
            raise Exception("""couple must be `None` or one of {0.0,-1.0,1.0,2.0}.""")
        self._cards[0].set_value("couple", value)

    @property
    def m(self) -> float:
        """Get or set the MADYMO/CAL3D Coupling option flag:
        EQ.0: use normal LS-DYNA rigid body updates,
        EQ.m: this rigid body corresponds to MADYMO/CAL3D system number m. Rigid body updates are performed by MADYMO/CAL3D.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def alias(self) -> typing.Optional[str]:
        """Get or set the VDA surface alias name, see keyword manual Appendix I.
        """ # nopep8
        return self._cards[0].get_value("alias")

    @alias.setter
    def alias(self, value: str) -> None:
        """Set the alias property."""
        self._cards[0].set_value("alias", value)

    @property
    def cmo(self) -> float:
        """Get or set the Constraint option, CMO (see Remark 5):
        EQ.+2.0: Constraints applied in global directions at the coordinate given by XSPC, YSPCand ZSPC or the initial coordinates of node SPCNID.Unless prescribed motion is applied to the rigid body, the constraint coordinates are fixed in time.
        EQ.+1.0: Constraints applied in global directions,
        EQ.0.0: No constraints,
        EQ.-1.0: Constraints applied in local directions(SPC constraint).
        EQ.-2.0: Constraints applied in local directions(SPC constraint) at the coordinate given by XSPC, YSPCand ZSPC or the initial coordinates of node SPCNID.Unless prescribed motion is applied to the rigid body, the constraint coordinates are fixed in time.
        """ # nopep8
        return self._cards[1].get_value("cmo")

    @cmo.setter
    def cmo(self, value: float) -> None:
        """Set the cmo property."""
        if value not in [0.0, -1.0, 1.0, -2.0, 2.0, None]:
            raise Exception("""cmo must be `None` or one of {0.0,-1.0,1.0,-2.0,2.0}.""")
        self._cards[1].set_value("cmo", value)

    @property
    def con1(self) -> typing.Optional[float]:
        """Get or set the First constraint parameter.
        If CMO > 0.0, then specify the global translational constraint:
        EQ.0: No constraints,
        EQ.1: Constrained x displacement,
        EQ.2: Constrained y displacement,
        EQ.3: Constrained z displacement,
        EQ.4: Constrained x and y displacements,
        EQ.5: Constrained y and z displacements,
        EQ.6: Constrained z and x displacements,
        EQ.7: Constrained x, y,and z displacements.
        If CMO < 0.0, then specify the local coordinate system ID. See *DEFINE_COORDINATE_OPTION. This coordinate system is fixed in time.
        """ # nopep8
        return self._cards[1].get_value("con1")

    @con1.setter
    def con1(self, value: float) -> None:
        """Set the con1 property."""
        self._cards[1].set_value("con1", value)

    @property
    def con2(self) -> typing.Optional[float]:
        """Get or set the Second constraint parameter:
        If CMO > 0.0, then specify the global rotational constraint:
        EQ.0: No constraints,
        EQ.1: Constrained x rotation,
        EQ.2: Constrained y rotation,
        EQ.3: Constrained z rotation,
        EQ.4: Constrained x and y rotations,
        EQ.5: Constrained y and z rotations,
        EQ.6: Constrained z and x rotations,
        EQ.7: Constrained x, y,and z rotations.
        If CMO < 0.0, then specify the local (SPC) constraint:
        EQ.000000: No constraint,
        EQ.100000: Constrained x translation,
        EQ.010000: Constrained y translation,
        EQ.001000: Constrained z translation,
        EQ.000100: Constrained x rotation,
        EQ.000010: Constrained y rotation,
        EQ.000001: Constrained z rotation.
        To specify a combination of local constraints, input the sum of the desired constraints.
        """ # nopep8
        return self._cards[1].get_value("con2")

    @con2.setter
    def con2(self, value: float) -> None:
        """Set the con2 property."""
        self._cards[1].set_value("con2", value)

    @property
    def spcnid(self) -> typing.Optional[int]:
        """Get or set the For |CMO| = 2.0, the constraints coordinates (see below) will be the (initial) coordinates of the node with this ID.
        """ # nopep8
        return self._cards[1].get_value("spcnid")

    @spcnid.setter
    def spcnid(self, value: int) -> None:
        """Set the spcnid property."""
        self._cards[1].set_value("spcnid", value)

    @property
    def xspc(self) -> typing.Optional[float]:
        """Get or set the Coordinate where the constraint will act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("xspc")

    @xspc.setter
    def xspc(self, value: float) -> None:
        """Set the xspc property."""
        self._cards[1].set_value("xspc", value)

    @property
    def yspc(self) -> typing.Optional[float]:
        """Get or set the Coordinate where the constraint will act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("yspc")

    @yspc.setter
    def yspc(self, value: float) -> None:
        """Set the yspc property."""
        self._cards[1].set_value("yspc", value)

    @property
    def zspc(self) -> typing.Optional[float]:
        """Get or set the Coordinate where the constraint will act. Superseded by SPCNID.
        """ # nopep8
        return self._cards[1].get_value("zspc")

    @zspc.setter
    def zspc(self, value: float) -> None:
        """Set the zspc property."""
        self._cards[1].set_value("zspc", value)

    @property
    def lco_or_a1(self) -> typing.Optional[float]:
        """Get or set the EQ.LCO: Local coordinate system number for output. See *DEFINE_COORDINATE,
        EQ.A1: Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("lco_or_a1")

    @lco_or_a1.setter
    def lco_or_a1(self, value: float) -> None:
        """Set the lco_or_a1 property."""
        self._cards[2].set_value("lco_or_a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[2].set_value("v3", value)

    @property
    def bndlco(self) -> int:
        """Get or set the Flag specifying whether the forces and moments in the bndout file, generated by *BOUNDARY_PRESCRIBED_MOTION_RIGID,
        are expressed in the rigid body's local coordinate system.
        EQ.0: Forces and moments are printed in the global coordinate system (default).
        EQ.1: Forces and moments are printed in the local coordinate system.
        """ # nopep8
        return self._cards[2].get_value("bndlco")

    @bndlco.setter
    def bndlco(self, value: int) -> None:
        """Set the bndlco property."""
        if value not in [0, 1, None]:
            raise Exception("""bndlco must be `None` or one of {0,1}.""")
        self._cards[2].set_value("bndlco", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

