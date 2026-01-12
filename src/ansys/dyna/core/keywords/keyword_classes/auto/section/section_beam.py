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

"""Module providing the SectionBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONBEAM_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 1),
    FieldSchema("shrf", float, 20, 10, 1.0),
    FieldSchema("qr/irid", int, 30, 10, 2),
    FieldSchema("cst", int, 40, 10, 0),
    FieldSchema("scoor", float, 50, 10, 0.0),
    FieldSchema("nsm", float, 60, 10, 0.0),
    FieldSchema("naupd", int, 70, 10, 0),
)

_SECTIONBEAM_CARD1 = (
    FieldSchema("ts1", float, 0, 10, None),
    FieldSchema("ts2", float, 10, 10, None),
    FieldSchema("tt1", float, 20, 10, None),
    FieldSchema("tt2", float, 30, 10, None),
    FieldSchema("nsloc", float, 40, 10, None),
    FieldSchema("ntloc", float, 50, 10, None),
)

_SECTIONBEAM_CARD2 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("iss", float, 10, 10, None),
    FieldSchema("itt", float, 20, 10, None),
    FieldSchema("j", float, 30, 10, None),
    FieldSchema("sa", float, 40, 10, None),
    FieldSchema("ist", float, 50, 10, None),
)

_SECTIONBEAM_CARD3 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("rampt", float, 10, 10, None),
    FieldSchema("stress", float, 20, 10, None),
)

_SECTIONBEAM_CARD4 = (
    FieldSchema("ts1", float, 0, 10, None),
    FieldSchema("ts2", float, 10, 10, None),
    FieldSchema("tt1", float, 20, 10, None),
    FieldSchema("tt2", float, 30, 10, None),
)

_SECTIONBEAM_CARD5 = (
    FieldSchema("vol", float, 0, 10, None),
    FieldSchema("iner", float, 10, 10, None),
    FieldSchema("cid", int, 20, 10, None),
    FieldSchema("ca", float, 30, 10, None),
    FieldSchema("offset", float, 40, 10, None),
    FieldSchema("rrcon", float, 50, 10, 0.0),
    FieldSchema("srcon", float, 60, 10, 0.0),
    FieldSchema("trcon", float, 70, 10, 0.0),
)

_SECTIONBEAM_CARD6 = (
    FieldSchema("ts1", float, 0, 10, None),
    FieldSchema("ts2", float, 10, 10, None),
    FieldSchema("tt1", float, 20, 10, None),
    FieldSchema("tt2", float, 30, 10, None),
    FieldSchema("print", float, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("itoff", float, 60, 10, None),
)

class SectionBeam(KeywordBase):
    """DYNA SECTION_BEAM keyword"""

    keyword = "SECTION"
    subkeyword = "BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD1,
                active_func=lambda: self.elform in [1,11],
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD2,
                active_func=lambda: self.elform in [2,12,13],
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD3,
                active_func=lambda: self.elform == 3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD4,
                active_func=lambda: self.elform in [4,5],
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD5,
                active_func=lambda: self.elform == 6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONBEAM_CARD6,
                active_func=lambda: self.elform == 9,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionBeam.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation options:
        EQ.1: Hughes-Liu with cross section integration (default),
        EQ.2: Belytschko-Schwer resultant beam (resultant),
        EQ.3: truss (resultant),
        EQ.4: Belytschko-Schwer full cross-section integration,
        EQ.5: Belytschko-Schwer tubular beam with cross-section integration,
        EQ.6: discrete beam/cable,
        EQ.7: 2D plane strain shell element (xy plane),
        EQ.8: 2D axisymmetric volume weighted shell element (xy plane),
        EQ.9: spotweld beam, see *MAT_SPOTWELD (Type 100).
        Note that the 2D and 3D element types must bot be mixed and different types of 2D elements must not be used together. For example, the plane strain element type must not be used with the axisymmetric element type. In 3D the different beam elements types, i.e., 1-6 and 9 can be freely mixed together.
        EQ.11: Integrated warped beam.
        EQ.12: resultant warped beam
        EQ.13: Small displacement, linear Timoshenko beam with exact stiffness.
        EQ.14: Integrated tubular Elbow element. User defined integration rule with tubular cross section (9) must be used.
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, None]:
            raise Exception("""elform must be `None` or one of {1,2,3,4,5,6,7,8,9,11,12,13,14}.""")
        self._cards[0].set_value("elform", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear factor. This factor is not needed for truss, resultant beam, discrete beam, and cable elements. The recommended value for rectangular sections is 5/6, the default is 1.0.
        """ # nopep8
        return self._cards[0].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[0].set_value("shrf", value)

    @property
    def qr_irid(self) -> int:
        """Get or set the Quadrature rule or rule number for user defined rule for integrated beams:
        EQ.1: one integration point,
        EQ.2: 2x2 Gauss quadrature (default beam),
        EQ.3: 3x3 Gauss quadrature,
        EQ.4: 3x3 Lobatto quadrature,
        EQ.5: 4x4 Gauss quadrature,
        EQ.-n: where |n| is the number of the user defined rule. IRID integration rule n is defined using *INTEGRATION_BEAM card.
        """ # nopep8
        return self._cards[0].get_value("qr/irid")

    @qr_irid.setter
    def qr_irid(self, value: int) -> None:
        """Set the qr_irid property."""
        self._cards[0].set_value("qr/irid", value)

    @property
    def cst(self) -> int:
        """Get or set the Cross section type, not needed for truss, resultant beam, discrete beam, and cable elements:
        EQ.0: rectangular (default),
        EQ.1: tubular,
        EQ.2: arbitrary (user defined integration rule).
        """ # nopep8
        return self._cards[0].get_value("cst")

    @cst.setter
    def cst(self, value: int) -> None:
        """Set the cst property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""cst must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("cst", value)

    @property
    def scoor(self) -> float:
        """Get or set the Location of triad for tracking the rotation of the discrete beam element. The force and moment resultants in the output databases are referenced to this triad:
        EQ.-1.0: beam node 1, the angular velocity of node 1 rotates triad,
        EQ. 0.0: centered between beam nodes 1 and 2, the average angular velocity of nodes 1 and 2 is used to rotate the triad (default),
        EQ.+1.0: beam node 2, the angular velocity of node 2 rotates triad.
        """ # nopep8
        return self._cards[0].get_value("scoor")

    @scoor.setter
    def scoor(self, value: float) -> None:
        """Set the scoor property."""
        if value not in [0.0, 1.0, 2.0, 3.0, 12.0, 13.0, -13.0, -12.0, -3.0, -2.0, -1.0, None]:
            raise Exception("""scoor must be `None` or one of {0.0,1.0,2.0,3.0,12.0,13.0,-13.0,-12.0,-3.0,-2.0,-1.0}.""")
        self._cards[0].set_value("scoor", value)

    @property
    def nsm(self) -> float:
        """Get or set the Nonstructural mass per unit length.  This option applies to beam types 1-5 and does not apply to discrete, 2D, and spotweld beams, respectively.
        """ # nopep8
        return self._cards[0].get_value("nsm")

    @nsm.setter
    def nsm(self, value: float) -> None:
        """Set the nsm property."""
        self._cards[0].set_value("nsm", value)

    @property
    def naupd(self) -> int:
        """Get or set the Neutral axis update option.  See Remark 11.
        EQ. 0:	Not used
        EQ.1.0: 	Update the neutral axis when damage or failure occurs at  one or more integration points.
        """ # nopep8
        return self._cards[0].get_value("naupd")

    @naupd.setter
    def naupd(self, value: int) -> None:
        """Set the naupd property."""
        if value not in [0, 1, None]:
            raise Exception("""naupd must be `None` or one of {0,1}.""")
        self._cards[0].set_value("naupd", value)

    @property
    def ts1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n1. Note that the thickness defined on the *ELEMENT_BEAM_THICKNESS card overrides the definition give here.
        """ # nopep8
        return self._cards[1].get_value("ts1")

    @ts1.setter
    def ts1(self, value: float) -> None:
        """Set the ts1 property."""
        self._cards[1].set_value("ts1", value)

    @property
    def ts2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n2 .
        """ # nopep8
        return self._cards[1].get_value("ts2")

    @ts2.setter
    def ts2(self, value: float) -> None:
        """Set the ts2 property."""
        self._cards[1].set_value("ts2", value)

    @property
    def tt1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n1.
        """ # nopep8
        return self._cards[1].get_value("tt1")

    @tt1.setter
    def tt1(self, value: float) -> None:
        """Set the tt1 property."""
        self._cards[1].set_value("tt1", value)

    @property
    def tt2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n2 .
        """ # nopep8
        return self._cards[1].get_value("tt2")

    @tt2.setter
    def tt2(self, value: float) -> None:
        """Set the tt2 property."""
        self._cards[1].set_value("tt2", value)

    @property
    def nsloc(self) -> typing.Optional[float]:
        """Get or set the Location of reference surface normal to s axis for Hughes-Liu beam elements only:
        EQ.1.0: side at s=1,
        EQ.0.0: center (default),
        EQ.-1.0: side at s=-1.
        """ # nopep8
        return self._cards[1].get_value("nsloc")

    @nsloc.setter
    def nsloc(self, value: float) -> None:
        """Set the nsloc property."""
        self._cards[1].set_value("nsloc", value)

    @property
    def ntloc(self) -> typing.Optional[float]:
        """Get or set the Location of reference surface normal to t axis for Hughes-Liu beam elements only:
        EQ.1.0: side at t=,
        EQ.0.0: center (default),
        EQ.-1: side at t=-1.
        """ # nopep8
        return self._cards[1].get_value("ntloc")

    @ntloc.setter
    def ntloc(self, value: float) -> None:
        """Set the ntloc property."""
        self._cards[1].set_value("ntloc", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Cross-sectional area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[2].set_value("a", value)

    @property
    def iss(self) -> typing.Optional[float]:
        """Get or set the Iss . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
        """ # nopep8
        return self._cards[2].get_value("iss")

    @iss.setter
    def iss(self, value: float) -> None:
        """Set the iss property."""
        self._cards[2].set_value("iss", value)

    @property
    def itt(self) -> typing.Optional[float]:
        """Get or set the Itt . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
        """ # nopep8
        return self._cards[2].get_value("itt")

    @itt.setter
    def itt(self, value: float) -> None:
        """Set the itt property."""
        self._cards[2].set_value("itt", value)

    @property
    def j(self) -> typing.Optional[float]:
        """Get or set the J, torsional constant. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here. If J is zero, then J is reset to the sum of ISS+ITT as an approximation.
        """ # nopep8
        return self._cards[2].get_value("j")

    @j.setter
    def j(self, value: float) -> None:
        """Set the j property."""
        self._cards[2].set_value("j", value)

    @property
    def sa(self) -> typing.Optional[float]:
        """Get or set the Shear area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
        """ # nopep8
        return self._cards[2].get_value("sa")

    @sa.setter
    def sa(self, value: float) -> None:
        """Set the sa property."""
        self._cards[2].set_value("sa", value)

    @property
    def ist(self) -> typing.Optional[float]:
        """Get or set the Ist, product moment of inertia w.r.t. local s- and t-axis. This is only nonzero for unsymmetric cross sections and it can take positive and negative values, e.g. it is negative for SECTION_03.
        """ # nopep8
        return self._cards[2].get_value("ist")

    @ist.setter
    def ist(self, value: float) -> None:
        """Set the ist property."""
        self._cards[2].set_value("ist", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Cross-sectional area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
        """ # nopep8
        return self._cards[3].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[3].set_value("a", value)

    @property
    def rampt(self) -> typing.Optional[float]:
        """Get or set the Optional ramp-up time for dynamic relaxation.
        """ # nopep8
        return self._cards[3].get_value("rampt")

    @rampt.setter
    def rampt(self, value: float) -> None:
        """Set the rampt property."""
        self._cards[3].set_value("rampt", value)

    @property
    def stress(self) -> typing.Optional[float]:
        """Get or set the Optional initial stress for dynamic relaxation
        """ # nopep8
        return self._cards[3].get_value("stress")

    @stress.setter
    def stress(self, value: float) -> None:
        """Set the stress property."""
        self._cards[3].set_value("stress", value)

    @property
    def ts1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s direction at node n1 . Note that the thickness defined on the *ELEMENT_ BEAM_THICKNESS card overrides the definition give here.
        """ # nopep8
        return self._cards[4].get_value("ts1")

    @ts1.setter
    def ts1(self, value: float) -> None:
        """Set the ts1 property."""
        self._cards[4].set_value("ts1", value)

    @property
    def ts2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s direction at node n2 .
        """ # nopep8
        return self._cards[4].get_value("ts2")

    @ts2.setter
    def ts2(self, value: float) -> None:
        """Set the ts2 property."""
        self._cards[4].set_value("ts2", value)

    @property
    def tt1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t direction at node n1 .
        """ # nopep8
        return self._cards[4].get_value("tt1")

    @tt1.setter
    def tt1(self, value: float) -> None:
        """Set the tt1 property."""
        self._cards[4].set_value("tt1", value)

    @property
    def tt2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t direction at node n2 .
        """ # nopep8
        return self._cards[4].get_value("tt2")

    @tt2.setter
    def tt2(self, value: float) -> None:
        """Set the tt2 property."""
        self._cards[4].set_value("tt2", value)

    @property
    def vol(self) -> typing.Optional[float]:
        """Get or set the Volume of discrete beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
        """ # nopep8
        return self._cards[5].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        """Set the vol property."""
        self._cards[5].set_value("vol", value)

    @property
    def iner(self) -> typing.Optional[float]:
        """Get or set the I, lumped inertia of discrete beam which have six degrees of freedom. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
        """ # nopep8
        return self._cards[5].get_value("iner")

    @iner.setter
    def iner(self, value: float) -> None:
        """Set the iner property."""
        self._cards[5].set_value("iner", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93 and 95), see *DEFINE_COORDINATE. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This is not defined for cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
        """ # nopep8
        return self._cards[5].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[5].set_value("cid", value)

    @property
    def ca(self) -> typing.Optional[float]:
        """Get or set the Cable area, materials type ID 71, *MAT_CABLE.
        """ # nopep8
        return self._cards[5].get_value("ca")

    @ca.setter
    def ca(self, value: float) -> None:
        """Set the ca property."""
        self._cards[5].set_value("ca", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset for cable. For a definition see materials type ID 71, *MAT_CABLE.
        """ # nopep8
        return self._cards[5].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[5].set_value("offset", value)

    @property
    def rrcon(self) -> float:
        """Get or set the r-rotational constraint for local coordinate system:
        EQ.0.0: Coordinate ID rotates about r axis with nodes (default),
        EQ.1.0: Rotation is constrained about the r-axis
        """ # nopep8
        return self._cards[5].get_value("rrcon")

    @rrcon.setter
    def rrcon(self, value: float) -> None:
        """Set the rrcon property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""rrcon must be `None` or one of {0.0,1.0}.""")
        self._cards[5].set_value("rrcon", value)

    @property
    def srcon(self) -> float:
        """Get or set the s-rotational constraint for local coordinate system:
        EQ.0.0: Coordinate ID rotates about s axis with nodes (default),
        EQ.1.0: Rotation is constrained about the s-axis
        """ # nopep8
        return self._cards[5].get_value("srcon")

    @srcon.setter
    def srcon(self, value: float) -> None:
        """Set the srcon property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""srcon must be `None` or one of {0.0,1.0}.""")
        self._cards[5].set_value("srcon", value)

    @property
    def trcon(self) -> float:
        """Get or set the t-rotational constraint for local coordinate system:
        EQ.0.0: Coordinate ID rotates about t axis with nodes (default),
        EQ.1.0: Rotation is constrained about the t-axis
        """ # nopep8
        return self._cards[5].get_value("trcon")

    @trcon.setter
    def trcon(self, value: float) -> None:
        """Set the trcon property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""trcon must be `None` or one of {0.0,1.0}.""")
        self._cards[5].set_value("trcon", value)

    @property
    def ts1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s direction at node n1 . Note that the thickness defined on the *ELEMENT_ BEAM_THICKNESS card overrides the definition give here.
        """ # nopep8
        return self._cards[6].get_value("ts1")

    @ts1.setter
    def ts1(self, value: float) -> None:
        """Set the ts1 property."""
        self._cards[6].set_value("ts1", value)

    @property
    def ts2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s direction at node n2 .
        """ # nopep8
        return self._cards[6].get_value("ts2")

    @ts2.setter
    def ts2(self, value: float) -> None:
        """Set the ts2 property."""
        self._cards[6].set_value("ts2", value)

    @property
    def tt1(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t direction at node n1 .
        """ # nopep8
        return self._cards[6].get_value("tt1")

    @tt1.setter
    def tt1(self, value: float) -> None:
        """Set the tt1 property."""
        self._cards[6].set_value("tt1", value)

    @property
    def tt2(self) -> typing.Optional[float]:
        """Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t direction at node n2 .
        """ # nopep8
        return self._cards[6].get_value("tt2")

    @tt2.setter
    def tt2(self, value: float) -> None:
        """Set the tt2 property."""
        self._cards[6].set_value("tt2", value)

    @property
    def print(self) -> typing.Optional[float]:
        """Get or set the Output spot force resultant from spotwelds.
        EQ.0.0: Data is output to SWFORC file.
        EQ.1.0: Output is surpressed.
        """ # nopep8
        return self._cards[6].get_value("print")

    @print.setter
    def print(self, value: float) -> None:
        """Set the print property."""
        self._cards[6].set_value("print", value)

    @property
    def itoff(self) -> typing.Optional[float]:
        """Get or set the Option to specify torsional behavior for spot weld beams.
        EQ.0.0:	Torsional stiffness is active.
        EQ.1.0 : Torsional stiffness is zero(free to twist).
        """ # nopep8
        return self._cards[6].get_value("itoff")

    @itoff.setter
    def itoff(self, value: float) -> None:
        """Set the itoff property."""
        self._cards[6].set_value("itoff", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

