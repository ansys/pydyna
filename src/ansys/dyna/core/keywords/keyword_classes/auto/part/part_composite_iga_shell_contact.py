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

"""Module providing the PartCompositeIgaShellContact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.hourglass.hourglass import Hourglass

_PARTCOMPOSITEIGASHELLCONTACT_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

_PARTCOMPOSITEIGASHELLCONTACT_CARD1 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 0),
    FieldSchema("shrf", float, 20, 10, None),
    FieldSchema("nloc", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("irl", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_PARTCOMPOSITEIGASHELLCONTACT_CARD2 = (
    FieldSchema("fs", float, 0, 10, None),
    FieldSchema("fd", float, 10, 10, None),
    FieldSchema("dc", float, 20, 10, None),
    FieldSchema("vc", float, 30, 10, None),
    FieldSchema("optt", float, 40, 10, None),
    FieldSchema("sft", float, 50, 10, None),
    FieldSchema("ssf", float, 60, 10, None),
    FieldSchema("cparm8", float, 70, 10, None),
)

_PARTCOMPOSITEIGASHELLCONTACT_CARD3 = (
    FieldSchema("mid1", int, 0, 10, None),
    FieldSchema("thick1", float, 10, 10, None),
    FieldSchema("b1", float, 20, 10, None),
    FieldSchema("tmid1", int, 30, 10, None),
    FieldSchema("mid2", int, 40, 10, None),
    FieldSchema("thick2", float, 50, 10, None),
    FieldSchema("b2", float, 60, 10, None),
    FieldSchema("tmid2", int, 70, 10, None),
)

class PartCompositeIgaShellContact(KeywordBase):
    """DYNA PART_COMPOSITE_IGA_SHELL_CONTACT keyword"""

    keyword = "PART"
    subkeyword = "COMPOSITE_IGA_SHELL_CONTACT"
    _link_fields = {
        "mid1": LinkType.MAT,
        "mid2": LinkType.MAT,
        "irl": LinkType.HOURGLASS,
    }

    def __init__(self, **kwargs):
        """Initialize the PartCompositeIgaShellContact class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARTCOMPOSITEIGASHELLCONTACT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _PARTCOMPOSITEIGASHELLCONTACT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _PARTCOMPOSITEIGASHELLCONTACT_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _PARTCOMPOSITEIGASHELLCONTACT_CARD3,
                **kwargs,
            ),
        ]
    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation options for IGA shells:
        EQ.0: Reissner - Mindlin with fibers at the control points
        EQ.1: Kirchhoff - Love with fibers at the control points
        EQ.2: Kirchhoff - Love with fibers at the integration point
        EQ.3: Reissner - Mindlin with fibers at the integration poin
        EQ.5: Shell with thickness stretch based on the ELFORM = 0.
        EQ.6: Shell with thickness stretch based on ELFORM = 3.
        """ # nopep8
        return self._cards[1].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        if value not in [0, 1, 2, 3, 5, 6, None]:
            raise Exception("""elform must be `None` or one of {0,1,2,3,5,6}.""")
        self._cards[1].set_value("elform", value)

    @property
    def shrf(self) -> typing.Optional[float]:
        """Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
        """ # nopep8
        return self._cards[1].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[1].set_value("shrf", value)

    @property
    def nloc(self) -> float:
        """Get or set the Location of reference surface, available for thin shells only.  If nonzero, the offset distance from the plane of the nodal points to the reference surface of the shell in the direction of the shell normal vector is a value:
        offset = -0.50xNLOCx(average  shell  thickness).
        This offset is not considered in the contact subroutines unless CNTCO is set to 1 or 3 in *CONTROL_SHELL. Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
        EQ.1.0: Top surface,
        EQ.0.0: Mid - surface(default),
        EQ. - 1.0: Bottom surface.
        """ # nopep8
        return self._cards[1].get_value("nloc")

    @nloc.setter
    def nloc(self, value: float) -> None:
        """Set the nloc property."""
        self._cards[1].set_value("nloc", value)

    @property
    def irl(self) -> int:
        """Get or set the Lamina integration rule:
        EQ.0: Reduced Gauss - Legendre
        EQ.1: Gauss - Legendre
        EQ.2: Patchwise reduced Gauss - Legendre(for biquadraticNURBS only)
        """ # nopep8
        return self._cards[1].get_value("irl")

    @irl.setter
    def irl(self, value: int) -> None:
        """Set the irl property."""
        self._cards[1].set_value("irl", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[2].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
        """ # nopep8
        return self._cards[2].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[2].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay coefficient.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact .
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[2].set_value("dc", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Coefficient for viscous friction.  This is necessary to limit the friction force to a maximum.  A limiting force is computed  .  Acont being the area of the segment contacted by the node in contact.  The suggested value for VC is to use the yield stress in shear   where  o is the yield stress of the contacted material.
        """ # nopep8
        return self._cards[2].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        """Set the vc property."""
        self._cards[2].set_value("vc", value)

    @property
    def optt(self) -> typing.Optional[float]:
        """Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells, and beams. For SOFT = 0 and 1 and Mortar contacts, it only applies to shells and beams.
        However, for the MPP version only, OPTT does affect the contact behavior of solid elements for SOFT = 0 and 1 but not by changing the contact thickness.
        In the case of MPP with SOFT = 0 and 1 for solids, OPTT overrides the thickness of the solid elements used for the calculation of the
        contact penetration release (see Table Error! Reference source not found.) but does not affect the contact thickness. This is not available in SMP.
        OPTT does not affect the contact thickness when applied to the parts on the tracked side (SURFA) of an AUTOMATIC_NODES_TO_SURFACE contact.
        However, it affects the contact thickness of this type of contact�s reference side (SURFB).
        """ # nopep8
        return self._cards[2].get_value("optt")

    @optt.setter
    def optt(self, value: float) -> None:
        """Set the optt property."""
        self._cards[2].set_value("optt", value)

    @property
    def sft(self) -> typing.Optional[float]:
        """Get or set the Optional thickness scale factor in automatic contact (scales the true thickness). This option applies only
        to contact with shell elements.The true thickness is the element thickness of the shell elements.
        """ # nopep8
        return self._cards[2].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[2].set_value("sft", value)

    @property
    def ssf(self) -> typing.Optional[float]:
        """Get or set the Scale factor on default tracked surface penalty stiffness for this  part ID whenever it appears in the contact definition.If zero, SSF is taken as unity.
        """ # nopep8
        return self._cards[2].get_value("ssf")

    @ssf.setter
    def ssf(self, value: float) -> None:
        """Set the ssf property."""
        self._cards[2].set_value("ssf", value)

    @property
    def cparm8(self) -> typing.Optional[float]:
        """Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_AUTOMATIC_GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT__MPP Optional Card.
        EQ.0: Flag is not set(default).
        EQ.1: Flag is set.
        EQ.2: Flag is set.CPARM8 = 2 has the additional effect of permitting contact treatment of spot weld(type 9) beams in AUTOMATIC_GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_GENERAL contacts.
        """ # nopep8
        return self._cards[2].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: float) -> None:
        """Set the cparm8 property."""
        self._cards[2].set_value("cparm8", value)

    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_   Section
        """ # nopep8
        return self._cards[3].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        """Set the mid1 property."""
        self._cards[3].set_value("mid1", value)

    @property
    def thick1(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point .
        """ # nopep8
        return self._cards[3].get_value("thick1")

    @thick1.setter
    def thick1(self, value: float) -> None:
        """Set the thick1 property."""
        self._cards[3].set_value("thick1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[3].set_value("b1", value)

    @property
    def tmid1(self) -> typing.Optional[int]:
        """Get or set the Thermal ID
        """ # nopep8
        return self._cards[3].get_value("tmid1")

    @tmid1.setter
    def tmid1(self, value: int) -> None:
        """Set the tmid1 property."""
        self._cards[3].set_value("tmid1", value)

    @property
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Material ID of integration point i, see *MAT_   Section
        """ # nopep8
        return self._cards[3].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        """Set the mid2 property."""
        self._cards[3].set_value("mid2", value)

    @property
    def thick2(self) -> typing.Optional[float]:
        """Get or set the Thickness of integration point
        """ # nopep8
        return self._cards[3].get_value("thick2")

    @thick2.setter
    def thick2(self, value: float) -> None:
        """Set the thick2 property."""
        self._cards[3].set_value("thick2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material angle of integration point i
        """ # nopep8
        return self._cards[3].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[3].set_value("b2", value)

    @property
    def tmid2(self) -> typing.Optional[int]:
        """Get or set the Thermal ID
        """ # nopep8
        return self._cards[3].get_value("tmid2")

    @tmid2.setter
    def tmid2(self, value: int) -> None:
        """Set the tmid2 property."""
        self._cards[3].set_value("tmid2", value)

    @property
    def mid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid1:
                return kwd
        return None

    @mid1_link.setter
    def mid1_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid1."""
        self.mid1 = value.mid

    @property
    def mid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid2:
                return kwd
        return None

    @mid2_link.setter
    def mid2_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid2."""
        self.mid2 = value.mid

    @property
    def irl_link(self) -> typing.Optional[Hourglass]:
        """Get the Hourglass object for irl."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("HOURGLASS", "HOURGLASS"):
            if kwd.hgid == self.irl:
                return kwd
        return None

    @irl_link.setter
    def irl_link(self, value: Hourglass) -> None:
        """Set the Hourglass object for irl."""
        self.irl = value.hgid

