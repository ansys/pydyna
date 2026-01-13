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

"""Module providing the ConstrainedInterpolationSpotweld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD0 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("nsid", int, 20, 10, None),
    FieldSchema("thick", float, 30, 10, None),
    FieldSchema("r", float, 40, 10, None),
    FieldSchema("stiff", float, 50, 10, None),
    FieldSchema("alpha1", float, 60, 10, None),
    FieldSchema("model", int, 70, 10, 1),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD1 = (
    FieldSchema("rn", float, 0, 10, None),
    FieldSchema("rs", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("lcf", int, 30, 10, None),
    FieldSchema("lcupf", int, 40, 10, None),
    FieldSchema("lcupr", int, 50, 10, None),
    FieldSchema("dens", float, 60, 10, None),
    FieldSchema("intp", int, 70, 10, 0),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD2 = (
    FieldSchema("upfn", float, 0, 10, None),
    FieldSchema("upfs", float, 10, 10, None),
    FieldSchema("alpha2", float, 20, 10, None),
    FieldSchema("beta2", float, 30, 10, None),
    FieldSchema("uprn", float, 40, 10, None),
    FieldSchema("uprs", float, 50, 10, None),
    FieldSchema("alpha3", float, 60, 10, None),
    FieldSchema("beta3", float, 70, 10, None),
)

_CONSTRAINEDINTERPOLATIONSPOTWELD_CARD3 = (
    FieldSchema("mrn", float, 0, 10, None),
    FieldSchema("mrs", float, 10, 10, None),
)

class ConstrainedInterpolationSpotweld(KeywordBase):
    """DYNA CONSTRAINED_INTERPOLATION_SPOTWELD keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "INTERPOLATION_SPOTWELD"
    _link_fields = {
        "lcf": LinkType.DEFINE_CURVE,
        "lcupf": LinkType.DEFINE_CURVE,
        "lcupr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedInterpolationSpotweld class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATIONSPOTWELD_CARD3,
                **kwargs,
            ),        ]
    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of first sheet
        GT.0:	Part ID
        LT.0 : | PID1 | is part set ID(for in - plane composed sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of second sheet. PID2 can be identical to PID1 if the spot weld location nodes from NSID lie in between the shell elements that should be self-connected.
        GT.0:	Part ID
        LT.0 : |PID2 | is part set ID(for in - plane composed sheets sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of spot weld location nodes.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Total thickness of both sheets.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[0].set_value("thick", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Spotweld radius.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def stiff(self) -> typing.Optional[float]:
        """Get or set the Elastic stiffness. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[0].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        """Set the stiff property."""
        self._cards[0].set_value("stiff", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Scaling factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[0].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[0].set_value("alpha1", value)

    @property
    def model(self) -> int:
        """Get or set the Material behavior and damage model, see remarks.
        EQ. 1:	SPR3 (default),
        EQ. 2:	SPR4,
        EQ.11:	same as 1 with selected material parameters as functions,
        EQ.12:	same as 2 with selected material parameters as functions,
        EQ.21:	same as 11 with slight modification, see remarks,
        EQ.22:	same as 12 with slight modification, see remarks.
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        """Set the model property."""
        if value not in [1, 2, 11, 12, 21, 22, None]:
            raise Exception("""model must be `None` or one of {1,2,11,12,21,22}.""")
        self._cards[0].set_value("model", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Tensile strength factor.
        GT.0.0:	Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remark 2).
        LT.0.0:	Load curve with ID | RN | giving R_n as a function of peel ratio(see Remark 5)
        """ # nopep8
        return self._cards[1].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        """Set the rn property."""
        self._cards[1].set_value("rn", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the Shear strength factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        """Set the rs property."""
        self._cards[1].set_value("rs", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic potential β_1. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def lcf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing force versus plastic displacement.
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        """Set the lcf property."""
        self._cards[1].set_value("lcf", value)

    @property
    def lcupf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[1].get_value("lcupf")

    @lcupf.setter
    def lcupf(self, value: int) -> None:
        """Set the lcupf property."""
        self._cards[1].set_value("lcupf", value)

    @property
    def lcupr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[1].get_value("lcupr")

    @lcupr.setter
    def lcupr(self, value: int) -> None:
        """Set the lcupr property."""
        self._cards[1].set_value("lcupr", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Spotweld density (necessary for time step calculation).
        """ # nopep8
        return self._cards[1].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        """Set the dens property."""
        self._cards[1].set_value("dens", value)

    @property
    def intp(self) -> int:
        """Get or set the Flag for interpolation.
        EQ.0:	linear (default),
        EQ.1:	uniform,
        EQ.2:	inverse distance weighting.
        """ # nopep8
        return self._cards[1].get_value("intp")

    @intp.setter
    def intp(self, value: int) -> None:
        """Set the intp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""intp must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("intp", value)

    @property
    def upfn(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("upfn")

    @upfn.setter
    def upfn(self, value: float) -> None:
        """Set the upfn property."""
        self._cards[2].set_value("upfn", value)

    @property
    def upfs(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("upfs")

    @upfs.setter
    def upfs(self, value: float) -> None:
        """Set the upfs property."""
        self._cards[2].set_value("upfs", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[2].set_value("alpha2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic initiation displacement.
        """ # nopep8
        return self._cards[2].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[2].set_value("beta2", value)

    @property
    def uprn(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("uprn")

    @uprn.setter
    def uprn(self, value: float) -> None:
        """Set the uprn property."""
        self._cards[2].set_value("uprn", value)

    @property
    def uprs(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("uprs")

    @uprs.setter
    def uprs(self, value: float) -> None:
        """Set the uprs property."""
        self._cards[2].set_value("uprs", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[2].set_value("alpha3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic rupture displacement.
        """ # nopep8
        return self._cards[2].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        """Set the beta3 property."""
        self._cards[2].set_value("beta3", value)

    @property
    def mrn(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RN.
        """ # nopep8
        return self._cards[3].get_value("mrn")

    @mrn.setter
    def mrn(self, value: float) -> None:
        """Set the mrn property."""
        self._cards[3].set_value("mrn", value)

    @property
    def mrs(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RS.
        """ # nopep8
        return self._cards[3].get_value("mrs")

    @mrs.setter
    def mrs(self, value: float) -> None:
        """Set the mrs property."""
        self._cards[3].set_value("mrs", value)

    @property
    def lcf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcf:
                return kwd
        return None

    @lcf_link.setter
    def lcf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcf."""
        self.lcf = value.lcid

    @property
    def lcupf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcupf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcupf:
                return kwd
        return None

    @lcupf_link.setter
    def lcupf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcupf."""
        self.lcupf = value.lcid

    @property
    def lcupr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcupr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcupr:
                return kwd
        return None

    @lcupr_link.setter
    def lcupr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcupr."""
        self.lcupr = value.lcid

