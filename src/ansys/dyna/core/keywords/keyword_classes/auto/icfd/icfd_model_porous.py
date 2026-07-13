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

"""Module providing the IcfdModelPorous class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDMODELPOROUS_CARD0 = (
    FieldSchema("pmmoid", int, 0, 10, None),
    FieldSchema("pmmtype", int, 10, 10, 1),
    FieldSchema("form", int, 20, 10, 0),
    FieldSchema("rhocp", float, 30, 10, 0.0),
    FieldSchema("kappa", float, 40, 10, 0.0),
)

_ICFDMODELPOROUS_CARD1 = (
    FieldSchema("por", float, 0, 10, 0.0),
    FieldSchema("per", float, 10, 10, 0.0),
    FieldSchema("ff", float, 20, 10, 0.0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("psflcid", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
)

class IcfdModelPorous(KeywordBase):
    """DYNA ICFD_MODEL_POROUS keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_POROUS"
    _link_fields = {
        "psflcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdModelPorous class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pmmoid(self) -> typing.Optional[int]:
        """Get or set the Porous media model ID.
        """ # nopep8
        return self._cards[0].get_value("pmmoid")

    @pmmoid.setter
    def pmmoid(self, value: int) -> None:
        """Set the pmmoid property."""
        self._cards[0].set_value("pmmoid", value)

    @property
    def pmmtype(self) -> int:
        """Get or set the Porous media model type:
        EQ.1: Isotropic porous media - Ergun Correlation.
        EQ.2: Isotropic porous media - Darcy-Forchheimer model.
        EQ.3: Isotropic porous media - Permeability defined through Pressure-Velocity Data.
        EQ.4: Anisotropic porous media - Fixed local reference frame (See Figure 5-3).
        EQ.5: Anisotropic porous media model - Moving local reference
        frame and permeability vector in local reference frame (, , ) defined by three Pressure-Velocity curves.
        EQ.6: Anisotropic porous media model - Moving local reference frame and permeability vector constant.
        EQ.7: Anisotropic porous media model - Moving local reference
        frame and permeability vector constant. This model differs
        from PMID = 6 in the way the local reference frame is moved.
        EQ.8: Main parachute model to be used jointly with *MESH_EMBEDSHELL for the parachute surface. Similar to PMID=2.
        EQ.10: Parachute model to be used jointly with *MESH_EMBEDSHELL where the fabric permeability and Forchheimer factor are computed from the Pressure - Velocity curves of experimental data given by a LOAD_CURVE.Similar to PMID = 3.
        EQ.11: Parachute model similar to PMID = 8 but pressure gradient is directly defined by coefficients  and  as
        """ # nopep8
        return self._cards[0].get_value("pmmtype")

    @pmmtype.setter
    def pmmtype(self, value: int) -> None:
        """Set the pmmtype property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, None]:
            raise Exception("""pmmtype must be `None` or one of {1,2,3,4,5,6,7,8,10,11}.""")
        self._cards[0].set_value("pmmtype", value)

    @property
    def form(self) -> int:
        """Get or set the Porous media formulation:
        EQ.0: Classical(default)
        EQ.2: Interstitial velocity
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        if value not in [0, 2, None]:
            raise Exception("""form must be `None` or one of {0,2}.""")
        self._cards[0].set_value("form", value)

    @property
    def rhocp(self) -> float:
        """Get or set the Density of the structure multiplied by the specific heat of the structure
        """ # nopep8
        return self._cards[0].get_value("rhocp")

    @rhocp.setter
    def rhocp(self, value: float) -> None:
        """Set the rhocp property."""
        self._cards[0].set_value("rhocp", value)

    @property
    def kappa(self) -> float:
        """Get or set the Thermal conductivity of the structure
        """ # nopep8
        return self._cards[0].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        """Set the kappa property."""
        self._cards[0].set_value("kappa", value)

    @property
    def por(self) -> float:
        """Get or set the Porosity e.
        """ # nopep8
        return self._cards[1].get_value("por")

    @por.setter
    def por(self, value: float) -> None:
        """Set the por property."""
        self._cards[1].set_value("por", value)

    @property
    def per(self) -> float:
        """Get or set the PER:Permeability x
        """ # nopep8
        return self._cards[1].get_value("per")

    @per.setter
    def per(self, value: float) -> None:
        """Set the per property."""
        self._cards[1].set_value("per", value)

    @property
    def ff(self) -> float:
        """Get or set the FF:Forchheimer factor to be defined if if PMMTYPE = 2 or 8
        """ # nopep8
        return self._cards[1].get_value("ff")

    @ff.setter
    def ff(self, value: float) -> None:
        """Set the ff property."""
        self._cards[1].set_value("ff", value)

    @property
    def psflcid(self) -> typing.Optional[int]:
        """Get or set the PSFLCID:Optional permeability scale factor load curve ID, *DEFINE_CURVE_FUNCTION ID or *DEFINE_FUNCTION ID. If a *DEFINE_FUNCTION is used, the following parameters are allowed: f( x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[1].get_value("psflcid")

    @psflcid.setter
    def psflcid(self, value: int) -> None:
        """Set the psflcid property."""
        self._cards[1].set_value("psflcid", value)

    @property
    def psflcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for psflcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.psflcid:
                return kwd
        return None

    @psflcid_link.setter
    def psflcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for psflcid."""
        self.psflcid = value.lcid

