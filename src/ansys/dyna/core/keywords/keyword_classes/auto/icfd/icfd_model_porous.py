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

_ICFDMODELPOROUS_CARD0 = (
    FieldSchema("pmmoid", int, 0, 10, None),
    FieldSchema("pmid", int, 10, 10, 1),
)

_ICFDMODELPOROUS_CARD1 = (
    FieldSchema("por", float, 0, 10, 0.0),
    FieldSchema("per/thx", float, 10, 10, 0.0),
    FieldSchema("ff/thy", float, 20, 10, 0.0),
    FieldSchema("thz", float, 30, 10, 0.0),
    FieldSchema("pvlcidx", int, 40, 10, None),
    FieldSchema("pvlcidy", int, 50, 10, None),
    FieldSchema("pvlcidz", int, 60, 10, None),
)

_ICFDMODELPOROUS_CARD2 = (
    FieldSchema("kxp'", float, 0, 10, 0.0),
    FieldSchema("kyp'", float, 10, 10, 0.0),
    FieldSchema("kzp'", float, 20, 10, 0.0),
)

_ICFDMODELPOROUS_CARD3 = (
    FieldSchema("p-x/pid1r", float, 0, 10, 0.0),
    FieldSchema("p-y/pid2r", float, 10, 10, 0.0),
    FieldSchema("projxp-z", float, 20, 10, 0.0),
    FieldSchema("projyp-x", float, 30, 10, 0.0),
    FieldSchema("projyp-y", float, 40, 10, 0.0),
    FieldSchema("projyp-z", float, 50, 10, 0.0),
)

class IcfdModelPorous(KeywordBase):
    """DYNA ICFD_MODEL_POROUS keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_POROUS"

    def __init__(self, **kwargs):
        """Initialize the IcfdModelPorous class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDMODELPOROUS_CARD3,
                **kwargs,
            ),        ]
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
    def pmid(self) -> int:
        """Get or set the Porous media model type :
        EQ.1 : Isotropic porous media - Ergun Correlation.
        EQ.2 : Isotropic porous media - Darcy-Forchheimer model.
        EQ.3 : Isotropic porous media - Permeability defined through Pressure-Velocity Data.
        EQ.4 : Anisotropic porous media - Fixed local reference frame (See Figure 5-3).
        EQ.5 : Anisotropic porous media model - Moving local reference
        frame and permeability vector in local reference frame	(𝑥’, 𝑦’, 𝑧’) defined by three Pressure-Velocity curves.
        EQ.6 : Anisotropic porous media model - Moving local reference frame and permeability vector constant.
        EQ.7: Anisotropic porous media model - Moving local reference
        frame and permeability vector constant. This model differs
        from PMID = 6 in the way the local reference frame is moved.
        EQ.8:	Main parachute model to be used jointly with *MESH_EMBEDSHELL for the parachute surface. Similar to PMID=2.
        EQ.10:	Parachute model to be used jointly with * MESH_EMBEDSHELL where the fabric permeability and Forchheimer factor are computed from the Pressure - Velocity curves of experimental data given by a LOAD_CURVE.Similar to PMID = 3.
        EQ.11 : Parachute model similar to PMID = 8 but pressure gradient is directly defined by coefficients α and β as
        """ # nopep8
        return self._cards[0].get_value("pmid")

    @pmid.setter
    def pmid(self, value: int) -> None:
        """Set the pmid property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, None]:
            raise Exception("""pmid must be `None` or one of {1,2,3,4,5,6,7,8,10,11}.""")
        self._cards[0].set_value("pmid", value)

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
    def per_thx(self) -> float:
        """Get or set the Permeability k if PMID = 1 or 2. Probe Thickness delta x if PMID = 3 or PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("per/thx")

    @per_thx.setter
    def per_thx(self, value: float) -> None:
        """Set the per_thx property."""
        self._cards[1].set_value("per/thx", value)

    @property
    def ff_thy(self) -> float:
        """Get or set the Forchheimer factor. To Be defined if PMID = 2. Probe Thickness delta y if PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("ff/thy")

    @ff_thy.setter
    def ff_thy(self, value: float) -> None:
        """Set the ff_thy property."""
        self._cards[1].set_value("ff/thy", value)

    @property
    def thz(self) -> float:
        """Get or set the Probe Thickness delta z if PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("thz")

    @thz.setter
    def thz(self, value: float) -> None:
        """Set the thz property."""
        self._cards[1].set_value("thz", value)

    @property
    def pvlcidx(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 3 and PMID = 5. If PMID = 5, this refers to P-V curve in global X direction..
        """ # nopep8
        return self._cards[1].get_value("pvlcidx")

    @pvlcidx.setter
    def pvlcidx(self, value: int) -> None:
        """Set the pvlcidx property."""
        self._cards[1].set_value("pvlcidx", value)

    @property
    def pvlcidy(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Y direction.
        """ # nopep8
        return self._cards[1].get_value("pvlcidy")

    @pvlcidy.setter
    def pvlcidy(self, value: int) -> None:
        """Set the pvlcidy property."""
        self._cards[1].set_value("pvlcidy", value)

    @property
    def pvlcidz(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Z direction.
        """ # nopep8
        return self._cards[1].get_value("pvlcidz")

    @pvlcidz.setter
    def pvlcidz(self, value: int) -> None:
        """Set the pvlcidz property."""
        self._cards[1].set_value("pvlcidz", value)

    @property
    def kxp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kxp'")

    @kxp_.setter
    def kxp_(self, value: float) -> None:
        """Set the kxp_ property."""
        self._cards[2].set_value("kxp'", value)

    @property
    def kyp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kyp'")

    @kyp_.setter
    def kyp_(self, value: float) -> None:
        """Set the kyp_ property."""
        self._cards[2].set_value("kyp'", value)

    @property
    def kzp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'. To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kzp'")

    @kzp_.setter
    def kzp_(self, value: float) -> None:
        """Set the kzp_ property."""
        self._cards[2].set_value("kzp'", value)

    @property
    def p_x_pid1r(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("p-x/pid1r")

    @p_x_pid1r.setter
    def p_x_pid1r(self, value: float) -> None:
        """Set the p_x_pid1r property."""
        self._cards[3].set_value("p-x/pid1r", value)

    @property
    def p_y_pid2r(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("p-y/pid2r")

    @p_y_pid2r.setter
    def p_y_pid2r(self, value: float) -> None:
        """Set the p_y_pid2r property."""
        self._cards[3].set_value("p-y/pid2r", value)

    @property
    def projxp_z(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projxp-z")

    @projxp_z.setter
    def projxp_z(self, value: float) -> None:
        """Set the projxp_z property."""
        self._cards[3].set_value("projxp-z", value)

    @property
    def projyp_x(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-x")

    @projyp_x.setter
    def projyp_x(self, value: float) -> None:
        """Set the projyp_x property."""
        self._cards[3].set_value("projyp-x", value)

    @property
    def projyp_y(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-y")

    @projyp_y.setter
    def projyp_y(self, value: float) -> None:
        """Set the projyp_y property."""
        self._cards[3].set_value("projyp-y", value)

    @property
    def projyp_z(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-z")

    @projyp_z.setter
    def projyp_z(self, value: float) -> None:
        """Set the projyp_z property."""
        self._cards[3].set_value("projyp-z", value)

