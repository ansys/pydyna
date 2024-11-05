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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdModelPorous(KeywordBase):
    """DYNA ICFD_MODEL_POROUS keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_POROUS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pmmoid",
                        int,
                        0,
                        10,
                        kwargs.get("pmmoid")
                    ),
                    Field(
                        "pmid",
                        int,
                        10,
                        10,
                        kwargs.get("pmid", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "por",
                        float,
                        0,
                        10,
                        kwargs.get("por", 0.0)
                    ),
                    Field(
                        "per/thx",
                        float,
                        10,
                        10,
                        kwargs.get("per/thx", 0.0)
                    ),
                    Field(
                        "ff/thy",
                        float,
                        20,
                        10,
                        kwargs.get("ff/thy", 0.0)
                    ),
                    Field(
                        "thz",
                        float,
                        30,
                        10,
                        kwargs.get("thz", 0.0)
                    ),
                    Field(
                        "pvlcidx",
                        int,
                        40,
                        10,
                        kwargs.get("pvlcidx")
                    ),
                    Field(
                        "pvlcidy",
                        int,
                        50,
                        10,
                        kwargs.get("pvlcidy")
                    ),
                    Field(
                        "pvlcidz",
                        int,
                        60,
                        10,
                        kwargs.get("pvlcidz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "kxp'",
                        float,
                        0,
                        10,
                        kwargs.get("kxp'", 0.0)
                    ),
                    Field(
                        "kyp'",
                        float,
                        10,
                        10,
                        kwargs.get("kyp'", 0.0)
                    ),
                    Field(
                        "kzp'",
                        float,
                        20,
                        10,
                        kwargs.get("kzp'", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p-x/pid1r",
                        float,
                        0,
                        10,
                        kwargs.get("p-x/pid1r", 0.0)
                    ),
                    Field(
                        "p-y/pid2r",
                        float,
                        10,
                        10,
                        kwargs.get("p-y/pid2r", 0.0)
                    ),
                    Field(
                        "projxp-z",
                        float,
                        20,
                        10,
                        kwargs.get("projxp-z", 0.0)
                    ),
                    Field(
                        "projyp-x",
                        float,
                        30,
                        10,
                        kwargs.get("projyp-x", 0.0)
                    ),
                    Field(
                        "projyp-y",
                        float,
                        40,
                        10,
                        kwargs.get("projyp-y", 0.0)
                    ),
                    Field(
                        "projyp-z",
                        float,
                        50,
                        10,
                        kwargs.get("projyp-z", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pmmoid(self) -> typing.Optional[int]:
        """Get or set the Porous media model ID.
        """ # nopep8
        return self._cards[0].get_value("pmmoid")

    @pmmoid.setter
    def pmmoid(self, value: int) -> None:
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
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 10, 11]:
            raise Exception("""pmid must be one of {1,2,3,4,5,6,7,8,10,11}""")
        self._cards[0].set_value("pmid", value)

    @property
    def por(self) -> float:
        """Get or set the Porosity e.
        """ # nopep8
        return self._cards[1].get_value("por")

    @por.setter
    def por(self, value: float) -> None:
        self._cards[1].set_value("por", value)

    @property
    def per_thx(self) -> float:
        """Get or set the Permeability k if PMID = 1 or 2. Probe Thickness delta x if PMID = 3 or PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("per/thx")

    @per_thx.setter
    def per_thx(self, value: float) -> None:
        self._cards[1].set_value("per/thx", value)

    @property
    def ff_thy(self) -> float:
        """Get or set the Forchheimer factor. To Be defined if PMID = 2. Probe Thickness delta y if PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("ff/thy")

    @ff_thy.setter
    def ff_thy(self, value: float) -> None:
        self._cards[1].set_value("ff/thy", value)

    @property
    def thz(self) -> float:
        """Get or set the Probe Thickness delta z if PMID = 5.
        """ # nopep8
        return self._cards[1].get_value("thz")

    @thz.setter
    def thz(self, value: float) -> None:
        self._cards[1].set_value("thz", value)

    @property
    def pvlcidx(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 3 and PMID = 5. If PMID = 5, this refers to P-V curve in global X direction..
        """ # nopep8
        return self._cards[1].get_value("pvlcidx")

    @pvlcidx.setter
    def pvlcidx(self, value: int) -> None:
        self._cards[1].set_value("pvlcidx", value)

    @property
    def pvlcidy(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Y direction.
        """ # nopep8
        return self._cards[1].get_value("pvlcidy")

    @pvlcidy.setter
    def pvlcidy(self, value: int) -> None:
        self._cards[1].set_value("pvlcidy", value)

    @property
    def pvlcidz(self) -> typing.Optional[int]:
        """Get or set the Pressure function of Velocity Load Curve ID. To be defined if PMID = 5. This refers to P-V curve in global Z direction.
        """ # nopep8
        return self._cards[1].get_value("pvlcidz")

    @pvlcidz.setter
    def pvlcidz(self, value: int) -> None:
        self._cards[1].set_value("pvlcidz", value)

    @property
    def kxp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kxp'")

    @kxp_.setter
    def kxp_(self, value: float) -> None:
        self._cards[2].set_value("kxp'", value)

    @property
    def kyp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'). To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kyp'")

    @kyp_.setter
    def kyp_(self, value: float) -> None:
        self._cards[2].set_value("kyp'", value)

    @property
    def kzp_(self) -> float:
        """Get or set the Permeability vector in local reference frame (x', y', z'. To be defined in PMID = 4, 5, 6 or 7. Those values become scale factors if	PMID = 5.
        """ # nopep8
        return self._cards[2].get_value("kzp'")

    @kzp_.setter
    def kzp_(self, value: float) -> None:
        self._cards[2].set_value("kzp'", value)

    @property
    def p_x_pid1r(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("p-x/pid1r")

    @p_x_pid1r.setter
    def p_x_pid1r(self, value: float) -> None:
        self._cards[3].set_value("p-x/pid1r", value)

    @property
    def p_y_pid2r(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("p-y/pid2r")

    @p_y_pid2r.setter
    def p_y_pid2r(self, value: float) -> None:
        self._cards[3].set_value("p-y/pid2r", value)

    @property
    def projxp_z(self) -> float:
        """Get or set the Projection of local permeability vector x' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projxp-z")

    @projxp_z.setter
    def projxp_z(self, value: float) -> None:
        self._cards[3].set_value("projxp-z", value)

    @property
    def projyp_x(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-x")

    @projyp_x.setter
    def projyp_x(self, value: float) -> None:
        self._cards[3].set_value("projyp-x", value)

    @property
    def projyp_y(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-y")

    @projyp_y.setter
    def projyp_y(self, value: float) -> None:
        self._cards[3].set_value("projyp-y", value)

    @property
    def projyp_z(self) -> float:
        """Get or set the Projection of local permeability vector y' in global reference frame(x, y, z).
        """ # nopep8
        return self._cards[3].get_value("projyp-z")

    @projyp_z.setter
    def projyp_z(self, value: float) -> None:
        self._cards[3].set_value("projyp-z", value)

