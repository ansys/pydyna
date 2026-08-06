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

"""Module providing the IcfdModelSpeciesTransport class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDMODELSPECIESTRANSPORT_CARD0 = (
    FieldSchema("sptrid", int, 0, 10, None),
    FieldSchema("spnmbr", int, 10, 10, None),
    FieldSchema("densrct", int, 20, 10, 0),
    FieldSchema("densfid", int, 30, 10, None),
    FieldSchema("hvac", int, 40, 10, None),
)

_ICFDMODELSPECIESTRANSPORT_CARD1 = (
    FieldSchema("massdif1", float, 0, 10, None),
    FieldSchema("massdif2", float, 10, 10, None),
    FieldSchema("massdif3", float, 20, 10, None),
    FieldSchema("massdif4", float, 30, 10, None),
    FieldSchema("massdif5", float, 40, 10, None),
    FieldSchema("massdif6", float, 50, 10, None),
    FieldSchema("massdif7", float, 60, 10, None),
    FieldSchema("massdif8", float, 70, 10, None),
)

class IcfdModelSpeciesTransport(KeywordBase):
    """DYNA ICFD_MODEL_SPECIES_TRANSPORT keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_SPECIES_TRANSPORT"

    def __init__(self, **kwargs):
        """Initialize the IcfdModelSpeciesTransport class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDMODELSPECIESTRANSPORT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDMODELSPECIESTRANSPORT_CARD1,
                **kwargs,
            ),
        ]
    @property
    def sptrid(self) -> typing.Optional[int]:
        """Get or set the Species transport model ID
        """ # nopep8
        return self._cards[0].get_value("sptrid")

    @sptrid.setter
    def sptrid(self, value: int) -> None:
        """Set the sptrid property."""
        self._cards[0].set_value("sptrid", value)

    @property
    def spnmbr(self) -> typing.Optional[int]:
        """Get or set the Number of species:
        EQ.1:	One passive species
        EQ.2 : Two species
        ..
        EQ.8 : Eight species.
        """ # nopep8
        return self._cards[0].get_value("spnmbr")

    @spnmbr.setter
    def spnmbr(self, value: int) -> None:
        """Set the spnmbr property."""
        self._cards[0].set_value("spnmbr", value)

    @property
    def densrct(self) -> int:
        """Get or set the Flag for kind of species transport:
        EQ.0: Passive species transport
        EQ.1: Passive species transport where the fluid density depends on the concentrations of the species.DENSFID gives the fluid density as a function of species concentrations.
        EQ.2: Reactive species transport.Card 3 gives the reaction rates for each species.
        """ # nopep8
        return self._cards[0].get_value("densrct")

    @densrct.setter
    def densrct(self, value: int) -> None:
        """Set the densrct property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""densrct must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("densrct", value)

    @property
    def densfid(self) -> typing.Optional[int]:
        """Get or set the Function ID for DENSRCT = 1 giving the fluid density as a function of species concentrations. The following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time,conc1,conc2,�,conc8). Here, x, y, and z are the coordinates, vx, vy, and vz are the components of the velocity, time is the time, pres is the pressure, conci is the concentration of the ith species.
        """ # nopep8
        return self._cards[0].get_value("densfid")

    @densfid.setter
    def densfid(self, value: int) -> None:
        """Set the densfid property."""
        self._cards[0].set_value("densfid", value)

    @property
    def hvac(self) -> typing.Optional[int]:
        """Get or set the Heating, ventilation, and air conditioning model flag:
        EQ.0:	Off
        EQ.1 : On(see Remark 1)
        """ # nopep8
        return self._cards[0].get_value("hvac")

    @hvac.setter
    def hvac(self, value: int) -> None:
        """Set the hvac property."""
        self._cards[0].set_value("hvac", value)

    @property
    def massdif1(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif1")

    @massdif1.setter
    def massdif1(self, value: float) -> None:
        """Set the massdif1 property."""
        self._cards[1].set_value("massdif1", value)

    @property
    def massdif2(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif2")

    @massdif2.setter
    def massdif2(self, value: float) -> None:
        """Set the massdif2 property."""
        self._cards[1].set_value("massdif2", value)

    @property
    def massdif3(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif3")

    @massdif3.setter
    def massdif3(self, value: float) -> None:
        """Set the massdif3 property."""
        self._cards[1].set_value("massdif3", value)

    @property
    def massdif4(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif4")

    @massdif4.setter
    def massdif4(self, value: float) -> None:
        """Set the massdif4 property."""
        self._cards[1].set_value("massdif4", value)

    @property
    def massdif5(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif5")

    @massdif5.setter
    def massdif5(self, value: float) -> None:
        """Set the massdif5 property."""
        self._cards[1].set_value("massdif5", value)

    @property
    def massdif6(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif6")

    @massdif6.setter
    def massdif6(self, value: float) -> None:
        """Set the massdif6 property."""
        self._cards[1].set_value("massdif6", value)

    @property
    def massdif7(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif7")

    @massdif7.setter
    def massdif7(self, value: float) -> None:
        """Set the massdif7 property."""
        self._cards[1].set_value("massdif7", value)

    @property
    def massdif8(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the i-species.
        """ # nopep8
        return self._cards[1].get_value("massdif8")

    @massdif8.setter
    def massdif8(self, value: float) -> None:
        """Set the massdif8 property."""
        self._cards[1].set_value("massdif8", value)

