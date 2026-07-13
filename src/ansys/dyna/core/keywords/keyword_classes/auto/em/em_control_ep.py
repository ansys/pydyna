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

"""Module providing the EmControlEp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCONTROLEP_CARD0 = (
    FieldSchema("epsol", int, 0, 10, None),
    FieldSchema("nsplitri", int, 10, 10, None),
    FieldSchema("actusig", float, 20, 10, None),
)

_EMCONTROLEP_CARD1 = (
    FieldSchema("spltend", float, 0, 10, 0.0),
    FieldSchema("spldt", float, 10, 10, 0.0),
)

class EmControlEp(KeywordBase):
    """DYNA EM_CONTROL_EP keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_EP"

    def __init__(self, **kwargs):
        """Initialize the EmControlEp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLEP_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCONTROLEP_CARD1,
                **kwargs,
            ),
        ]
    @property
    def epsol(self) -> typing.Optional[int]:
        """Get or set the Electrophysiology solver selector:
        EQ.11:	Monodomain
        EQ.12 : Bidomain
        EQ.13 : Monodomain coupled with bidomain
        EQ.14 : Pure eikonal model.Activation times are computed and output in VTK format to the / vtk directory.See * EM_EP_EIKONAL.
        EQ.15 : Reaction eikonal(RE - ) model based on[1].See * EM_EP_EIKONAL.
        EQ.16 : Reaction eikonal(RE + ) model based on[1].See * EM_EP_EIKONAL.
        """ # nopep8
        return self._cards[0].get_value("epsol")

    @epsol.setter
    def epsol(self, value: int) -> None:
        """Set the epsol property."""
        self._cards[0].set_value("epsol", value)

    @property
    def nsplitri(self) -> typing.Optional[int]:
        """Get or set the Split ratio between the ionic cell model time step and the monodomain time step. In other words, we will do NSPLITRI cell model time steps for each diffusion time step
        """ # nopep8
        return self._cards[0].get_value("nsplitri")

    @nsplitri.setter
    def nsplitri(self, value: int) -> None:
        """Set the nsplitri property."""
        self._cards[0].set_value("nsplitri", value)

    @property
    def actusig(self) -> typing.Optional[float]:
        """Get or set the Time period at which the electrical conductivity is updated.
        """ # nopep8
        return self._cards[0].get_value("actusig")

    @actusig.setter
    def actusig(self, value: float) -> None:
        """Set the actusig property."""
        self._cards[0].set_value("actusig", value)

    @property
    def spltend(self) -> float:
        """Get or set the Optional end time for building the spline.
        """ # nopep8
        return self._cards[1].get_value("spltend")

    @spltend.setter
    def spltend(self, value: float) -> None:
        """Set the spltend property."""
        self._cards[1].set_value("spltend", value)

    @property
    def spldt(self) -> float:
        """Get or set the Time step for building the spline.
        """ # nopep8
        return self._cards[1].get_value("spldt")

    @spldt.setter
    def spldt(self, value: float) -> None:
        """Set the spldt property."""
        self._cards[1].set_value("spldt", value)

