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

"""Module providing the Contact2DNodeToSolidTied class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACT2DNODETOSOLIDTIED_CARD0 = (
    FieldSchema("sph", int, 0, 10, None),
    FieldSchema("solid", int, 10, 10, None),
    FieldSchema("tbirth", int, 20, 10, None),
    FieldSchema("tdeath", int, 30, 10, None),
)

_CONTACT2DNODETOSOLIDTIED_CARD1 = (
    FieldSchema("soft", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("vc", float, 20, 10, None),
    FieldSchema("offd", float, 30, 10, None),
    FieldSchema("pen", float, 40, 10, 1.0),
    FieldSchema("fs", float, 50, 10, 0.0),
    FieldSchema("fd", float, 60, 10, 0.0),
    FieldSchema("dc", float, 70, 10, 0.0),
)

class Contact2DNodeToSolidTied(KeywordBase):
    """DYNA CONTACT_2D_NODE_TO_SOLID_TIED keyword"""

    keyword = "CONTACT"
    subkeyword = "2D_NODE_TO_SOLID_TIED"

    def __init__(self, **kwargs):
        """Initialize the Contact2DNodeToSolidTied class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACT2DNODETOSOLIDTIED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DNODETOSOLIDTIED_CARD1,
                **kwargs,
            ),        ]
    @property
    def sph(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID or part set ID for SPH nodes. If SPH>0, a node set ID is assumed, if SPH<0 a part set ID is assumed
        """ # nopep8
        return self._cards[0].get_value("sph")

    @sph.setter
    def sph(self, value: int) -> None:
        """Set the sph property."""
        self._cards[0].set_value("sph", value)

    @property
    def solid(self) -> typing.Optional[int]:
        """Get or set the Solid part set ID. SOLID<0 since only part set is allowed
        """ # nopep8
        return self._cards[0].get_value("solid")

    @solid.setter
    def solid(self, value: int) -> None:
        """Set the solid property."""
        self._cards[0].set_value("solid", value)

    @property
    def tbirth(self) -> typing.Optional[int]:
        """Get or set the Birth time for contact
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: int) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> typing.Optional[int]:
        """Get or set the Death time for contact
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: int) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def soft(self) -> int:
        """Get or set the Soft constraint option:
        EQ.0: penalty formulation,
        EQ.1: soft constraint formulation.
        The soft constraint may be necessary if the material constants of the
        parts in contact have a wide variation in the elastic bulk moduli. In
        the soft constraint option, the interface stiffness is based on the
        nodal mass and the global time step size. The soft constraint option
        is also recommended for axisymmetric simulations.
        """ # nopep8
        return self._cards[1].get_value("soft")

    @soft.setter
    def soft(self, value: int) -> None:
        """Set the soft property."""
        if value not in [0, 1, None]:
            raise Exception("""soft must be `None` or one of {0,1}.""")
        self._cards[1].set_value("soft", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        """Set the vc property."""
        self._cards[1].set_value("vc", value)

    @property
    def offd(self) -> typing.Optional[float]:
        """Get or set the Contact offset distance for SPH nodes. It does not currently apply to tied contacts. Recommended to be half of the original particle
        spacing in contact direction.
        """ # nopep8
        return self._cards[1].get_value("offd")

    @offd.setter
    def offd(self, value: float) -> None:
        """Set the offd property."""
        self._cards[1].set_value("offd", value)

    @property
    def pen(self) -> float:
        """Get or set the Scale factor for penalty. 	EQ. 0.0: default set to: 1.0
        """ # nopep8
        return self._cards[1].get_value("pen")

    @pen.setter
    def pen(self, value: float) -> None:
        """Set the pen property."""
        self._cards[1].set_value("pen", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[1].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[1].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient.
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[1].set_value("dc", value)

