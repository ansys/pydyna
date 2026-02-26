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

"""Module providing the Contact2DSlidingVoids class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACT2DSLIDINGVOIDS_CARD0 = (
    FieldSchema("surfa", int, 0, 10, None),
    FieldSchema("surfb", int, 10, 10, None),
    FieldSchema("tbirth", float, 20, 10, 0.0),
    FieldSchema("tdeath", float, 30, 10, 1e+20),
)

_CONTACT2DSLIDINGVOIDS_CARD1 = (
    FieldSchema("ext_pas", int, 0, 10, 0),
    FieldSchema("theta1", float, 10, 10, 0.0),
    FieldSchema("theta2", float, 20, 10, 0.0),
    FieldSchema("tol_ig", float, 30, 10, 0.001),
    FieldSchema("pen", float, 40, 10, 0.1),
    FieldSchema("toloff", float, 50, 10, 0.25),
    FieldSchema("frcscl", float, 60, 10, 0.1),
    FieldSchema("oneway", float, 70, 10, 0.0),
)

class Contact2DSlidingVoids(KeywordBase):
    """DYNA CONTACT_2D_SLIDING_VOIDS keyword"""

    keyword = "CONTACT"
    subkeyword = "2D_SLIDING_VOIDS"

    def __init__(self, **kwargs):
        """Initialize the Contact2DSlidingVoids class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACT2DSLIDINGVOIDS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DSLIDINGVOIDS_CARD1,
                **kwargs,
            ),        ]
    @property
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the SURFA nodes, see *SET_‌NODE.  The surface specified with SURFA must be to the left of the surface specified with SURFB. For nonsymmetric contact, this surface is the tracked surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        """Set the surfa property."""
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the SURFB nodes, see *SET_‌NODE.  For nonsymmetric contact, this surface is the reference surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        """Set the surfb property."""
        self._cards[0].set_value("surfb", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time for contact.
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for contact
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def ext_pas(self) -> int:
        """Get or set the Slideline extension bypass option.
        EQ.0: extensions are used (default),
        EQ.1: extensions are not used.
        """ # nopep8
        return self._cards[1].get_value("ext_pas")

    @ext_pas.setter
    def ext_pas(self, value: int) -> None:
        """Set the ext_pas property."""
        if value not in [0, 1, None]:
            raise Exception("""ext_pas must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ext_pas", value)

    @property
    def theta1(self) -> float:
        """Get or set the Angle in degrees of slideline extension at first SURFB node.
        EQ.0.0: extension remains tangent to first SURFB segment (default).
        """ # nopep8
        return self._cards[1].get_value("theta1")

    @theta1.setter
    def theta1(self, value: float) -> None:
        """Set the theta1 property."""
        self._cards[1].set_value("theta1", value)

    @property
    def theta2(self) -> float:
        """Get or set the Angle in degrees of slideline extension at last SURFB node.
        EQ.0.0: extension remains tangent to last DURFB segment (default).
        """ # nopep8
        return self._cards[1].get_value("theta2")

    @theta2.setter
    def theta2(self, value: float) -> None:
        """Set the theta2 property."""
        self._cards[1].set_value("theta2", value)

    @property
    def tol_ig(self) -> float:
        """Get or set the Tolerance for determining initial gaps. Default is set to 1.0E-03.
        """ # nopep8
        return self._cards[1].get_value("tol_ig")

    @tol_ig.setter
    def tol_ig(self, value: float) -> None:
        """Set the tol_ig property."""
        self._cards[1].set_value("tol_ig", value)

    @property
    def pen(self) -> float:
        """Get or set the Scale factor or penalty. Default is set to 1.0E-01.
        """ # nopep8
        return self._cards[1].get_value("pen")

    @pen.setter
    def pen(self, value: float) -> None:
        """Set the pen property."""
        self._cards[1].set_value("pen", value)

    @property
    def toloff(self) -> float:
        """Get or set the Tolerance for stiffness insertion for implicit solution only. The contact stiffness is inserted when a node approaches a segment a distance equal to the segment length multiplied by TOLOFF. The stiffness is increased as the node moves closer with the full stiffness being used when the nodal point finally makes contact. default set to 0.25.
        """ # nopep8
        return self._cards[1].get_value("toloff")

    @toloff.setter
    def toloff(self, value: float) -> None:
        """Set the toloff property."""
        self._cards[1].set_value("toloff", value)

    @property
    def frcscl(self) -> float:
        """Get or set the Scale factor for the interface friction.
        """ # nopep8
        return self._cards[1].get_value("frcscl")

    @frcscl.setter
    def frcscl(self, value: float) -> None:
        """Set the frcscl property."""
        self._cards[1].set_value("frcscl", value)

    @property
    def oneway(self) -> float:
        """Get or set the Flag for one way treatment. if set to 1.0 the nodal points on the slave surface are constrained to the master surface. This option is generally recommended if the master surface is rigid.
        """ # nopep8
        return self._cards[1].get_value("oneway")

    @oneway.setter
    def oneway(self, value: float) -> None:
        """Set the oneway property."""
        self._cards[1].set_value("oneway", value)

