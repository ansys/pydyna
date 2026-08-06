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

"""Module providing the EmControlSolution class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_EMCONTROLSOLUTION_CARD0 = (
    FieldSchema("ncylfem", int, 0, 10, 5000),
    FieldSchema("ncylbem", int, 10, 10, 5000),
    FieldSchema("autofem", int, 20, 10, 0),
    FieldSchema("autobem", int, 30, 10, 0),
    FieldSchema("tol1fem", float, 40, 10, 0.3),
    FieldSchema("tol2fem", float, 50, 10, 0.1),
    FieldSchema("tol1bem", float, 60, 10, 0.3),
    FieldSchema("tol2bem", float, 70, 10, 0.1),
)

_EMCONTROLSOLUTION_CARD1 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
    FieldSchema("nid4", int, 30, 10, None),
    FieldSchema("nid5", int, 40, 10, None),
    FieldSchema("nid6", int, 50, 10, None),
    FieldSchema("ang", float, 60, 10, 1e+16),
    FieldSchema("dist", float, 70, 10, 1e+16),
)

class EmControlSolution(KeywordBase):
    """DYNA EM_CONTROL_SOLUTION keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_SOLUTION"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "nid4": LinkType.NODE,
        "nid5": LinkType.NODE,
        "nid6": LinkType.NODE,
        "ang": LinkType.NODE,
        "dist": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmControlSolution class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLSOLUTION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCONTROLSOLUTION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ncylfem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLFEM function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylfem")

    @ncylfem.setter
    def ncylfem(self, value: int) -> None:
        """Set the ncylfem property."""
        self._cards[0].set_value("ncylfem", value)

    @property
    def ncylbem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLBEM function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylbem")

    @ncylbem.setter
    def ncylbem(self, value: int) -> None:
        """Set the ncylbem property."""
        self._cards[0].set_value("ncylbem", value)

    @property
    def autofem(self) -> int:
        """Get or set the In addition to NCYLFEM, this triggers an automatic recomputation of the FEM matrices based on an error calculation of the conductors' relative deformation and electrical conductivity changes.
        EQ.0:Autorecomputation off.
        EQ.1:Autorecomputation on.
        """ # nopep8
        return self._cards[0].get_value("autofem")

    @autofem.setter
    def autofem(self, value: int) -> None:
        """Set the autofem property."""
        if value not in [0, 1, None]:
            raise Exception("""autofem must be `None` or one of {0,1}.""")
        self._cards[0].set_value("autofem", value)

    @property
    def autobem(self) -> int:
        """Get or set the In addition to NCYLBEM, this triggers an automatic recomputation of the BEM matrices based on an error calculation of the conductors' relative displacements.
        EQ.0:Autorecomputation off.
        EQ.1:Autorecomputation on.
        EQ.2: Autorecomputation on. Skip EM solve. See Remark 4.
        """ # nopep8
        return self._cards[0].get_value("autobem")

    @autobem.setter
    def autobem(self, value: int) -> None:
        """Set the autobem property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""autobem must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("autobem", value)

    @property
    def tol1fem(self) -> float:
        """Get or set the Only used if AUTOFEM=1. If a conducting element experiences a deformation or a conductivity change that reaches an error larger than TOL1FEM, the solver reassembles the FEM matrices. If negative, the absolute value refers to a load curve that gives the tolerance as a function of time.
        """ # nopep8
        return self._cards[0].get_value("tol1fem")

    @tol1fem.setter
    def tol1fem(self, value: float) -> None:
        """Set the tol1fem property."""
        self._cards[0].set_value("tol1fem", value)

    @property
    def tol2fem(self) -> float:
        """Get or set the Only used if AUTOFEM=1. If (number of conducting elements)xTOL2FEM elements experience a deformation or a conductivity change that reaches an error larger than TOL2FEM, the solver recomputes the FEM matrices. If negative, the absolute value refers to a load curve that gives TOL2FEM as a function of time.
        """ # nopep8
        return self._cards[0].get_value("tol2fem")

    @tol2fem.setter
    def tol2fem(self, value: float) -> None:
        """Set the tol2fem property."""
        self._cards[0].set_value("tol2fem", value)

    @property
    def tol1bem(self) -> float:
        """Get or set the Only used if AUTOBEM=1. If a conducting element experiences a displacement that reaches an error larger than TOL1BEM, then LS-DYNA reassembles the BEM matrices. If negative, the absolute value refers to a load curve that gives the tolerance as a function of time.
        """ # nopep8
        return self._cards[0].get_value("tol1bem")

    @tol1bem.setter
    def tol1bem(self, value: float) -> None:
        """Set the tol1bem property."""
        self._cards[0].set_value("tol1bem", value)

    @property
    def tol2bem(self) -> float:
        """Get or set the Only used if AUTOBEM=1. If (number of conducting elements)xTOL2BEM elements experience a displacement that reaches an error larger than TOL2BEM, then the BEM matrices are recomputed. If negative, the absolute value refers to a load curve that gives TOL2BEM as a function of time.
        """ # nopep8
        return self._cards[0].get_value("tol2bem")

    @tol2bem.setter
    def tol2bem(self, value: float) -> None:
        """Set the tol2bem property."""
        self._cards[0].set_value("tol2bem", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID1, NID2, and NID3. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID1, NID2, and NID3. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID1, NID2, and NID3. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID4, NID5, and NID6. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID4, NID5, and NID6. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system specified with NID4, NID5, and NID6. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[1].set_value("nid6", value)

    @property
    def ang(self) -> float:
        """Get or set the Angle (in degrees) tolerance criterion that triggers a BEM matrix recomputation. See Remark 3.
        LT.0.0: |ANG| is a load curve ID giving the angle tolerance criterion as a function of time
        """ # nopep8
        return self._cards[1].get_value("ang")

    @ang.setter
    def ang(self, value: float) -> None:
        """Set the ang property."""
        self._cards[1].set_value("ang", value)

    @property
    def dist(self) -> float:
        """Get or set the Distance tolerance criterion that triggers a BEM matrix recomputation. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        """Set the dist property."""
        self._cards[1].set_value("dist", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def nid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid4."""
        return self._get_link_by_attr("NODE", "nid", self.nid4, "parts")

    @property
    def nid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid5."""
        return self._get_link_by_attr("NODE", "nid", self.nid5, "parts")

    @property
    def nid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid6."""
        return self._get_link_by_attr("NODE", "nid", self.nid6, "parts")

    @property
    def ang_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given ang."""
        return self._get_link_by_attr("NODE", "nid", self.ang, "parts")

    @property
    def dist_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given dist."""
        return self._get_link_by_attr("NODE", "nid", self.dist, "parts")

