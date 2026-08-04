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

"""Module providing the IcfdBoundaryPrescribedSptranspConc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDBOUNDARYPRESCRIBEDSPTRANSPCONC_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_ICFDBOUNDARYPRESCRIBEDSPTRANSPCONC_CARD1 = (
    FieldSchema("lcid1", int, 0, 10, None),
    FieldSchema("lcid2", int, 10, 10, None),
    FieldSchema("lcid3", int, 20, 10, None),
    FieldSchema("lcid4", int, 30, 10, None),
    FieldSchema("lcid5", int, 40, 10, None),
    FieldSchema("lcid6", int, 50, 10, None),
    FieldSchema("lcid7", int, 60, 10, None),
    FieldSchema("lcid8", int, 70, 10, None),
)

class IcfdBoundaryPrescribedSptranspConc(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_SPTRANSP_CONC keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_SPTRANSP_CONC"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPrescribedSptranspConc class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDSPTRANSPCONC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDSPTRANSPCONC_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the boundary with the concentrations boundary conditions.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        """Set the lcid1 property."""
        self._cards[1].set_value("lcid1", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        """Set the lcid2 property."""
        self._cards[1].set_value("lcid2", value)

    @property
    def lcid3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid3")

    @lcid3.setter
    def lcid3(self, value: int) -> None:
        """Set the lcid3 property."""
        self._cards[1].set_value("lcid3", value)

    @property
    def lcid4(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid4")

    @lcid4.setter
    def lcid4(self, value: int) -> None:
        """Set the lcid4 property."""
        self._cards[1].set_value("lcid4", value)

    @property
    def lcid5(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid5")

    @lcid5.setter
    def lcid5(self, value: int) -> None:
        """Set the lcid5 property."""
        self._cards[1].set_value("lcid5", value)

    @property
    def lcid6(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid6")

    @lcid6.setter
    def lcid6(self, value: int) -> None:
        """Set the lcid6 property."""
        self._cards[1].set_value("lcid6", value)

    @property
    def lcid7(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid7")

    @lcid7.setter
    def lcid7(self, value: int) -> None:
        """Set the lcid7 property."""
        self._cards[1].set_value("lcid7", value)

    @property
    def lcid8(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the concentration of species i at the boundary as a function of time. . Note that all the transported species must have a load curve defined. For instance, in the 4-species case, LCID1, LCID2, LCID3, and LCID4 must be specified.
        """ # nopep8
        return self._cards[1].get_value("lcid8")

    @lcid8.setter
    def lcid8(self, value: int) -> None:
        """Set the lcid8 property."""
        self._cards[1].set_value("lcid8", value)

