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

"""Module providing the CeseBoundaryPrescribedSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class CeseBoundaryPrescribedSegment(KeywordBase):
    """DYNA CESE_BOUNDARY_PRESCRIBED_SEGMENT keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_PRESCRIBED_SEGMENT"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryPrescribedSegment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2 ",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "idcomp",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc_u",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc_v ",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc_w",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc_rho",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc_p ",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc_t",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sf_u",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sf_v ",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sf_w",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sf_rho",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sf_p ",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sf_t",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2_(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n2 ")

    @n2_.setter
    def n2_(self, value: int) -> None:
        """Set the n2_ property."""
        self._cards[0].set_value("n2 ", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining a segment.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def idcomp(self) -> typing.Optional[int]:
        """Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain as defined with a *CHEMISTRY_COMPOSITION card.
        """ # nopep8
        return self._cards[0].get_value("idcomp")

    @idcomp.setter
    def idcomp(self, value: int) -> None:
        """Set the idcomp property."""
        self._cards[0].set_value("idcomp", value)

    @property
    def lc_u(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the x-component of the velocity versus time
        """ # nopep8
        return self._cards[1].get_value("lc_u")

    @lc_u.setter
    def lc_u(self, value: int) -> None:
        """Set the lc_u property."""
        self._cards[1].set_value("lc_u", value)

    @property
    def lc_v_(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the y-component of the velocity versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_v ")

    @lc_v_.setter
    def lc_v_(self, value: int) -> None:
        """Set the lc_v_ property."""
        self._cards[1].set_value("lc_v ", value)

    @property
    def lc_w(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the z-component of the velocity versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_w")

    @lc_w.setter
    def lc_w(self, value: int) -> None:
        """Set the lc_w property."""
        self._cards[1].set_value("lc_w", value)

    @property
    def lc_rho(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the density versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_rho")

    @lc_rho.setter
    def lc_rho(self, value: int) -> None:
        """Set the lc_rho property."""
        self._cards[1].set_value("lc_rho", value)

    @property
    def lc_p_(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the pressure versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_p ")

    @lc_p_.setter
    def lc_p_(self, value: int) -> None:
        """Set the lc_p_ property."""
        self._cards[1].set_value("lc_p ", value)

    @property
    def lc_t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID to describe the temperature versus time.
        """ # nopep8
        return self._cards[1].get_value("lc_t")

    @lc_t.setter
    def lc_t(self, value: int) -> None:
        """Set the lc_t property."""
        self._cards[1].set_value("lc_t", value)

    @property
    def sf_u(self) -> float:
        """Get or set the Scale factor for LC_U
        """ # nopep8
        return self._cards[2].get_value("sf_u")

    @sf_u.setter
    def sf_u(self, value: float) -> None:
        """Set the sf_u property."""
        self._cards[2].set_value("sf_u", value)

    @property
    def sf_v_(self) -> float:
        """Get or set the Scale factor for LC_V
        """ # nopep8
        return self._cards[2].get_value("sf_v ")

    @sf_v_.setter
    def sf_v_(self, value: float) -> None:
        """Set the sf_v_ property."""
        self._cards[2].set_value("sf_v ", value)

    @property
    def sf_w(self) -> float:
        """Get or set the Scale factor for LC_W
        """ # nopep8
        return self._cards[2].get_value("sf_w")

    @sf_w.setter
    def sf_w(self, value: float) -> None:
        """Set the sf_w property."""
        self._cards[2].set_value("sf_w", value)

    @property
    def sf_rho(self) -> float:
        """Get or set the Scale factor for LC_RHO
        """ # nopep8
        return self._cards[2].get_value("sf_rho")

    @sf_rho.setter
    def sf_rho(self, value: float) -> None:
        """Set the sf_rho property."""
        self._cards[2].set_value("sf_rho", value)

    @property
    def sf_p_(self) -> float:
        """Get or set the Scale factor for LC_P
        """ # nopep8
        return self._cards[2].get_value("sf_p ")

    @sf_p_.setter
    def sf_p_(self, value: float) -> None:
        """Set the sf_p_ property."""
        self._cards[2].set_value("sf_p ", value)

    @property
    def sf_t(self) -> float:
        """Get or set the Scale factor for LC_T
        """ # nopep8
        return self._cards[2].get_value("sf_t")

    @sf_t.setter
    def sf_t(self, value: float) -> None:
        """Set the sf_t property."""
        self._cards[2].set_value("sf_t", value)

