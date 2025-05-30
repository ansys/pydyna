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

"""Module providing the BoundaryThermalWeld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundaryThermalWeld(KeywordBase):
    """DYNA BOUNDARY_THERMAL_WELD keyword"""

    keyword = "BOUNDARY"
    subkeyword = "THERMAL_WELD"

    def __init__(self, **kwargs):
        """Initialize the BoundaryThermalWeld class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ptyp",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "nid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nflag",
                        int,
                        30,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "x0",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "y0",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "z0",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2id",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "cf",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "cr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "q",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ff",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fr",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tx",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ty",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tz",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID to which weld source is applied.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ptyp(self) -> int:
        """Get or set the PID type:
        EQ.1: PID defines a single part ID (default),
        EQ.2: PID defines a part set ID.
        """ # nopep8
        return self._cards[0].get_value("ptyp")

    @ptyp.setter
    def ptyp(self, value: int) -> None:
        """Set the ptyp property."""
        if value not in [1, 2, None]:
            raise Exception("""ptyp must be `None` or one of {1,2}.""")
        self._cards[0].set_value("ptyp", value)

    @property
    def nid(self) -> int:
        """Get or set the Node ID giving location of weld source.
        EQ.0: location defined by (X0,Y0,Z0) below (default).
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def nflag(self) -> int:
        """Get or set the Flag controlling motion of weld source:
        EQ.1: source moves with node NID (default),
        EQ.2: source is fixed in space at original position of node NID.
        """ # nopep8
        return self._cards[0].get_value("nflag")

    @nflag.setter
    def nflag(self, value: int) -> None:
        """Set the nflag property."""
        if value not in [1, 2, None]:
            raise Exception("""nflag must be `None` or one of {1,2}.""")
        self._cards[0].set_value("nflag", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of weld source, which remains fixed in space.
        Ignored if NID above is nonzero.
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of weld source, which remains fixed in space.
        Ignored if NID above is nonzero.
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[0].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of weld source, which remains fixed in space.
        Ignored if NID above is nonzero.
        """ # nopep8
        return self._cards[0].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[0].set_value("z0", value)

    @property
    def n2id(self) -> typing.Optional[int]:
        """Get or set the Second node ID for weld beam aiming direction:
        GT. 0: beam is aimed from N2ID to NID, moves with these nodes,
        EQ.-1: beam aiming direction is (tx,ty,tz) input on optional card 3.
        """ # nopep8
        return self._cards[0].get_value("n2id")

    @n2id.setter
    def n2id(self, value: int) -> None:
        """Set the n2id property."""
        self._cards[0].set_value("n2id", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Weld pool width.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Weld pool depth (in beam aiming direction).
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def cf(self) -> typing.Optional[float]:
        """Get or set the Weld pool forward direction.
        """ # nopep8
        return self._cards[1].get_value("cf")

    @cf.setter
    def cf(self, value: float) -> None:
        """Set the cf property."""
        self._cards[1].set_value("cf", value)

    @property
    def cr(self) -> typing.Optional[float]:
        """Get or set the Weld pool rearward direction.
        """ # nopep8
        return self._cards[1].get_value("cr")

    @cr.setter
    def cr(self, value: float) -> None:
        """Set the cr property."""
        self._cards[1].set_value("cr", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for weld energy input rate vs. time
        EQ.0: use constant multiplier value Q.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Curve multiplier for weld energy input rate [energy/time, e.g., Watt]
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[1].set_value("q", value)

    @property
    def ff(self) -> typing.Optional[float]:
        """Get or set the Forward distribution fraction.
        Note: FF + FR = 2.0.
        """ # nopep8
        return self._cards[1].get_value("ff")

    @ff.setter
    def ff(self, value: float) -> None:
        """Set the ff property."""
        self._cards[1].set_value("ff", value)

    @property
    def fr(self) -> typing.Optional[float]:
        """Get or set the Rearward distribution fraction.
        Note: FF + FR = 2.0.
        """ # nopep8
        return self._cards[1].get_value("fr")

    @fr.setter
    def fr(self, value: float) -> None:
        """Set the fr property."""
        self._cards[1].set_value("fr", value)

    @property
    def tx(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of weld beam direction vector in global coordinates.
        Define only if N2ID = -1.
        """ # nopep8
        return self._cards[2].get_value("tx")

    @tx.setter
    def tx(self, value: float) -> None:
        """Set the tx property."""
        self._cards[2].set_value("tx", value)

    @property
    def ty(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of weld beam direction vector in global coordinates.
        Define only if N2ID = -1.
        """ # nopep8
        return self._cards[2].get_value("ty")

    @ty.setter
    def ty(self, value: float) -> None:
        """Set the ty property."""
        self._cards[2].set_value("ty", value)

    @property
    def tz(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of weld beam direction vector in global coordinates.
        Define only if N2ID = -1.
        """ # nopep8
        return self._cards[2].get_value("tz")

    @tz.setter
    def tz(self, value: float) -> None:
        """Set the tz property."""
        self._cards[2].set_value("tz", value)

