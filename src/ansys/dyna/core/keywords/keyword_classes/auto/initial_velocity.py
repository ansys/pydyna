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

"""Module providing the InitialVelocity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialVelocity(KeywordBase):
    """DYNA INITIAL_VELOCITY keyword"""

    keyword = "INITIAL"
    subkeyword = "VELOCITY"

    def __init__(self, **kwargs):
        """Initialize the InitialVelocity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nsidex",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "irigid",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "icid",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vx",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vy",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vz",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vxr",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vyr",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vzr",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vxe",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vye",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vze",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vxre",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vyre",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vzre",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
                lambda: self.nsidex > 0,
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID, see *SET_NODE, containing nodes for initial velocity:
        EQ.0: all nodes are included.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Nodal set ID, see *SET_NODE, containing nodes that are exempted from the imposed velocities and may have other initial velocities.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in the box which belong to NSID are initialized. Nodes outside the box are not initalized. Exempted nodes are initialized to velocities defined by VXE, VYE, and VZE below regardless of their location relative to the box.
        Note VXE,VYE and VZE will only be shown once a value is input for NSIDEX.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def irigid(self) -> int:
        """Get or set the Option to overwrite rigid body velocities defined on *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA cards.
        GE.1: part set ID, containing ID of parts to overwrite. Centre of gravity of part must lie within box BOXID. If BOXID is not defined then all parts defined in the set are overwritten.
        EQ.-1: Overwrite velocities for all *PART_INERTIA's and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA 's with a centre of gravity within box BOXID. If BOXID is not defined then all are overwritten.
        EQ.-2: Overwrite velocities for all *PART_INERTIA's and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA's.
        """ # nopep8
        return self._cards[0].get_value("irigid")

    @irigid.setter
    def irigid(self, value: int) -> None:
        """Set the irigid property."""
        self._cards[0].set_value("irigid", value)

    @property
    def icid(self) -> int:
        """Get or set the Local coordinate system ID. The initial velocity is specified in the local coordinate system if ICID is greater than zero.
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        """Set the icid property."""
        self._cards[0].set_value("icid", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial velocity in x-direction.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial velocity in y-direction.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial velocity in z-direction.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def vxr(self) -> float:
        """Get or set the Initial rotational velocity about the x-axis.
        """ # nopep8
        return self._cards[1].get_value("vxr")

    @vxr.setter
    def vxr(self, value: float) -> None:
        """Set the vxr property."""
        self._cards[1].set_value("vxr", value)

    @property
    def vyr(self) -> float:
        """Get or set the Initial rotational velocity about the y-axis.
        """ # nopep8
        return self._cards[1].get_value("vyr")

    @vyr.setter
    def vyr(self, value: float) -> None:
        """Set the vyr property."""
        self._cards[1].set_value("vyr", value)

    @property
    def vzr(self) -> float:
        """Get or set the Initial rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[1].get_value("vzr")

    @vzr.setter
    def vzr(self, value: float) -> None:
        """Set the vzr property."""
        self._cards[1].set_value("vzr", value)

    @property
    def vxe(self) -> float:
        """Get or set the Initial velocity in x-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vxe")

    @vxe.setter
    def vxe(self, value: float) -> None:
        """Set the vxe property."""
        self._cards[2].set_value("vxe", value)

    @property
    def vye(self) -> float:
        """Get or set the Initial velocity in y-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vye")

    @vye.setter
    def vye(self, value: float) -> None:
        """Set the vye property."""
        self._cards[2].set_value("vye", value)

    @property
    def vze(self) -> float:
        """Get or set the Initial velocity in z-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vze")

    @vze.setter
    def vze(self, value: float) -> None:
        """Set the vze property."""
        self._cards[2].set_value("vze", value)

    @property
    def vxre(self) -> float:
        """Get or set the Initial rotational velocity in x-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vxre")

    @vxre.setter
    def vxre(self, value: float) -> None:
        """Set the vxre property."""
        self._cards[2].set_value("vxre", value)

    @property
    def vyre(self) -> float:
        """Get or set the Initial rotational velocity in y-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vyre")

    @vyre.setter
    def vyre(self, value: float) -> None:
        """Set the vyre property."""
        self._cards[2].set_value("vyre", value)

    @property
    def vzre(self) -> float:
        """Get or set the Initial rotational velocity in z-direction of exempted nodes.
        """ # nopep8
        return self._cards[2].get_value("vzre")

    @vzre.setter
    def vzre(self, value: float) -> None:
        """Set the vzre property."""
        self._cards[2].set_value("vzre", value)

