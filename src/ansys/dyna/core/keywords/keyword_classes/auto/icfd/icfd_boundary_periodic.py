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

"""Module providing the IcfdBoundaryPeriodic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDBOUNDARYPERIODIC_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ptype", int, 10, 10, 1),
    FieldSchema("pid2", int, 20, 10, None),
    FieldSchema("pdlcid", int, 30, 10, None),
    FieldSchema("axe", int, 40, 10, None),
    FieldSchema("ptid", int, 50, 10, None),
    FieldSchema("angle", int, 60, 10, None),
)

class IcfdBoundaryPeriodic(KeywordBase):
    """DYNA ICFD_BOUNDARY_PERIODIC keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PERIODIC"
    _link_fields = {
        "pdlcid": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPeriodic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPERIODIC_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ptype(self) -> int:
        """Get or set the Boundary type:
        EQ.1:	Periodic rotation boundary condition.
        EQ.2 : Periodic reflective boundary condition.
        EQ.3 : Sliding mesh boundary condition
        .
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""ptype must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("ptype", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the PID for the second surface mesh. The boundary condition defined in PTYPE will applied between PID and PID2. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

    @property
    def pdlcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID to describe the pressure drop value versus time between PID and PID2.
        """ # nopep8
        return self._cards[0].get_value("pdlcid")

    @pdlcid.setter
    def pdlcid(self, value: int) -> None:
        """Set the pdlcid property."""
        self._cards[0].set_value("pdlcid", value)

    @property
    def axe(self) -> typing.Optional[int]:
        """Get or set the If PTYPE=1 :
        EQ.1:	Rotation around X - Axis.
        EQ.2 : Rotation around Y - Axis.
        EQ.3 : Rotation around Z - Axis.
        If PTYPE = 3 :
        EQ.0 : The contact distance between two faces of PID and PID2 is based on the characteristic local element size.
        EQ.1 : The contact distance between two faces of PID and PID2 is based on the characteristic local element size scaled by a factor given by ANGLE.
        EQ.2 : The contact distance between two faces of PID and PID2 is based on the length given by ANGLE.
        """ # nopep8
        return self._cards[0].get_value("axe")

    @axe.setter
    def axe(self, value: int) -> None:
        """Set the axe property."""
        self._cards[0].set_value("axe", value)

    @property
    def ptid(self) -> typing.Optional[int]:
        """Get or set the Origin point ID for PTYPE=1 and PTYPE=2 (See *ICFD_DEFINE_POINT).
        """ # nopep8
        return self._cards[0].get_value("ptid")

    @ptid.setter
    def ptid(self, value: int) -> None:
        """Set the ptid property."""
        self._cards[0].set_value("ptid", value)

    @property
    def angle(self) -> typing.Optional[int]:
        """Get or set the Rotation angle for PTYPE=1. Characterizes contact distance for PTYPE=3 and axe different then 0.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: int) -> None:
        """Set the angle property."""
        self._cards[0].set_value("angle", value)

    @property
    def pdlcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for pdlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.pdlcid:
                return kwd
        return None

    @pdlcid_link.setter
    def pdlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for pdlcid."""
        self.pdlcid = value.lcid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

