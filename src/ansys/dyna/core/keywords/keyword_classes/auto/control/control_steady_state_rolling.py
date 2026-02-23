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

"""Module providing the ControlSteadyStateRolling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLSTEADYSTATEROLLING_CARD0 = (
    FieldSchema("imass", int, 0, 10, 0),
    FieldSchema("lcdmu", int, 10, 10, 0),
    FieldSchema("lcdmur", int, 20, 10, 0),
    FieldSchema("ivel", int, 30, 10, 0),
    FieldSchema("scl_k", int, 40, 10, 0),
)

class ControlSteadyStateRolling(KeywordBase):
    """DYNA CONTROL_STEADY_STATE_ROLLING keyword"""

    keyword = "CONTROL"
    subkeyword = "STEADY_STATE_ROLLING"
    _link_fields = {
        "lcdmu": LinkType.DEFINE_CURVE,
        "lcdmur": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlSteadyStateRolling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLSTEADYSTATEROLLING_CARD0,
                **kwargs,
            ),        ]
    @property
    def imass(self) -> int:
        """Get or set the Inertia switching flag
        EQ.0:  include inertia during an implicit dynamic simulation.
        EQ.1:  treat steady state rolling subsystems as quasi-static during implicit dynamic simulations.
        """ # nopep8
        return self._cards[0].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        """Set the imass property."""
        if value not in [0, 1, None]:
            raise Exception("""imass must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imass", value)

    @property
    def lcdmu(self) -> int:
        """Get or set the Optional load curve for scaling the friction forces in contact.
        """ # nopep8
        return self._cards[0].get_value("lcdmu")

    @lcdmu.setter
    def lcdmu(self, value: int) -> None:
        """Set the lcdmu property."""
        self._cards[0].set_value("lcdmu", value)

    @property
    def lcdmur(self) -> int:
        """Get or set the Optional load curve for scaling the friction forces in contact during dynamic relaxation. If  LCDMUR isn’t specified, LCDMU is used.
        """ # nopep8
        return self._cards[0].get_value("lcdmur")

    @lcdmur.setter
    def lcdmur(self, value: int) -> None:
        """Set the lcdmur property."""
        self._cards[0].set_value("lcdmur", value)

    @property
    def ivel(self) -> int:
        """Get or set the Velocity switching flag.
        EQ.0:  eliminate the steady state rolling body forces and set the velocities of the nodes after dynamic relaxation.
        EQ.1:  keep the steady state rolling body forces after dynamic relaxation instead of setting the velocities.
        """ # nopep8
        return self._cards[0].get_value("ivel")

    @ivel.setter
    def ivel(self, value: int) -> None:
        """Set the ivel property."""
        self._cards[0].set_value("ivel", value)

    @property
    def scl_k(self) -> int:
        """Get or set the Scale factor for the friction stiffness during contact loading and unloading. The default values are 1.0 and 0.01 for explicit and implicit, respectively. Any scaling applied here applies only to contact involving the subsystem of parts defined for steady state rolling.
        """ # nopep8
        return self._cards[0].get_value("scl_k")

    @scl_k.setter
    def scl_k(self, value: int) -> None:
        """Set the scl_k property."""
        self._cards[0].set_value("scl_k", value)

    @property
    def lcdmu_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcdmu."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdmu:
                return kwd
        return None

    @lcdmu_link.setter
    def lcdmu_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdmu."""
        self.lcdmu = value.lcid

    @property
    def lcdmur_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcdmur."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdmur:
                return kwd
        return None

    @lcdmur_link.setter
    def lcdmur_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdmur."""
        self.lcdmur = value.lcid

