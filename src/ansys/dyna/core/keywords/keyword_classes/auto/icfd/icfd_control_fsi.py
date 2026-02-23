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

"""Module providing the IcfdControlFsi class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDCONTROLFSI_CARD0 = (
    FieldSchema("owc", int, 0, 10, 0),
    FieldSchema("bt", float, 10, 10, 0.0),
    FieldSchema("dt", float, 20, 10, 1e+28),
    FieldSchema("idc", float, 30, 10, 0.25),
    FieldSchema("lcidsf", int, 40, 10, None),
    FieldSchema("xproj", int, 50, 10, 0),
)

_ICFDCONTROLFSI_CARD1 = (
    FieldSchema("nsub", int, 0, 10, None),
)

class IcfdControlFsi(KeywordBase):
    """DYNA ICFD_CONTROL_FSI keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_FSI"
    _link_fields = {
        "lcidsf": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdControlFsi class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLFSI_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLFSI_CARD1,
                **kwargs,
            ),        ]
    @property
    def owc(self) -> int:
        """Get or set the Indicates the coupling direction to the solver.
        EQ.0:	Two - way coupling.Loads and displacements are transferred across the FSI interface and the full non - linear problem is solved.Weak FSI coupling when coupled to explicit mechanical solver, strong FSI coupling when coupled to implicit mechanical solver.
        EQ.1 : One - way coupling.The solid mechanics solver transfers displacements to the fluid solver.
        EQ.2 : One - way coupling.The fluid solver transfers stresses to the solid mechanics solver.
        EQ.3 : Two - way coupling.Forces weak coupling(no sub - stepping) with implicit mechanical solver.
        """ # nopep8
        return self._cards[0].get_value("owc")

    @owc.setter
    def owc(self, value: int) -> None:
        """Set the owc property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""owc must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("owc", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time for the FSI coupling. Before BT the fluid solver will not pass any loads to the structure but it will receive displacements from the solid solver.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for the FSI coupling. After DT the fluid solver will not trans# fer any loads to the solid solver but it will continue to deform with the solid.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def idc(self) -> float:
        """Get or set the Interaction detection coefficient.
        """ # nopep8
        return self._cards[0].get_value("idc")

    @idc.setter
    def idc(self, value: float) -> None:
        """Set the idc property."""
        self._cards[0].set_value("idc", value)

    @property
    def lcidsf(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID to apply a scaling factor on the forces transferred to the solid :
        GT.0: Load curve ID function of iterations.
        LT.0: Load curve ID function of time.
        """ # nopep8
        return self._cards[0].get_value("lcidsf")

    @lcidsf.setter
    def lcidsf(self, value: int) -> None:
        """Set the lcidsf property."""
        self._cards[0].set_value("lcidsf", value)

    @property
    def xproj(self) -> int:
        """Get or set the Projection of the nodes of the CFD domain that are at the FSI interface onto the structural mesh.
        EQ.0:No projection
        EQ.1:Projection
        """ # nopep8
        return self._cards[0].get_value("xproj")

    @xproj.setter
    def xproj(self, value: int) -> None:
        """Set the xproj property."""
        if value not in [0, 1, None]:
            raise Exception("""xproj must be `None` or one of {0,1}.""")
        self._cards[0].set_value("xproj", value)

    @property
    def nsub(self) -> typing.Optional[int]:
        """Get or set the Optional limit on the number of FSI fluid subiterations. This avoids the sometimes unneeded excessive number of FSI subiterations when the fluid and very light structures (like parachutes) develop a resonance-like mode inside the FSI subiterations (coupling iterations)
        """ # nopep8
        return self._cards[1].get_value("nsub")

    @nsub.setter
    def nsub(self, value: int) -> None:
        """Set the nsub property."""
        self._cards[1].set_value("nsub", value)

    @property
    def lcidsf_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidsf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidsf:
                return kwd
        return None

    @lcidsf_link.setter
    def lcidsf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidsf."""
        self.lcidsf = value.lcid

