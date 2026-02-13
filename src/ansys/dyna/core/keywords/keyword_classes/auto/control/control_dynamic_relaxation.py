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

"""Module providing the ControlDynamicRelaxation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_CONTROLDYNAMICRELAXATION_CARD0 = (
    FieldSchema("nrcyck", int, 0, 10, 250),
    FieldSchema("drtol", float, 10, 10, 0.001),
    FieldSchema("drfctr", float, 20, 10, 0.995),
    FieldSchema("drterm", float, 30, 10, None),
    FieldSchema("tssfdr", float, 40, 10, 0.0),
    FieldSchema("irelal", int, 50, 10, 0),
    FieldSchema("edttl", float, 60, 10, 0.04),
    FieldSchema("idrflg", int, 70, 10, 0),
)

_CONTROLDYNAMICRELAXATION_CARD1 = (
    FieldSchema("drpset", int, 0, 10, 0),
)

_CONTROLDYNAMICRELAXATION_CARD2 = (
    FieldSchema("nc", int, 0, 10, 100),
    FieldSchema("np", int, 10, 10, 0),
)

_CONTROLDYNAMICRELAXATION_CARD3 = (
    FieldSchema("psid", int, 0, 10, 0),
    FieldSchema("vecid", int, 10, 10, 0),
)

class ControlDynamicRelaxation(KeywordBase):
    """DYNA CONTROL_DYNAMIC_RELAXATION keyword"""

    keyword = "CONTROL"
    subkeyword = "DYNAMIC_RELAXATION"
    _link_fields = {
        "vecid": LinkType.DEFINE_VECTOR,
        "drpset": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlDynamicRelaxation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLDYNAMICRELAXATION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDYNAMICRELAXATION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDYNAMICRELAXATION_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLDYNAMICRELAXATION_CARD3,
                **kwargs,
            ),        ]
    @property
    def nrcyck(self) -> int:
        """Get or set the Number of iterations between convergence checks, for dynamic relaxation option (default=250).
        """ # nopep8
        return self._cards[0].get_value("nrcyck")

    @nrcyck.setter
    def nrcyck(self, value: int) -> None:
        """Set the nrcyck property."""
        self._cards[0].set_value("nrcyck", value)

    @property
    def drtol(self) -> float:
        """Get or set the Convergence tolerance for dynamic relaxation option (default=0.001).
        """ # nopep8
        return self._cards[0].get_value("drtol")

    @drtol.setter
    def drtol(self, value: float) -> None:
        """Set the drtol property."""
        self._cards[0].set_value("drtol", value)

    @property
    def drfctr(self) -> float:
        """Get or set the Dynamic relaxation factor (default=0.995).
        """ # nopep8
        return self._cards[0].get_value("drfctr")

    @drfctr.setter
    def drfctr(self, value: float) -> None:
        """Set the drfctr property."""
        self._cards[0].set_value("drfctr", value)

    @property
    def drterm(self) -> typing.Optional[float]:
        """Get or set the Optional termination time for dynamic relaxation (default: infinity).
        """ # nopep8
        return self._cards[0].get_value("drterm")

    @drterm.setter
    def drterm(self, value: float) -> None:
        """Set the drterm property."""
        self._cards[0].set_value("drterm", value)

    @property
    def tssfdr(self) -> float:
        """Get or set the Scale factor for computed time step during dynamic relaxation:
        EQ.0.0: Value is set to SCRT defined on *CONTROL_TIMESTEP. After converging, the scale factor is reset to SCRT.
        """ # nopep8
        return self._cards[0].get_value("tssfdr")

    @tssfdr.setter
    def tssfdr(self, value: float) -> None:
        """Set the tssfdr property."""
        self._cards[0].set_value("tssfdr", value)

    @property
    def irelal(self) -> int:
        """Get or set the Automatic control for dynamic relaxation option.
        EQ.0: not active,
        EQ.1: active.
        """ # nopep8
        return self._cards[0].get_value("irelal")

    @irelal.setter
    def irelal(self, value: int) -> None:
        """Set the irelal property."""
        if value not in [0, 1, None]:
            raise Exception("""irelal must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irelal", value)

    @property
    def edttl(self) -> float:
        """Get or set the Convergence tolerance on automatic control of dynamic relaxation.
        """ # nopep8
        return self._cards[0].get_value("edttl")

    @edttl.setter
    def edttl(self, value: float) -> None:
        """Set the edttl property."""
        self._cards[0].set_value("edttl", value)

    @property
    def idrflg(self) -> int:
        """Get or set the Dynamic relaxation flag for stress initialization:
        EQ.-999: dynamic relaxation not activated even if specified on a load curve, see *DEFINE_CURVE,
        EQ.-3:	dynamic relaxation is activated as with IDRFLG = 1, but the convergence check is made based only on the part set specified by DRPSET.
        All parts are active during the dynamic relaxation phase
        EQ.-1: dynamic relaxation is activated and time history output is produced during dynamic relaxation,
        EQ.0: not active,
        EQ.1: dynamic relaxation is activated,
        EQ.2: initialization to a prescribed geometry.
        EQ.3 dynamic relaxation is activated as with IDRFLG=1, but with a part set ID for convergence checking
        EQ.5: initialize implicitly
        EQ.6 initialize implicity but only for the part set specified by DRPSET.
        """ # nopep8
        return self._cards[0].get_value("idrflg")

    @idrflg.setter
    def idrflg(self, value: int) -> None:
        """Set the idrflg property."""
        if value not in [0, -999, -3, -1, 1, 2, 3, 5, 6, None]:
            raise Exception("""idrflg must be `None` or one of {0,-999,-3,-1,1,2,3,5,6}.""")
        self._cards[0].set_value("idrflg", value)

    @property
    def drpset(self) -> int:
        """Get or set the Part set ID for convergence checking (for IDRFLG=3 only).
        """ # nopep8
        return self._cards[1].get_value("drpset")

    @drpset.setter
    def drpset(self, value: int) -> None:
        """Set the drpset property."""
        self._cards[1].set_value("drpset", value)

    @property
    def nc(self) -> int:
        """Get or set the Number of time steps for initializing geometry of IDRFLG = 2.
        """ # nopep8
        return self._cards[2].get_value("nc")

    @nc.setter
    def nc(self, value: int) -> None:
        """Set the nc property."""
        self._cards[2].set_value("nc", value)

    @property
    def np(self) -> int:
        """Get or set the Number of part sets specified for IDRFLG = 2.
        """ # nopep8
        return self._cards[2].get_value("np")

    @np.setter
    def np(self, value: int) -> None:
        """Set the np property."""
        self._cards[2].set_value("np", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID for IDRFLG = 2.
        """ # nopep8
        return self._cards[3].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[3].set_value("psid", value)

    @property
    def vecid(self) -> int:
        """Get or set the Vector ID for defining origin and axis of rotation for IDRFLG = 2.
        """ # nopep8
        return self._cards[3].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[3].set_value("vecid", value)

    @property
    def vecid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vecid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vecid:
                return kwd
        return None

    @vecid_link.setter
    def vecid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vecid."""
        self.vecid = value.vid

    @property
    def drpset_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for drpset."""
        return self._get_set_link("PART", self.drpset)

    @drpset_link.setter
    def drpset_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for drpset."""
        self.drpset = value.sid

