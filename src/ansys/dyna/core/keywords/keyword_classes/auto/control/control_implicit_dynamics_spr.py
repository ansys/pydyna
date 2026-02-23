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

"""Module providing the ControlImplicitDynamicsSpr class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLIMPLICITDYNAMICSSPR_CARD0 = (
    FieldSchema("imass", int, 0, 10, 0),
    FieldSchema("gamma", float, 10, 10, 0.5),
    FieldSchema("beta", float, 20, 10, 0.25),
    FieldSchema("tdybir", float, 30, 10, 0.0),
    FieldSchema("tdydth", float, 40, 10, 1e+28),
    FieldSchema("tdybur", float, 50, 10, 1e+28),
    FieldSchema("irate", int, 60, 10, 0),
    FieldSchema("alpha", float, 70, 10, 0.0),
)

_CONTROLIMPLICITDYNAMICSSPR_CARD1 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("angle", float, 10, 10, 90.0),
)

class ControlImplicitDynamicsSpr(KeywordBase):
    """DYNA CONTROL_IMPLICIT_DYNAMICS_SPR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_DYNAMICS_SPR"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitDynamicsSpr class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITDYNAMICSSPR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITDYNAMICSSPR_CARD1,
                **kwargs,
            ),        ]
    @property
    def imass(self) -> int:
        """Get or set the Implicit analysis type:
        LT.0: curve ID=(-IMASS) used to control amount of implicit dynamic effect applied to the analysis. TDYBIR, TDYDTH and TDYBUR are ignored with this option
        EQ.0: static analysis
        EQ.1: dynamic analysis using Newmark time integration.
        EQ.2: dynamic analysis by modal superposition following the solution of the eigenvalue problem.
        EQ.3: dynamic analysis by modal superposition using the eigenvalue solution in d3eigv files that are in the runtime directory.
        """ # nopep8
        return self._cards[0].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        """Set the imass property."""
        self._cards[0].set_value("imass", value)

    @property
    def gamma(self) -> float:
        """Get or set the Newmark time integration constant (default = 0.50).
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def beta(self) -> float:
        """Get or set the Newmark time integration constant (default = 0.25).
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def tdybir(self) -> float:
        """Get or set the Birth time for application of dynamic terms.
        """ # nopep8
        return self._cards[0].get_value("tdybir")

    @tdybir.setter
    def tdybir(self, value: float) -> None:
        """Set the tdybir property."""
        self._cards[0].set_value("tdybir", value)

    @property
    def tdydth(self) -> float:
        """Get or set the Death time for application of dynamic terms.
        """ # nopep8
        return self._cards[0].get_value("tdydth")

    @tdydth.setter
    def tdydth(self, value: float) -> None:
        """Set the tdydth property."""
        self._cards[0].set_value("tdydth", value)

    @property
    def tdybur(self) -> float:
        """Get or set the Burial time for application of dynamic terms.
        """ # nopep8
        return self._cards[0].get_value("tdybur")

    @tdybur.setter
    def tdybur(self, value: float) -> None:
        """Set the tdybur property."""
        self._cards[0].set_value("tdybur", value)

    @property
    def irate(self) -> int:
        """Get or set the Rate effects switch:
        EQ.-1: rate effects are on in constitutive models even in implicit statics
        EQ.0: rate effects are on in constitutive models, except implicit statics
        EQ.1: rate effects are off in constitutive models
        EQ.2: rate effects are off in constitutive models for both explicit and implicit
        """ # nopep8
        return self._cards[0].get_value("irate")

    @irate.setter
    def irate(self, value: int) -> None:
        """Set the irate property."""
        self._cards[0].set_value("irate", value)

    @property
    def alpha(self) -> float:
        """Get or set the Composite time integration constant (see Remark 2).
        GT.0: Bathe composite scheme is activated
        LT.0.AND.GT. - 1 : HHT scheme is activated
        LE. - 1 : Specify part sets for finite rotational dynamics
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for a body undergoing rotational (spinning) motion.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def angle(self) -> float:
        """Get or set the Target angle increment during a single time step, in degrees.
        """ # nopep8
        return self._cards[1].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[1].set_value("angle", value)

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

