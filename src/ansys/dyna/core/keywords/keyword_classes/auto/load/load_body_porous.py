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

"""Module providing the LoadBodyPorous class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADBODYPOROUS_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("sidtyp", int, 10, 10, 0),
    FieldSchema("ax", float, 20, 10, 0.0),
    FieldSchema("ay", float, 30, 10, 0.0),
    FieldSchema("az", float, 40, 10, 0.0),
    FieldSchema("bx", float, 50, 10, 0.0),
    FieldSchema("by", float, 60, 10, 0.0),
    FieldSchema("bz", float, 70, 10, 0.0),
)

_LOADBODYPOROUS_CARD1 = (
    FieldSchema("aopt", int, 0, 10, 0),
)

class LoadBodyPorous(KeywordBase):
    """DYNA LOAD_BODY_POROUS keyword"""

    keyword = "LOAD"
    subkeyword = "BODY_POROUS"

    def __init__(self, **kwargs):
        """Initialize the LoadBodyPorous class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBODYPOROUS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADBODYPOROUS_CARD1,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID of the ALE fluid part subjected to porous flow condition
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set ID type of the SID above.  If SIDTYP=0 (default), then the SID=PSID (part set ID).  If SIDTYP=1, then SID=PID (part ID).
        """ # nopep8
        return self._cards[0].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        """Set the sidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sidtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def ax(self) -> float:
        """Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        """Set the ax property."""
        self._cards[0].set_value("ax", value)

    @property
    def ay(self) -> float:
        """Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        """Set the ay property."""
        self._cards[0].set_value("ay", value)

    @property
    def az(self) -> float:
        """Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        """Set the az property."""
        self._cards[0].set_value("az", value)

    @property
    def bx(self) -> float:
        """Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("bx")

    @bx.setter
    def bx(self, value: float) -> None:
        """Set the bx property."""
        self._cards[0].set_value("bx", value)

    @property
    def by(self) -> float:
        """Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("by")

    @by.setter
    def by(self, value: float) -> None:
        """Set the by property."""
        self._cards[0].set_value("by", value)

    @property
    def bz(self) -> float:
        """Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
        """ # nopep8
        return self._cards[0].get_value("bz")

    @bz.setter
    def bz(self, value: float) -> None:
        """Set the bz property."""
        self._cards[0].set_value("bz", value)

    @property
    def aopt(self) -> int:
        """Get or set the Material axis option:
        EQ.0: inactive.
        EQ.1: The forces are applied in a local system attached to the ALE solid (see CTYPE=12 and DIREC=1 in *CONSTRAINED_LAGRANGE_IN_SOLID).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        """Set the aopt property."""
        if value not in [0, 1, None]:
            raise Exception("""aopt must be `None` or one of {0,1}.""")
        self._cards[1].set_value("aopt", value)

