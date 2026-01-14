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

"""Module providing the InitialAxialForceBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_INITIALAXIALFORCEBEAM_CARD0 = (
    FieldSchema("bsid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("scale", float, 20, 10, 1.0),
    FieldSchema("kbend", int, 30, 10, 0),
)

class InitialAxialForceBeam(KeywordBase):
    """DYNA INITIAL_AXIAL_FORCE_BEAM keyword"""

    keyword = "INITIAL"
    subkeyword = "AXIAL_FORCE_BEAM"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialAxialForceBeam class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALAXIALFORCEBEAM_CARD0,
                **kwargs,
            ),        ]
    @property
    def bsid(self) -> typing.Optional[int]:
        """Get or set the Beam set ID
        """ # nopep8
        return self._cards[0].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        """Set the bsid property."""
        self._cards[0].set_value("bsid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining preload stress versus time.  When the load curve ends or goes to zero, the initialization is assumed to be completed
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def scale(self) -> float:
        """Get or set the Scale factor on load curve.
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        """Set the scale property."""
        self._cards[0].set_value("scale", value)

    @property
    def kbend(self) -> int:
        """Get or set the Bending stiffness flag
        EQ.0:	Bending stiffness is negligible since all integration points are assigned the same axial stress
        EQ.1:	Bending stiffness is retained by keeping the axial stress gradient
        EQ.2:	Same as 1, but also allows for lining up several beams with prescribed axial force.
        """ # nopep8
        return self._cards[0].get_value("kbend")

    @kbend.setter
    def kbend(self, value: int) -> None:
        """Set the kbend property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""kbend must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("kbend", value)

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

