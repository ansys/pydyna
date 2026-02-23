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

"""Module providing the BoundaryAmbientEos class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_BOUNDARYAMBIENTEOS_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lc1", int, 10, 10, None),
    FieldSchema("lc2", int, 20, 10, None),
)

class BoundaryAmbientEos(KeywordBase):
    """DYNA BOUNDARY_AMBIENT_EOS keyword"""

    keyword = "BOUNDARY"
    subkeyword = "AMBIENT_EOS"
    _link_fields = {
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryAmbientEos class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYAMBIENTEOS_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the The ambient Part ID for which the thermodynamic state is being defined.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for internal energy per unit reference specific volume (or a temperature load curve ID if *EOS_IDEAL_GAS is being used).
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for relative volume, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[0].set_value("lc2", value)

    @property
    def lc1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

