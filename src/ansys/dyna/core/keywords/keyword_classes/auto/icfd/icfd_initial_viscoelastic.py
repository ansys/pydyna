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

"""Module providing the IcfdInitialViscoelastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDINITIALVISCOELASTIC_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("fidxx", int, 10, 10, None),
    FieldSchema("fidyy", int, 20, 10, None),
    FieldSchema("fidzz", int, 30, 10, None),
    FieldSchema("fidxy", int, 40, 10, None),
    FieldSchema("fidxz", int, 50, 10, None),
    FieldSchema("fidyz", int, 60, 10, None),
)

class IcfdInitialViscoelastic(KeywordBase):
    """DYNA ICFD_INITIAL_VISCOELASTIC keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_VISCOELASTIC"
    _link_fields = {
        "fidxx": LinkType.DEFINE_CURVE,
        "fidyy": LinkType.DEFINE_CURVE,
        "fidzz": LinkType.DEFINE_CURVE,
        "fidxy": LinkType.DEFINE_CURVE,
        "fidxz": LinkType.DEFINE_CURVE,
        "fidyz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdInitialViscoelastic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALVISCOELASTIC_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the volume elements or the surface elements where the conformation tensor components are initialized (see *ICFD_PART_VOL and *ICFD_PART). PID set to 0 means assigning the values to all nodes at once.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def fidxx(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidxx")

    @fidxx.setter
    def fidxx(self, value: int) -> None:
        """Set the fidxx property."""
        self._cards[0].set_value("fidxx", value)

    @property
    def fidyy(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidyy")

    @fidyy.setter
    def fidyy(self, value: int) -> None:
        """Set the fidyy property."""
        self._cards[0].set_value("fidyy", value)

    @property
    def fidzz(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidzz")

    @fidzz.setter
    def fidzz(self, value: int) -> None:
        """Set the fidzz property."""
        self._cards[0].set_value("fidzz", value)

    @property
    def fidxy(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidxy")

    @fidxy.setter
    def fidxy(self, value: int) -> None:
        """Set the fidxy property."""
        self._cards[0].set_value("fidxy", value)

    @property
    def fidxz(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidxz")

    @fidxz.setter
    def fidxz(self, value: int) -> None:
        """Set the fidxz property."""
        self._cards[0].set_value("fidxz", value)

    @property
    def fidyz(self) -> typing.Optional[int]:
        """Get or set the Function IDs (see *DEFINE_FUNCTION) for the conformation tensor components. The components can spatially vary. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[0].get_value("fidyz")

    @fidyz.setter
    def fidyz(self, value: int) -> None:
        """Set the fidyz property."""
        self._cards[0].set_value("fidyz", value)

    @property
    def fidxx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidxx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidxx:
                return kwd
        return None

    @fidxx_link.setter
    def fidxx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidxx."""
        self.fidxx = value.lcid

    @property
    def fidyy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidyy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidyy:
                return kwd
        return None

    @fidyy_link.setter
    def fidyy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidyy."""
        self.fidyy = value.lcid

    @property
    def fidzz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidzz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidzz:
                return kwd
        return None

    @fidzz_link.setter
    def fidzz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidzz."""
        self.fidzz = value.lcid

    @property
    def fidxy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidxy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidxy:
                return kwd
        return None

    @fidxy_link.setter
    def fidxy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidxy."""
        self.fidxy = value.lcid

    @property
    def fidxz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidxz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidxz:
                return kwd
        return None

    @fidxz_link.setter
    def fidxz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidxz."""
        self.fidxz = value.lcid

    @property
    def fidyz_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fidyz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fidyz:
                return kwd
        return None

    @fidyz_link.setter
    def fidyz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fidyz."""
        self.fidyz = value.lcid

