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

"""Module providing the AirbagLoadCurveId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGLOADCURVEID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_AIRBAGLOADCURVEID_CARD1 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("sidtyp", int, 10, 10, 0),
    FieldSchema("rbid", int, 20, 10, 0),
    FieldSchema("vsca", float, 30, 10, 1.0),
    FieldSchema("psca", float, 40, 10, 1.0),
    FieldSchema("vini", float, 50, 10, 0.0),
    FieldSchema("mwd", float, 60, 10, 0.0),
    FieldSchema("spsf", float, 70, 10, 0.0),
)

_AIRBAGLOADCURVEID_CARD2 = (
    FieldSchema("stime", float, 0, 10, 0.0),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("ro", float, 20, 10, None),
    FieldSchema("pe", float, 30, 10, None),
    FieldSchema("p0", float, 40, 10, None),
    FieldSchema("t", float, 50, 10, None),
    FieldSchema("t0", float, 60, 10, None),
)

class AirbagLoadCurveId(KeywordBase):
    """DYNA AIRBAG_LOAD_CURVE_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "LOAD_CURVE_ID"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagLoadCurveId class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGLOADCURVEID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGLOADCURVEID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _AIRBAGLOADCURVEID_CARD2,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[1].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[1].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        """Set the sidtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sidtyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[1].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        """Set the rbid property."""
        self._cards[1].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        """Set the vsca property."""
        self._cards[1].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        """Set the psca property."""
        self._cards[1].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        """Set the vini property."""
        self._cards[1].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        """Set the mwd property."""
        self._cards[1].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        """Set the spsf property."""
        self._cards[1].set_value("spsf", value)

    @property
    def stime(self) -> float:
        """Get or set the Time at which pressure is applied. The load curve is offset by this amount (default=0.0).
        """ # nopep8
        return self._cards[2].get_value("stime")

    @stime.setter
    def stime(self, value: float) -> None:
        """Set the stime property."""
        self._cards[2].set_value("stime", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining pressure versus time, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[2].set_value("lcid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Initial density of gas (ignored if LCID > 0).
        """ # nopep8
        return self._cards[2].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[2].set_value("ro", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure (ignored if LCID > 0).
        """ # nopep8
        return self._cards[2].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        """Set the pe property."""
        self._cards[2].set_value("pe", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Initial gauge pressure (ignored if LCID > 0).
        """ # nopep8
        return self._cards[2].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[2].set_value("p0", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Gas temperature (ignored if LCID > 0).
        """ # nopep8
        return self._cards[2].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[2].set_value("t", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Absolute zero on temperature scale (ignored if LCID > 0).
        """ # nopep8
        return self._cards[2].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[2].set_value("t0", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
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

