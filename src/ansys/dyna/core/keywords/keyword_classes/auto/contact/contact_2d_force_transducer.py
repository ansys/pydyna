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

"""Module providing the Contact2DForceTransducer class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTACT2DFORCETRANSDUCER_CARD0 = (
    FieldSchema("surfa", int, 0, 10, None),
    FieldSchema("surfb", int, 10, 10, None),
    FieldSchema("sfact", float, 20, 10, 1.0),
    FieldSchema("freq", int, 30, 10, 50),
    FieldSchema("fs", float, 40, 10, 0.0),
    FieldSchema("fd", float, 50, 10, 0.0),
    FieldSchema("dc", float, 60, 10, 0.0),
    FieldSchema("unused", int, 70, 10, None),
)

_CONTACT2DFORCETRANSDUCER_CARD1 = (
    FieldSchema("tbirth", float, 0, 10, 0.0),
    FieldSchema("tdeath", float, 10, 10, 1e+20),
    FieldSchema("soa", float, 20, 10, 1.0),
    FieldSchema("sob", float, 30, 10, 1.0),
    FieldSchema("nda", int, 40, 10, 0),
    FieldSchema("ndb", int, 50, 10, 0),
    FieldSchema("cof", int, 60, 10, 0),
    FieldSchema("init", int, 70, 10, 0),
)

class Contact2DForceTransducer(KeywordBase):
    """DYNA CONTACT_2D_FORCE_TRANSDUCER keyword"""

    keyword = "CONTACT"
    subkeyword = "2D_FORCE_TRANSDUCER"
    _link_fields = {
        "surfa": LinkType.SET_PART,
        "surfb": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the Contact2DForceTransducer class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACT2DFORCETRANSDUCER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DFORCETRANSDUCER_CARD1,
                **kwargs,
            ),        ]
    @property
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Set ID for SURFA.  If SURFA > 0, a part set is assumed; see *SET_‌PART.  If SURFA < 0, a node set with ID equal to the absolute value of SURFA is assumed; see *SET_‌NODE. For nonsymmetric contact, this surface is the tracked surface.
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        """Set the surfa property."""
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Set ID to define the SURFB surface.  If SURFB > 0, a part set is assumed; see *SET_‌PART.  If SURFB < 0, a node set with ID equal to the absolute value of SURFB is assumed; see *SET_‌NODE.  Do not define for single surface contact. For nonsymmetric contact, this surface is the reference surface.
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        """Set the surfb property."""
        self._cards[0].set_value("surfb", value)

    @property
    def sfact(self) -> float:
        """Get or set the Scale factor for the penalty force stiffness (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sfact")

    @sfact.setter
    def sfact(self, value: float) -> None:
        """Set the sfact property."""
        self._cards[0].set_value("sfact", value)

    @property
    def freq(self) -> int:
        """Get or set the Search frequency. The number of time steps between bucket sorts (default=50).
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: int) -> None:
        """Set the freq property."""
        self._cards[0].set_value("freq", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[0].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[0].set_value("dc", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time for contact (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[1].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for contact (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[1].set_value("tdeath", value)

    @property
    def soa(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFA surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("soa")

    @soa.setter
    def soa(self, value: float) -> None:
        """Set the soa property."""
        self._cards[1].set_value("soa", value)

    @property
    def sob(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFB surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sob")

    @sob.setter
    def sob(self, value: float) -> None:
        """Set the sob property."""
        self._cards[1].set_value("sob", value)

    @property
    def nda(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFA surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("nda")

    @nda.setter
    def nda(self, value: int) -> None:
        """Set the nda property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""nda must be `None` or one of {0,1,-1}.""")
        self._cards[1].set_value("nda", value)

    @property
    def ndb(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFB surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("ndb")

    @ndb.setter
    def ndb(self, value: int) -> None:
        """Set the ndb property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""ndb must be `None` or one of {0,1,-1}.""")
        self._cards[1].set_value("ndb", value)

    @property
    def cof(self) -> int:
        """Get or set the COF: Closing/opening flag for implicit analysis.
        EQ.0: Recommended for most problems where gaps are only closing (default),
        EQ.1: Recommended when gaps are opening to avoid sticking.
        """ # nopep8
        return self._cards[1].get_value("cof")

    @cof.setter
    def cof(self, value: int) -> None:
        """Set the cof property."""
        if value not in [0, 1, None]:
            raise Exception("""cof must be `None` or one of {0,1}.""")
        self._cards[1].set_value("cof", value)

    @property
    def init(self) -> int:
        """Get or set the Special processing during initialization.
        EQ.0: No special processing,
        EQ.1: Forming option.
        """ # nopep8
        return self._cards[1].get_value("init")

    @init.setter
    def init(self, value: int) -> None:
        """Set the init property."""
        if value not in [0, 1, None]:
            raise Exception("""init must be `None` or one of {0,1}.""")
        self._cards[1].set_value("init", value)

    @property
    def surfa_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for surfa."""
        return self._get_set_link("PART", self.surfa)

    @surfa_link.setter
    def surfa_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for surfa."""
        self.surfa = value.sid

    @property
    def surfb_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for surfb."""
        return self._get_set_link("PART", self.surfb)

    @surfb_link.setter
    def surfb_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for surfb."""
        self.surfb = value.sid

