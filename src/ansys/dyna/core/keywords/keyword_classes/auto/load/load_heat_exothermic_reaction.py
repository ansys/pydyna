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

"""Module providing the LoadHeatExothermicReaction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_LOADHEATEXOTHERMICREACTION_CARD0 = (
    FieldSchema("hsid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, None),
    FieldSchema("nsid", int, 20, 10, None),
    FieldSchema("bt", float, 30, 10, 0.0),
    FieldSchema("dt", float, 40, 10, 1e+16),
    FieldSchema("tmin", float, 50, 10, 0.0),
    FieldSchema("tmax", float, 60, 10, 1e+16),
    FieldSchema("toff", float, 70, 10, 0.0),
)

_LOADHEATEXOTHERMICREACTION_CARD1 = (
    FieldSchema("csei0", float, 0, 10, 0.0),
    FieldSchema("asei", float, 10, 10, 0.0),
    FieldSchema("easei", float, 20, 10, 0.0),
    FieldSchema("msei", float, 30, 10, 0.0),
    FieldSchema("hsei", float, 40, 10, 0.0),
    FieldSchema("wc", float, 50, 10, 0.0),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("ru", float, 70, 10, 8.314),
)

_LOADHEATEXOTHERMICREACTION_CARD2 = (
    FieldSchema("cne0", float, 0, 10, 0.0),
    FieldSchema("ane", float, 10, 10, 0.0),
    FieldSchema("eane", float, 20, 10, 0.0),
    FieldSchema("mne", float, 30, 10, 0.0),
    FieldSchema("hne", float, 40, 10, 0.0),
    FieldSchema("wcne", float, 50, 10, 0.0),
    FieldSchema("tsei0", float, 60, 10, 0.0),
    FieldSchema("tseir", float, 70, 10, 0.0),
)

_LOADHEATEXOTHERMICREACTION_CARD3 = (
    FieldSchema("alpha0", float, 0, 10, 0.0),
    FieldSchema("ape", float, 10, 10, 0.0),
    FieldSchema("eape", float, 20, 10, 0.0),
    FieldSchema("mpep1", float, 30, 10, 0.0),
    FieldSchema("hpe", float, 40, 10, 0.0),
    FieldSchema("wpe", float, 50, 10, 0.0),
    FieldSchema("mpep2", float, 60, 10, 0.0),
)

_LOADHEATEXOTHERMICREACTION_CARD4 = (
    FieldSchema("ce0", float, 0, 10, 0.0),
    FieldSchema("ae", float, 10, 10, 0.0),
    FieldSchema("eae", float, 20, 10, 0.0),
    FieldSchema("me", float, 30, 10, 0.0),
    FieldSchema("he", float, 40, 10, 0.0),
    FieldSchema("we", float, 50, 10, 0.0),
)

class LoadHeatExothermicReaction(KeywordBase):
    """DYNA LOAD_HEAT_EXOTHERMIC_REACTION keyword"""

    keyword = "LOAD"
    subkeyword = "HEAT_EXOTHERMIC_REACTION"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadHeatExothermicReaction class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADHEATEXOTHERMICREACTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADHEATEXOTHERMICREACTION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADHEATEXOTHERMICREACTION_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADHEATEXOTHERMICREACTION_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADHEATEXOTHERMICREACTION_CARD4,
                **kwargs,
            ),        ]
    @property
    def hsid(self) -> typing.Optional[int]:
        """Get or set the Heat Source ID.
        """ # nopep8
        return self._cards[0].get_value("hsid")

    @hsid.setter
    def hsid(self, value: int) -> None:
        """Set the hsid property."""
        self._cards[0].set_value("hsid", value)

    @property
    def stype(self) -> typing.Optional[int]:
        """Get or set the Heat Source model type:
        EQ.0 or EQ 1 : heat source defined by NREL's 4 - Equation model.See Remark 1.
        EQ.2 : heat source defined by 1 - Equation model.See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        self._cards[0].set_value("stype", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node Set ID.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time for application of heat source term.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for application of heat source term.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def tmin(self) -> float:
        """Get or set the Minimum temperature before heat source activation is triggered.
        """ # nopep8
        return self._cards[0].get_value("tmin")

    @tmin.setter
    def tmin(self, value: float) -> None:
        """Set the tmin property."""
        self._cards[0].set_value("tmin", value)

    @property
    def tmax(self) -> float:
        """Get or set the Maximum temperature before heat source activation is triggered.
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[0].set_value("tmax", value)

    @property
    def toff(self) -> float:
        """Get or set the Option offset for temperature used in heat source calculation.
        """ # nopep8
        return self._cards[0].get_value("toff")

    @toff.setter
    def toff(self, value: float) -> None:
        """Set the toff property."""
        self._cards[0].set_value("toff", value)

    @property
    def csei0(self) -> float:
        """Get or set the Initial concentration of Solid Electrolyte Interphase (SEI)
        """ # nopep8
        return self._cards[1].get_value("csei0")

    @csei0.setter
    def csei0(self, value: float) -> None:
        """Set the csei0 property."""
        self._cards[1].set_value("csei0", value)

    @property
    def asei(self) -> float:
        """Get or set the SEI-decomposition frequency factor.
        """ # nopep8
        return self._cards[1].get_value("asei")

    @asei.setter
    def asei(self, value: float) -> None:
        """Set the asei property."""
        self._cards[1].set_value("asei", value)

    @property
    def easei(self) -> float:
        """Get or set the SEI-decomposition activation energy.
        """ # nopep8
        return self._cards[1].get_value("easei")

    @easei.setter
    def easei(self, value: float) -> None:
        """Set the easei property."""
        self._cards[1].set_value("easei", value)

    @property
    def msei(self) -> float:
        """Get or set the Reaction order for CSEI.
        """ # nopep8
        return self._cards[1].get_value("msei")

    @msei.setter
    def msei(self, value: float) -> None:
        """Set the msei property."""
        self._cards[1].set_value("msei", value)

    @property
    def hsei(self) -> float:
        """Get or set the SEI-decomposition heat release.
        """ # nopep8
        return self._cards[1].get_value("hsei")

    @hsei.setter
    def hsei(self, value: float) -> None:
        """Set the hsei property."""
        self._cards[1].set_value("hsei", value)

    @property
    def wc(self) -> float:
        """Get or set the Specific carbon content in jellyroll.
        """ # nopep8
        return self._cards[1].get_value("wc")

    @wc.setter
    def wc(self, value: float) -> None:
        """Set the wc property."""
        self._cards[1].set_value("wc", value)

    @property
    def ru(self) -> float:
        """Get or set the Reaction constant.
        """ # nopep8
        return self._cards[1].get_value("ru")

    @ru.setter
    def ru(self, value: float) -> None:
        """Set the ru property."""
        self._cards[1].set_value("ru", value)

    @property
    def cne0(self) -> float:
        """Get or set the Initial concentration value of NE
        """ # nopep8
        return self._cards[2].get_value("cne0")

    @cne0.setter
    def cne0(self, value: float) -> None:
        """Set the cne0 property."""
        self._cards[2].set_value("cne0", value)

    @property
    def ane(self) -> float:
        """Get or set the Negative Solvent frequency factor.
        """ # nopep8
        return self._cards[2].get_value("ane")

    @ane.setter
    def ane(self, value: float) -> None:
        """Set the ane property."""
        self._cards[2].set_value("ane", value)

    @property
    def eane(self) -> float:
        """Get or set the Negative Solvent Activation Energy.
        """ # nopep8
        return self._cards[2].get_value("eane")

    @eane.setter
    def eane(self, value: float) -> None:
        """Set the eane property."""
        self._cards[2].set_value("eane", value)

    @property
    def mne(self) -> float:
        """Get or set the Reaction order for CNE.
        """ # nopep8
        return self._cards[2].get_value("mne")

    @mne.setter
    def mne(self, value: float) -> None:
        """Set the mne property."""
        self._cards[2].set_value("mne", value)

    @property
    def hne(self) -> float:
        """Get or set the Negative Solvent Heat release.
        """ # nopep8
        return self._cards[2].get_value("hne")

    @hne.setter
    def hne(self, value: float) -> None:
        """Set the hne property."""
        self._cards[2].set_value("hne", value)

    @property
    def wcne(self) -> float:
        """Get or set the Specific carbon content in jellyroll.
        """ # nopep8
        return self._cards[2].get_value("wcne")

    @wcne.setter
    def wcne(self, value: float) -> None:
        """Set the wcne property."""
        self._cards[2].set_value("wcne", value)

    @property
    def tsei0(self) -> float:
        """Get or set the Initial value of TSEI.
        """ # nopep8
        return self._cards[2].get_value("tsei0")

    @tsei0.setter
    def tsei0(self, value: float) -> None:
        """Set the tsei0 property."""
        self._cards[2].set_value("tsei0", value)

    @property
    def tseir(self) -> float:
        """Get or set the Reference TSEI value.
        """ # nopep8
        return self._cards[2].get_value("tseir")

    @tseir.setter
    def tseir(self, value: float) -> None:
        """Set the tseir property."""
        self._cards[2].set_value("tseir", value)

    @property
    def alpha0(self) -> float:
        """Get or set the Initial value of alfa.
        """ # nopep8
        return self._cards[3].get_value("alpha0")

    @alpha0.setter
    def alpha0(self, value: float) -> None:
        """Set the alpha0 property."""
        self._cards[3].set_value("alpha0", value)

    @property
    def ape(self) -> float:
        """Get or set the Positive solvent frequency factor.
        """ # nopep8
        return self._cards[3].get_value("ape")

    @ape.setter
    def ape(self, value: float) -> None:
        """Set the ape property."""
        self._cards[3].set_value("ape", value)

    @property
    def eape(self) -> float:
        """Get or set the Positive solvent activation energy.
        """ # nopep8
        return self._cards[3].get_value("eape")

    @eape.setter
    def eape(self, value: float) -> None:
        """Set the eape property."""
        self._cards[3].set_value("eape", value)

    @property
    def mpep1(self) -> float:
        """Get or set the Reaction order for alfa.
        """ # nopep8
        return self._cards[3].get_value("mpep1")

    @mpep1.setter
    def mpep1(self, value: float) -> None:
        """Set the mpep1 property."""
        self._cards[3].set_value("mpep1", value)

    @property
    def hpe(self) -> float:
        """Get or set the Positive solvent heat release.
        """ # nopep8
        return self._cards[3].get_value("hpe")

    @hpe.setter
    def hpe(self, value: float) -> None:
        """Set the hpe property."""
        self._cards[3].set_value("hpe", value)

    @property
    def wpe(self) -> float:
        """Get or set the Specific positive active content.
        """ # nopep8
        return self._cards[3].get_value("wpe")

    @wpe.setter
    def wpe(self, value: float) -> None:
        """Set the wpe property."""
        self._cards[3].set_value("wpe", value)

    @property
    def mpep2(self) -> float:
        """Get or set the Reaction order for (1-ALFA).
        """ # nopep8
        return self._cards[3].get_value("mpep2")

    @mpep2.setter
    def mpep2(self, value: float) -> None:
        """Set the mpep2 property."""
        self._cards[3].set_value("mpep2", value)

    @property
    def ce0(self) -> float:
        """Get or set the Initial concentration value of CE.
        """ # nopep8
        return self._cards[4].get_value("ce0")

    @ce0.setter
    def ce0(self, value: float) -> None:
        """Set the ce0 property."""
        self._cards[4].set_value("ce0", value)

    @property
    def ae(self) -> float:
        """Get or set the Electrolyte decomposition frequency factor.
        """ # nopep8
        return self._cards[4].get_value("ae")

    @ae.setter
    def ae(self, value: float) -> None:
        """Set the ae property."""
        self._cards[4].set_value("ae", value)

    @property
    def eae(self) -> float:
        """Get or set the Electrolyte activation energy.
        """ # nopep8
        return self._cards[4].get_value("eae")

    @eae.setter
    def eae(self, value: float) -> None:
        """Set the eae property."""
        self._cards[4].set_value("eae", value)

    @property
    def me(self) -> float:
        """Get or set the Reaction order for CE.
        """ # nopep8
        return self._cards[4].get_value("me")

    @me.setter
    def me(self, value: float) -> None:
        """Set the me property."""
        self._cards[4].set_value("me", value)

    @property
    def he(self) -> float:
        """Get or set the Electrolyte decomposition heat release.
        """ # nopep8
        return self._cards[4].get_value("he")

    @he.setter
    def he(self, value: float) -> None:
        """Set the he property."""
        self._cards[4].set_value("he", value)

    @property
    def we(self) -> float:
        """Get or set the Specific Electrolyte content.
        """ # nopep8
        return self._cards[4].get_value("we")

    @we.setter
    def we(self, value: float) -> None:
        """Set the we property."""
        self._cards[4].set_value("we", value)

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

