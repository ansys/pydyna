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

"""Module providing the EmControlCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMCONTROLCOUPLING_CARD0 = (
    FieldSchema("thcpl", int, 0, 10, 0),
    FieldSchema("smcpl", int, 10, 10, 0),
    FieldSchema("thlcid", int, 20, 10, 0),
    FieldSchema("smlcid", int, 30, 10, 0),
    FieldSchema("thcplfl", int, 40, 10, 0),
    FieldSchema("smcplfl", int, 50, 10, 0),
)

_EMCONTROLCOUPLING_CARD1 = (
    FieldSchema("smmod", int, 0, 10, 0),
    FieldSchema("dfx", int, 10, 10, None),
    FieldSchema("dfy", int, 20, 10, None),
    FieldSchema("dfz", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("fsib", int, 50, 10, 0),
    FieldSchema("interp", int, 60, 10, 0),
)

_EMCONTROLCOUPLING_CARD2 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
)

class EmControlCoupling(KeywordBase):
    """DYNA EM_CONTROL_COUPLING keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_COUPLING"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "nid3": LinkType.NODE,
        "thlcid": LinkType.DEFINE_CURVE,
        "smlcid": LinkType.DEFINE_CURVE,
        "thcplfl": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmControlCoupling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLCOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCONTROLCOUPLING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCONTROLCOUPLING_CARD2,
                **kwargs,
            ),
        ]
    @property
    def thcpl(self) -> int:
        """Get or set the Coupling to the thermal solver. When turned on, the EM solver will transfer the Joule heating terms to the solid mechanics thermal solver.
        EQ.0:Coupling on.
        EQ.1:Coupling off.
        EQ.2: Coupling On. It forces all EM heating terms to be expressed at the element level.
        """ # nopep8
        return self._cards[0].get_value("thcpl")

    @thcpl.setter
    def thcpl(self, value: int) -> None:
        """Set the thcpl property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""thcpl must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("thcpl", value)

    @property
    def smcpl(self) -> int:
        """Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer forces to the solid mechanics solver.
        EQ.0: Coupling on.Lorentz force density is transferred.
        EQ.1: Coupling off.
        EQ.2: Coupling on.Magnetic force surface density is transferred.More accurate representation of EM forces in cases involving magnets or nonlinear ferromagnets.See *EM_SOLVER_FEMBEM_MONOLITHIC.
        EQ.3: Coupling on.Magnetic force surface density is transferred to magnets and ferromagnets while Lorentz force density is transferred to regular conductors.
        """ # nopep8
        return self._cards[0].get_value("smcpl")

    @smcpl.setter
    def smcpl(self, value: int) -> None:
        """Set the smcpl property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""smcpl must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("smcpl", value)

    @property
    def thlcid(self) -> int:
        """Get or set the Optional load curve ID. When defined, the heat rate transferred to the thermal solver will be scaled by the value returned by THLCID.

        """ # nopep8
        return self._cards[0].get_value("thlcid")

    @thlcid.setter
    def thlcid(self, value: int) -> None:
        """Set the thlcid property."""
        self._cards[0].set_value("thlcid", value)

    @property
    def smlcid(self) -> int:
        """Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID.

        """ # nopep8
        return self._cards[0].get_value("smlcid")

    @smlcid.setter
    def smlcid(self, value: int) -> None:
        """Set the smlcid property."""
        self._cards[0].set_value("smlcid", value)

    @property
    def thcplfl(self) -> int:
        """Get or set the Coupling to the heat equation when EM quantities are solved on fluid elements. When turned on, the EM solver will transfer the Joule heating terms to the ICFD solver.
        EQ.0: Coupling off.
        EQ.1: Coupling on.
        """ # nopep8
        return self._cards[0].get_value("thcplfl")

    @thcplfl.setter
    def thcplfl(self, value: int) -> None:
        """Set the thcplfl property."""
        if value not in [0, 1, None]:
            raise Exception("""thcplfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("thcplfl", value)

    @property
    def smcplfl(self) -> int:
        """Get or set the Interaction between the solid mechanics solver and the ICFD solver when EM quantities are solved on fluid elements.
        EQ.0: The fluid pressure will be passed to the solid mechanics solver(default).
        EQ.1: The fluid pressure is replaced by the electrostatic pressure.
        EQ.2: The fluid and electrostatic pressure are passed on to the solid mechanics solver.
        EQ.4: The EM solver will send the Lorentz Force as a volumetric source term to the fluid solver.
        """ # nopep8
        return self._cards[0].get_value("smcplfl")

    @smcplfl.setter
    def smcplfl(self, value: int) -> None:
        """Set the smcplfl property."""
        if value not in [0, 1, 2, 4, None]:
            raise Exception("""smcplfl must be `None` or one of {0,1,2,4}.""")
        self._cards[0].set_value("smcplfl", value)

    @property
    def smmod(self) -> int:
        """Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer forces to the solid mechanics solver.
        EQ.0: Off.
        EQ.1: Force calculation at ethe lement level is decided by *DEFINE_FUNCTION.See DFX, DFYand DFZ.
        EQ.2: Force calculation at the element level is decided by the usermat routine.See dyn21em.f and user_getEMForceArray routine
        """ # nopep8
        return self._cards[1].get_value("smmod")

    @smmod.setter
    def smmod(self, value: int) -> None:
        """Set the smmod property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""smmod must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("smmod", value)

    @property
    def dfx(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION IDs for the force three components if SMMOD = 1. Arguments for the *DEFINE_FUNCTIONs are the same as in *EM_EOS_TABULATED2. See Remark Error! Reference source not found. of *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfx")

    @dfx.setter
    def dfx(self, value: int) -> None:
        """Set the dfx property."""
        self._cards[1].set_value("dfx", value)

    @property
    def dfy(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION IDs for the force three components if SMMOD = 1. Arguments for the *DEFINE_FUNCTIONs are the same as in *EM_EOS_TABULATED2. See Remark Error! Reference source not found. of *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfy")

    @dfy.setter
    def dfy(self, value: int) -> None:
        """Set the dfy property."""
        self._cards[1].set_value("dfy", value)

    @property
    def dfz(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION IDs for the force three components if SMMOD = 1. Arguments for the *DEFINE_FUNCTIONs are the same as in *EM_EOS_TABULATED2. See Remark Error! Reference source not found. of *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfz")

    @dfz.setter
    def dfz(self, value: int) -> None:
        """Set the dfz property."""
        self._cards[1].set_value("dfz", value)

    @property
    def fsib(self) -> int:
        """Get or set the Solid mechanics -  ICFD FSI boundaries. When turned on, the EM solver the coupled FSI interface to impose the continuity of the scalar potential between the two domains. See Remark 1.
        EQ.0: Off.
        EQ.1: On.
        """ # nopep8
        return self._cards[1].get_value("fsib")

    @fsib.setter
    def fsib(self, value: int) -> None:
        """Set the fsib property."""
        if value not in [0, 1, None]:
            raise Exception("""fsib must be `None` or one of {0,1}.""")
        self._cards[1].set_value("fsib", value)

    @property
    def interp(self) -> int:
        """Get or set the Solid mechanics -  EM force interpolation. By default, the solid mechanics solver will interpolate at each time the EM force coming from the last two EM steps. See Remark 3.
        EQ.0: Time Interpolation on(Default)
        EQ.1: No Time Interpolation.
        """ # nopep8
        return self._cards[1].get_value("interp")

    @interp.setter
    def interp(self, value: int) -> None:
        """Set the interp property."""
        if value not in [0, 1, None]:
            raise Exception("""interp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("interp", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Three node IDs defining a local coordinate system. See Remark 4
        """ # nopep8
        return self._cards[2].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[2].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Three node IDs defining a local coordinate system. See Remark 4
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[2].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Three node IDs defining a local coordinate system. See Remark 4
        """ # nopep8
        return self._cards[2].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[2].set_value("nid3", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def nid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid3."""
        return self._get_link_by_attr("NODE", "nid", self.nid3, "parts")

    @property
    def thlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for thlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.thlcid:
                return kwd
        return None

    @thlcid_link.setter
    def thlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for thlcid."""
        self.thlcid = value.lcid

    @property
    def smlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for smlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.smlcid:
                return kwd
        return None

    @smlcid_link.setter
    def smlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for smlcid."""
        self.smlcid = value.lcid

    @property
    def thcplfl_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for thcplfl."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.thcplfl:
                return kwd
        return None

    @thcplfl_link.setter
    def thcplfl_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for thcplfl."""
        self.thcplfl = value.lcid

