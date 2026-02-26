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

"""Module providing the IcfdControlTurbulence class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDCONTROLTURBULENCE_CARD0 = (
    FieldSchema("tmod", int, 0, 10, 0),
    FieldSchema("submod", int, 10, 10, 0),
    FieldSchema("wlaw", int, 20, 10, 0),
    FieldSchema("ks", float, 30, 10, 0.0),
    FieldSchema("cs", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("lcids1", int, 60, 10, 0),
    FieldSchema("lcids2", int, 70, 10, 0),
)

_ICFDCONTROLTURBULENCE_CARD1 = (
    FieldSchema("ce1", float, 0, 10, 1.44),
    FieldSchema("ce2", float, 10, 10, 1.92),
    FieldSchema("qe", float, 20, 10, 1.3),
    FieldSchema("qk", float, 30, 10, 1.0),
    FieldSchema("cu", float, 40, 10, 0.09),
    FieldSchema("ccut", float, 50, 10, -1.0),
)

_ICFDCONTROLTURBULENCE_CARD2 = (
    FieldSchema("cs", float, 0, 10, 0.18),
)

_ICFDCONTROLTURBULENCE_CARD3 = (
    FieldSchema("cs", float, 0, 10, 0.18),
)

_ICFDCONTROLTURBULENCE_CARD4 = (
    FieldSchema("r", float, 0, 10, 1.44),
    FieldSchema("beta_01", float, 10, 10, 0.072, "beta-01"),
    FieldSchema("beta_w1", float, 20, 10, 2.0, "beta-w1"),
    FieldSchema("sigma_w1", float, 30, 10, 2.0, "sigma-w1"),
    FieldSchema("sigma_k1", float, 40, 10, 0.09, "sigma-k1"),
    FieldSchema("ccut", float, 50, 10, -1.0),
)

_ICFDCONTROLTURBULENCE_CARD5 = (
    FieldSchema("alpha1", float, 0, 10, 0.31),
    FieldSchema("beta_02", float, 10, 10, 0.0828, "beta-02"),
    FieldSchema("sigma_w2", float, 20, 10, 2.0, "sigma-w2"),
    FieldSchema("sigma_k2", float, 30, 10, 2.0, "sigma-k2"),
    FieldSchema("cl", float, 40, 10, 0.875),
)

_ICFDCONTROLTURBULENCE_CARD6 = (
    FieldSchema("cb1", float, 0, 10, 0.1355),
    FieldSchema("cb2", float, 10, 10, 0.622),
    FieldSchema("sigma_v", float, 20, 10, 0.66, "sigma-v"),
    FieldSchema("cv1", float, 30, 10, 7.2),
    FieldSchema("cw1", float, 40, 10, 0.3),
    FieldSchema("cw2", float, 50, 10, 2.0),
)

class IcfdControlTurbulence(KeywordBase):
    """DYNA ICFD_CONTROL_TURBULENCE keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TURBULENCE"
    _link_fields = {
        "lcids1": LinkType.DEFINE_CURVE,
        "lcids2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdControlTurbulence class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD1,
                active_func=lambda: self.tmod==1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD2,
                active_func=lambda: self.tmod==2 or self.tmod==3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD3,
                active_func=lambda: self.tmod==4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD4,
                active_func=lambda: self.tmod==4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD5,
                active_func=lambda: self.tmod==5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLTURBULENCE_CARD6,
                **kwargs,
            ),        ]
    @property
    def tmod(self) -> int:
        """Get or set the Indicates what turbulence model will be used.
        EQ.0: Turbulence model based on a variational multiscale approach is used by default.
        EQ.1: RANS k-epsilon approach.
        EQ.2: LES Smagorinsky sub-grid scale model.
        EQ.3: LES Wall adapting local eddy-viscosity (WALE) model.
        EQ.4: RANS k-omega approach.
        EQ.5: RANS Spalart Allmaras approach.
        """ # nopep8
        return self._cards[0].get_value("tmod")

    @tmod.setter
    def tmod(self, value: int) -> None:
        """Set the tmod property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""tmod must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("tmod", value)

    @property
    def submod(self) -> int:
        """Get or set the Turbulence sub-model.
        If TMOD = 1:
        EQ.1:Standard model.
        EQ.2:Realizable model.
        If TMOD = 4:
        EQ.1:Standard Wilcox 98 model.
        EQ.2:Standard Wilcox 06 model.
        EQ.3:SST Menter 2003.
        """ # nopep8
        return self._cards[0].get_value("submod")

    @submod.setter
    def submod(self, value: int) -> None:
        """Set the submod property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""submod must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("submod", value)

    @property
    def wlaw(self) -> int:
        """Get or set the Law of the wall ID is RANS turbulence model selected:
        EQ.1: Standard classic law of the wall.
        EQ.2: Standard Launder and Spalding law of the wall.
        EQ.4: Non equilibrium Launder and Spalding law of the wall.
        EQ.5: Automatic classic law of the wall.
        """ # nopep8
        return self._cards[0].get_value("wlaw")

    @wlaw.setter
    def wlaw(self, value: int) -> None:
        """Set the wlaw property."""
        if value not in [0, 1, 2, 4, 5, None]:
            raise Exception("""wlaw must be `None` or one of {0,1,2,4,5}.""")
        self._cards[0].set_value("wlaw", value)

    @property
    def ks(self) -> float:
        """Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
        """ # nopep8
        return self._cards[0].get_value("ks")

    @ks.setter
    def ks(self, value: float) -> None:
        """Set the ks property."""
        self._cards[0].set_value("ks", value)

    @property
    def cs(self) -> float:
        """Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
        """ # nopep8
        return self._cards[0].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[0].set_value("cs", value)

    @property
    def lcids1(self) -> int:
        """Get or set the Load curve describing user defined source term in turbulent kinetic energy equation function of time.
        """ # nopep8
        return self._cards[0].get_value("lcids1")

    @lcids1.setter
    def lcids1(self, value: int) -> None:
        """Set the lcids1 property."""
        self._cards[0].set_value("lcids1", value)

    @property
    def lcids2(self) -> int:
        """Get or set the Load curve describing user defined source term in turbulent dissipation equation function of time.
        """ # nopep8
        return self._cards[0].get_value("lcids2")

    @lcids2.setter
    def lcids2(self, value: int) -> None:
        """Set the lcids2 property."""
        self._cards[0].set_value("lcids2", value)

    @property
    def ce1(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ce1")

    @ce1.setter
    def ce1(self, value: float) -> None:
        """Set the ce1 property."""
        self._cards[1].set_value("ce1", value)

    @property
    def ce2(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ce2")

    @ce2.setter
    def ce2(self, value: float) -> None:
        """Set the ce2 property."""
        self._cards[1].set_value("ce2", value)

    @property
    def qe(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("qe")

    @qe.setter
    def qe(self, value: float) -> None:
        """Set the qe property."""
        self._cards[1].set_value("qe", value)

    @property
    def qk(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("qk")

    @qk.setter
    def qk(self, value: float) -> None:
        """Set the qk property."""
        self._cards[1].set_value("qk", value)

    @property
    def cu(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("cu")

    @cu.setter
    def cu(self, value: float) -> None:
        """Set the cu property."""
        self._cards[1].set_value("cu", value)

    @property
    def ccut(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ccut")

    @ccut.setter
    def ccut(self, value: float) -> None:
        """Set the ccut property."""
        self._cards[1].set_value("ccut", value)
        self._cards[4].set_value("ccut", value)

    @property
    def cs(self) -> float:
        """Get or set the Smagorinsky constant.
        """ # nopep8
        return self._cards[2].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[2].set_value("cs", value)

    @property
    def cs(self) -> float:
        """Get or set the WALE constant.
        """ # nopep8
        return self._cards[3].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[3].set_value("cs", value)

    @property
    def r(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[4].set_value("r", value)

    @property
    def beta_01(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("beta_01")

    @beta_01.setter
    def beta_01(self, value: float) -> None:
        """Set the beta_01 property."""
        self._cards[4].set_value("beta_01", value)

    @property
    def beta_w1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("beta_w1")

    @beta_w1.setter
    def beta_w1(self, value: float) -> None:
        """Set the beta_w1 property."""
        self._cards[4].set_value("beta_w1", value)

    @property
    def sigma_w1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("sigma_w1")

    @sigma_w1.setter
    def sigma_w1(self, value: float) -> None:
        """Set the sigma_w1 property."""
        self._cards[4].set_value("sigma_w1", value)

    @property
    def sigma_k1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("sigma_k1")

    @sigma_k1.setter
    def sigma_k1(self, value: float) -> None:
        """Set the sigma_k1 property."""
        self._cards[4].set_value("sigma_k1", value)

    @property
    def alpha1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[5].set_value("alpha1", value)

    @property
    def beta_02(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("beta_02")

    @beta_02.setter
    def beta_02(self, value: float) -> None:
        """Set the beta_02 property."""
        self._cards[5].set_value("beta_02", value)

    @property
    def sigma_w2(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("sigma_w2")

    @sigma_w2.setter
    def sigma_w2(self, value: float) -> None:
        """Set the sigma_w2 property."""
        self._cards[5].set_value("sigma_w2", value)

    @property
    def sigma_k2(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("sigma_k2")

    @sigma_k2.setter
    def sigma_k2(self, value: float) -> None:
        """Set the sigma_k2 property."""
        self._cards[5].set_value("sigma_k2", value)

    @property
    def cl(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        """Set the cl property."""
        self._cards[5].set_value("cl", value)

    @property
    def cb1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cb1")

    @cb1.setter
    def cb1(self, value: float) -> None:
        """Set the cb1 property."""
        self._cards[6].set_value("cb1", value)

    @property
    def cb2(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cb2")

    @cb2.setter
    def cb2(self, value: float) -> None:
        """Set the cb2 property."""
        self._cards[6].set_value("cb2", value)

    @property
    def sigma_v(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("sigma_v")

    @sigma_v.setter
    def sigma_v(self, value: float) -> None:
        """Set the sigma_v property."""
        self._cards[6].set_value("sigma_v", value)

    @property
    def cv1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cv1")

    @cv1.setter
    def cv1(self, value: float) -> None:
        """Set the cv1 property."""
        self._cards[6].set_value("cv1", value)

    @property
    def cw1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cw1")

    @cw1.setter
    def cw1(self, value: float) -> None:
        """Set the cw1 property."""
        self._cards[6].set_value("cw1", value)

    @property
    def cw2(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cw2")

    @cw2.setter
    def cw2(self, value: float) -> None:
        """Set the cw2 property."""
        self._cards[6].set_value("cw2", value)

    @property
    def lcids1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcids1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcids1:
                return kwd
        return None

    @lcids1_link.setter
    def lcids1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcids1."""
        self.lcids1 = value.lcid

    @property
    def lcids2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcids2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcids2:
                return kwd
        return None

    @lcids2_link.setter
    def lcids2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcids2."""
        self.lcids2 = value.lcid

