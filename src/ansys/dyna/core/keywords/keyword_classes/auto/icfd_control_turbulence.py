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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdControlTurbulence(KeywordBase):
    """DYNA ICFD_CONTROL_TURBULENCE keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_TURBULENCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "tmod",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "submod",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "wlaw",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ks",
                        float,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cs",
                        float,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcids1",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcids2",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ce1",
                        float,
                        0,
                        10,
                        1.44,
                        **kwargs,
                    ),
                    Field(
                        "ce2",
                        float,
                        10,
                        10,
                        1.92,
                        **kwargs,
                    ),
                    Field(
                        "qe",
                        float,
                        20,
                        10,
                        1.3,
                        **kwargs,
                    ),
                    Field(
                        "qk",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "cu",
                        float,
                        40,
                        10,
                        0.09,
                        **kwargs,
                    ),
                    Field(
                        "ccut",
                        float,
                        50,
                        10,
                        -1.0,
                        **kwargs,
                    ),
                ],
                lambda: self.tmod==1,
            ),
            Card(
                [
                    Field(
                        "cs",
                        float,
                        0,
                        10,
                        0.18,
                        **kwargs,
                    ),
                ],
                lambda: self.tmod==2 or self.tmod==3,
            ),
            Card(
                [
                    Field(
                        "cs",
                        float,
                        0,
                        10,
                        0.18,
                        **kwargs,
                    ),
                ],
                lambda: self.tmod==4,
            ),
            Card(
                [
                    Field(
                        "r",
                        float,
                        0,
                        10,
                        1.44,
                        **kwargs,
                    ),
                    Field(
                        "beta-01",
                        float,
                        10,
                        10,
                        0.072,
                        **kwargs,
                    ),
                    Field(
                        "beta-w1",
                        float,
                        20,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "sigma-w1",
                        float,
                        30,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "sigma-k1",
                        float,
                        40,
                        10,
                        0.09,
                        **kwargs,
                    ),
                    Field(
                        "ccut",
                        float,
                        50,
                        10,
                        -1.0,
                        **kwargs,
                    ),
                ],
                lambda: self.tmod==4,
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        0.31,
                        **kwargs,
                    ),
                    Field(
                        "beta-02",
                        float,
                        10,
                        10,
                        0.0828,
                        **kwargs,
                    ),
                    Field(
                        "sigma-w2",
                        float,
                        20,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "sigma-k2",
                        float,
                        30,
                        10,
                        2,
                        **kwargs,
                    ),
                    Field(
                        "cl",
                        float,
                        40,
                        10,
                        0.875,
                        **kwargs,
                    ),
                ],
                lambda: self.tmod==5,
            ),
            Card(
                [
                    Field(
                        "cb1",
                        float,
                        0,
                        10,
                        0.1355,
                        **kwargs,
                    ),
                    Field(
                        "cb2",
                        float,
                        10,
                        10,
                        0.622,
                        **kwargs,
                    ),
                    Field(
                        "sigma-v",
                        float,
                        20,
                        10,
                        0.66,
                        **kwargs,
                    ),
                    Field(
                        "cv1",
                        float,
                        30,
                        10,
                        7.2,
                        **kwargs,
                    ),
                    Field(
                        "cw1",
                        float,
                        40,
                        10,
                        0.3,
                        **kwargs,
                    ),
                    Field(
                        "cw2",
                        float,
                        50,
                        10,
                        2.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

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
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""tmod must be `None` or one of {0,1,2,3,4,5}""")
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
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""submod must be `None` or one of {0,1,2,3}""")
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
        if value not in [0, 1, 2, 4, 5, None]:
            raise Exception("""wlaw must be `None` or one of {0,1,2,4,5}""")
        self._cards[0].set_value("wlaw", value)

    @property
    def ks(self) -> float:
        """Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
        """ # nopep8
        return self._cards[0].get_value("ks")

    @ks.setter
    def ks(self, value: float) -> None:
        self._cards[0].set_value("ks", value)

    @property
    def cs(self) -> float:
        """Get or set the Roughness physical height and Roughness constant. Only used if RANS turbulence model selected.
        """ # nopep8
        return self._cards[0].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        self._cards[0].set_value("cs", value)

    @property
    def lcids1(self) -> int:
        """Get or set the Load curve describing user defined source term in turbulent kinetic energy equation function of time.
        """ # nopep8
        return self._cards[0].get_value("lcids1")

    @lcids1.setter
    def lcids1(self, value: int) -> None:
        self._cards[0].set_value("lcids1", value)

    @property
    def lcids2(self) -> int:
        """Get or set the Load curve describing user defined source term in turbulent dissipation equation function of time.
        """ # nopep8
        return self._cards[0].get_value("lcids2")

    @lcids2.setter
    def lcids2(self, value: int) -> None:
        self._cards[0].set_value("lcids2", value)

    @property
    def ce1(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ce1")

    @ce1.setter
    def ce1(self, value: float) -> None:
        self._cards[1].set_value("ce1", value)

    @property
    def ce2(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ce2")

    @ce2.setter
    def ce2(self, value: float) -> None:
        self._cards[1].set_value("ce2", value)

    @property
    def qe(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("qe")

    @qe.setter
    def qe(self, value: float) -> None:
        self._cards[1].set_value("qe", value)

    @property
    def qk(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("qk")

    @qk.setter
    def qk(self, value: float) -> None:
        self._cards[1].set_value("qk", value)

    @property
    def cu(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("cu")

    @cu.setter
    def cu(self, value: float) -> None:
        self._cards[1].set_value("cu", value)

    @property
    def ccut(self) -> float:
        """Get or set the k-epsilon model constants
        """ # nopep8
        return self._cards[1].get_value("ccut")

    @ccut.setter
    def ccut(self, value: float) -> None:
        self._cards[1].set_value("ccut", value)
        self._cards[4].set_value("ccut", value)

    @property
    def cs(self) -> float:
        """Get or set the Smagorinsky constant.
        """ # nopep8
        return self._cards[2].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        self._cards[2].set_value("cs", value)

    @property
    def cs(self) -> float:
        """Get or set the WALE constant.
        """ # nopep8
        return self._cards[3].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        self._cards[3].set_value("cs", value)

    @property
    def r(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[4].set_value("r", value)

    @property
    def beta_01(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("beta-01")

    @beta_01.setter
    def beta_01(self, value: float) -> None:
        self._cards[4].set_value("beta-01", value)

    @property
    def beta_w1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("beta-w1")

    @beta_w1.setter
    def beta_w1(self, value: float) -> None:
        self._cards[4].set_value("beta-w1", value)

    @property
    def sigma_w1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("sigma-w1")

    @sigma_w1.setter
    def sigma_w1(self, value: float) -> None:
        self._cards[4].set_value("sigma-w1", value)

    @property
    def sigma_k1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[4].get_value("sigma-k1")

    @sigma_k1.setter
    def sigma_k1(self, value: float) -> None:
        self._cards[4].set_value("sigma-k1", value)

    @property
    def alpha1(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[5].set_value("alpha1", value)

    @property
    def beta_02(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("beta-02")

    @beta_02.setter
    def beta_02(self, value: float) -> None:
        self._cards[5].set_value("beta-02", value)

    @property
    def sigma_w2(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("sigma-w2")

    @sigma_w2.setter
    def sigma_w2(self, value: float) -> None:
        self._cards[5].set_value("sigma-w2", value)

    @property
    def sigma_k2(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("sigma-k2")

    @sigma_k2.setter
    def sigma_k2(self, value: float) -> None:
        self._cards[5].set_value("sigma-k2", value)

    @property
    def cl(self) -> float:
        """Get or set the k-omega model constants
        """ # nopep8
        return self._cards[5].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        self._cards[5].set_value("cl", value)

    @property
    def cb1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cb1")

    @cb1.setter
    def cb1(self, value: float) -> None:
        self._cards[6].set_value("cb1", value)

    @property
    def cb2(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cb2")

    @cb2.setter
    def cb2(self, value: float) -> None:
        self._cards[6].set_value("cb2", value)

    @property
    def sigma_v(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("sigma-v")

    @sigma_v.setter
    def sigma_v(self, value: float) -> None:
        self._cards[6].set_value("sigma-v", value)

    @property
    def cv1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cv1")

    @cv1.setter
    def cv1(self, value: float) -> None:
        self._cards[6].set_value("cv1", value)

    @property
    def cw1(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cw1")

    @cw1.setter
    def cw1(self, value: float) -> None:
        self._cards[6].set_value("cw1", value)

    @property
    def cw2(self) -> float:
        """Get or set the Spalart-Allmaras constants
        """ # nopep8
        return self._cards[6].get_value("cw2")

    @cw2.setter
    def cw2(self, value: float) -> None:
        self._cards[6].set_value("cw2", value)

