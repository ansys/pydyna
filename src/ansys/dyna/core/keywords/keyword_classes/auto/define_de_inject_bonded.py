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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineDeInjectBonded(KeywordBase):
    """DYNA DEFINE_DE_INJECT_BONDED keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECT_BONDED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "sid",
                        int,
                        10,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "xc",
                        float,
                        20,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        30,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        40,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                    Field(
                        "xl",
                        float,
                        50,
                        10,
                        kwargs.get("xl", 0.0)
                    ),
                    Field(
                        "yl",
                        float,
                        60,
                        10,
                        kwargs.get("yl", 0.0)
                    ),
                    Field(
                        "cid",
                        int,
                        70,
                        10,
                        kwargs.get("cid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rmass",
                        float,
                        0,
                        10,
                        kwargs.get("rmass", 0.0)
                    ),
                    Field(
                        "vx",
                        float,
                        10,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "xy",
                        float,
                        20,
                        10,
                        kwargs.get("xy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        30,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "tbeg",
                        float,
                        40,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        50,
                        10,
                        kwargs.get("tend", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pbn",
                        float,
                        0,
                        10,
                        kwargs.get("pbn")
                    ),
                    Field(
                        "pbs",
                        float,
                        10,
                        10,
                        kwargs.get("pbs")
                    ),
                    Field(
                        "pbn_s",
                        float,
                        20,
                        10,
                        kwargs.get("pbn_s", 0.0)
                    ),
                    Field(
                        "pbs_s",
                        float,
                        30,
                        10,
                        kwargs.get("pbs_s", 0.0)
                    ),
                    Field(
                        "sfa",
                        float,
                        40,
                        10,
                        kwargs.get("sfa", 0.0)
                    ),
                    Field(
                        "alpha",
                        float,
                        50,
                        10,
                        kwargs.get("alpha", 0.0)
                    ),
                    Field(
                        "maxgap",
                        float,
                        60,
                        10,
                        kwargs.get("maxgap", 1.0E-4)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nshape",
                        int,
                        0,
                        10,
                        kwargs.get("nshape", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ishape",
                        int,
                        0,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        10,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        20,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        30,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        40,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        50,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        60,
                        10,
                        kwargs.get("ishape")
                    ),
                    Field(
                        "ishape",
                        int,
                        70,
                        10,
                        kwargs.get("ishape")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeInjectBonded.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID.  Nodes and DES properties are generated automatically during input phase based on the user input and assigned to this SID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def xc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[0].set_value("zc", value)

    @property
    def xl(self) -> float:
        """Get or set the For rectangular planes XL specifies the planar length along the x-axis in the coordinate system specified by CID.  For elliptical planes XL specifies the length of the major axis
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the For rectangular planes YL specifies the planar length along the y-axis in the coordinate system specified by CID.  For elliptical planes YL specifies the length of the minor axis
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        self._cards[0].set_value("yl", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Optional local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def rmass(self) -> float:
        """Get or set the Mass flow rate
        GE.0.0:	Constant mass flow rate
        LT.0.0 : RMASS is a curve ID defining the mass flow rate as a function of time.
        """ # nopep8
        return self._cards[1].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        self._cards[1].set_value("rmass", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[1].set_value("vx", value)

    @property
    def xy(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("xy")

    @xy.setter
    def xy(self, value: float) -> None:
        self._cards[1].set_value("xy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[1].set_value("vz", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time; time for injection to begin
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Death time; time for injection to end
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def pbn(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond modulus [Pa].
        """ # nopep8
        return self._cards[2].get_value("pbn")

    @pbn.setter
    def pbn(self, value: float) -> None:
        self._cards[2].set_value("pbn", value)

    @property
    def pbs(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond stiffness ratio.  Shear stiffness/normal stiffness
        """ # nopep8
        return self._cards[2].get_value("pbs")

    @pbs.setter
    def pbs(self, value: float) -> None:
        self._cards[2].set_value("pbs", value)

    @property
    def pbn_s(self) -> float:
        """Get or set the Parallel-bond maximum normal stress.  A zero value defines an infinite maximum normal stress
        """ # nopep8
        return self._cards[2].get_value("pbn_s")

    @pbn_s.setter
    def pbn_s(self, value: float) -> None:
        self._cards[2].set_value("pbn_s", value)

    @property
    def pbs_s(self) -> float:
        """Get or set the Parallel-bond maximum shear stress.  A zero value defines an infinite maximum shear stress
        """ # nopep8
        return self._cards[2].get_value("pbs_s")

    @pbs_s.setter
    def pbs_s(self, value: float) -> None:
        self._cards[2].set_value("pbs_s", value)

    @property
    def sfa(self) -> float:
        """Get or set the Bond radius multiplier
        """ # nopep8
        return self._cards[2].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        self._cards[2].set_value("sfa", value)

    @property
    def alpha(self) -> float:
        """Get or set the Numerical damping
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[2].set_value("alpha", value)

    @property
    def maxgap(self) -> float:
        """Get or set the Maximum gap between two bonded spheres
        GT.0.0:	When MAXGAP is positive, the maximum allowed gap is determined on a bond - by - bond basis as a function of the radii of the two involved spheres.The maximum gap is determined by multiplying the minimum of the two radii by the value of MAXGAP.
        LT.0.0 : Absolute value is used as the maximum gap
        """ # nopep8
        return self._cards[2].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        self._cards[2].set_value("maxgap", value)

    @property
    def nshape(self) -> int:
        """Get or set the Number of shape patterns
        """ # nopep8
        return self._cards[3].get_value("nshape")

    @nshape.setter
    def nshape(self, value: int) -> None:
        self._cards[3].set_value("nshape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        self._cards[4].set_value("ishape", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

