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

class Mat249Crash(KeywordBase):
    """DYNA MAT_249_CRASH keyword"""

    keyword = "MAT"
    subkeyword = "249_CRASH"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "em",
                        float,
                        20,
                        10,
                        kwargs.get("em")
                    ),
                    Field(
                        "prm",
                        float,
                        30,
                        10,
                        kwargs.get("prm")
                    ),
                    Field(
                        "lcsigy",
                        int,
                        40,
                        10,
                        kwargs.get("lcsigy")
                    ),
                    Field(
                        "beta",
                        float,
                        50,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "pfl",
                        float,
                        60,
                        10,
                        kwargs.get("pfl")
                    ),
                    Field(
                        "visc",
                        float,
                        70,
                        10,
                        kwargs.get("visc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nfib",
                        int,
                        0,
                        10,
                        kwargs.get("nfib")
                    ),
                    Field(
                        "aopt",
                        float,
                        10,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "mangl",
                        float,
                        60,
                        10,
                        kwargs.get("mangl")
                    ),
                    Field(
                        "thick",
                        float,
                        70,
                        10,
                        kwargs.get("thick")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vg1",
                        float,
                        0,
                        10,
                        kwargs.get("vg1")
                    ),
                    Field(
                        "vb1",
                        float,
                        10,
                        10,
                        kwargs.get("vb1")
                    ),
                    Field(
                        "vg2",
                        float,
                        20,
                        10,
                        kwargs.get("vg2")
                    ),
                    Field(
                        "vb2",
                        float,
                        30,
                        10,
                        kwargs.get("vb2")
                    ),
                    Field(
                        "vg3",
                        float,
                        40,
                        10,
                        kwargs.get("vg3")
                    ),
                    Field(
                        "vb3",
                        float,
                        50,
                        10,
                        kwargs.get("vb3")
                    ),
                    Field(
                        "vg4",
                        float,
                        40,
                        10,
                        kwargs.get("vg4")
                    ),
                    Field(
                        "vb4",
                        float,
                        50,
                        10,
                        kwargs.get("vb4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf1",
                        int,
                        0,
                        10,
                        kwargs.get("idf1")
                    ),
                    Field(
                        "alph1",
                        int,
                        10,
                        10,
                        kwargs.get("alph1")
                    ),
                    Field(
                        "ef1",
                        int,
                        20,
                        10,
                        kwargs.get("ef1")
                    ),
                    Field(
                        "lcef1",
                        int,
                        30,
                        10,
                        kwargs.get("lcef1")
                    ),
                    Field(
                        "g23_1",
                        int,
                        40,
                        10,
                        kwargs.get("g23_1")
                    ),
                    Field(
                        "g31_1",
                        int,
                        50,
                        10,
                        kwargs.get("g31_1")
                    ),
                    Field(
                        "daf1",
                        int,
                        60,
                        10,
                        kwargs.get("daf1")
                    ),
                    Field(
                        "dam1",
                        int,
                        70,
                        10,
                        kwargs.get("dam1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g12",
                        int,
                        0,
                        10,
                        kwargs.get("g12")
                    ),
                    Field(
                        "lcg12",
                        int,
                        10,
                        10,
                        kwargs.get("lcg12")
                    ),
                    Field(
                        "aloc12",
                        int,
                        20,
                        10,
                        kwargs.get("aloc12")
                    ),
                    Field(
                        "gloc12",
                        int,
                        30,
                        10,
                        kwargs.get("gloc12")
                    ),
                    Field(
                        "meth12",
                        int,
                        40,
                        10,
                        kwargs.get("meth12")
                    ),
                    Field(
                        "dam12",
                        int,
                        50,
                        10,
                        kwargs.get("dam12")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf2",
                        int,
                        0,
                        10,
                        kwargs.get("idf2")
                    ),
                    Field(
                        "alph2",
                        float,
                        10,
                        10,
                        kwargs.get("alph2")
                    ),
                    Field(
                        "ef2",
                        float,
                        20,
                        10,
                        kwargs.get("ef2")
                    ),
                    Field(
                        "lcef2",
                        int,
                        30,
                        10,
                        kwargs.get("lcef2")
                    ),
                    Field(
                        "g23_2",
                        float,
                        40,
                        10,
                        kwargs.get("g23_2")
                    ),
                    Field(
                        "g31_2",
                        float,
                        50,
                        10,
                        kwargs.get("g31_2")
                    ),
                    Field(
                        "daf2",
                        int,
                        60,
                        10,
                        kwargs.get("daf2")
                    ),
                    Field(
                        "dam2",
                        int,
                        70,
                        10,
                        kwargs.get("dam2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g23",
                        float,
                        0,
                        10,
                        kwargs.get("g23")
                    ),
                    Field(
                        "lcg23",
                        int,
                        10,
                        10,
                        kwargs.get("lcg23")
                    ),
                    Field(
                        "aloc23",
                        float,
                        20,
                        10,
                        kwargs.get("aloc23")
                    ),
                    Field(
                        "gloc23",
                        float,
                        30,
                        10,
                        kwargs.get("gloc23")
                    ),
                    Field(
                        "meth23",
                        int,
                        40,
                        10,
                        kwargs.get("meth23")
                    ),
                    Field(
                        "dam23",
                        int,
                        50,
                        10,
                        kwargs.get("dam23")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf3",
                        int,
                        0,
                        10,
                        kwargs.get("idf3")
                    ),
                    Field(
                        "alph3",
                        float,
                        10,
                        10,
                        kwargs.get("alph3")
                    ),
                    Field(
                        "ef3",
                        float,
                        20,
                        10,
                        kwargs.get("ef3")
                    ),
                    Field(
                        "lcef3",
                        int,
                        30,
                        10,
                        kwargs.get("lcef3")
                    ),
                    Field(
                        "g23_3",
                        float,
                        40,
                        10,
                        kwargs.get("g23_3")
                    ),
                    Field(
                        "g31_3",
                        float,
                        50,
                        10,
                        kwargs.get("g31_3")
                    ),
                    Field(
                        "daf3",
                        int,
                        60,
                        10,
                        kwargs.get("daf3")
                    ),
                    Field(
                        "dam3",
                        int,
                        70,
                        10,
                        kwargs.get("dam3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "postv",
                        float,
                        0,
                        10,
                        kwargs.get("postv")
                    ),
                    Field(
                        "viscs",
                        float,
                        10,
                        10,
                        kwargs.get("viscs")
                    ),
                    Field(
                        "ihis",
                        float,
                        20,
                        10,
                        kwargs.get("ihis")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat249Crash.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the MAss density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def em(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of matrix material
        """ # nopep8
        return self._cards[0].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        self._cards[0].set_value("em", value)

    @property
    def prm(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for matrix material
        """ # nopep8
        return self._cards[0].get_value("prm")

    @prm.setter
    def prm(self, value: float) -> None:
        self._cards[0].set_value("prm", value)

    @property
    def lcsigy(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for strain hardening of the matrix. If a curve, then it specifies yield stress as a function of effective plastic strain. If a table, then temperatures are the table values indexing curves giving yield stress as a function of effective plastic strain (see *DEFINE_‌TABLE).
        """ # nopep8
        return self._cards[0].get_value("lcsigy")

    @lcsigy.setter
    def lcsigy(self, value: int) -> None:
        self._cards[0].set_value("lcsigy", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Parameter for mixed hardening. Set BETA = 0 for pure kinematic hardening and BETA = 1 for pure isotropic hardening
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def pfl(self) -> typing.Optional[float]:
        """Get or set the Percentage of layers that must fail to initiate failure of the element (Default is 100).
        """ # nopep8
        return self._cards[0].get_value("pfl")

    @pfl.setter
    def pfl(self, value: float) -> None:
        self._cards[0].set_value("pfl", value)

    @property
    def visc(self) -> typing.Optional[float]:
        """Get or set the Viscous formulation for fibers:
        EQ.0.0:	Elastic behavior.
        EQ.1.0 : Viscoelastic behavior modeled with Prony series.
        """ # nopep8
        return self._cards[0].get_value("visc")

    @visc.setter
    def visc(self, value: float) -> None:
        self._cards[0].set_value("visc", value)

    @property
    def nfib(self) -> typing.Optional[int]:
        """Get or set the Number of fiber families to be considered
        """ # nopep8
        return self._cards[1].get_value("nfib")

    @nfib.setter
    def nfib(self, value: int) -> None:
        self._cards[1].set_value("nfib", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle MANGL.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def mangl(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
        """ # nopep8
        return self._cards[2].get_value("mangl")

    @mangl.setter
    def mangl(self, value: float) -> None:
        self._cards[2].set_value("mangl", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Balance thickness changes of the material due to the matrix response when calculating the fiber stresses. Stresses can be scaled to account for the fact that fiber cross-sectional usually does not change.
        EQ.0:	No scaling
        EQ.1 : Scaling
        """ # nopep8
        return self._cards[2].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[2].set_value("thick", value)

    @property
    def vg1(self) -> typing.Optional[float]:
        """Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vg1")

    @vg1.setter
    def vg1(self, value: float) -> None:
        self._cards[3].set_value("vg1", value)

    @property
    def vb1(self) -> typing.Optional[float]:
        """Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vb1")

    @vb1.setter
    def vb1(self, value: float) -> None:
        self._cards[3].set_value("vb1", value)

    @property
    def vg2(self) -> typing.Optional[float]:
        """Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vg2")

    @vg2.setter
    def vg2(self, value: float) -> None:
        self._cards[3].set_value("vg2", value)

    @property
    def vb2(self) -> typing.Optional[float]:
        """Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vb2")

    @vb2.setter
    def vb2(self, value: float) -> None:
        self._cards[3].set_value("vb2", value)

    @property
    def vg3(self) -> typing.Optional[float]:
        """Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vg3")

    @vg3.setter
    def vg3(self, value: float) -> None:
        self._cards[3].set_value("vg3", value)

    @property
    def vb3(self) -> typing.Optional[float]:
        """Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vb3")

    @vb3.setter
    def vb3(self, value: float) -> None:
        self._cards[3].set_value("vb3", value)

    @property
    def vg4(self) -> typing.Optional[float]:
        """Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vg4")

    @vg4.setter
    def vg4(self, value: float) -> None:
        self._cards[3].set_value("vg4", value)

    @property
    def vb4(self) -> typing.Optional[float]:
        """Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
        """ # nopep8
        return self._cards[3].get_value("vb4")

    @vb4.setter
    def vb4(self, value: float) -> None:
        self._cards[3].set_value("vb4", value)

    @property
    def idf1(self) -> typing.Optional[int]:
        """Get or set the ID for 1st fiber family for post-processing
        """ # nopep8
        return self._cards[4].get_value("idf1")

    @idf1.setter
    def idf1(self, value: int) -> None:
        self._cards[4].set_value("idf1", value)

    @property
    def alph1(self) -> typing.Optional[int]:
        """Get or set the Orientation angle ALPHA for 1st fiber with respect to overall material direction
        """ # nopep8
        return self._cards[4].get_value("alph1")

    @alph1.setter
    def alph1(self, value: int) -> None:
        self._cards[4].set_value("alph1", value)

    @property
    def ef1(self) -> typing.Optional[int]:
        """Get or set the Young's modulus for 1st fiber family
        """ # nopep8
        return self._cards[4].get_value("ef1")

    @ef1.setter
    def ef1(self, value: int) -> None:
        self._cards[4].set_value("ef1", value)

    @property
    def lcef1(self) -> typing.Optional[int]:
        """Get or set the Curve ID for stress versus fiber elongation of 1st fiber. With this option active, EF1 is ignored
        """ # nopep8
        return self._cards[4].get_value("lcef1")

    @lcef1.setter
    def lcef1(self, value: int) -> None:
        self._cards[4].set_value("lcef1", value)

    @property
    def g23_1(self) -> typing.Optional[int]:
        """Get or set the Transversal shear modulus orthogonal to direction of fiber 1
        """ # nopep8
        return self._cards[4].get_value("g23_1")

    @g23_1.setter
    def g23_1(self, value: int) -> None:
        self._cards[4].set_value("g23_1", value)

    @property
    def g31_1(self) -> typing.Optional[int]:
        """Get or set the Transversal shear modulus in direction of fiber 1
        """ # nopep8
        return self._cards[4].get_value("g31_1")

    @g31_1.setter
    def g31_1(self, value: int) -> None:
        self._cards[4].set_value("g31_1", value)

    @property
    def daf1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_1^ffor 1st fiber (see Remark 2). If a curve, DAF1 specifies damage as a function of fiber strain (for compression and elongation). If DAF1 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
        The damager parameter d_1 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
        """ # nopep8
        return self._cards[4].get_value("daf1")

    @daf1.setter
    def daf1(self, value: int) -> None:
        self._cards[4].set_value("daf1", value)

    @property
    def dam1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_1^mfor matrix material based on the current deformation status of the 1st fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
        The damager parameter d_1 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_1^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
        """ # nopep8
        return self._cards[4].get_value("dam1")

    @dam1.setter
    def dam1(self, value: int) -> None:
        self._cards[4].set_value("dam1", value)

    @property
    def g12(self) -> typing.Optional[int]:
        """Get or set the Linear shear modulus for shearing between fiber 1 and 2
        """ # nopep8
        return self._cards[5].get_value("g12")

    @g12.setter
    def g12(self, value: int) -> None:
        self._cards[5].set_value("g12", value)

    @property
    def lcg12(self) -> typing.Optional[int]:
        """Get or set the Curve ID for shear stress as a function of shearing type as specified with METH12 between the 1st and 2nd fibers.
        """ # nopep8
        return self._cards[5].get_value("lcg12")

    @lcg12.setter
    def lcg12(self, value: int) -> None:
        self._cards[5].set_value("lcg12", value)

    @property
    def aloc12(self) -> typing.Optional[int]:
        """Get or set the Locking angle (in radians) for shear between fiber families 1 and 2
        """ # nopep8
        return self._cards[5].get_value("aloc12")

    @aloc12.setter
    def aloc12(self, value: int) -> None:
        self._cards[5].set_value("aloc12", value)

    @property
    def gloc12(self) -> typing.Optional[int]:
        """Get or set the Linear shear modulus for shear angles larger than ALOC12
        """ # nopep8
        return self._cards[5].get_value("gloc12")

    @gloc12.setter
    def gloc12(self, value: int) -> None:
        self._cards[5].set_value("gloc12", value)

    @property
    def meth12(self) -> typing.Optional[int]:
        """Get or set the Option for shear response between fiber 1 and 2 (see Remark 1):
        EQ.0:	Elastic shear response.Curve LCG12 specifies shear stress as a function of the scalar product of the fiber directions
        EQ.1 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of the normalized scalar product of the fiber directions.
        EQ.2 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.
        EQ.3 : Elasto - plastic shear response.Curve LCG12 defines yield shear stress as a function of normalized shear angle between the fibers.
        EQ.4 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.This option is a special implementation for non - crimped fabrics, where one of the fiber families corresponds to a stitching.
        EQ.5 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of normalized shear angle between the fibers.This option is a special implementation for non - crimped fabrics, where one of the fiber families corresponds to a stitching.
        EQ.10 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.This option is tailored for woven fabricsand guarantees a pure shear stress response.
        EQ.11 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of normalized shear angle.This option is tailored for woven fabricsand guarantees a pure shear stress response
        """ # nopep8
        return self._cards[5].get_value("meth12")

    @meth12.setter
    def meth12(self, value: int) -> None:
        self._cards[5].set_value("meth12", value)

    @property
    def dam12(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the damage parameter d_12^m for the matrix as function of shear angle (radians) between the 1st and 2nd fiber (see Remark 2). The damage parameter d_12^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_12^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
        """ # nopep8
        return self._cards[5].get_value("dam12")

    @dam12.setter
    def dam12(self, value: int) -> None:
        self._cards[5].set_value("dam12", value)

    @property
    def idf2(self) -> typing.Optional[int]:
        """Get or set the ID for 2nd fiber family for post-processing
        """ # nopep8
        return self._cards[6].get_value("idf2")

    @idf2.setter
    def idf2(self, value: int) -> None:
        self._cards[6].set_value("idf2", value)

    @property
    def alph2(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for 2nd fiber with respect to overall material direction
        """ # nopep8
        return self._cards[6].get_value("alph2")

    @alph2.setter
    def alph2(self, value: float) -> None:
        self._cards[6].set_value("alph2", value)

    @property
    def ef2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for 2nd fiber family
        """ # nopep8
        return self._cards[6].get_value("ef2")

    @ef2.setter
    def ef2(self, value: float) -> None:
        self._cards[6].set_value("ef2", value)

    @property
    def lcef2(self) -> typing.Optional[int]:
        """Get or set the Curve ID for stress as a functionof fiber elongation of 2nd fiber. With this option active, EF2 is ignored
        """ # nopep8
        return self._cards[6].get_value("lcef2")

    @lcef2.setter
    def lcef2(self, value: int) -> None:
        self._cards[6].set_value("lcef2", value)

    @property
    def g23_2(self) -> typing.Optional[float]:
        """Get or set the Transversal shear modulus orthogonal to direction of fiber 2
        """ # nopep8
        return self._cards[6].get_value("g23_2")

    @g23_2.setter
    def g23_2(self, value: float) -> None:
        self._cards[6].set_value("g23_2", value)

    @property
    def g31_2(self) -> typing.Optional[float]:
        """Get or set the Transversal shear modulus in direction of fiber 2
        """ # nopep8
        return self._cards[6].get_value("g31_2")

    @g31_2.setter
    def g31_2(self, value: float) -> None:
        self._cards[6].set_value("g31_2", value)

    @property
    def daf2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_2^ffor 2nd fiber (see Remark 2). If a curve, DAF2 specifies damage as a function of fiber strain (for compression and elongation). If DAF2 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
        The damager parameter d_2 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
        """ # nopep8
        return self._cards[6].get_value("daf2")

    @daf2.setter
    def daf2(self, value: int) -> None:
        self._cards[6].set_value("daf2", value)

    @property
    def dam2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_2^mfor matrix material based on the current deformation status of the 2nd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
        The damager parameter d_2 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_2^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
        """ # nopep8
        return self._cards[6].get_value("dam2")

    @dam2.setter
    def dam2(self, value: int) -> None:
        self._cards[6].set_value("dam2", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the Linear shear modulus for shearing between fiber families 2 and 3
        """ # nopep8
        return self._cards[7].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        self._cards[7].set_value("g23", value)

    @property
    def lcg23(self) -> typing.Optional[int]:
        """Get or set the Curve ID for shear stress as a function of shearing type as specifies with METH23 between the 2nd and 3rd fibers.
        """ # nopep8
        return self._cards[7].get_value("lcg23")

    @lcg23.setter
    def lcg23(self, value: int) -> None:
        self._cards[7].set_value("lcg23", value)

    @property
    def aloc23(self) -> typing.Optional[float]:
        """Get or set the Locking angle (in radians) for shear between fiber families 2 and 3
        """ # nopep8
        return self._cards[7].get_value("aloc23")

    @aloc23.setter
    def aloc23(self, value: float) -> None:
        self._cards[7].set_value("aloc23", value)

    @property
    def gloc23(self) -> typing.Optional[float]:
        """Get or set the Linear shear modulus for shear angles larger than ALOC23
        """ # nopep8
        return self._cards[7].get_value("gloc23")

    @gloc23.setter
    def gloc23(self, value: float) -> None:
        self._cards[7].set_value("gloc23", value)

    @property
    def meth23(self) -> typing.Optional[int]:
        """Get or set the Option for shear response between fibers 2 and 3:
        EQ.0: Elastic shear response, curve LCG23 defines shear stress as a function of scalar product of fibers directions. ALOC23 and GLOC23 are ignored.
        EQ.1: Elasto-plastic shear response, curve LCG23 defines shear stress as a function of the scalar product of fiber directions.
        EQ.2: Elastic shear response, curve LCG23 defines shear stress as a function of shear angle between fiber given in radians. ALOC23 and GLOC23 are ignored.
        EQ.3: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians.
        EQ.4: Elastic shear response, curve LCG23 defines shear stress vs. shear angle between fiber given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching. ALOC23 and GLOC23 are ignored.
        EQ.5: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching
        """ # nopep8
        return self._cards[7].get_value("meth23")

    @meth23.setter
    def meth23(self, value: int) -> None:
        self._cards[7].set_value("meth23", value)

    @property
    def dam23(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the damage parameter d_23^m for the matrix as function of shear angle (in rad) between 1st and 2nd fiber. The damager parameter d_23^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_23^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
        """ # nopep8
        return self._cards[7].get_value("dam23")

    @dam23.setter
    def dam23(self, value: int) -> None:
        self._cards[7].set_value("dam23", value)

    @property
    def idf3(self) -> typing.Optional[int]:
        """Get or set the ID for 3rd fiber family for post-processing
        """ # nopep8
        return self._cards[8].get_value("idf3")

    @idf3.setter
    def idf3(self, value: int) -> None:
        self._cards[8].set_value("idf3", value)

    @property
    def alph3(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for 3rd fiber with respect to overall material direction
        """ # nopep8
        return self._cards[8].get_value("alph3")

    @alph3.setter
    def alph3(self, value: float) -> None:
        self._cards[8].set_value("alph3", value)

    @property
    def ef3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for 3rd fiber family
        """ # nopep8
        return self._cards[8].get_value("ef3")

    @ef3.setter
    def ef3(self, value: float) -> None:
        self._cards[8].set_value("ef3", value)

    @property
    def lcef3(self) -> typing.Optional[int]:
        """Get or set the Curve ID for stress versus fiber elongation of 3rd fiber. With this option active, EF3 is ignored
        """ # nopep8
        return self._cards[8].get_value("lcef3")

    @lcef3.setter
    def lcef3(self, value: int) -> None:
        self._cards[8].set_value("lcef3", value)

    @property
    def g23_3(self) -> typing.Optional[float]:
        """Get or set the Transverse shear modulus orthogonal to direction of fiber 3
        """ # nopep8
        return self._cards[8].get_value("g23_3")

    @g23_3.setter
    def g23_3(self, value: float) -> None:
        self._cards[8].set_value("g23_3", value)

    @property
    def g31_3(self) -> typing.Optional[float]:
        """Get or set the Transverse shear modulus in direction of fiber 3
        """ # nopep8
        return self._cards[8].get_value("g31_3")

    @g31_3.setter
    def g31_3(self, value: float) -> None:
        self._cards[8].set_value("g31_3", value)

    @property
    def daf3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_3^ffor 3rd fiber (see Remark 2). If a curve, DAF3 specifies damage as a function of fiber strain (for compression and elongation). If DAF3 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
        The damager parameter d_3 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
        """ # nopep8
        return self._cards[8].get_value("daf3")

    @daf3.setter
    def daf3(self, value: int) -> None:
        self._cards[8].set_value("daf3", value)

    @property
    def dam3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for damage parameter d_3^mfor matrix material based on the current deformation status of the 3rd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
        The damager parameter d_3 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_3^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
        """ # nopep8
        return self._cards[8].get_value("dam3")

    @dam3.setter
    def dam3(self, value: int) -> None:
        self._cards[8].set_value("dam3", value)

    @property
    def postv(self) -> typing.Optional[float]:
        """Get or set the Parameter for outputting additional history variables that might be useful for post-processing.
        """ # nopep8
        return self._cards[9].get_value("postv")

    @postv.setter
    def postv(self, value: float) -> None:
        self._cards[9].set_value("postv", value)

    @property
    def viscs(self) -> typing.Optional[float]:
        """Get or set the Portion of viscous relaxation moduli VGk that is accounted for in time step size calculation
        """ # nopep8
        return self._cards[9].get_value("viscs")

    @viscs.setter
    def viscs(self, value: float) -> None:
        self._cards[9].set_value("viscs", value)

    @property
    def ihis(self) -> typing.Optional[float]:
        """Get or set the Flag for material properties initialization :
        EQ.0 : Material properties defined in Cards 1 - 9 are used
        GE.1 : Use * INITIAL_‌STRESS_‌SHELL to initialize some material properties on an element - by - element basis
        """ # nopep8
        return self._cards[9].get_value("ihis")

    @ihis.setter
    def ihis(self, value: float) -> None:
        self._cards[9].set_value("ihis", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[10].cards[0].set_value("title", value)

