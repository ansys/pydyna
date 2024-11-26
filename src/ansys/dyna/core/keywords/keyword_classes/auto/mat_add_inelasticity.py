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

class MatAddInelasticity(KeywordBase):
    """DYNA MAT_ADD_INELASTICITY keyword"""

    keyword = "MAT"
    subkeyword = "ADD_INELASTICITY"
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
                        "nielinks",
                        int,
                        10,
                        10,
                        kwargs.get("nielinks", 1)
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "g",
                        float,
                        30,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "k",
                        float,
                        40,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "aopt",
                        float,
                        50,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        float,
                        60,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "beta",
                        float,
                        70,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
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
                ],
            ),
            Card(
                [
                    Field(
                        "nielaws",
                        int,
                        0,
                        10,
                        kwargs.get("nielaws", 1)
                    ),
                    Field(
                        "weight",
                        float,
                        10,
                        10,
                        kwargs.get("weight")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "law",
                        int,
                        0,
                        10,
                        kwargs.get("law", 3)
                    ),
                    Field(
                        "model",
                        int,
                        10,
                        10,
                        kwargs.get("model")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddInelasticity.option_specs[0],
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
        """Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def nielinks(self) -> int:
        """Get or set the Number of links/networks/phases specified by the user, An additional link may be added internally if the weights below do not sum up to unity.
        """ # nopep8
        return self._cards[0].get_value("nielinks")

    @nielinks.setter
    def nielinks(self, value: int) -> None:
        self._cards[0].set_value("nielinks", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Characteristic shear modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set G=E/2(1+ν) ..
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Characteristic bulk modulus, used for some of the inelasticity models. This should reflect the elastic stiffness for the material without any inelasticity effects. For instance, if *MAT_ELASTIC is used, set K=E/3(1-2ν) ..
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option, see AOPT on *MAT_002 for a detailed description.
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center.This option is for solid elements only.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector, v,and an originating point, P, defining the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID.
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[0].set_value("aopt", value)

    @property
    def macf(self) -> float:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        """ # nopep8
        return self._cards[0].get_value("macf")

    @macf.setter
    def macf(self, value: float) -> None:
        self._cards[0].set_value("macf", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells only) and AOPT = 3 (all element types). This angle may be overriden on the element card; see *ELEMENT_‌SHELL_‌BETA and *ELEMENT_‌SOLID_‌ORTHO.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point for AOPT = 1 and 4, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 2, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 2, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 2, see MAT_002.
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector for AOPT = 3 and 4, see MAT_002.
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def nielaws(self) -> int:
        """Get or set the Number of inelasticity laws that apply to this material model at this link, each contributing in its own way to the total inelastic strain (rate).
        """ # nopep8
        return self._cards[3].get_value("nielaws")

    @nielaws.setter
    def nielaws(self, value: int) -> None:
        self._cards[3].set_value("nielaws", value)

    @property
    def weight(self) -> typing.Optional[float]:
        """Get or set the Weight of this link/network/phase, used when computing total stress.
        """ # nopep8
        return self._cards[3].get_value("weight")

    @weight.setter
    def weight(self, value: float) -> None:
        self._cards[3].set_value("weight", value)

    @property
    def law(self) -> int:
        """Get or set the Inelasticity law, one of laws listed below must be chosen
        LAW.EQ.3: Isotropic hardening plasticity.
        LAW.EQ.5: Creep.
        LAW.EQ.6: Viscoelasticity
        """ # nopep8
        return self._cards[4].get_value("law")

    @law.setter
    def law(self, value: int) -> None:
        if value not in [3, 5, 6]:
            raise Exception("""law must be one of {3,5,6}""")
        self._cards[4].set_value("law", value)

    @property
    def model(self) -> typing.Optional[int]:
        """Get or set the Model definition with choice dependent on the specified law above. A valid combination of law and model must be chosen.
        For isotropic hardening plasticity(LAW = 3), choices areEQ.1:	Linear hardening
        EQ.2 : Hardening from curve / table
        For creep(LAW = 5), choices are
        EQ.1 : Norton incremental formulation
        EQ.2 : Norton total formulation
        EQ.3 : Norton - Bailey formulation
        EQ.4 : Bergström - Boyce formulation
        For viscoelasticity(LAW = 6), choices are
        EQ.1 : Bulk and shear decay, with optional temperature shifts, hypoelastic version
        EQ.2 : Bulk and shear decay, with optional temperature shifts, hyperelastic version #1
        EQ.3:	Bulk and shear decay, with optional temperature shifts, hyperelastic version #2
        EQ.4:	Norton - Bailey formulation
        EQ.5 : Bergström - Boyce formulation
        """ # nopep8
        return self._cards[4].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        self._cards[4].set_value("model", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Virgin yield stress.
        """ # nopep8
        return self._cards[5].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[5].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Hardening.
        """ # nopep8
        return self._cards[5].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[5].set_value("p2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

