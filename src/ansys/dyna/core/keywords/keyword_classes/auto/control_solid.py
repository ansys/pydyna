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

class ControlSolid(KeywordBase):
    """DYNA CONTROL_SOLID keyword"""

    keyword = "CONTROL"
    subkeyword = "SOLID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "esort",
                        int,
                        0,
                        10,
                        kwargs.get("esort", 0)
                    ),
                    Field(
                        "fmatrix",
                        int,
                        10,
                        10,
                        kwargs.get("fmatrix", 0)
                    ),
                    Field(
                        "niptets",
                        int,
                        20,
                        10,
                        kwargs.get("niptets", 4)
                    ),
                    Field(
                        "swlocl",
                        int,
                        30,
                        10,
                        kwargs.get("swlocl", 1)
                    ),
                    Field(
                        "psfail",
                        int,
                        40,
                        10,
                        kwargs.get("psfail", 0)
                    ),
                    Field(
                        "t10jtol",
                        float,
                        50,
                        10,
                        kwargs.get("t10jtol", 0.0)
                    ),
                    Field(
                        "icoh",
                        int,
                        60,
                        10,
                        kwargs.get("icoh", 0)
                    ),
                    Field(
                        "tet13k",
                        int,
                        70,
                        10,
                        kwargs.get("tet13k", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pm1",
                        int,
                        0,
                        8,
                        kwargs.get("pm1")
                    ),
                    Field(
                        "pm2",
                        int,
                        8,
                        8,
                        kwargs.get("pm2")
                    ),
                    Field(
                        "pm3",
                        int,
                        16,
                        8,
                        kwargs.get("pm3")
                    ),
                    Field(
                        "pm4",
                        int,
                        24,
                        8,
                        kwargs.get("pm4")
                    ),
                    Field(
                        "pm5",
                        int,
                        32,
                        8,
                        kwargs.get("pm5")
                    ),
                    Field(
                        "pm6",
                        int,
                        40,
                        8,
                        kwargs.get("pm6")
                    ),
                    Field(
                        "pm7",
                        int,
                        48,
                        8,
                        kwargs.get("pm7")
                    ),
                    Field(
                        "pm8",
                        int,
                        56,
                        8,
                        kwargs.get("pm8")
                    ),
                    Field(
                        "pm9",
                        int,
                        64,
                        8,
                        kwargs.get("pm9")
                    ),
                    Field(
                        "pm10",
                        int,
                        72,
                        8,
                        kwargs.get("pm10")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tet13v",
                        int,
                        0,
                        10,
                        kwargs.get("tet13v")
                    ),
                ],
            ),
        ]

    @property
    def esort(self) -> int:
        """Get or set the Automatic sorting of tetrahedron and pentahedron elements to treat degenerate tetrahedron and pentahedron elements as tetrahedron (formulation 10)  and pentahedron (formulation 15) solids, respective. See *SECTION_SOLID.
        EQ.0: no sorting(default).
        EQ.1: sort tetrahedron to type 10, pentahedron to type 15.
        EQ.2: sort tetrahedron to type 10, 1-point integrated pentahedron to type 115, fully integrated pentahedron to type 15.
        EQ.3: same as EQ.1 but also print switched elements in message file.
        EQ.4: same as EQ.2 but also print switched elements in message file
        """ # nopep8
        return self._cards[0].get_value("esort")

    @esort.setter
    def esort(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""esort must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("esort", value)

    @property
    def fmatrix(self) -> int:
        """Get or set the Default method used in the calculation of the defomation gradient matrix.
        EQ.1: Update incrementally in time. This is the default for explicit.
        EQ.2: Directly compute F. This is the default for implicit and implicit/explicit switching.
        """ # nopep8
        return self._cards[0].get_value("fmatrix")

    @fmatrix.setter
    def fmatrix(self, value: int) -> None:
        self._cards[0].set_value("fmatrix", value)

    @property
    def niptets(self) -> int:
        """Get or set the Number of integration points used in the quadratic tetrahedron elements. Either 4 or 5 can be specified. This option applies to the type 4 and type 16 tetrahedron elements.
        """ # nopep8
        return self._cards[0].get_value("niptets")

    @niptets.setter
    def niptets(self, value: int) -> None:
        self._cards[0].set_value("niptets", value)

    @property
    def swlocl(self) -> int:
        """Get or set the Output option for stresses in solid elements used as spot welds with material *MAT_SPOTWELD.
        EQ.1: Global (default),
        EQ.2: Local
        """ # nopep8
        return self._cards[0].get_value("swlocl")

    @swlocl.setter
    def swlocl(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""swlocl must be one of {1,2}""")
        self._cards[0].set_value("swlocl", value)

    @property
    def psfail(self) -> int:
        """Get or set the A nonzero PSFAIL has the same effect as setting ERODE = 1 in *CONTROL_TIMESTEP except that solid element erosion due to negative volume is limited to only the solid elements in part set PSFAIL.In other words, when PSFAIL is nonzero, the time-step-based criterion for erosion (TSMIN) applies to all solid elements (except formulations 11 and 12) while the negative volume criterion for erosion applies only to solids in part set PSFAIL.
        """ # nopep8
        return self._cards[0].get_value("psfail")

    @psfail.setter
    def psfail(self, value: int) -> None:
        self._cards[0].set_value("psfail", value)

    @property
    def t10jtol(self) -> float:
        """Get or set the Tolerance for jacobian in 4-point 10-noded quadratic tetrahedra (type 16).If the quotient between the minimum and maximum jacobian value falls below this tolerance, a warning message is issued in the messag file. This is useful for tracking badly shaped elements in implicit analysis that deteriorates convergence, a value of 1.0 indicates a perfectly shaped element.
        """ # nopep8
        return self._cards[0].get_value("t10jtol")

    @t10jtol.setter
    def t10jtol(self, value: float) -> None:
        self._cards[0].set_value("t10jtol", value)

    @property
    def icoh(self) -> int:
        """Get or set the Breaking LS-DYNA convention ICOH is interpreted digit-wise, namely as,
        ICOH = [LK] = K + 10×L .
        The first digit, in the one’s place, which we shall call K is interpreted as follows:K.EQ.0:	No cohesive element deletion due to neighbor failure.
        K.EQ.1:	Solid elements having ELFORM = 19 – 22 (or ELFORM = 1, 2, 15 being used with * MAT_169) will be eroded when neighboring shell or solid elements fail.This works for nodewise connected partsand tied contacts.
        The second digit, in the ten’s place is, which we shall call L is interpreted as stated below.Note that if ICOH is less than 10 (having a single digit) then L defaults to zero.
        L.EQ.0 : Default stable time step estimate, which is computed from the stiffnessand the nodal masses of the topand bottom as with discrete elements.
        L.EQ.1 : Most conservative(smallest) stable time step estimate.This method calculates mass by integrating the density.
        L.EQ.2 : Intermediate stable time step estimate.Same as the default except reduced by a factor of 1 / √2 corresponding to halving the masses.
        """ # nopep8
        return self._cards[0].get_value("icoh")

    @icoh.setter
    def icoh(self, value: int) -> None:
        self._cards[0].set_value("icoh", value)

    @property
    def tet13k(self) -> int:
        """Get or set the Set to 1 to invoke a consistent tangent stiffness matrix for the pressure averaged tetrahedron (type 13). This is a feature only for implicit analysis and only supported in SMP. This element type averages the volumetric strain over adjacent elements to alleviate volumetric locking, which implies that the corresponding material tangent stiffness should be treated accordingly. Due to the vaste amount of neighbors any given element may have in an arbitrary tetrahedral mesh, the expense for the matrix assembly is at the moment too high for this to pay off in a nonlinear implicit simulation. Whence this is an option that preferably is activated only in linear or eigenvalue analysis to exploit the stiffness characteristics of the type 13 tetrahedron.
        """ # nopep8
        return self._cards[0].get_value("tet13k")

    @tet13k.setter
    def tet13k(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tet13k must be one of {0,1}""")
        self._cards[0].set_value("tet13k", value)

    @property
    def pm1(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm1")

    @pm1.setter
    def pm1(self, value: int) -> None:
        self._cards[1].set_value("pm1", value)

    @property
    def pm2(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm2")

    @pm2.setter
    def pm2(self, value: int) -> None:
        self._cards[1].set_value("pm2", value)

    @property
    def pm3(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm3")

    @pm3.setter
    def pm3(self, value: int) -> None:
        self._cards[1].set_value("pm3", value)

    @property
    def pm4(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm4")

    @pm4.setter
    def pm4(self, value: int) -> None:
        self._cards[1].set_value("pm4", value)

    @property
    def pm5(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm5")

    @pm5.setter
    def pm5(self, value: int) -> None:
        self._cards[1].set_value("pm5", value)

    @property
    def pm6(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm6")

    @pm6.setter
    def pm6(self, value: int) -> None:
        self._cards[1].set_value("pm6", value)

    @property
    def pm7(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm7")

    @pm7.setter
    def pm7(self, value: int) -> None:
        self._cards[1].set_value("pm7", value)

    @property
    def pm8(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm8")

    @pm8.setter
    def pm8(self, value: int) -> None:
        self._cards[1].set_value("pm8", value)

    @property
    def pm9(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm9")

    @pm9.setter
    def pm9(self, value: int) -> None:
        self._cards[1].set_value("pm9", value)

    @property
    def pm10(self) -> typing.Optional[int]:
        """Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
        """ # nopep8
        return self._cards[1].get_value("pm10")

    @pm10.setter
    def pm10(self, value: int) -> None:
        self._cards[1].set_value("pm10", value)

    @property
    def tet13v(self) -> typing.Optional[int]:
        """Get or set the Choice of type 13 solid implementation:
        EQ.0:	Efficient version(default).With the single precision version of LS - DYNA, a little noise in the solution for elements that are moving long distances with rigid body motion could be observed.
        EQ.1 : More accurate version(smoother results) with an additional cost of about 15 % .
        """ # nopep8
        return self._cards[2].get_value("tet13v")

    @tet13v.setter
    def tet13v(self, value: int) -> None:
        self._cards[2].set_value("tet13v", value)

