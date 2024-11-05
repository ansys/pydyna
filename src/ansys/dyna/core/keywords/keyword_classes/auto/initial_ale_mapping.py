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

class InitialAleMapping(KeywordBase):
    """DYNA INITIAL_ALE_MAPPING keyword"""

    keyword = "INITIAL"
    subkeyword = "ALE_MAPPING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "typ",
                        int,
                        10,
                        10,
                        kwargs.get("typ", 0)
                    ),
                    Field(
                        "ammsid",
                        int,
                        20,
                        10,
                        kwargs.get("ammsid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xo",
                        float,
                        0,
                        10,
                        kwargs.get("xo", 0.0)
                    ),
                    Field(
                        "yo",
                        float,
                        10,
                        10,
                        kwargs.get("yo", 0.0)
                    ),
                    Field(
                        "zo",
                        float,
                        20,
                        10,
                        kwargs.get("zo", 0.0)
                    ),
                    Field(
                        "vecid",
                        int,
                        30,
                        10,
                        kwargs.get("vecid")
                    ),
                    Field(
                        "angle",
                        float,
                        40,
                        10,
                        kwargs.get("angle")
                    ),
                    Field(
                        "sym",
                        int,
                        50,
                        10,
                        kwargs.get("sym", 0)
                    ),
                    Field(
                        "tbeg",
                        float,
                        60,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def typ(self) -> int:
        """Get or set the Type of “PID” (see remark 1):
        EQ.0:  part set ID (PSID).
        EQ.1:  part ID (PID)

        """ # nopep8
        return self._cards[0].get_value("typ")

    @typ.setter
    def typ(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""typ must be one of {0,1}""")
        self._cards[0].set_value("typ", value)

    @property
    def ammsid(self) -> typing.Optional[int]:
        """Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP
        """ # nopep8
        return self._cards[0].get_value("ammsid")

    @ammsid.setter
    def ammsid(self, value: int) -> None:
        self._cards[0].set_value("ammsid", value)

    @property
    def xo(self) -> float:
        """Get or set the Origin position in global X-direction
        """ # nopep8
        return self._cards[1].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        self._cards[1].set_value("xo", value)

    @property
    def yo(self) -> float:
        """Get or set the Origin position in global Y-direction
        """ # nopep8
        return self._cards[1].get_value("yo")

    @yo.setter
    def yo(self, value: float) -> None:
        self._cards[1].set_value("yo", value)

    @property
    def zo(self) -> float:
        """Get or set the Origin position in global Z-direction
        """ # nopep8
        return self._cards[1].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        self._cards[1].set_value("zo", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[1].set_value("vecid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Angle of rotation in degrees around an axis defined by *DEFINE_VECTOR for the 3D to 3D mapping. See Remark 4
        """ # nopep8
        return self._cards[1].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[1].set_value("angle", value)

    @property
    def sym(self) -> int:
        """Get or set the Treatment of the elements and nodes that are out of the mapping bounds (meaning the coordinates of their projections on the previous mesh are outside this mesh).
        SYM is a 6-digit parameter. Each digit represents a plane for a box that encloses the previous mesh.
        These planes are parallel to the previous coordinate system:
        EQ.00000p: Rule for the X - plane along the lower previous mesh bound
        EQ.0000p0 : Rule for the X - plane along the upper previous mesh bound
        EQ.000p00 : Rule for the Y - plane along the lower previous mesh bound
        EQ.00p000 : Rule for the Y - plane along the upper previous mesh bound
        EQ.0p0000 : Rule for the Z - plane along the lower previous mesh bound
        EQ.p00000 : Rule for the Z - plane along the upper previous mesh bound
        The value of p defines the rule to apply in relation to the box plane :
        EQ.0 : Do nothing.
        EQ.1 : Translational symmetry(direction of translation orthogonal to the box plane)
        EQ.2 : Mirror - image symmetry about the box plane
        EQ.3 : Continuity of boundary elements and nodes along the box plane
        """ # nopep8
        return self._cards[1].get_value("sym")

    @sym.setter
    def sym(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""sym must be one of {0,1,2,3}""")
        self._cards[1].set_value("sym", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Time to start the run. It replaces the termination time of the previous run that generated the mapping file if TBEG is the larger
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

