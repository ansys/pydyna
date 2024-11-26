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

class ControlRefineAle2D(KeywordBase):
    """DYNA CONTROL_REFINE_ALE2D keyword"""

    keyword = "CONTROL"
    subkeyword = "REFINE_ALE2D"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "nlvl",
                        int,
                        20,
                        10,
                        kwargs.get("nlvl", 1)
                    ),
                    Field(
                        "mmsid",
                        int,
                        30,
                        10,
                        kwargs.get("mmsid", 0)
                    ),
                    Field(
                        "ibox",
                        int,
                        40,
                        10,
                        kwargs.get("ibox", 0)
                    ),
                    Field(
                        "ielout",
                        int,
                        50,
                        10,
                        kwargs.get("ielout", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ntotrf",
                        int,
                        0,
                        10,
                        kwargs.get("ntotrf", 0)
                    ),
                    Field(
                        "ncycrf",
                        float,
                        10,
                        10,
                        kwargs.get("ncycrf", 0.0)
                    ),
                    Field(
                        "critrf",
                        int,
                        20,
                        10,
                        kwargs.get("critrf", 0)
                    ),
                    Field(
                        "valrf",
                        float,
                        30,
                        10,
                        kwargs.get("valrf", 0.0)
                    ),
                    Field(
                        "begrf",
                        float,
                        40,
                        10,
                        kwargs.get("begrf", 0.0)
                    ),
                    Field(
                        "endrf",
                        float,
                        50,
                        10,
                        kwargs.get("endrf", 0.0)
                    ),
                    Field(
                        "layrf",
                        int,
                        60,
                        10,
                        kwargs.get("layrf", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "maxrm",
                        int,
                        0,
                        10,
                        kwargs.get("maxrm", 0)
                    ),
                    Field(
                        "ncycrm",
                        float,
                        10,
                        10,
                        kwargs.get("ncycrm", 0.0)
                    ),
                    Field(
                        "critrm",
                        int,
                        20,
                        10,
                        kwargs.get("critrm", 0)
                    ),
                    Field(
                        "valrm",
                        float,
                        30,
                        10,
                        kwargs.get("valrm", 0.0)
                    ),
                    Field(
                        "begrm",
                        float,
                        40,
                        10,
                        kwargs.get("begrm", 0.0)
                    ),
                    Field(
                        "endrm",
                        float,
                        50,
                        10,
                        kwargs.get("endrm", 0.0)
                    ),
                    Field(
                        "mmsrm",
                        int,
                        60,
                        10,
                        kwargs.get("mmsrm", 0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the Set type:
        EQ.0: ALE Part Set,
        EQ.1: ALE Part,
        EQ.2: Lagrangian Part Set coupled to ALE (see Remarks 1 and 2),
        EQ.3: Lagrangian Part coupled to ALE (see Remarks 1 and 2),
        EQ.4: Lagrangian Shell Set coupled to ALE (see Remarks 1 and 2),
        EQ.5: ALE shell Set.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""type must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("type", value)

    @property
    def nlvl(self) -> int:
        """Get or set the Number of refinement levels (see Remark 3).
        """ # nopep8
        return self._cards[0].get_value("nlvl")

    @nlvl.setter
    def nlvl(self, value: int) -> None:
        self._cards[0].set_value("nlvl", value)

    @property
    def mmsid(self) -> int:
        """Get or set the Multi-Material Set ID (see Remark 4):
        LT.0: only ALE elements with all the multi-material groups listed in
        *SET_MULTI_MATERIAL_GROUP_LIST can be refined (or re	moved otherwise)
        GT.0: ALE elements with at least one of the multi-material groups
        can be refined (or removed).
        """ # nopep8
        return self._cards[0].get_value("mmsid")

    @mmsid.setter
    def mmsid(self, value: int) -> None:
        self._cards[0].set_value("mmsid", value)

    @property
    def ibox(self) -> int:
        """Get or set the Box ID (See *DEFINE_BOX) defining a region in which the ALE elements are refined.
        """ # nopep8
        return self._cards[0].get_value("ibox")

    @ibox.setter
    def ibox(self, value: int) -> None:
        self._cards[0].set_value("ibox", value)

    @property
    def ielout(self) -> int:
        """Get or set the Flag to handle child data in elout(see Remarks 10 and 11).
        """ # nopep8
        return self._cards[0].get_value("ielout")

    @ielout.setter
    def ielout(self, value: int) -> None:
        self._cards[0].set_value("ielout", value)

    @property
    def ntotrf(self) -> int:
        """Get or set the Total number of ALE elements to refine (see Remark 5):
        GT.0: The number is given by |NTOTRF| * 4 ** (NLVL-1) (see Remark 2)
        EQ.0: NTOTRF = number of shell elements / 100.
        """ # nopep8
        return self._cards[1].get_value("ntotrf")

    @ntotrf.setter
    def ntotrf(self, value: int) -> None:
        self._cards[1].set_value("ntotrf", value)

    @property
    def ncycrf(self) -> float:
        """Get or set the Number of cycles between each refinement.
        LT.0: |NCYCRF| is the time interval.
        """ # nopep8
        return self._cards[1].get_value("ncycrf")

    @ncycrf.setter
    def ncycrf(self, value: float) -> None:
        self._cards[1].set_value("ncycrf", value)

    @property
    def critrf(self) -> int:
        """Get or set the Refinement criterion(a negative CRITRF reverses the conditions below):
        EQ.0: static refinement (as if only the 1st card is defined),
        EQ.1: Pressure (if pressure > VALRF),
        EQ.2: Relative Volume (if V/Vo < VALRF) ,
        EQ.3: Volume Fraction (if Volume fraction > VALRF),
        EQ.5: User defined criterion. The fortran routine al2rfn_criteria5 in the
        file dynrfn_user.f should be used to develop the criterion. The file is
        part of the general package usermat.
        """ # nopep8
        return self._cards[1].get_value("critrf")

    @critrf.setter
    def critrf(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 5]:
            raise Exception("""critrf must be one of {0,1,2,3,5}""")
        self._cards[1].set_value("critrf", value)

    @property
    def valrf(self) -> float:
        """Get or set the Criterion value to reach for the refinement.
        """ # nopep8
        return self._cards[1].get_value("valrf")

    @valrf.setter
    def valrf(self, value: float) -> None:
        self._cards[1].set_value("valrf", value)

    @property
    def begrf(self) -> float:
        """Get or set the Time to begin the refinement.
        """ # nopep8
        return self._cards[1].get_value("begrf")

    @begrf.setter
    def begrf(self, value: float) -> None:
        self._cards[1].set_value("begrf", value)

    @property
    def endrf(self) -> float:
        """Get or set the Time to end the refinement.
        """ # nopep8
        return self._cards[1].get_value("endrf")

    @endrf.setter
    def endrf(self, value: float) -> None:
        self._cards[1].set_value("endrf", value)

    @property
    def layrf(self) -> int:
        """Get or set the Number of element layers to refine around a element reaching the refinement criterion (see Remark 6).
        """ # nopep8
        return self._cards[1].get_value("layrf")

    @layrf.setter
    def layrf(self, value: int) -> None:
        self._cards[1].set_value("layrf", value)

    @property
    def maxrm(self) -> int:
        """Get or set the Maximum number of child clusters to remove (see Remark 5):
        LT.0: for the whole run,
        GT.0: every NCYCRM cycles.
        """ # nopep8
        return self._cards[2].get_value("maxrm")

    @maxrm.setter
    def maxrm(self, value: int) -> None:
        self._cards[2].set_value("maxrm", value)

    @property
    def ncycrm(self) -> float:
        """Get or set the Number of cycles between each deletion.
        LT.0: |NCYCRM| is the time interval.
        """ # nopep8
        return self._cards[2].get_value("ncycrm")

    @ncycrm.setter
    def ncycrm(self, value: float) -> None:
        self._cards[2].set_value("ncycrm", value)

    @property
    def critrm(self) -> int:
        """Get or set the Deletion criterion(a negative CRITRM reverses the conditions below):
        EQ.0: no deletion (as if only the 1st and 2nd card are defined),
        EQ.1: Pressure (if pressure < VALRM),
        EQ.2: Relative Volume (if V/Vo > VALRM) ,
        EQ.3: Volume Fraction (if Volume fraction < VALRM),
        EQ.5: User defined criterion. The fortran routine alermv_criteria5 in
        the file dynrfn_user.f should be used to develop the criterion. The file is
        part of the general package usermat.
        """ # nopep8
        return self._cards[2].get_value("critrm")

    @critrm.setter
    def critrm(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""critrm must be one of {0,1,2,3,4,5}""")
        self._cards[2].set_value("critrm", value)

    @property
    def valrm(self) -> float:
        """Get or set the Criterion value to reach in each child elements of a cluster for its deletion.
        """ # nopep8
        return self._cards[2].get_value("valrm")

    @valrm.setter
    def valrm(self, value: float) -> None:
        self._cards[2].set_value("valrm", value)

    @property
    def begrm(self) -> float:
        """Get or set the Time to begin the deletion.
        LT.0: |BEGRM| represents a critical percent of NTOTRF below
        which the deletion should begin (0.0 < |BEGRM| < 1.0). (See Remark 8).
        """ # nopep8
        return self._cards[2].get_value("begrm")

    @begrm.setter
    def begrm(self, value: float) -> None:
        self._cards[2].set_value("begrm", value)

    @property
    def endrm(self) -> float:
        """Get or set the Time to end the deletion.
        """ # nopep8
        return self._cards[2].get_value("endrm")

    @endrm.setter
    def endrm(self, value: float) -> None:
        self._cards[2].set_value("endrm", value)

    @property
    def mmsrm(self) -> int:
        """Get or set the Multi-Material Set ID for the deletion. (See Remark 7).
        """ # nopep8
        return self._cards[2].get_value("mmsrm")

    @mmsrm.setter
    def mmsrm(self, value: int) -> None:
        self._cards[2].set_value("mmsrm", value)

