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

class CosimFmiInterface(KeywordBase):
    """DYNA COSIM_FMI_INTERFACE keyword"""

    keyword = "COSIM"
    subkeyword = "FMI_INTERFACE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "appid",
                        str,
                        0,
                        20,
                        kwargs.get("appid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "impexp",
                        str,
                        0,
                        10,
                        kwargs.get("impexp", "IMP")
                    ),
                    Field(
                        "regtyp",
                        str,
                        10,
                        10,
                        kwargs.get("regtyp", "NODE")
                    ),
                    Field(
                        "regid",
                        int,
                        20,
                        10,
                        kwargs.get("regid")
                    ),
                    Field(
                        "field",
                        str,
                        30,
                        10,
                        kwargs.get("field")
                    ),
                    Field(
                        "winit",
                        float,
                        40,
                        10,
                        kwargs.get("winit")
                    ),
                    Field(
                        "ratio",
                        float,
                        50,
                        10,
                        kwargs.get("ratio")
                    ),
                    Field(
                        "cid",
                        int,
                        60,
                        10,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "ref",
                        int,
                        70,
                        10,
                        kwargs.get("ref", 0)
                    ),
                ],
            ),
        ]

    @property
    def appid(self) -> typing.Optional[str]:
        """Get or set the FMU (functional mock-up unit) identification. Each FMU must have a unique APPID
        """ # nopep8
        return self._cards[0].get_value("appid")

    @appid.setter
    def appid(self, value: str) -> None:
        self._cards[0].set_value("appid", value)

    @property
    def impexp(self) -> str:
        """Get or set the Import/export flag:
        EQ.IMP:	Variables are to be imported into LS - DYNA.
        EQ.EXP : Variables are to be exported from LS - DYNA.
        """ # nopep8
        return self._cards[1].get_value("impexp")

    @impexp.setter
    def impexp(self, value: str) -> None:
        if value not in ["IMP", "EXP"]:
            raise Exception("""impexp must be one of {"IMP","EXP"}""")
        self._cards[1].set_value("impexp", value)

    @property
    def regtyp(self) -> str:
        """Get or set the Type of interface region:
        EQ.NODE:	Single node
        EQ.NSET : Node set
        EQ.SSET : Segment set
        EQ.PART : Rigid part
        EQ.FUNC : User defined curve function to be exported only.See Remark 1 and Example 2.
        EQ.CURV : User defined curve to be imported only.See Remark 2 and Example 3.
        EQ.SESW : Sense switch to be imported only.REGID is neglected with the sense switch specified in FIELD.See Remark 3 and Example 4.
        EQ.BAG:	Control-volume airbag.
        """ # nopep8
        return self._cards[1].get_value("regtyp")

    @regtyp.setter
    def regtyp(self, value: str) -> None:
        if value not in ["NODE", "NSET", "SSET", "PART", "FUNC", "CURV", "SESW", "BAG"]:
            raise Exception("""regtyp must be one of {"NODE","NSET","SSET","PART","FUNC","CURV","SESW","BAG"}""")
        self._cards[1].set_value("regtyp", value)

    @property
    def regid(self) -> typing.Optional[int]:
        """Get or set the ID of the corresponding region type in LS-DYNA. If region ID is negative for a node set and FIELD = FX, FY or FZ, all nodes within this set share the same imported force value from the FMU. Otherwise, a distinct force value will be imported for each node.
        """ # nopep8
        return self._cards[1].get_value("regid")

    @regid.setter
    def regid(self, value: int) -> None:
        self._cards[1].set_value("regid", value)

    @property
    def field(self) -> typing.Optional[str]:
        """Get or set the Field data to be imported or exported, depending on the region type. See Remark 4 for a full list of fields
        """ # nopep8
        return self._cards[1].get_value("field")

    @field.setter
    def field(self, value: str) -> None:
        self._cards[1].set_value("field", value)

    @property
    def winit(self) -> typing.Optional[float]:
        """Get or set the Initial value
        """ # nopep8
        return self._cards[1].get_value("winit")

    @winit.setter
    def winit(self, value: float) -> None:
        self._cards[1].set_value("winit", value)

    @property
    def ratio(self) -> typing.Optional[float]:
        """Get or set the Scale factor applied during co-simulation (not FMU generation). When LS-DYNA exports variables, the actual value to be sent is RATIO ?LS-DYNA value. When LS-DYNA imports data from FMU, the actual value received is the FMU value / RATIO.
        """ # nopep8
        return self._cards[1].get_value("ratio")

    @ratio.setter
    def ratio(self, value: float) -> None:
        self._cards[1].set_value("ratio", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID (see *DEFINE_?COORDINATE).
        EQ.0:	global(default)
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def ref(self) -> int:
        """Get or set the Control how the coordinate system is used for the variable output when CID? > 0 (see Remark 5):
        EQ.0:	Variable output is in the local system fixed from the beginning.Note that you should set FLAG = 0 in * DEFINE_COORDINATE_NODES.
        EQ.1 : Variable output is projected onto the moving local system.
        EQ.2 : Variable output is the projection of the nodal translation motion relative to node N1 of the local coordinate system, COOR.Double precision LS - DYNA is recommended with REF = 2
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ref must be one of {0,1,2}""")
        self._cards[1].set_value("ref", value)

