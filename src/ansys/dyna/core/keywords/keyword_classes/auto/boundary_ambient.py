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

class BoundaryAmbient(KeywordBase):
    """DYNA BOUNDARY_AMBIENT keyword"""

    keyword = "BOUNDARY"
    subkeyword = "AMBIENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "mmg",
                        int,
                        10,
                        10,
                        kwargs.get("mmg")
                    ),
                    Field(
                        "ambtyp",
                        int,
                        20,
                        10,
                        kwargs.get("ambtyp")
                    ),
                    Field(
                        "sidr",
                        int,
                        30,
                        10,
                        kwargs.get("sidr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid1",
                        int,
                        0,
                        10,
                        kwargs.get("lcid1")
                    ),
                    Field(
                        "lcid2",
                        int,
                        10,
                        10,
                        kwargs.get("lcid2")
                    ),
                ],
            ),
        ]

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the The ambient element set ID for which the thermodynamic state is being defined. The element set can be *SET_SOLID for a 3D ALE model, *SET_SHELL for a 2D ALE model or *SET_BEAM for a 1D ALE model.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def mmg(self) -> typing.Optional[int]:
        """Get or set the ALE multi-material group ID.In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
        """ # nopep8
        return self._cards[0].get_value("mmg")

    @mmg.setter
    def mmg(self, value: int) -> None:
        self._cards[0].set_value("mmg", value)

    @property
    def ambtyp(self) -> typing.Optional[int]:
        """Get or set the Ambient element type:
        EQ.4:	Pressure inflow/outflow (see Remarks 1 and 2)
        EQ.5:	Receptor for blast load (See *LOAD_BLAST_ENHANCED)
        """ # nopep8
        return self._cards[0].get_value("ambtyp")

    @ambtyp.setter
    def ambtyp(self, value: int) -> None:
        self._cards[0].set_value("ambtyp", value)

    @property
    def sidr(self) -> int:
        """Get or set the Flag controlling the use of this keyword during dynamic relaxation.
        EQ.0:	the keyword is applied in normal analysis phase only,
        EQ.1:	the keyword is applied in dynamic relaxation phase but not the normal analysis phase,
        EQ.2:	the keyword is applied in both dynamic relaxation phase and normal analysis phase.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sidr must be one of {0,1,2}""")
        self._cards[0].set_value("sidr", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the A load curve ID for internal energy per unit reference volume (see Remark 4 and the *EOS section for details). If *EOS_‌IDEAL_‌GAS is being used, this ID refers to a temperature load curve ID
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        self._cards[1].set_value("lcid1", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for relative volume, v_r=(v⁄v_0 =ρ_0⁄ρ).  (See Remark 3 and the *EOS section for details).
        """ # nopep8
        return self._cards[1].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[1].set_value("lcid2", value)

