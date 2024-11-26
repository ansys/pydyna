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

class AleStructuredMeshMotion(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_MOTION keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_MOTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mshid",
                        int,
                        0,
                        10,
                        kwargs.get("mshid")
                    ),
                    Field(
                        "option",
                        str,
                        10,
                        10,
                        kwargs.get("option", "FOLLOW_GC")
                    ),
                    Field(
                        "ammgsid",
                        int,
                        20,
                        10,
                        kwargs.get("ammgsid", 0)
                    ),
                    Field(
                        "explim",
                        float,
                        30,
                        10,
                        kwargs.get("explim", 1.0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "symcod",
                        int,
                        70,
                        10,
                        kwargs.get("symcod", 0)
                    ),
                ],
            ),
        ]

    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE Mesh ID.  A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        self._cards[0].set_value("mshid", value)

    @property
    def option(self) -> str:
        """Get or set the FOLLOW_GC/COVER_LAG
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["FOLLOW_GC", "COVER_LAG"]:
            raise Exception("""option must be one of {"FOLLOW_GC","COVER_LAG"}""")
        self._cards[0].set_value("option", value)

    @property
    def ammgsid(self) -> int:
        """Get or set the The set of ALE multi-material group list IDs which the mesh follows.
        Please refer to *SET_MULTI-MATERIAL_GROUP_LIST card for details.
        """ # nopep8
        return self._cards[0].get_value("ammgsid")

    @ammgsid.setter
    def ammgsid(self, value: int) -> None:
        self._cards[0].set_value("ammgsid", value)

    @property
    def explim(self) -> float:
        """Get or set the Limit ratio for mesh expansion and contraction. The distance between the nodes is not allowed to increase by
        more than a factor EXPLIM or decrease to less than a factor 1/EXPLIM.  Default value of 1.0 means no expansion/contraction.
        """ # nopep8
        return self._cards[0].get_value("explim")

    @explim.setter
    def explim(self, value: float) -> None:
        self._cards[0].set_value("explim", value)

    @property
    def symcod(self) -> int:
        """Get or set the A three digit number to define symmetry. Each digit specifies one direction (local x,y,z defined in *ALE_STRUCTURED_MESH) and can be of 0,1 or 2. Code 0 means no symmetry; 1 symmetry defined at minus face; 2 plus face
        """ # nopep8
        return self._cards[0].get_value("symcod")

    @symcod.setter
    def symcod(self, value: int) -> None:
        self._cards[0].set_value("symcod", value)

