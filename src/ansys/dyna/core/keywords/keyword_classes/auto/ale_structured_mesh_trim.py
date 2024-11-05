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

class AleStructuredMeshTrim(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_TRIM keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_TRIM"

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
                        kwargs.get("mshid", 0)
                    ),
                    Field(
                        "option",
                        str,
                        10,
                        10,
                        kwargs.get("option", "PARTSET")
                    ),
                    Field(
                        "oper",
                        int,
                        20,
                        10,
                        kwargs.get("oper", 0)
                    ),
                    Field(
                        "ioutin",
                        int,
                        30,
                        10,
                        kwargs.get("ioutin", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        40,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "dist",
                        float,
                        50,
                        10,
                        kwargs.get("dist")
                    ),
                    Field(
                        "e3",
                        float,
                        60,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "e4",
                        float,
                        70,
                        10,
                        kwargs.get("e4")
                    ),
                ],
            ),
        ]

    @property
    def mshid(self) -> int:
        """Get or set the S-ALE Mesh ID. The ID of the Structured ALE mesh to be trimed/un-trimed.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        self._cards[0].set_value("mshid", value)

    @property
    def option(self) -> str:
        """Get or set the There are six available options. They are trim by: PARTSET, SEGSET, PLANE,
        CYLINDER, BOXCOR, BOXCPT and SPHERE.  See the table below for more details.
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["PARTSET", "SEGSET", "PLANE", "CYLINDER", "BOXCOR", "BOXCPT", "SPHERE"]:
            raise Exception("""option must be one of {"PARTSET","SEGSET","PLANE","CYLINDER","BOXCOR","BOXCPT","SPHERE"}""")
        self._cards[0].set_value("option", value)

    @property
    def oper(self) -> int:
        """Get or set the To trim or un-trim, that is, to delete the picked elements or keep them.
        EQ.0:	trim (default)
        EQ.1:	keep.
        """ # nopep8
        return self._cards[0].get_value("oper")

    @oper.setter
    def oper(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""oper must be one of {0,1}""")
        self._cards[0].set_value("oper", value)

    @property
    def ioutin(self) -> int:
        """Get or set the Flag to select which elements to trim, that is, "outside" or "inside" the specified object defined with the OPTION and En.
        For PARTSET and SEGSET options, "outside" is defined as the region to which the segment normal points.
        EQ.0:	outside (default)
        EQ.1:	inside.
        """ # nopep8
        return self._cards[0].get_value("ioutin")

    @ioutin.setter
    def ioutin(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ioutin must be one of {0,1}""")
        self._cards[0].set_value("ioutin", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the shell part set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def dist(self) -> typing.Optional[float]:
        """Get or set the the distance.  Elements farther away than the distance in the direction of the shell normal vectors (depending on the value of IOUTIN) are deleted/kept.
        Please note, only elements on one side will be deleted.
        To delete the elements on both sides, repeat the card with the IOUTIN value reversed.
        """ # nopep8
        return self._cards[0].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[0].set_value("dist", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the -.
        """ # nopep8
        return self._cards[0].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        self._cards[0].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the -.
        """ # nopep8
        return self._cards[0].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        self._cards[0].set_value("e4", value)

