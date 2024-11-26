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

class PartDuplicate(KeywordBase):
    """DYNA PART_DUPLICATE keyword"""

    keyword = "PART"
    subkeyword = "DUPLICATE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ptype",
                        str,
                        0,
                        10,
                        kwargs.get("ptype", "PART")
                    ),
                    Field(
                        "typeid",
                        int,
                        10,
                        10,
                        kwargs.get("typeid")
                    ),
                    Field(
                        "idpoff",
                        int,
                        20,
                        10,
                        kwargs.get("idpoff", 0)
                    ),
                    Field(
                        "ideoff",
                        int,
                        30,
                        10,
                        kwargs.get("ideoff", 0)
                    ),
                    Field(
                        "idnoff",
                        int,
                        40,
                        10,
                        kwargs.get("idnoff", 0)
                    ),
                    Field(
                        "tranid",
                        int,
                        50,
                        10,
                        kwargs.get("tranid", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        60,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "zmin",
                        float,
                        70,
                        10,
                        kwargs.get("zmin", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ptype(self) -> str:
        """Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: str) -> None:
        if value not in ["PART", "PSET"]:
            raise Exception("""ptype must be one of {"PART","PSET"}""")
        self._cards[0].set_value("ptype", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the ID of part or part set to be duplicated.
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        self._cards[0].set_value("typeid", value)

    @property
    def idpoff(self) -> int:
        """Get or set the ID offset of newly created parts
        """ # nopep8
        return self._cards[0].get_value("idpoff")

    @idpoff.setter
    def idpoff(self, value: int) -> None:
        self._cards[0].set_value("idpoff", value)

    @property
    def ideoff(self) -> int:
        """Get or set the ID offset of newly created elements.
        """ # nopep8
        return self._cards[0].get_value("ideoff")

    @ideoff.setter
    def ideoff(self, value: int) -> None:
        self._cards[0].set_value("ideoff", value)

    @property
    def idnoff(self) -> int:
        """Get or set the ID offset of newly created nodes
        """ # nopep8
        return self._cards[0].get_value("idnoff")

    @idnoff.setter
    def idnoff(self, value: int) -> None:
        self._cards[0].set_value("idnoff", value)

    @property
    def tranid(self) -> int:
        """Get or set the ID of *DEFINE_TRANSFORMATION to transform the existing nodes in a part or part set..
        """ # nopep8
        return self._cards[0].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        self._cards[0].set_value("tranid", value)

    @property
    def boxid(self) -> int:
        """Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def zmin(self) -> float:
        """Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
        """ # nopep8
        return self._cards[0].get_value("zmin")

    @zmin.setter
    def zmin(self, value: float) -> None:
        self._cards[0].set_value("zmin", value)

