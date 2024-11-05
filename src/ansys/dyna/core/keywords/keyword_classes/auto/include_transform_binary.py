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

class IncludeTransformBinary(KeywordBase):
    """DYNA INCLUDE_TRANSFORM_BINARY keyword"""

    keyword = "INCLUDE"
    subkeyword = "TRANSFORM_BINARY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idnoff",
                        int,
                        0,
                        10,
                        kwargs.get("idnoff", 0)
                    ),
                    Field(
                        "ideoff",
                        int,
                        10,
                        10,
                        kwargs.get("ideoff", 0)
                    ),
                    Field(
                        "idpoff",
                        int,
                        20,
                        10,
                        kwargs.get("idpoff", 0)
                    ),
                    Field(
                        "idmoff",
                        int,
                        30,
                        10,
                        kwargs.get("idmoff", 0)
                    ),
                    Field(
                        "idsoff",
                        int,
                        40,
                        10,
                        kwargs.get("idsoff", 0)
                    ),
                    Field(
                        "idfoff",
                        int,
                        50,
                        10,
                        kwargs.get("idfoff", 0)
                    ),
                    Field(
                        "iddoff",
                        int,
                        60,
                        10,
                        kwargs.get("iddoff", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idroff",
                        int,
                        0,
                        10,
                        kwargs.get("idroff", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "prefix",
                        str,
                        20,
                        10,
                        kwargs.get("prefix")
                    ),
                    Field(
                        "suffix",
                        str,
                        30,
                        10,
                        kwargs.get("suffix")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fctmas",
                        float,
                        0,
                        10,
                        kwargs.get("fctmas", 1.0)
                    ),
                    Field(
                        "fcttim",
                        float,
                        10,
                        10,
                        kwargs.get("fcttim", 1.0)
                    ),
                    Field(
                        "fctlen",
                        float,
                        20,
                        10,
                        kwargs.get("fctlen", 1.0)
                    ),
                    Field(
                        "fcttem",
                        str,
                        30,
                        10,
                        kwargs.get("fcttem", "1.0")
                    ),
                    Field(
                        "incout1",
                        int,
                        40,
                        10,
                        kwargs.get("incout1", 1)
                    ),
                    Field(
                        "fctchg",
                        float,
                        50,
                        10,
                        kwargs.get("fctchg")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tranid",
                        int,
                        0,
                        10,
                        kwargs.get("tranid", 0)
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def idnoff(self) -> int:
        """Get or set the Offset to node ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idnoff")

    @idnoff.setter
    def idnoff(self, value: int) -> None:
        self._cards[1].set_value("idnoff", value)

    @property
    def ideoff(self) -> int:
        """Get or set the Offset to element ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("ideoff")

    @ideoff.setter
    def ideoff(self, value: int) -> None:
        self._cards[1].set_value("ideoff", value)

    @property
    def idpoff(self) -> int:
        """Get or set the Offset to part ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idpoff")

    @idpoff.setter
    def idpoff(self, value: int) -> None:
        self._cards[1].set_value("idpoff", value)

    @property
    def idmoff(self) -> int:
        """Get or set the Offset to material ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idmoff")

    @idmoff.setter
    def idmoff(self, value: int) -> None:
        self._cards[1].set_value("idmoff", value)

    @property
    def idsoff(self) -> int:
        """Get or set the Offset to set ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idsoff")

    @idsoff.setter
    def idsoff(self, value: int) -> None:
        self._cards[1].set_value("idsoff", value)

    @property
    def idfoff(self) -> int:
        """Get or set the Offset to function ID, table ID, and curve ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idfoff")

    @idfoff.setter
    def idfoff(self, value: int) -> None:
        self._cards[1].set_value("idfoff", value)

    @property
    def iddoff(self) -> int:
        """Get or set the Offset to any ID defined through *DEFINE except the FUNCTION, TABLE, and CURVE options (see IDFOFF).
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("iddoff")

    @iddoff.setter
    def iddoff(self, value: int) -> None:
        self._cards[1].set_value("iddoff", value)

    @property
    def idroff(self) -> int:
        """Get or set the Used for all offsets except for those listed above.
        """ # nopep8
        return self._cards[2].get_value("idroff")

    @idroff.setter
    def idroff(self, value: int) -> None:
        self._cards[2].set_value("idroff", value)

    @property
    def prefix(self) -> typing.Optional[str]:
        """Get or set the Prefix added to the beginning of the titles/heads defined in the keywords (like *MAT, *PART, *SECTION, *DEFINE, for examples) of the included file.  A dot, "." is automatically added between the prefix and the existing title
        """ # nopep8
        return self._cards[2].get_value("prefix")

    @prefix.setter
    def prefix(self, value: str) -> None:
        self._cards[2].set_value("prefix", value)

    @property
    def suffix(self) -> typing.Optional[str]:
        """Get or set the Suffix added to the end of the titles/heads defined in the keywords of the included file.  A dot, "." is automatically added between the suffix and the existing title
        """ # nopep8
        return self._cards[2].get_value("suffix")

    @suffix.setter
    def suffix(self, value: str) -> None:
        self._cards[2].set_value("suffix", value)

    @property
    def fctmas(self) -> float:
        """Get or set the Mass transformation factor. For example, FCTMAS=1000.0 when the original mass units are in tons and the new unit is kg.
        """ # nopep8
        return self._cards[3].get_value("fctmas")

    @fctmas.setter
    def fctmas(self, value: float) -> None:
        self._cards[3].set_value("fctmas", value)

    @property
    def fcttim(self) -> float:
        """Get or set the Time transformation factor. For example, FCTTIM=0.001 when the original time units are in milliseconds and the new time unit is seconds.
        """ # nopep8
        return self._cards[3].get_value("fcttim")

    @fcttim.setter
    def fcttim(self, value: float) -> None:
        self._cards[3].set_value("fcttim", value)

    @property
    def fctlen(self) -> float:
        """Get or set the Length transformation factor.
        """ # nopep8
        return self._cards[3].get_value("fctlen")

    @fctlen.setter
    def fctlen(self, value: float) -> None:
        self._cards[3].set_value("fctlen", value)

    @property
    def fcttem(self) -> str:
        """Get or set the Temperature transformation factor: F to C (Farenheit to Centigrade), C to F, F to K, K to F, and so on.
        """ # nopep8
        return self._cards[3].get_value("fcttem")

    @fcttem.setter
    def fcttem(self, value: str) -> None:
        self._cards[3].set_value("fcttem", value)

    @property
    def incout1(self) -> int:
        """Get or set the Set to 1 for the creation of a file, DYNA.INC, which contains the transformed data. The data in this file can be used in future include files and should be checked to ensure that all the data was transformed correctly.
        """ # nopep8
        return self._cards[3].get_value("incout1")

    @incout1.setter
    def incout1(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""incout1 must be one of {1,0}""")
        self._cards[3].set_value("incout1", value)

    @property
    def fctchg(self) -> typing.Optional[float]:
        """Get or set the Electric charge transformation factor, currently only applied to piezoelectric material related cards, see *MAT_ADD_PZELECTRIC for details.
        """ # nopep8
        return self._cards[3].get_value("fctchg")

    @fctchg.setter
    def fctchg(self, value: float) -> None:
        self._cards[3].set_value("fctchg", value)

    @property
    def tranid(self) -> int:
        """Get or set the Transformation ID.
        EQ.0: no tranformation will be applied.  See the input *DEFINE_TRANSFORM.
        """ # nopep8
        return self._cards[4].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        self._cards[4].set_value("tranid", value)

