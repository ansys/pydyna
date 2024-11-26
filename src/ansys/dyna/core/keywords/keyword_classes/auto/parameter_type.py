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

class ParameterType(KeywordBase):
    """DYNA PARAMETER_TYPE keyword"""

    keyword = "PARAMETER"
    subkeyword = "TYPE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "prmr",
                        str,
                        0,
                        10,
                        kwargs.get("prmr")
                    ),
                    Field(
                        "val",
                        int,
                        10,
                        10,
                        kwargs.get("val")
                    ),
                    Field(
                        "prtyp",
                        str,
                        20,
                        10,
                        kwargs.get("prtyp", "   ")
                    ),
                ],
            ),
        ]

    @property
    def prmr(self) -> typing.Optional[str]:
        """Get or set the Define the nth parameter in a field of 10.
        """ # nopep8
        return self._cards[0].get_value("prmr")

    @prmr.setter
    def prmr(self, value: str) -> None:
        self._cards[0].set_value("prmr", value)

    @property
    def val(self) -> typing.Optional[int]:
        """Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with preceding definition for PRMRn
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: int) -> None:
        self._cards[0].set_value("val", value)

    @property
    def prtyp(self) -> str:
        """Get or set the The TYPE references the entity allowing for improved LSPrepost/HM renumbering
        """ # nopep8
        return self._cards[0].get_value("prtyp")

    @prtyp.setter
    def prtyp(self, value: str) -> None:
        if value not in ["   ", "SSID", "PSID", "PID", "NSID", "NID", "BEAMID", "SHELLID", "SOLIDID", "TSHELLID", "MID", "BEAMSID", "SHELLSID", "SOLIDSID", "TSHELLSID", "EOSID", "CURVEID"]:
            raise Exception("""prtyp must be one of {"   ","SSID","PSID","PID","NSID","NID","BEAMID","SHELLID","SOLIDID","TSHELLID","MID","BEAMSID","SHELLSID","SOLIDSID","TSHELLSID","EOSID","CURVEID"}""")
        self._cards[0].set_value("prtyp", value)

