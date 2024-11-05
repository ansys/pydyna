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

class EmIsopotential(KeywordBase):
    """DYNA EM_ISOPOTENTIAL keyword"""

    keyword = "EM"
    subkeyword = "ISOPOTENTIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "isoid",
                        int,
                        0,
                        10,
                        kwargs.get("isoid")
                    ),
                    Field(
                        "settype",
                        int,
                        10,
                        10,
                        kwargs.get("settype", 1)
                    ),
                    Field(
                        "setid",
                        int,
                        20,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "rdltype",
                        int,
                        30,
                        10,
                        kwargs.get("rdltype", 0)
                    ),
                ],
            ),
        ]

    @property
    def isoid(self) -> typing.Optional[int]:
        """Get or set the ID of the Isopotential.
        """ # nopep8
        return self._cards[0].get_value("isoid")

    @isoid.setter
    def isoid(self, value: int) -> None:
        self._cards[0].set_value("isoid", value)

    @property
    def settype(self) -> int:
        """Get or set the Set type:
        EQ.1:Segment Set.
        EQ.2:Node Set.
        EQ.3:Fluid surface part. See *ICFD_PART.
        """ # nopep8
        return self._cards[0].get_value("settype")

    @settype.setter
    def settype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""settype must be one of {1,2,3}""")
        self._cards[0].set_value("settype", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def rdltype(self) -> int:
        """Get or set the Used for the application: composite Tshell battery, with *EM_RANDLES_TSHELL.Selects which layers of the underlying EM mesh is included in the isopotential:
        EQ.0: Default. No specific treatment.
        EQ.1: Current Collector Positive.
        EQ.2: Positive Electrode.
        EQ.3: Separator.
        EQ.4: Negative Electrode.
        EQ.5: Current Collector Negative.
        The layers functions are defined in *EM_MAT_001.
        """ # nopep8
        return self._cards[0].get_value("rdltype")

    @rdltype.setter
    def rdltype(self, value: int) -> None:
        self._cards[0].set_value("rdltype", value)

