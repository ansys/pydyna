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

class AleFsiToLoadNode(KeywordBase):
    """DYNA ALE_FSI_TO_LOAD_NODE keyword"""

    keyword = "ALE"
    subkeyword = "FSI_TO_LOAD_NODE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "nsid",
                        int,
                        10,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "iopt",
                        int,
                        20,
                        10,
                        kwargs.get("iopt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "path",
                        str,
                        0,
                        80,
                        kwargs.get("path")
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Output intervals.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node Set ID.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def iopt(self) -> int:
        """Get or set the Options to create the keyword file alefsiloadnode.k
        EQ.0: The keyword is created at the end of the run by LS-DYNA.
        EQ.1: The database of coupling forces is dumped without the conversion in keyword file at the end of the run. The database is then treated by a program (alefsiloadnode.exe) to write alefsiloadnode.k.
        EQ.2: The database of coupling forces is read back from the temporary files created by IOPT = 1 to directly apply the nodal forces without using *LOAD_NODE. The parameters DT and NSID are not read.
        EQ.3:	A database of coupling accelerations is dumped at the end of the run (see Remark 3).
        EQ.4:	The database of coupling accelerations created by IOPT = 3 (see Remark 3) is read back. The structure meshes can be different.
        The accelerations are interpolated at the nodes provided by NSID. The parameters DT and NSID are read
        """ # nopep8
        return self._cards[0].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""iopt must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("iopt", value)

    @property
    def path(self) -> typing.Optional[str]:
        """Get or set the Path to the directory where the databases are created.
        """ # nopep8
        return self._cards[1].get_value("path")

    @path.setter
    def path(self, value: str) -> None:
        self._cards[1].set_value("path", value)

