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

class NodeTransform(KeywordBase):
    """DYNA NODE_TRANSFORM keyword"""

    keyword = "NODE"
    subkeyword = "TRANSFORM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "trsid",
                        int,
                        0,
                        10,
                        kwargs.get("trsid")
                    ),
                    Field(
                        "nsid",
                        int,
                        10,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "immed",
                        int,
                        20,
                        10,
                        kwargs.get("immed", 0)
                    ),
                ],
            ),
        ]

    @property
    def trsid(self) -> typing.Optional[int]:
        """Get or set the The ID of the transformation defined under *DEFINE_TRANSFOR-MATION
        """ # nopep8
        return self._cards[0].get_value("trsid")

    @trsid.setter
    def trsid(self, value: int) -> None:
        self._cards[0].set_value("trsid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of the set of nodes to be subject to the transformation.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def immed(self) -> int:
        """Get or set the Optional flag for transformation processing :
        EQ.0:	Node transformation is performed after all input cards are read through.It is more efficient,and the definition sequence of  NODE_TRANSFORMand its NSID is irrelevant, i.e., the referred NSID doesn�t have to be defined prior to* NODE_TRANSFORM.However, for example, if nodes in NSID are used in POS6N of* DEFINE_TRANSFORMATION, its original coordinates, not the transformed coordinates, will be used to define the transformation matrix.
        EQ.1 : Node transformation is performed immediately after * NODE_TRANSFORM is read.The referred NSID and its nodes have to be defined prior to * NODE_TRANSFORM
        """ # nopep8
        return self._cards[0].get_value("immed")

    @immed.setter
    def immed(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""immed must be one of {0,1}""")
        self._cards[0].set_value("immed", value)

