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

class ControlImplicitModalDynamic(KeywordBase):
    """DYNA CONTROL_IMPLICIT_MODAL_DYNAMIC keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_MODAL_DYNAMIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mdflag",
                        int,
                        0,
                        10,
                        kwargs.get("mdflag", 0)
                    ),
                    Field(
                        "zeta",
                        float,
                        10,
                        10,
                        kwargs.get("zeta")
                    ),
                    Field(
                        "md_strs",
                        int,
                        20,
                        10,
                        kwargs.get("md_strs")
                    ),
                    Field(
                        "dtout",
                        float,
                        30,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "integ",
                        int,
                        40,
                        10,
                        kwargs.get("integ", 0)
                    ),
                    Field(
                        "nsid",
                        int,
                        50,
                        10,
                        kwargs.get("nsid")
                    ),
                ],
            ),
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
                        "filename2",
                        str,
                        0,
                        80,
                        kwargs.get("filename2")
                    ),
                ],
            ),
        ]

    @property
    def mdflag(self) -> int:
        """Get or set the Modal Dynamic flag
        EQ.0: no modal dynamic analysis
        EQ.1: perform modal dynamic analysis.
        EQ.2:	perform modal dynamic analysis with prescribed motion constraints on the constraint modes input with Card 3.  See Remark 7
        """ # nopep8
        return self._cards[0].get_value("mdflag")

    @mdflag.setter
    def mdflag(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""mdflag must be one of {0,1,2}""")
        self._cards[0].set_value("mdflag", value)

    @property
    def zeta(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping constant.
        """ # nopep8
        return self._cards[0].get_value("zeta")

    @zeta.setter
    def zeta(self, value: float) -> None:
        self._cards[0].set_value("zeta", value)

    @property
    def md_strs(self) -> typing.Optional[int]:
        """Get or set the Calculate modal dynamic stress.
        """ # nopep8
        return self._cards[0].get_value("md_strs")

    @md_strs.setter
    def md_strs(self, value: int) -> None:
        self._cards[0].set_value("md_strs", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Modal dynamics output interval.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def integ(self) -> int:
        """Get or set the Integration method
        EQ.0:	defaults to 1.
        EQ.1:	perform modal dynamic analysis with explicit time integration.
        EQ.2:	perform modal dynamic analysis with implicit time integration..
        """ # nopep8
        return self._cards[0].get_value("integ")

    @integ.setter
    def integ(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""integ must be one of {0,1,2}""")
        self._cards[0].set_value("integ", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the The node set ID of the nodes in the modal model that are subjected to loads. If the set is not specified, then the forces are summed over all the nodes, and that is usually much more expensive than summing over only those subjected to a load..
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the If specified the eigenmodes are read from the specified file. Otherwise
        the eigenmodes are computed as specified on *CONTROL_IMPLICIT_	EIGENVALUE.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

    @property
    def filename2(self) -> typing.Optional[str]:
        """Get or set the If specified the eigenmodes are read from the specified file. Otherwise
        the eigenmodes are computed as specified on *CONTROL_IMPLICIT_	EIGENVALUE.
        """ # nopep8
        return self._cards[2].get_value("filename2")

    @filename2.setter
    def filename2(self, value: str) -> None:
        self._cards[2].set_value("filename2", value)

