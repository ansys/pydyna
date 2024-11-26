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

class ControlFormingBestfit(KeywordBase):
    """DYNA CONTROL_FORMING_BESTFIT keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_BESTFIT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ifit",
                        int,
                        0,
                        10,
                        kwargs.get("ifit", 0)
                    ),
                    Field(
                        "nskip",
                        int,
                        10,
                        10,
                        kwargs.get("nskip", -3)
                    ),
                    Field(
                        "gaponly",
                        int,
                        20,
                        10,
                        kwargs.get("gaponly", 0)
                    ),
                    Field(
                        "ifast",
                        int,
                        30,
                        10,
                        kwargs.get("ifast", 1)
                    ),
                    Field(
                        "ifset",
                        int,
                        40,
                        10,
                        kwargs.get("ifset", 0)
                    ),
                    Field(
                        "nsets",
                        int,
                        50,
                        10,
                        kwargs.get("nsets")
                    ),
                    Field(
                        "nsett",
                        int,
                        60,
                        10,
                        kwargs.get("nsett")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        256,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

    @property
    def ifit(self) -> int:
        """Get or set the Best fit program activation flag:
        IFIT.EQ.0:	do not perform best-fit.
        IFIT.EQ.1:	activate the best-fit program.
        """ # nopep8
        return self._cards[0].get_value("ifit")

    @ifit.setter
    def ifit(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ifit must be one of {0,1}""")
        self._cards[0].set_value("ifit", value)

    @property
    def nskip(self) -> int:
        """Get or set the Number of nodes to skip in bucket searching.  NSKIP of “1” does not skip any nodes in searching therefore computing speed is the slowest but accuracy is the highest.  Higher values of NSKIP speed up the calculation time with slightly deteriorating accuracies.  NSKIP of “2” is recommended with IFAST=1.  See Table 0-1 for the effect of NSKIP on the accuracy of the fitting.
        """ # nopep8
        return self._cards[0].get_value("nskip")

    @nskip.setter
    def nskip(self, value: int) -> None:
        self._cards[0].set_value("nskip", value)

    @property
    def gaponly(self) -> int:
        """Get or set the Separation distance calculation flag:
        GAPONLY.EQ.0:	perform best-fit, calculate separation distances between the two best-fitted mesh parts.
        GAPONLY.EQ.1:	no best-fit, just calculate separation distances between the two existing mesh parts.
        GAPONLY.EQ.2:	User is responsible to move the parts closer in distance and orientation, in situation where target and source are not similar in shape.  Also see NSETS and NSETT (recommended method).
        """ # nopep8
        return self._cards[0].get_value("gaponly")

    @gaponly.setter
    def gaponly(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""gaponly must be one of {0,1,2}""")
        self._cards[0].set_value("gaponly", value)

    @property
    def ifast(self) -> int:
        """Get or set the Computing performance optimization flag:
        IFAST.EQ.0:	no computing speed optimization.
        IFAST.EQ.1:	activate computing speed optimization, and is recommended.
        """ # nopep8
        return self._cards[0].get_value("ifast")

    @ifast.setter
    def ifast(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""ifast must be one of {1,0}""")
        self._cards[0].set_value("ifast", value)

    @property
    def ifset(self) -> int:
        """Get or set the Optional flag to define a node set to be included or excluded in the best fitting.  The node set can be defined in a file that also includes the part to be best fitted to the target mesh.  Only keyword cards *NODE, *ELEMENT_SHELL and *CONSTRAINED_ADAPTIVITY (if applicable) need to be present in the file. The file can be included in an input deck (Example 1) using *INCLUDE.  A node set can be defined using LS-PrePost via menu options Model→CreEnt→Set Data→*SET_NODE→Cre.
        IFSET.EQ.0:	all nodes in the included file will be best fitted.
        IFSET.GT.0:	the input value is a node set ID; only the nodes in the set will be best fitted.
        IFSET.LT.0:	the absolute value is a node set ID; all nodes excluding those in the set will be best fitted.
        """ # nopep8
        return self._cards[0].get_value("ifset")

    @ifset.setter
    def ifset(self, value: int) -> None:
        self._cards[0].set_value("ifset", value)

    @property
    def nsets(self) -> typing.Optional[int]:
        """Get or set the An optional node set ID of three nodes from the source mesh.  The nodes should be selected based on distinctive geometry features, such as, the center of an arc, the center of a dart, or the end node of a take-up bead (see Example 3 and Figure 0-2).  The three nodes must not be aligned in one straight line.  Define NSETS only if the orientation of the source mesh deviates from the target is large (>~more than 30 degrees in any direction).  This is the recommended method.
        """ # nopep8
        return self._cards[0].get_value("nsets")

    @nsets.setter
    def nsets(self, value: int) -> None:
        self._cards[0].set_value("nsets", value)

    @property
    def nsett(self) -> typing.Optional[int]:
        """Get or set the An optional node set ID from the target mesh, consists of the corresponding three nodes from the same geometry features of the source mesh.  The three nodes should be input in the same order as those from the source mesh.  Approximate locations are acceptable.  Define NSETT only if NSETS is defined.  See Example 3 and Figure 0-2 for details.  This is the recommended method.
        """ # nopep8
        return self._cards[0].get_value("nsett")

    @nsett.setter
    def nsett(self, value: int) -> None:
        self._cards[0].set_value("nsett", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Target mesh in keyword format, where only *NODE and *ELEMENT_SHELL should be included.  The target mesh is typically the scanned part converted from the STL format file.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

