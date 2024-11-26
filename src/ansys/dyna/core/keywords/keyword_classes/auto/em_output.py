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

class EmOutput(KeywordBase):
    """DYNA EM_OUTPUT keyword"""

    keyword = "EM"
    subkeyword = "OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mats",
                        int,
                        0,
                        10,
                        kwargs.get("mats", 0)
                    ),
                    Field(
                        "matf",
                        int,
                        10,
                        10,
                        kwargs.get("matf", 0)
                    ),
                    Field(
                        "sols",
                        int,
                        20,
                        10,
                        kwargs.get("sols", 0)
                    ),
                    Field(
                        "solf",
                        int,
                        30,
                        10,
                        kwargs.get("solf", 0)
                    ),
                    Field(
                        "mesh",
                        int,
                        40,
                        10,
                        kwargs.get("mesh", 0)
                    ),
                    Field(
                        "mem",
                        int,
                        50,
                        10,
                        kwargs.get("mem", 0)
                    ),
                    Field(
                        "timing",
                        int,
                        60,
                        10,
                        kwargs.get("timing", 0)
                    ),
                ],
            ),
        ]

    @property
    def mats(self) -> int:
        """Get or set the Level of matrix assembly output to the screen:
        EQ.0: No output
        EQ.1: Basic assembly steps
        EQ.2: Basic assembly steps+percentage completed+final statistics
        EQ.3: Basic assembly steps+percentage completed+statistics at each percentage of completion

        """ # nopep8
        return self._cards[0].get_value("mats")

    @mats.setter
    def mats(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""mats must be one of {0,1,2,3}""")
        self._cards[0].set_value("mats", value)

    @property
    def matf(self) -> int:
        """Get or set the Level of matrix assembly output to the messag file:
        EQ.0: No output
        EQ.1: Basic assembly steps
        EQ.2: Basic assembly steps+percentage completed+final statistics
        EQ.3: Basic assembly steps+percentage completed+statistics at each percentage of completion

        """ # nopep8
        return self._cards[0].get_value("matf")

    @matf.setter
    def matf(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""matf must be one of {0,1,2,3}""")
        self._cards[0].set_value("matf", value)

    @property
    def sols(self) -> int:
        """Get or set the Level of solver output on the screen:
        EQ.0: No output
        EQ.1: Global information at each FEM iteration
        EQ.2: Detailed information at each FEM iteration

        """ # nopep8
        return self._cards[0].get_value("sols")

    @sols.setter
    def sols(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sols must be one of {0,1,2}""")
        self._cards[0].set_value("sols", value)

    @property
    def solf(self) -> int:
        """Get or set the Level of solver output to the messag file:
        EQ.0: No output
        EQ.1: Global information at each FEM iteration
        EQ.2: Detailed information at each FEM iteration

        """ # nopep8
        return self._cards[0].get_value("solf")

    @solf.setter
    def solf(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""solf must be one of {0,1,2}""")
        self._cards[0].set_value("solf", value)

    @property
    def mesh(self) -> int:
        """Get or set the Controls the output of the mesh data to the d3hsp file
        EQ.0: No mesh output
        EQ.1: Mesh info is written to the d3hsp file

        """ # nopep8
        return self._cards[0].get_value("mesh")

    @mesh.setter
    def mesh(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mesh must be one of {0,1}""")
        self._cards[0].set_value("mesh", value)

    @property
    def mem(self) -> int:
        """Get or set the Controls the output of information about the memory used by the EM solve to the messag file:
        EQ. 0 : no memory information written.
        EQ .1  memory information written

        """ # nopep8
        return self._cards[0].get_value("mem")

    @mem.setter
    def mem(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mem must be one of {0,1}""")
        self._cards[0].set_value("mem", value)

    @property
    def timing(self) -> int:
        """Get or set the Controls the output of information about the time spent in the different parts of the EM solver to the messag file
        EQ. 0 : no timing information written.
        EQ. 1 : timing information written

        """ # nopep8
        return self._cards[0].get_value("timing")

    @timing.setter
    def timing(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""timing must be one of {0,1}""")
        self._cards[0].set_value("timing", value)

