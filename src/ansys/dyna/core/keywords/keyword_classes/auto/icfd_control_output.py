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

class IcfdControlOutput(KeywordBase):
    """DYNA ICFD_CONTROL_OUTPUT keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "msgl",
                        int,
                        0,
                        10,
                        kwargs.get("msgl", 0)
                    ),
                    Field(
                        "outl",
                        int,
                        10,
                        10,
                        kwargs.get("outl", 0)
                    ),
                    Field(
                        "dtout",
                        float,
                        20,
                        10,
                        kwargs.get("dtout", 0.0)
                    ),
                    Field(
                        "lsppout",
                        int,
                        30,
                        10,
                        kwargs.get("lsppout", 1)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "itout",
                        int,
                        50,
                        10,
                        kwargs.get("itout", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pitout",
                        int,
                        0,
                        10,
                        kwargs.get("pitout")
                    ),
                ],
            ),
        ]

    @property
    def msgl(self) -> int:
        """Get or set the Message level.
        EQ. 0: only time step information is output.
        EQ. 1: first level solver information.
        EQ. 2: full output information with details about linear algebra and convergence steps.
        EQ.4:	full output information is also copied to the message file
        """ # nopep8
        return self._cards[0].get_value("msgl")

    @msgl.setter
    def msgl(self, value: int) -> None:
        if value not in [0, 1, 2, 4]:
            raise Exception("""msgl must be one of {0,1,2,4}""")
        self._cards[0].set_value("msgl", value)

    @property
    def outl(self) -> int:
        """Get or set the Output the fluid results in other file formats apart from d3plot.
        EQ. 0: only d3plot output
        EQ. 2: output a file with mesh statistics and the fluid results in GMV format. A directory named output/gmv has to be created one level above the executable.
        EQ. 6: output a file with mesh statistics and the fluid results in Paraview format. A directory named vtk will be created in the work directory where the output files will be written.
        EQ.7: output a file with mesh statistic and the fluid results in VTU format readable by Paraview. A directory named vtk will be created in the work directory where the output files will be written.
        """ # nopep8
        return self._cards[0].get_value("outl")

    @outl.setter
    def outl(self, value: int) -> None:
        if value not in [0, 2, 6, 7]:
            raise Exception("""outl must be one of {0,2,6,7}""")
        self._cards[0].set_value("outl", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output when OUTL is different than 0.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def lsppout(self) -> int:
        """Get or set the EQ.1:	outputs a file with the automatically created fluid volume mesh in  a format compatible for LSPP at each remesh. Also outputs the fluid volume mesh in a format compatible with a subsequent ICFD analysis.
        EQ.3:	Outputs the fluid volume mesh in a format compatible with a subsequent ICFD analysis at each DTOUT
        """ # nopep8
        return self._cards[0].get_value("lsppout")

    @lsppout.setter
    def lsppout(self, value: int) -> None:
        if value not in [1, 3]:
            raise Exception("""lsppout must be one of {1,3}""")
        self._cards[0].set_value("lsppout", value)

    @property
    def itout(self) -> int:
        """Get or set the Iteration interval to print the output, including the d3plot files when the steady state slover is selected
        """ # nopep8
        return self._cards[0].get_value("itout")

    @itout.setter
    def itout(self, value: int) -> None:
        self._cards[0].set_value("itout", value)

    @property
    def pitout(self) -> typing.Optional[int]:
        """Get or set the Pressure iteration limit output. If the number of pressure iterations in the fractional step solve goes above PITOUT, an extra d3plot will be dumped. This is mainly a debugging feature which can help the user identify problematic areas in the model which often precede a divergence
        """ # nopep8
        return self._cards[1].get_value("pitout")

    @pitout.setter
    def pitout(self, value: int) -> None:
        self._cards[1].set_value("pitout", value)

