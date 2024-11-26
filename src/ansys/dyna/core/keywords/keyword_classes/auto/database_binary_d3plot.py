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

class DatabaseBinaryD3Plot(KeywordBase):
    """DYNA DATABASE_BINARY_D3PLOT keyword"""

    keyword = "DATABASE"
    subkeyword = "BINARY_D3PLOT"

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
                        "lcdt",
                        int,
                        10,
                        10,
                        kwargs.get("lcdt")
                    ),
                    Field(
                        "beam",
                        int,
                        20,
                        10,
                        kwargs.get("beam", 0)
                    ),
                    Field(
                        "npltc",
                        int,
                        30,
                        10,
                        kwargs.get("npltc")
                    ),
                    Field(
                        "psetid",
                        int,
                        40,
                        10,
                        kwargs.get("psetid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ioopt",
                        int,
                        0,
                        10,
                        kwargs.get("ioopt", 0)
                    ),
                    Field(
                        "rate",
                        float,
                        10,
                        10,
                        kwargs.get("rate")
                    ),
                    Field(
                        "cutoff",
                        float,
                        20,
                        10,
                        kwargs.get("cutoff")
                    ),
                    Field(
                        "window",
                        float,
                        30,
                        10,
                        kwargs.get("window")
                    ),
                    Field(
                        "type",
                        int,
                        40,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "pset",
                        int,
                        50,
                        10,
                        kwargs.get("pset", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the This field defines the time interval between output states, DT, for all options except D3DUMP, RUNRSF, and D3DRLF.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def lcdt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID specifying the output time interval as a function of time. This variable is only available for options D3PLOT, D3PART, D3THDT, INTFOR and BLSTFOR.
        """ # nopep8
        return self._cards[0].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        self._cards[0].set_value("lcdt", value)

    @property
    def beam(self) -> int:
        """Get or set the Discrete element option flag (*DATABASE_‌BINARY_‌D3PLOT only):
        EQ.0:	Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements.The discrete elements’ global x, global y, global zand resultant forces(moments) and change in length(rotation) are written to the database where LS - PrePost(incorrectly) labels them as though they were beam quantities, such as axial force, S - shear resultant, T - shear resultant, etc.
        EQ.1 : No discrete spring, damperand seatbelt elements are added to the d3plot database.This option is useful when translating old LS - DYNA input decks to KEYWORD input.In older input decks there is no requirement that beam and spring elements have unique IDs,and beam elements may be created for the springand dampers with identical IDs to existing beam elements causing a fatal error.However, this option comes with some limitationsand, therefore, should be used with caution.
        Contact interfaces which are based on part IDs of seatbelt elements will not be properly generated if this option is used.
        DEFORMABLE_TO_RIGID will not work if PID refers to discrete, damper, or seatbelt elements.
        EQ.2 : Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements(similar to option 0).In this option the element resultant force is written to its first database position allowing beam axial forces and spring resultant forces to be plotted at the same time.This can be useful during some post - processing applications.
        This flag, set in* DATABASE_BINARY_D3PLOT, also affects the display of discrete elements in several other databases, such as d3drlfand d3part.
        """ # nopep8
        return self._cards[0].get_value("beam")

    @beam.setter
    def beam(self, value: int) -> None:
        self._cards[0].set_value("beam", value)

    @property
    def npltc(self) -> typing.Optional[int]:
        """Get or set the DT=ENDTIM/NPLTC.  Applies to D3PLOT, D3PART, DEMFOR, and INTFOR options only.  This overrides the DT specified in the first field. ENDTIM is specified in *CONTROL_TERMINATION
        """ # nopep8
        return self._cards[0].get_value("npltc")

    @npltc.setter
    def npltc(self, value: int) -> None:
        self._cards[0].set_value("npltc", value)

    @property
    def psetid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for D3PART and D3PLOT options only.  See *SET_‌PART.  Parts in PSETID will excluded in the d3plot database.  Only parts in PSETID are included in the d3part database.
        """ # nopep8
        return self._cards[0].get_value("psetid")

    @psetid.setter
    def psetid(self, value: int) -> None:
        self._cards[0].set_value("psetid", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of plot frequency load curve:
        EQ.1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time.(this is the default behavior).
        EQ 2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at time T.
        EQ 3: A plot is generated for each ordinate point in the load curve definition. The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[1].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""ioopt must be one of {0,1,2,3}""")
        self._cards[1].set_value("ioopt", value)

    @property
    def rate(self) -> typing.Optional[float]:
        """Get or set the Time interval T between filter sampling.  See Remark 7
        """ # nopep8
        return self._cards[1].get_value("rate")

    @rate.setter
    def rate(self, value: float) -> None:
        self._cards[1].set_value("rate", value)

    @property
    def cutoff(self) -> typing.Optional[float]:
        """Get or set the Frequency cut-off  in Hz.  See Remark 7
        """ # nopep8
        return self._cards[1].get_value("cutoff")

    @cutoff.setter
    def cutoff(self, value: float) -> None:
        self._cards[1].set_value("cutoff", value)

    @property
    def window(self) -> typing.Optional[float]:
        """Get or set the The width of the window W in units of time for storing the single, forward filtering required for the TYPE = 2 filter option.
        Increasing the width of the window will increase the memory required for the analysis.
        A window that is too narrow will reduce the amplitude of the filtered result significantly, and values below 15 are not recommended for that reason.
        In general, the results for the TYPE = 2 option are sensitive to the width of the window and experimentation is required.  See Remark 7.
        """ # nopep8
        return self._cards[1].get_value("window")

    @window.setter
    def window(self, value: float) -> None:
        self._cards[1].set_value("window", value)

    @property
    def type(self) -> int:
        """Get or set the Flag for filtering options.  See Remark 7.
        EQ.0:	No filtering (default).
        EQ.1:	Single pass, forward Butterworth filtering.
        EQ.2:	Two pass filtering over the specified time window. Backward Butterworth filtering is applied to the forward Butterworth results that have been stored.
        This option improves the phase accuracy significantly at the expense of memory
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""type must be one of {0,1,2}""")
        self._cards[1].set_value("type", value)

    @property
    def pset(self) -> int:
        """Get or set the Part set ID for filtering.  If no set is specified, all parts are included.  For each element integration point in the d3plot file,
        24 words of memory are required in LS-DYNA for the single pass filtering, and more for the two pass filtering.
        Specifying PSET is recommended to minimize the memory requirements.  See Remark 7.
        """ # nopep8
        return self._cards[1].get_value("pset")

    @pset.setter
    def pset(self, value: int) -> None:
        self._cards[1].set_value("pset", value)

