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

class ControlCpm(KeywordBase):
    """DYNA CONTROL_CPM keyword"""

    keyword = "CONTROL"
    subkeyword = "CPM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cpmout",
                        int,
                        0,
                        10,
                        kwargs.get("cpmout", 11)
                    ),
                    Field(
                        "np2p",
                        int,
                        10,
                        10,
                        kwargs.get("np2p", 5)
                    ),
                    Field(
                        "ncpmts",
                        int,
                        20,
                        10,
                        kwargs.get("ncpmts", 0)
                    ),
                    Field(
                        "cpmerr",
                        int,
                        30,
                        10,
                        kwargs.get("cpmerr", 0)
                    ),
                    Field(
                        "sffdc",
                        float,
                        40,
                        10,
                        kwargs.get("sffdc", 1.0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "cpmmf",
                        int,
                        60,
                        10,
                        kwargs.get("cpmmf", 0)
                    ),
                ],
            ),
        ]

    @property
    def cpmout(self) -> int:
        """Get or set the Control CPM output database to d3plot
        EQ.11: full CPM database in version 3 format (default)
        EQ.21: full CPM database in version 4 format
        EQ.22: CPM coordinates only in version 4 format
        EQ.23: CPM summary only in version 4 format
        """ # nopep8
        return self._cards[0].get_value("cpmout")

    @cpmout.setter
    def cpmout(self, value: int) -> None:
        if value not in [11, 21, 22, 23]:
            raise Exception("""cpmout must be one of {11,21,22,23}""")
        self._cards[0].set_value("cpmout", value)

    @property
    def np2p(self) -> int:
        """Get or set the Number of cycles for repartition particle among processors. This option is only used in LS-DYNA/MPP. (Default=5)
        """ # nopep8
        return self._cards[0].get_value("np2p")

    @np2p.setter
    def np2p(self, value: int) -> None:
        self._cards[0].set_value("np2p", value)

    @property
    def ncpmts(self) -> int:
        """Get or set the Time step size estimation:
        EQ.0: not consider CPM (default)
        EQ.1: use 1 micro-second as CPM time step size. This provides a better time step size if the model is made by rigid body
        """ # nopep8
        return self._cards[0].get_value("ncpmts")

    @ncpmts.setter
    def ncpmts(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ncpmts must be one of {0,1}""")
        self._cards[0].set_value("ncpmts", value)

    @property
    def cpmerr(self) -> int:
        """Get or set the EQ.0: disable checking and only output warning messages (Default)
        EQ.1: enable error checking. If it detects any problem, the code
        will error terminate the job, or try to fix the problem. Activated checks include:
        1.  Airbag integrity (see Remark 2)
        2.  Chamber integrity: this step applies the airbag in	tegrity check to the chamber.
        3.  Inconsistent orientation between the shell reference geometry and FEM shell connectivity
        """ # nopep8
        return self._cards[0].get_value("cpmerr")

    @cpmerr.setter
    def cpmerr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cpmerr must be one of {0,1}""")
        self._cards[0].set_value("cpmerr", value)

    @property
    def sffdc(self) -> float:
        """Get or set the Scale factor for the force decay constant. The default value is 1.0 and allowable arrange is [0.01,100.0].
        """ # nopep8
        return self._cards[0].get_value("sffdc")

    @sffdc.setter
    def sffdc(self, value: float) -> None:
        self._cards[0].set_value("sffdc", value)

    @property
    def cpmmf(self) -> int:
        """Get or set the Flag to consider airbag system velocity based on the coordinates system defined by fields NID1, NID2, and NID3 on *AIRBAG_PARTICLE:
        EQ.0:	no(default)
        EQ.1 : yes.The flow energy from the rigid body motion is fed back to the CPM particles.
        """ # nopep8
        return self._cards[0].get_value("cpmmf")

    @cpmmf.setter
    def cpmmf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cpmmf must be one of {0,1}""")
        self._cards[0].set_value("cpmmf", value)

