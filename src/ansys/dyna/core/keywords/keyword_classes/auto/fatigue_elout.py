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

"""Module providing the FatigueElout class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class FatigueElout(KeywordBase):
    """DYNA FATIGUE_ELOUT keyword"""

    keyword = "FATIGUE"
    subkeyword = "ELOUT"

    def __init__(self, **kwargs):
        """Initialize the FatigueElout class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "strsn",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "index",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "restrt",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "texpos",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dmgmin",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
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
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def strsn(self) -> int:
        """Get or set the Type of fatigue analysis variable:
        EQ.0: Stress (default)
        EQ.1: Strain
        """ # nopep8
        return self._cards[0].get_value("strsn")

    @strsn.setter
    def strsn(self, value: int) -> None:
        """Set the strsn property."""
        if value not in [0, 1, None]:
            raise Exception("""strsn must be `None` or one of {0,1}.""")
        self._cards[0].set_value("strsn", value)

    @property
    def index(self) -> int:
        """Get or set the Stress/strain index for performing fatigue analysis:
        EQ.0: Von-Mises stress/strain
        EQ.1: Maximum principal stress/strain
        EQ.2: Maximum shear stress/strain
        """ # nopep8
        return self._cards[0].get_value("index")

    @index.setter
    def index(self, value: int) -> None:
        """Set the index property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""index must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("index", value)

    @property
    def restrt(self) -> int:
        """Get or set the Restart options. This flag is used to save an LS-DYNA transient
        analysis if the binary database for stress/strain time history data
        has been created in last runs. See Remark 3.
        EQ.0: initial run
        EQ.1: restart with existing stress/strain binary database
        """ # nopep8
        return self._cards[0].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        """Set the restrt property."""
        if value not in [0, 1, None]:
            raise Exception("""restrt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("restrt", value)

    @property
    def texpos(self) -> float:
        """Get or set the Exposure time. If this is 0, the exposure time is the same as ENDTIM in *CONTROL_TERMINATION.
        """ # nopep8
        return self._cards[0].get_value("texpos")

    @texpos.setter
    def texpos(self, value: float) -> None:
        """Set the texpos property."""
        self._cards[0].set_value("texpos", value)

    @property
    def dmgmin(self) -> float:
        """Get or set the Minimum fatigue damage ratio for parts undergoing fatigue analysis:
        EQ.0:	no change on computed fatigue damage ratio
        LT.0 : for each part, the minimum fatigue damage ratio dumped to D3FTG is | DMGMIN | x the computed nonzero minimum fatigue damage ratio computed on the current part.
        GT.0 : for each part, the minimum fatigue damage ratio dumped to D3FTG is DMGMIN.
        """ # nopep8
        return self._cards[0].get_value("dmgmin")

    @dmgmin.setter
    def dmgmin(self, value: float) -> None:
        """Set the dmgmin property."""
        self._cards[0].set_value("dmgmin", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Time step for saving the stress/strain data in transient analysis
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

