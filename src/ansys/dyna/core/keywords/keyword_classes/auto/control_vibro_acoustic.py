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

class ControlVibroAcoustic(KeywordBase):
    """DYNA CONTROL_VIBRO_ACOUSTIC keyword"""

    keyword = "CONTROL"
    subkeyword = "VIBRO_ACOUSTIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vaflag",
                        int,
                        0,
                        10,
                        kwargs.get("vaflag", 0)
                    ),
                    Field(
                        "vaprld",
                        int,
                        10,
                        10,
                        kwargs.get("vaprld", 0)
                    ),
                    Field(
                        "vastrs",
                        int,
                        20,
                        10,
                        kwargs.get("vastrs", 0)
                    ),
                    Field(
                        "vapsd",
                        int,
                        30,
                        10,
                        kwargs.get("vapsd", 0)
                    ),
                    Field(
                        "varms",
                        int,
                        40,
                        10,
                        kwargs.get("varms", 0)
                    ),
                    Field(
                        "vaplot",
                        int,
                        50,
                        10,
                        kwargs.get("vaplot", 0)
                    ),
                    Field(
                        "ipanelu",
                        int,
                        60,
                        10,
                        kwargs.get("ipanelu")
                    ),
                    Field(
                        "ipanelv",
                        int,
                        70,
                        10,
                        kwargs.get("ipanelv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "restart",
                        int,
                        0,
                        10,
                        kwargs.get("restart", 0)
                    ),
                    Field(
                        "nmodstr",
                        int,
                        10,
                        10,
                        kwargs.get("nmodstr")
                    ),
                ],
            ),
        ]

    @property
    def vaflag(self) -> int:
        """Get or set the Loading type:
        EQ.0: No vibro-acoustic structural analysis.
        EQ.1: Base acceleration.
        EQ.2: Random pressure.
        EQ.3: Plane wave.
        EQ.4: Shock.
        EQ.5: Progressive wave.
        EQ.6: Reverberant wave.
        EQ.7: Turbulent boundary layer.
        EQ.8: Nodal force.
        EQ.9: Modal stresses/strains output only.
        """ # nopep8
        return self._cards[0].get_value("vaflag")

    @vaflag.setter
    def vaflag(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""vaflag must be one of {0,1,2,3,4,5,6,7,8,9}""")
        self._cards[0].set_value("vaflag", value)

    @property
    def vaprld(self) -> int:
        """Get or set the Flag for including preload:
        EQ.0: No preload.
        EQ.1: Thermal preload due to temperature difference from the neutral temperature.
        EQ.2: Mechanical preload due to static pressure.
        EQ.3: Mechanical preload due to concentrated nodal force
        """ # nopep8
        return self._cards[0].get_value("vaprld")

    @vaprld.setter
    def vaprld(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""vaprld must be one of {0,1,2,3}""")
        self._cards[0].set_value("vaprld", value)

    @property
    def vastrs(self) -> int:
        """Get or set the Flag for including stress analysis:
        EQ.0: No stress analysis, only displacement analysis is requested.
        EQ.1: Both stress and displacement analyses are requested.
        """ # nopep8
        return self._cards[0].get_value("vastrs")

    @vastrs.setter
    def vastrs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""vastrs must be one of {0,1}""")
        self._cards[0].set_value("vastrs", value)

    @property
    def vapsd(self) -> int:
        """Get or set the Flag for PSD output:
        EQ.0: No PSD output is requested.
        EQ.1: PSD output is requested
        """ # nopep8
        return self._cards[0].get_value("vapsd")

    @vapsd.setter
    def vapsd(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""vapsd must be one of {0,1}""")
        self._cards[0].set_value("vapsd", value)

    @property
    def varms(self) -> int:
        """Get or set the Flag for RMS output:
        EQ.0: No RMS output is requested.
        EQ.1: RMS output is requested.
        """ # nopep8
        return self._cards[0].get_value("varms")

    @varms.setter
    def varms(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""varms must be one of {0,1}""")
        self._cards[0].set_value("varms", value)

    @property
    def vaplot(self) -> int:
        """Get or set the Flag for PSD broadband plots:
        EQ.0: No PSD broadband plot is requested.
        EQ.1: PSD broadband plots are requested.
        """ # nopep8
        return self._cards[0].get_value("vaplot")

    @vaplot.setter
    def vaplot(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""vaplot must be one of {0,1}""")
        self._cards[0].set_value("vaplot", value)

    @property
    def ipanelu(self) -> typing.Optional[int]:
        """Get or set the Number of strips in U direction
        """ # nopep8
        return self._cards[0].get_value("ipanelu")

    @ipanelu.setter
    def ipanelu(self, value: int) -> None:
        self._cards[0].set_value("ipanelu", value)

    @property
    def ipanelv(self) -> typing.Optional[int]:
        """Get or set the Number of strips in V direction
        """ # nopep8
        return self._cards[0].get_value("ipanelv")

    @ipanelv.setter
    def ipanelv(self, value: int) -> None:
        self._cards[0].set_value("ipanelv", value)

    @property
    def restart(self) -> int:
        """Get or set the EQ.0: No restart will be requested. All intermediate output is deleted.
        EQ.1: Intermediate output is retained for restart.
        EQ.2: Restart based on intermediate output in last run. All intermediate output is deleted after the current run.
        EQ.3: Restart based on intermediate output in last run. All intermediate output is retained for next restart run..
        """ # nopep8
        return self._cards[1].get_value("restart")

    @restart.setter
    def restart(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""restart must be one of {0,1,2,3}""")
        self._cards[1].set_value("restart", value)

    @property
    def nmodstr(self) -> typing.Optional[int]:
        """Get or set the Number of modes in modal stresses/strains output.
        """ # nopep8
        return self._cards[1].get_value("nmodstr")

    @nmodstr.setter
    def nmodstr(self, value: int) -> None:
        self._cards[1].set_value("nmodstr", value)

