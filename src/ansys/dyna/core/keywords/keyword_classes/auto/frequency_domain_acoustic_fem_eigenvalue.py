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

class FrequencyDomainAcousticFemEigenvalue(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_FEM_EIGENVALUE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_FEM_EIGENVALUE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ro",
                        float,
                        0,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "c",
                        float,
                        10,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "fmin",
                        float,
                        20,
                        10,
                        kwargs.get("fmin")
                    ),
                    Field(
                        "fmax",
                        float,
                        30,
                        10,
                        kwargs.get("fmax")
                    ),
                    Field(
                        "nfreq",
                        int,
                        40,
                        10,
                        kwargs.get("nfreq", 0)
                    ),
                    Field(
                        "dtout",
                        float,
                        50,
                        10,
                        kwargs.get("dtout", 0)
                    ),
                    Field(
                        "tstart",
                        float,
                        60,
                        10,
                        kwargs.get("tstart", 0)
                    ),
                    Field(
                        "pref",
                        float,
                        70,
                        10,
                        kwargs.get("pref", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "fftwin",
                        int,
                        10,
                        10,
                        kwargs.get("fftwin", 0)
                    ),
                    Field(
                        "mixdmp",
                        int,
                        20,
                        10,
                        kwargs.get("mixdmp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "ptyp",
                        int,
                        10,
                        10,
                        kwargs.get("ptyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "styp",
                        int,
                        10,
                        10,
                        kwargs.get("styp", 0)
                    ),
                    Field(
                        "vad",
                        int,
                        20,
                        10,
                        kwargs.get("vad", 0)
                    ),
                    Field(
                        "dof",
                        int,
                        30,
                        10,
                        kwargs.get("dof", 0)
                    ),
                    Field(
                        "lcid1",
                        int,
                        40,
                        10,
                        kwargs.get("lcid1", 0)
                    ),
                    Field(
                        "lcid2",
                        int,
                        50,
                        10,
                        kwargs.get("lcid2", 0)
                    ),
                    Field(
                        "sf",
                        float,
                        60,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "vid",
                        int,
                        70,
                        10,
                        kwargs.get("vid", 0)
                    ),
                ],
            ),
        ]

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Sound speed of the fluid.
        GT.0: real constant sound speed.
        LT.0: |C| is the load curve ID,which defines the frequency dependent complex sound speed.See *FREQUENCY_DOMAIN_ACOUSTIC_SOUND_SPEED.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum value of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        self._cards[0].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum value of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        self._cards[0].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[0].set_value("nfreq", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time step for writing velocity or acceleration in the binary file.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def tstart(self) -> float:
        """Get or set the Start time for recording velocity or acceleration in transient analysis.
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[0].set_value("tstart", value)

    @property
    def pref(self) -> float:
        """Get or set the Reference pressure, for converting the acoustic pressure to dB.
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        self._cards[0].set_value("pref", value)

    @property
    def fftwin(self) -> int:
        """Get or set the FFT windows (Default=0):
        EQ.0:	Rectangular window.
        EQ.1:	Hanning window.
        EQ.2:	Hamming window.
        EQ.3:	Blackman window.
        EQ.4:	Raised cosine window.
        """ # nopep8
        return self._cards[1].get_value("fftwin")

    @fftwin.setter
    def fftwin(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""fftwin must be one of {0,1,2,3,4}""")
        self._cards[1].set_value("fftwin", value)

    @property
    def mixdmp(self) -> int:
        """Get or set the Acoustic stiffness and mass matrices dumping (when using the option EIGENVALUE):
        EQ.0:	no dumping.
        EQ.1:	dumping globally assembled acoustic stiffness and mass matrices in Harwell-Boeing sparse matrix format.
        """ # nopep8
        return self._cards[1].get_value("mixdmp")

    @mixdmp.setter
    def mixdmp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mixdmp must be one of {0,1}""")
        self._cards[1].set_value("mixdmp", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, or part set ID to define the acoustic domain.
        """ # nopep8
        return self._cards[2].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[2].set_value("pid", value)

    @property
    def ptyp(self) -> int:
        """Get or set the Set type:
        EQ.0: part, see *PART.
        EQ.1: part set, see *SET_PART.
        """ # nopep8
        return self._cards[2].get_value("ptyp")

    @ptyp.setter
    def ptyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ptyp must be one of {0,1}""")
        self._cards[2].set_value("ptyp", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Part ID, or part set ID, or segment set ID, or node set ID to define the boundary where vibration boundary condition is provided
        """ # nopep8
        return self._cards[3].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[3].set_value("sid", value)

    @property
    def styp(self) -> int:
        """Get or set the Set type:
        EQ.0: part, see *PART.
        EQ.1: part set, see *SET_PART.
        EQ.2: segment set, see *SET_SEGMENT.
        EQ.3: node set, see *SET_NODE.
        """ # nopep8
        return self._cards[3].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""styp must be one of {0,1,2,3}""")
        self._cards[3].set_value("styp", value)

    @property
    def vad(self) -> int:
        """Get or set the Boundary condition flag:
        EQ.0: velocity by steady state dynamics (SSD).
        EQ.1: velocity by transient analysis.
        EQ.2: opening(zero pressure).
        EQ.11: velocity by LCID1 (amplitude) and LCID2 (phase).
        EQ.12: velocity by LCID1 (real) and LCID2 (imaginary).
        EQ.21: acceleration by LCID1 (amplitude) and LCID2 (phase).
        EQ.22: acceleration by LCID1 (real) and LCID2 (imaginary).
        EQ.31: displacement by LCID1 (amplitude) and LCID2 (phase).
        EQ.32: displacement by LCID1 (real) and LCID2 (imaginary).
        EQ.41: impedance by LCID1 (amplitude) and LCID2 (phase).
        EQ.42: impedance by LCID1 (real) and LCID2 (imaginary).
        EQ.51: pressure by LCID1 (amplitude) and LCID2 (phase).
        EQ.52: pressure by LCID1 (real) and LCID2 (imaginary).
        """ # nopep8
        return self._cards[3].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        if value not in [0, 1, 2, 11, 12, 21, 22, 31, 32, 41, 42, 51, 52]:
            raise Exception("""vad must be one of {0,1,2,11,12,21,22,31,32,41,42,51,52}""")
        self._cards[3].set_value("vad", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.0: determined by steady state dynamics.
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: translational motion in direction given by VID,
        EQ.5: normal direction of the element or segment.
        """ # nopep8
        return self._cards[3].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""dof must be one of {0,1,2,3,4,5}""")
        self._cards[3].set_value("dof", value)

    @property
    def lcid1(self) -> int:
        """Get or set the Load curve ID to describe the amplitude (or real part) of velocity, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[3].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        self._cards[3].set_value("lcid1", value)

    @property
    def lcid2(self) -> int:
        """Get or set the Load curve ID to describe the phase (or imaginary part) of velocity, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[3].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[3].set_value("lcid2", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[3].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[3].set_value("sf", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector ID for DOF values of 4.
        """ # nopep8
        return self._cards[3].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[3].set_value("vid", value)

