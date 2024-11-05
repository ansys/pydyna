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

class FrequencyDomainAcousticBemHalfSpace(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_BEM_HALF_SPACE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_BEM_HALF_SPACE"

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
                        kwargs.get("dtout", 0.0)
                    ),
                    Field(
                        "tstart",
                        float,
                        60,
                        10,
                        kwargs.get("tstart", 0.0)
                    ),
                    Field(
                        "pref",
                        float,
                        70,
                        10,
                        kwargs.get("pref", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsidext",
                        int,
                        0,
                        10,
                        kwargs.get("nsidext", 0)
                    ),
                    Field(
                        "typext",
                        int,
                        10,
                        10,
                        kwargs.get("typext", 0)
                    ),
                    Field(
                        "nsidint",
                        int,
                        20,
                        10,
                        kwargs.get("nsidint", 0)
                    ),
                    Field(
                        "typint",
                        int,
                        30,
                        10,
                        kwargs.get("typint", 0)
                    ),
                    Field(
                        "fftwin",
                        int,
                        40,
                        10,
                        kwargs.get("fftwin", 0)
                    ),
                    Field(
                        "trslt",
                        int,
                        50,
                        10,
                        kwargs.get("trslt", 0)
                    ),
                    Field(
                        "ipfile",
                        int,
                        60,
                        10,
                        kwargs.get("ipfile", 0)
                    ),
                    Field(
                        "iunits",
                        int,
                        70,
                        10,
                        kwargs.get("iunits", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method", 0)
                    ),
                    Field(
                        "maxit",
                        int,
                        10,
                        10,
                        kwargs.get("maxit", 100)
                    ),
                    Field(
                        "tolitr",
                        float,
                        20,
                        10,
                        kwargs.get("tolitr", 1E-4)
                    ),
                    Field(
                        "ndd",
                        int,
                        30,
                        10,
                        kwargs.get("ndd", 1)
                    ),
                    Field(
                        "tollr",
                        float,
                        40,
                        10,
                        kwargs.get("tollr", 1E-6)
                    ),
                    Field(
                        "tolfct",
                        float,
                        50,
                        10,
                        kwargs.get("tolfct", 1E-6)
                    ),
                    Field(
                        "ibdim",
                        int,
                        60,
                        10,
                        kwargs.get("ibdim", 1000)
                    ),
                    Field(
                        "npg",
                        int,
                        70,
                        10,
                        kwargs.get("npg", 2)
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
                        "nbc",
                        int,
                        10,
                        10,
                        kwargs.get("nbc", 1)
                    ),
                    Field(
                        "restrt",
                        int,
                        20,
                        10,
                        kwargs.get("restrt", 0)
                    ),
                    Field(
                        "iedge",
                        int,
                        30,
                        10,
                        kwargs.get("iedge", 0)
                    ),
                    Field(
                        "noel",
                        int,
                        40,
                        10,
                        kwargs.get("noel", 0)
                    ),
                    Field(
                        "nfrup",
                        int,
                        50,
                        10,
                        kwargs.get("nfrup", 0)
                    ),
                    Field(
                        "velout",
                        int,
                        60,
                        10,
                        kwargs.get("velout", 0)
                    ),
                    Field(
                        "dba",
                        int,
                        70,
                        10,
                        kwargs.get("dba", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid", 0)
                    ),
                    Field(
                        "sstype",
                        int,
                        10,
                        10,
                        kwargs.get("sstype", 0)
                    ),
                    Field(
                        "norm",
                        int,
                        20,
                        10,
                        kwargs.get("norm", 0)
                    ),
                    Field(
                        "bemtype",
                        int,
                        30,
                        10,
                        kwargs.get("bemtype", 0)
                    ),
                    Field(
                        "lc1",
                        int,
                        40,
                        10,
                        kwargs.get("lc1")
                    ),
                    Field(
                        "lc2",
                        int,
                        50,
                        10,
                        kwargs.get("lc2")
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
                        kwargs.get("pid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t_hold",
                        float,
                        0,
                        10,
                        kwargs.get("t_hold", 0.0)
                    ),
                    Field(
                        "decay",
                        float,
                        10,
                        10,
                        kwargs.get("decay", 0.02)
                    ),
                ],
            ),
        ]

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid Density.
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
        """Get or set the Time interval between writing velocity or acceleration, and pressure at boundary
        elements in the binary file, to be proceeded at the end of LS-DYNA simulation.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def tstart(self) -> float:
        """Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[0].set_value("tstart", value)

    @property
    def pref(self) -> float:
        """Get or set the Reference pressure to be used to output pressure in dB, in file Press_dB. If
        Ref_Pres=0, Press_dB file will not be generated. A file called Press_Pa is
        generated and contains the pressure at output nodes.
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        self._cards[0].set_value("pref", value)

    @property
    def nsidext(self) -> int:
        """Get or set the Set ID, or Segment set ID of output exterior field points.
        """ # nopep8
        return self._cards[1].get_value("nsidext")

    @nsidext.setter
    def nsidext(self, value: int) -> None:
        self._cards[1].set_value("nsidext", value)

    @property
    def typext(self) -> int:
        """Get or set the Output exterior field point type.
        EQ.0: node ID.
        EQ.1: Node set ID.
        EQ.2: Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("typext")

    @typext.setter
    def typext(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""typext must be one of {0,1,2}""")
        self._cards[1].set_value("typext", value)

    @property
    def nsidint(self) -> int:
        """Get or set the Node set ID, or Segment set ID of output interior field points.
        """ # nopep8
        return self._cards[1].get_value("nsidint")

    @nsidint.setter
    def nsidint(self, value: int) -> None:
        self._cards[1].set_value("nsidint", value)

    @property
    def typint(self) -> int:
        """Get or set the Output interior field point type.
        EQ.0: node ID.
        EQ.1: Node set ID.
        EQ.2: Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("typint")

    @typint.setter
    def typint(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""typint must be one of {0,1,2}""")
        self._cards[1].set_value("typint", value)

    @property
    def fftwin(self) -> int:
        """Get or set the FFT windows (Default=0).
        EQ.0: Rectangular window
        EQ.1: Hanning window
        EQ.2: Hamming window
        EQ.3: Blackman window
        EQ.4: Raised cosine window
        EQ.5: Exponential window.
        """ # nopep8
        return self._cards[1].get_value("fftwin")

    @fftwin.setter
    def fftwin(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""fftwin must be one of {0,1,2,3,4,5}""")
        self._cards[1].set_value("fftwin", value)

    @property
    def trslt(self) -> int:
        """Get or set the EQ.0: No time domain results are requested;
        EQ.1: Time domain results are requested.
        EQ.2: time domain results are requested (Press_Pa_t gives real value pressure vs. time).
        """ # nopep8
        return self._cards[1].get_value("trslt")

    @trslt.setter
    def trslt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""trslt must be one of {0,1,2}""")
        self._cards[1].set_value("trslt", value)

    @property
    def ipfile(self) -> int:
        """Get or set the Flag for output files (Default=0).
        EQ.0: Press_Pa (magnitude of pressure vs. frequency), Press_dB (sound
        pressure level vs. frequency) and bepres (ASCII database file for LSPrepost)
        are provided.
        EQ.1: Press_Pa_real (real part of the pressure vs. frequency) and
        Press_Pa_imag (imaginary part of the pressure vs. frequency) are
        included, in addition to Press_Pa, Press_dB and bepres.
        EQ.10: files for IPFILE = 0, and fringe files for acoustic pressure.
        EQ.11: files for IPFILE = 1, and fringe files for acoustic pressure.
        EQ.20: files for IPFILE = 0, and fringe files for sound pressure level.
        EQ.21: files for IPFILE = 1, and fringe files for sound pressure level.
        EQ.31: files for IPFILE = 1, and fringe files for acoustic pressure(real part).
        EQ.41: files for IPFILE = 1, and fringe files for acoustic pressure(imaginary part).
        """ # nopep8
        return self._cards[1].get_value("ipfile")

    @ipfile.setter
    def ipfile(self, value: int) -> None:
        if value not in [0, 1, 10, 11, 20, 21, 31, 41]:
            raise Exception("""ipfile must be one of {0,1,10,11,20,21,31,41}""")
        self._cards[1].set_value("ipfile", value)

    @property
    def iunits(self) -> int:
        """Get or set the Flag for unit changes
        EQ.0: No unit change applied;
        EQ.1: MKS units are used, no change needed;
        EQ.2: Units (lbfxs2/in, inch, s, lbf, psi, etc.) are used, changed to MKS
        in BEM Acoustic computation;
        EQ.3: Units (kg, mm, ms, kN, GPa, etc.) are used, changed to MKS in
        BEM Acoustic computation;
        EQ.4: Units (ton, mm, s, N, MPa, etc.) are used, changed to MKS in
        BEM Acoustic computation.
        """ # nopep8
        return self._cards[1].get_value("iunits")

    @iunits.setter
    def iunits(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""iunits must be one of {0,1,2,3,4}""")
        self._cards[1].set_value("iunits", value)

    @property
    def method(self) -> int:
        """Get or set the Method used in acoustic analysis (Default =0)
        EQ.0: Rayleigh method (very fast)
        EQ.1: Kirchhoff method coupled to FEM for acoustics
        (*MAT_ACOUSTIC) (see Remark 4)
        EQ.2: Variational Indirect BEM
        EQ.3: Collocation BEM
        EQ.4: Collocation BEM with Burton-Miller formulation for exterior
        problems (no irregular frequency phenomenon).
        """ # nopep8
        return self._cards[2].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""method must be one of {0,1,2,3,4}""")
        self._cards[2].set_value("method", value)

    @property
    def maxit(self) -> int:
        """Get or set the Maximum number of iterations for iterative solver (Default =100)	(Used only if METHOD>=2).
        """ # nopep8
        return self._cards[2].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        self._cards[2].set_value("maxit", value)

    @property
    def tolitr(self) -> float:
        """Get or set the Tolerance for iterative solver (Default=1.E-4).
        """ # nopep8
        return self._cards[2].get_value("tolitr")

    @tolitr.setter
    def tolitr(self, value: float) -> None:
        self._cards[2].set_value("tolitr", value)

    @property
    def ndd(self) -> int:
        """Get or set the Number of Domain Decomposition, used for memory saving.
        For large problems, the boundary mesh is decomposed into NDD
        domains for less memory allocation.
        This option is only used if METHOD>=2..
        """ # nopep8
        return self._cards[2].get_value("ndd")

    @ndd.setter
    def ndd(self, value: int) -> None:
        self._cards[2].set_value("ndd", value)

    @property
    def tollr(self) -> float:
        """Get or set the Tolerance for low rank approximation of dense matrix (Default=1.E-6).
        """ # nopep8
        return self._cards[2].get_value("tollr")

    @tollr.setter
    def tollr(self, value: float) -> None:
        self._cards[2].set_value("tollr", value)

    @property
    def tolfct(self) -> float:
        """Get or set the Tolerance in factorization of low rank matrix (Default=1.E-6).
        """ # nopep8
        return self._cards[2].get_value("tolfct")

    @tolfct.setter
    def tolfct(self, value: float) -> None:
        self._cards[2].set_value("tolfct", value)

    @property
    def ibdim(self) -> int:
        """Get or set the Inner iteration limit in GMRES iterative solver (Default=1000).
        """ # nopep8
        return self._cards[2].get_value("ibdim")

    @ibdim.setter
    def ibdim(self, value: int) -> None:
        self._cards[2].set_value("ibdim", value)

    @property
    def npg(self) -> int:
        """Get or set the Number of Gauss integration points (Default=2).
        """ # nopep8
        return self._cards[2].get_value("npg")

    @npg.setter
    def npg(self, value: int) -> None:
        self._cards[2].set_value("npg", value)

    @property
    def nbc(self) -> int:
        """Get or set the Number of boundary condition cards (Card 5) (default = 1).
        """ # nopep8
        return self._cards[3].get_value("nbc")

    @nbc.setter
    def nbc(self, value: int) -> None:
        self._cards[3].set_value("nbc", value)

    @property
    def restrt(self) -> int:
        """Get or set the This flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed(default = 0).
        EQ.0: LS-DYNA time domain analysis is processed and generates a new binary file.
        EQ.1: LS-DYNA time domain analysis is not processed.The binary files from previous run are used. The files include the binary output file filename, and the binary file bin_velfreq, which saves the boundary velocity from FFT.
        EQ.2: LS-DYNA restarts from d3dump file by using "R="command line parameter. This is useful when the last run was interrupted by sense switches such as "sw1".
        EQ.3: LS-DYNA reads in user provided velocity history saved in an ASCII file, bevel.
        EQ.-3:	LS-DYNA reads in user provided velocity spectrum saved in an ASCII file, bevelf
        EQ.4: run acoustic computation on a boundary element mesh with velocity information given with a denser finite element mesh in last run. This option requires both "bem = filename" and "lbem = filename2" in the command line, where filename2 is the name of the binary file generated in the last run with denser mesh.
        EQ.5: LS-DYNA time domain analysis is not processed. The binary file filename from previous run is used. An FFT is performed to get the new frequency domain boundary velocity and the results are saved in bin_velfreq.
        """ # nopep8
        return self._cards[3].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        if value not in [0, 1, 2, 3, -3, 4, 5]:
            raise Exception("""restrt must be one of {0,1,2,3,-3,4,5}""")
        self._cards[3].set_value("restrt", value)

    @property
    def iedge(self) -> int:
        """Get or set the Free edge and multi-connection constraints option (default = 0).
        EQ.0: free edge and multi-connection constraints not considered.
        EQ.1: free edge and multi-connection constraints considered.
        EQ.2: only free edge constraints are considered.
        EQ.3: only multi-connection constraints are considered.
        """ # nopep8
        return self._cards[3].get_value("iedge")

    @iedge.setter
    def iedge(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""iedge must be one of {0,1,2,3}""")
        self._cards[3].set_value("iedge", value)

    @property
    def noel(self) -> int:
        """Get or set the Location where normal velocity or acceleration is taken (default = 0).
        EQ.0: elements or segments.
        EQ.1: nodes.
        """ # nopep8
        return self._cards[3].get_value("noel")

    @noel.setter
    def noel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""noel must be one of {0,1}""")
        self._cards[3].set_value("noel", value)

    @property
    def nfrup(self) -> int:
        """Get or set the Preconditioner update option.
        EQ.0: updated at every frequency.
        EQ.N: updated for every N frequencies.
        """ # nopep8
        return self._cards[3].get_value("nfrup")

    @nfrup.setter
    def nfrup(self, value: int) -> None:
        self._cards[3].set_value("nfrup", value)

    @property
    def velout(self) -> int:
        """Get or set the Flag for writing out nodal or elemental velocity data.
        EQ.0: No writing out velocity data.
        EQ.1: write out time domain velocity data (in x, y and z directions).
        EQ.2: write out frequency domain velocity data (in normal direction).
        """ # nopep8
        return self._cards[3].get_value("velout")

    @velout.setter
    def velout(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""velout must be one of {0,1,2}""")
        self._cards[3].set_value("velout", value)

    @property
    def dba(self) -> int:
        """Get or set the Flag for writing out weighted SPL file Press_dBA with different weighting options.
        EQ.0: No writing out Press_dBA.
        EQ.1: write out Press_dBA and use A-weighting.
        EQ.2: write out Press_dBA and use B-weighting.
        EQ.3: write out Press_dBA and use C-weighting.
        EQ.4: write out Press_dBA and use D-weighting.
        """ # nopep8
        return self._cards[3].get_value("dba")

    @dba.setter
    def dba(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""dba must be one of {0,1,2,3,4}""")
        self._cards[3].set_value("dba", value)

    @property
    def ssid(self) -> int:
        """Get or set the Part, part set ID, or segment set ID of boundary elements.
        """ # nopep8
        return self._cards[4].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[4].set_value("ssid", value)

    @property
    def sstype(self) -> int:
        """Get or set the Boundary element type:
        EQ.0: part Set ID
        EQ.1: part ID
        EQ.2: segment set ID.
        """ # nopep8
        return self._cards[4].get_value("sstype")

    @sstype.setter
    def sstype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sstype must be one of {0,1,2}""")
        self._cards[4].set_value("sstype", value)

    @property
    def norm(self) -> int:
        """Get or set the NORM should be set such that the normal vectors point away from the fluid.
        EQ.0: normal vectors are not inverted (default).
        EQ.1: normal vectors are inverted.
        """ # nopep8
        return self._cards[4].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""norm must be one of {0,1}""")
        self._cards[4].set_value("norm", value)

    @property
    def bemtype(self) -> int:
        """Get or set the Type of input boundary values in BEM analysis.
        EQ.0: boundary velocity will be processed in BEM analysis.
        EQ.1: boundary acceleration will be processed in BEM analysis.
        EQ.2: pressure is prescribed and the real and imaginary parts are given by LC1 and LC2.
        EQ.3: normal velocity is prescribed and the real and imaginary parts are given by LC1 and LC2.
        EQ.4: impedance is prescribed and the real and imaginary parts are given by LC1 and LC2.
        EQ.-n: normal velocity (only real part) is prescribed, through load
        curve n. An amplitude versus. frequency load curve (with curve ID n) needs to be defined.
        """ # nopep8
        return self._cards[4].get_value("bemtype")

    @bemtype.setter
    def bemtype(self, value: int) -> None:
        self._cards[4].set_value("bemtype", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for defining real part of pressure, normal velocity or impedance.
        """ # nopep8
        return self._cards[4].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[4].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for defining imaginary part of pressure, normal velocity or impedance.
        """ # nopep8
        return self._cards[4].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[4].set_value("lc2", value)

    @property
    def pid(self) -> int:
        """Get or set the Plane ID for defining the half-space problem, see keyword *DEFINE_PLANE.
        """ # nopep8
        return self._cards[5].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[5].set_value("pid", value)

    @property
    def t_hold(self) -> float:
        """Get or set the Hold-off period before the exponential window. The length of the hold-off period should coincide with the pre-trigger time to reduce the effects of noise in the captured time domain data. It is only used when FFTWIN = 5.
        """ # nopep8
        return self._cards[6].get_value("t_hold")

    @t_hold.setter
    def t_hold(self, value: float) -> None:
        self._cards[6].set_value("t_hold", value)

    @property
    def decay(self) -> float:
        """Get or set the Decay ratio at the end of capture duration. For example, if the DECAY = 0.02, it means that the vibration is forced to decay to 2% of its amplitude within the capture duration. This field is only used when FFTWIN = 5.
        """ # nopep8
        return self._cards[6].get_value("decay")

    @decay.setter
    def decay(self, value: float) -> None:
        self._cards[6].set_value("decay", value)

