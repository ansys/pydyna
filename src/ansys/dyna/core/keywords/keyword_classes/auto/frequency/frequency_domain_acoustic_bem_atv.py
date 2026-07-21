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

"""Module providing the FrequencyDomainAcousticBemAtv class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_FREQUENCYDOMAINACOUSTICBEMATV_CARD0 = (
    FieldSchema("ro", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("fmin", float, 20, 10, None),
    FieldSchema("fmax", float, 30, 10, None),
    FieldSchema("nfreq", int, 40, 10, 0),
    FieldSchema("dtout", float, 50, 10, 0.0),
    FieldSchema("tstart", float, 60, 10, 0.0),
    FieldSchema("pref", float, 70, 10, 0.0),
)

_FREQUENCYDOMAINACOUSTICBEMATV_CARD1 = (
    FieldSchema("nsidext", int, 0, 10, 0),
    FieldSchema("typext", int, 10, 10, 0),
    FieldSchema("nsidint", int, 20, 10, 0),
    FieldSchema("typint", int, 30, 10, 0),
    FieldSchema("fftwin", int, 40, 10, 0),
    FieldSchema("trslt", int, 50, 10, 0),
    FieldSchema("ipfile", int, 60, 10, 0),
    FieldSchema("iunits", int, 70, 10, 0),
)

_FREQUENCYDOMAINACOUSTICBEMATV_CARD2 = (
    FieldSchema("method", int, 0, 10, 0),
    FieldSchema("maxit", int, 10, 10, 100),
    FieldSchema("tolitr", float, 20, 10, 0.0001),
    FieldSchema("ndd", int, 30, 10, 1),
    FieldSchema("tollr", float, 40, 10, 1e-06),
    FieldSchema("tolfct", float, 50, 10, 1e-06),
    FieldSchema("ibdim", int, 60, 10, 200),
    FieldSchema("npg", int, 70, 10, 2),
)

_FREQUENCYDOMAINACOUSTICBEMATV_CARD3 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("nbc", int, 10, 10, 1),
    FieldSchema("restrt", int, 20, 10, 0),
    FieldSchema("iedge", int, 30, 10, 0),
    FieldSchema("noel", int, 40, 10, 0),
    FieldSchema("nfrup", int, 50, 10, 0),
    FieldSchema("velout", int, 60, 10, 0),
    FieldSchema("dba", int, 70, 10, 0),
)

_FREQUENCYDOMAINACOUSTICBEMATV_CARD4 = (
    FieldSchema("ssid", int, 0, 10, 0),
    FieldSchema("sstype", int, 10, 10, 0),
    FieldSchema("norm", int, 20, 10, 0),
    FieldSchema("bemtype", int, 30, 10, 0),
    FieldSchema("lc1", int, 40, 10, None),
    FieldSchema("lc2", int, 50, 10, None),
)

_FREQUENCYDOMAINACOUSTICBEMATV_CARD5 = (
    FieldSchema("t_hold_alpha", float, 0, 10, 0.0, "t_hold/alpha"),
    FieldSchema("decay__", float, 10, 10, 0.02, "decay/-"),
)

class FrequencyDomainAcousticBemAtv(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_BEM_ATV keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_BEM_ATV"
    _link_fields = {
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainAcousticBemAtv class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICBEMATV_CARD5,
                **kwargs,
            ),
        ]
    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid Density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
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
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum value of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        """Set the fmin property."""
        self._cards[0].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum value of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[0].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[0].set_value("nfreq", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval between writing velocity or acceleration, and pressure at boundary
        elements in the binary file, to be processeded at the end of transient simulation.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def tstart(self) -> float:
        """Get or set the Start time for recording velocity or acceleration in simulation.
        """ # nopep8
        return self._cards[0].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        """Set the tstart property."""
        self._cards[0].set_value("tstart", value)

    @property
    def pref(self) -> float:
        """Get or set the Reference pressure used to output pressure in dB to file Press_dB. If PREF=0.0, Press_dB file is not generated. A file called Press_Pa is
        generated and contains the pressure at output nodes.
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        """Set the pref property."""
        self._cards[0].set_value("pref", value)

    @property
    def nsidext(self) -> int:
        """Get or set the Node ID, Node set ID, or Segment set ID of output exterior field points.
        """ # nopep8
        return self._cards[1].get_value("nsidext")

    @nsidext.setter
    def nsidext(self, value: int) -> None:
        """Set the nsidext property."""
        self._cards[1].set_value("nsidext", value)

    @property
    def typext(self) -> int:
        """Get or set the Output exterior field point type.
        EQ.0: Node ID.
        EQ.1: Node set ID.
        EQ.2: Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("typext")

    @typext.setter
    def typext(self, value: int) -> None:
        """Set the typext property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""typext must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("typext", value)

    @property
    def nsidint(self) -> int:
        """Get or set the Node ID, Node set ID, or Segment set ID of output interior field points.
        """ # nopep8
        return self._cards[1].get_value("nsidint")

    @nsidint.setter
    def nsidint(self, value: int) -> None:
        """Set the nsidint property."""
        self._cards[1].set_value("nsidint", value)

    @property
    def typint(self) -> int:
        """Get or set the Output interior field point type.
        EQ.0: Node ID.
        EQ.1: Node set ID.
        EQ.2: Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("typint")

    @typint.setter
    def typint(self, value: int) -> None:
        """Set the typint property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""typint must be `None` or one of {0,1,2}.""")
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
        EQ.6: Triangular window
        EQ.7: Kaiser window
        EQ.8: Auto exponential window
        """ # nopep8
        return self._cards[1].get_value("fftwin")

    @fftwin.setter
    def fftwin(self, value: int) -> None:
        """Set the fftwin property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""fftwin must be `None` or one of {0,1,2,3,4,5,6,7,8}.""")
        self._cards[1].set_value("fftwin", value)

    @property
    def trslt(self) -> int:
        """Get or set the Flag to output time domain results (see Remark 4):
        EQ.0: Do not output time domain results.
        EQ.1: Request time domain results(Press_Pa_t gives absolute value pressure as a function of time).
        EQ.2: Requiest time domain results(Press_Pa_t gives real value pressure as a function of time).
        """ # nopep8
        return self._cards[1].get_value("trslt")

    @trslt.setter
    def trslt(self, value: int) -> None:
        """Set the trslt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""trslt must be `None` or one of {0,1,2}.""")
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
        """ # nopep8
        return self._cards[1].get_value("ipfile")

    @ipfile.setter
    def ipfile(self, value: int) -> None:
        """Set the ipfile property."""
        if value not in [0, 1, None]:
            raise Exception("""ipfile must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ipfile", value)

    @property
    def iunits(self) -> int:
        """Get or set the Flag for unit changes (see Remark 5):E
        Q.0: Do not apply a unit change.
        EQ.1: Use MKS units; thus, no change is needed.
        EQ.2: Use units: lbf x s  ** 2 / in , inch, s, lbf, psi, etc.,and change to MKS in the BEM acoustic computation.
        EQ.3: Use units: kg, mm, ms, kN, GPa, etc.,and change to MKS in the BEM acoustic computation.
        EQ.4: Use units: ton, mm, s, N, MPa, etc.,and change to MKS in the BEM acoustic computation.
        """ # nopep8
        return self._cards[1].get_value("iunits")

    @iunits.setter
    def iunits(self, value: int) -> None:
        """Set the iunits property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""iunits must be `None` or one of {0,1,2,3,4}.""")
        self._cards[1].set_value("iunits", value)

    @property
    def method(self) -> int:
        """Get or set the Method used in the acoustic analysis (Default =0)
        EQ.0: Rayleigh method (very fast)
        EQ.1: Kirchhoff method coupled to FEM for acoustics
        (*MAT_ACOUSTIC) (see Remark 4)
        EQ.2: Variational indirect BEM
        EQ.3: Collocation BEM
        EQ.4: Collocation BEM with Burton-Miller formulation for exterior
        problems (no irregular frequency phenomenon).
        """ # nopep8
        return self._cards[2].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        """Set the method property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""method must be `None` or one of {0,1,2,3,4}.""")
        self._cards[2].set_value("method", value)

    @property
    def maxit(self) -> int:
        """Get or set the Maximum number of iterations for the iterative solver (Default =100) (Used only if METHOD>=2).
        """ # nopep8
        return self._cards[2].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        """Set the maxit property."""
        self._cards[2].set_value("maxit", value)

    @property
    def tolitr(self) -> float:
        """Get or set the Tolerance for iterative solver (Default=1.E-4).
        """ # nopep8
        return self._cards[2].get_value("tolitr")

    @tolitr.setter
    def tolitr(self, value: float) -> None:
        """Set the tolitr property."""
        self._cards[2].set_value("tolitr", value)

    @property
    def ndd(self) -> int:
        """Get or set the Number of domain decompositions. It is used for saving memory. For large problems, the boundary mesh is decomposed into NDD domains for less memory allocation.
        NDD = 0 invokes using an internally-determined, recommended domain decomposition. NDD is only used if METHOD >= 2. See Remark 9.
        """ # nopep8
        return self._cards[2].get_value("ndd")

    @ndd.setter
    def ndd(self, value: int) -> None:
        """Set the ndd property."""
        self._cards[2].set_value("ndd", value)

    @property
    def tollr(self) -> float:
        """Get or set the Tolerance for low rank approximation of a dense matrix (Default=1.E-6).
        """ # nopep8
        return self._cards[2].get_value("tollr")

    @tollr.setter
    def tollr(self, value: float) -> None:
        """Set the tollr property."""
        self._cards[2].set_value("tollr", value)

    @property
    def tolfct(self) -> float:
        """Get or set the Tolerance in the factorization of low rank matrix (Default=1.E-6).
        """ # nopep8
        return self._cards[2].get_value("tolfct")

    @tolfct.setter
    def tolfct(self, value: float) -> None:
        """Set the tolfct property."""
        self._cards[2].set_value("tolfct", value)

    @property
    def ibdim(self) -> int:
        """Get or set the Inner iteration limit in GMRES iterative solver (Default=1000).
        """ # nopep8
        return self._cards[2].get_value("ibdim")

    @ibdim.setter
    def ibdim(self, value: int) -> None:
        """Set the ibdim property."""
        self._cards[2].set_value("ibdim", value)

    @property
    def npg(self) -> int:
        """Get or set the Number of Gauss integration points (Default=2).
        """ # nopep8
        return self._cards[2].get_value("npg")

    @npg.setter
    def npg(self, value: int) -> None:
        """Set the npg property."""
        self._cards[2].set_value("npg", value)

    @property
    def nbc(self) -> int:
        """Get or set the Number of boundary condition cards (Card 5) (default = 1).
        """ # nopep8
        return self._cards[3].get_value("nbc")

    @nbc.setter
    def nbc(self, value: int) -> None:
        """Set the nbc property."""
        self._cards[3].set_value("nbc", value)

    @property
    def restrt(self) -> int:
        """Get or set the This flag can save an LS-DYNA transient analysis if the binary output file in the (bem=filename) option has not been changed (default = 0).
        EQ.-30:	Read in the user-provided velocity spectrum saved in a binary file, bevelf.lsda. This option is supported for METHOD = 0, 2, 3, and 4.
        EQ.-3:	Read in the user-provided velocity spectrum saved in an ASCII file, bevelf. This option is supported for METHOD = 0, 2, 3, and 4.
        EQ.0:	Process the LS-DYNA time domain analysis, generating a new binary file.
        EQ.1:	Do not process the LS-DYNA time domain analysis. Use the binary files from the previous run. The files include the binary output file filename and the binary file bin_velfreq, which saves the boundary velocity from FFT.
        EQ.2:	Restart from the d3dump file by using the �R=� command line parameter.  This is useful when the last run was interrupted by sense switches such as �sw1�.
        EQ.3:	Read in user-provided velocity history saved in an ASCII file, bevel.
        EQ.4:	Run acoustic computation on a boundary element mesh with velocity information given with a denser finite element mesh in the last run. This option requires both �bem=filename� and �lbem=filename2� in the command line, where filename2 is the name of the binary file generated in the last run with denser mesh.
        EQ.5:	Do not process the LS-DYNA time domain analysis. Use the binary file filename from the previous run. An FFT computes the new frequency domain boundary velocity with results saved in bin_velfreq.
        EQ.30:	Read in user-provided velocity history saved in a binary file, bevel.lsda.
        """ # nopep8
        return self._cards[3].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        """Set the restrt property."""
        if value not in [0, 1, 2, 3, 4, 5, -30, -3, 30, None]:
            raise Exception("""restrt must be `None` or one of {0,1,2,3,4,5,-30,-3,30}.""")
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
        """Set the iedge property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iedge must be `None` or one of {0,1,2,3}.""")
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
        """Set the noel property."""
        if value not in [0, 1, None]:
            raise Exception("""noel must be `None` or one of {0,1}.""")
        self._cards[3].set_value("noel", value)

    @property
    def nfrup(self) -> int:
        """Get or set the Preconditioner update option.
        EQ.0: updated at every frequency.
        GE.1: Updated for every NFRUP frequencies.
        """ # nopep8
        return self._cards[3].get_value("nfrup")

    @nfrup.setter
    def nfrup(self, value: int) -> None:
        """Set the nfrup property."""
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
        """Set the velout property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""velout must be `None` or one of {0,1,2}.""")
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
        """Set the dba property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""dba must be `None` or one of {0,1,2,3,4}.""")
        self._cards[3].set_value("dba", value)

    @property
    def ssid(self) -> int:
        """Get or set the Part, part set ID, or segment set ID of boundary elements.
        """ # nopep8
        return self._cards[4].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
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
        """Set the sstype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sstype must be `None` or one of {0,1,2}.""")
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
        """Set the norm property."""
        if value not in [0, 1, None]:
            raise Exception("""norm must be `None` or one of {0,1}.""")
        self._cards[4].set_value("norm", value)

    @property
    def bemtype(self) -> int:
        """Get or set the ype of input boundary values in BEM analysis.
        EQ.0: The BEM analysis processes the boundary velocity.
        EQ.1: The BEM analysis processes the boundary acceleration.
        EQ.2: LC1 and LC2 prescribe the real and imaginary parts of the pressure.
        EQ.3: LC1 and LC2 prescribed the real and imaginary parts of the normal velocity.
        EQ.4: LC1 and LC2 prescribe the real and imaginary parts of the impedance.
        EQ.5: LC1 gives the acoustic absorption coefficient.
        EQ.6: Specify a symmetry condition(or rigid wall).
        LT.0: Set the normal velocity(only the real part) through an amplitude as a function of frequency load curve with curve ID | BEMTYP | .
        """ # nopep8
        return self._cards[4].get_value("bemtype")

    @bemtype.setter
    def bemtype(self, value: int) -> None:
        """Set the bemtype property."""
        self._cards[4].set_value("bemtype", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for defining the real part of pressure, the real part of normal velocity, the real part of impedance, or the real acoustic absorption coefficient. See BEMTYP = 2, 3, 4, and 5, respectively
        """ # nopep8
        return self._cards[4].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[4].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for defining imaginary part of pressure, normal velocity, or impedance. See BEMTYP = 2, 3, and 4, respectively.
        """ # nopep8
        return self._cards[4].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[4].set_value("lc2", value)

    @property
    def t_hold_alpha(self) -> float:
        """Get or set the If FFTWIN=5 or 8: T_HOLD: Hold-off period before the exponential window. The length of the hold-off period should coincide with the pre-trigger time to reduce the effects of noise in the captured time domain data.
        If FFTWIN=7: ALPHA: A non-negative real number that determines the shape of the Kaiser window and, therefore, controls the tradeoff between main-lobe width and side lobe labels of the spectral leakage pattern
        """ # nopep8
        return self._cards[5].get_value("t_hold_alpha")

    @t_hold_alpha.setter
    def t_hold_alpha(self, value: float) -> None:
        """Set the t_hold_alpha property."""
        self._cards[5].set_value("t_hold_alpha", value)

    @property
    def decay__(self) -> float:
        """Get or set the If FFTWIN=5 or 8: Decay ratio at the end of capture duration. For example, if the DECAY = 0.02, it means that the vibration is forced to decay to 2% of its amplitude within the capture duration. This field is only used when FFTWIN = 5.
        """ # nopep8
        return self._cards[5].get_value("decay__")

    @decay__.setter
    def decay__(self, value: float) -> None:
        """Set the decay__ property."""
        self._cards[5].set_value("decay__", value)

    @property
    def lc1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

