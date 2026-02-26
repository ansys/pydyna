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

"""Module providing the BoundaryElementMethodAcoustic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYELEMENTMETHODACOUSTIC_CARD0 = (
    FieldSchema("ro", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("fmin", float, 20, 10, None),
    FieldSchema("fmax", float, 30, 10, None),
    FieldSchema("nfreq", int, 40, 10, None),
    FieldSchema("dt_out", int, 50, 10, None),
    FieldSchema("t_start", float, 60, 10, None),
    FieldSchema("pref", float, 70, 10, None),
)

_BOUNDARYELEMENTMETHODACOUSTIC_CARD1 = (
    FieldSchema("nsid_ext", int, 0, 10, None),
    FieldSchema("type_ext", int, 10, 10, 1),
    FieldSchema("nsid_int", int, 20, 10, None),
    FieldSchema("type_int", int, 30, 10, 1),
    FieldSchema("fft_win", int, 40, 10, 0),
)

_BOUNDARYELEMENTMETHODACOUSTIC_CARD2 = (
    FieldSchema("method", int, 0, 10, 0),
    FieldSchema("maxit", int, 10, 10, None),
    FieldSchema("res", float, 20, 10, None),
    FieldSchema("ndd", int, 30, 10, None),
)

_BOUNDARYELEMENTMETHODACOUSTIC_CARD3 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("sstype", int, 10, 10, 0),
    FieldSchema("norm", int, 20, 10, 0),
    FieldSchema("bem_type", int, 30, 10, None),
    FieldSchema("restart", int, 40, 10, 0),
)

class BoundaryElementMethodAcoustic(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_ACOUSTIC keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_ACOUSTIC"

    def __init__(self, **kwargs):
        """Initialize the BoundaryElementMethodAcoustic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODACOUSTIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODACOUSTIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODACOUSTIC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODACOUSTIC_CARD3,
                **kwargs,
            ),        ]
    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Sound speed of the Fluid.
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
    def nfreq(self) -> typing.Optional[int]:
        """Get or set the Number of output frequencies.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[0].set_value("nfreq", value)

    @property
    def dt_out(self) -> typing.Optional[int]:
        """Get or set the Time interval between writing velocity or acceleration, and pressure at boundary elements in the binary file, to be proceeded at the end of LS-DYNA simulation
        """ # nopep8
        return self._cards[0].get_value("dt_out")

    @dt_out.setter
    def dt_out(self, value: int) -> None:
        """Set the dt_out property."""
        self._cards[0].set_value("dt_out", value)

    @property
    def t_start(self) -> typing.Optional[float]:
        """Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
        """ # nopep8
        return self._cards[0].get_value("t_start")

    @t_start.setter
    def t_start(self, value: float) -> None:
        """Set the t_start property."""
        self._cards[0].set_value("t_start", value)

    @property
    def pref(self) -> typing.Optional[float]:
        """Get or set the Reference pressure to be used to output pressure in dB, in the file Press_dB. If Ref_Pres=0, the Press_dB file will not be generated. A file called Press_Pa is generated and contains the pressure at the output nodes
        """ # nopep8
        return self._cards[0].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        """Set the pref property."""
        self._cards[0].set_value("pref", value)

    @property
    def nsid_ext(self) -> typing.Optional[int]:
        """Get or set the set ID, or Segment set ID of output exterior field points.
        """ # nopep8
        return self._cards[1].get_value("nsid_ext")

    @nsid_ext.setter
    def nsid_ext(self, value: int) -> None:
        """Set the nsid_ext property."""
        self._cards[1].set_value("nsid_ext", value)

    @property
    def type_ext(self) -> int:
        """Get or set the Output exterior field point type.
        EQ.1:  Node set ID.
        EQ.2:  Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("type_ext")

    @type_ext.setter
    def type_ext(self, value: int) -> None:
        """Set the type_ext property."""
        if value not in [1, 2, None]:
            raise Exception("""type_ext must be `None` or one of {1,2}.""")
        self._cards[1].set_value("type_ext", value)

    @property
    def nsid_int(self) -> typing.Optional[int]:
        """Get or set the Node set ID, or Segment set ID of output interior field points.
        """ # nopep8
        return self._cards[1].get_value("nsid_int")

    @nsid_int.setter
    def nsid_int(self, value: int) -> None:
        """Set the nsid_int property."""
        self._cards[1].set_value("nsid_int", value)

    @property
    def type_int(self) -> int:
        """Get or set the Output interior field point type.
        EQ.1:  Node set ID.
        EQ.2:  Segment set ID.
        """ # nopep8
        return self._cards[1].get_value("type_int")

    @type_int.setter
    def type_int(self, value: int) -> None:
        """Set the type_int property."""
        if value not in [1, 2, None]:
            raise Exception("""type_int must be `None` or one of {1,2}.""")
        self._cards[1].set_value("type_int", value)

    @property
    def fft_win(self) -> int:
        """Get or set the FFT windows (Default=0).
        EQ.0: Rectangular window
        EQ.1: Hanning window
        EQ.2: Hamming window
        EQ.3: Blackman window
        EQ.4: Raised cosine window
        """ # nopep8
        return self._cards[1].get_value("fft_win")

    @fft_win.setter
    def fft_win(self, value: int) -> None:
        """Set the fft_win property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""fft_win must be `None` or one of {0,1,2,3,4}.""")
        self._cards[1].set_value("fft_win", value)

    @property
    def method(self) -> int:
        """Get or set the Method used in acoustic analysis (Default =0)
        EQ.0:  Rayleigh method (very fast)
        EQ.1: Kirchhoff method coupled to FEM for acoustics (*MAT_ACOUSTIC) (see Remark 1)
        EQ.2:  BEM
        """ # nopep8
        return self._cards[2].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        """Set the method property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""method must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("method", value)

    @property
    def maxit(self) -> typing.Optional[int]:
        """Get or set the Maximum number of iterations for Iterative solver (Default =100)(Used only if IBEM_Met=2)
        """ # nopep8
        return self._cards[2].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        """Set the maxit property."""
        self._cards[2].set_value("maxit", value)

    @property
    def res(self) -> typing.Optional[float]:
        """Get or set the Residual for the iterative solver (Default=1.E-6)
        """ # nopep8
        return self._cards[2].get_value("res")

    @res.setter
    def res(self, value: float) -> None:
        """Set the res property."""
        self._cards[2].set_value("res", value)

    @property
    def ndd(self) -> typing.Optional[int]:
        """Get or set the Number of Domain Decomposition, used for memory saving.For large problems, the boundary mesh is decomposed into NDD domains for less memory allocation.This option is only used if IBEM_Met=2.
        """ # nopep8
        return self._cards[2].get_value("ndd")

    @ndd.setter
    def ndd(self, value: int) -> None:
        """Set the ndd property."""
        self._cards[2].set_value("ndd", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Part, Part set ID, or Segment set ID of boundary elements.
        """ # nopep8
        return self._cards[3].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[3].set_value("ssid", value)

    @property
    def sstype(self) -> int:
        """Get or set the Boundary element type.
        EQ.0:  Part Set ID
        EQ.1:  Part ID
        EQ.2:  Segment set ID.
        """ # nopep8
        return self._cards[3].get_value("sstype")

    @sstype.setter
    def sstype(self, value: int) -> None:
        """Set the sstype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sstype must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("sstype", value)

    @property
    def norm(self) -> int:
        """Get or set the NORM should be set such that the normal vectors face toward the Fluid.
        EQ.0: Normal vectors are not inverted (Default).
        EQ.1: Normals are inverted.
        """ # nopep8
        return self._cards[3].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        """Set the norm property."""
        if value not in [0, 1, None]:
            raise Exception("""norm must be `None` or one of {0,1}.""")
        self._cards[3].set_value("norm", value)

    @property
    def bem_type(self) -> typing.Optional[int]:
        """Get or set the Type of input boundary values in BEM Analysis.
        EQ.0: Boundary velocity will be processed in BEM Analysis
        EQ.1:  Boundary acceleration will be processed in BEM analysis
        EQ.- n:  Velocity is given in frequency domain, through the load curve n. An amplitude vs. frequency load curve (with Curve ID n) needs to be defined.
        """ # nopep8
        return self._cards[3].get_value("bem_type")

    @bem_type.setter
    def bem_type(self, value: int) -> None:
        """Set the bem_type property."""
        self._cards[3].set_value("bem_type", value)

    @property
    def restart(self) -> int:
        """Get or set the This Flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed. (Default = 0).
        EQ.0:  LS-DYNA analysis is processed and generates a new binary file.
        EQ.1:  LS-DYNA analysis is not processed. The binary file from previous run is used.
        """ # nopep8
        return self._cards[3].get_value("restart")

    @restart.setter
    def restart(self, value: int) -> None:
        """Set the restart property."""
        if value not in [0, 1, None]:
            raise Exception("""restart must be `None` or one of {0,1}.""")
        self._cards[3].set_value("restart", value)

