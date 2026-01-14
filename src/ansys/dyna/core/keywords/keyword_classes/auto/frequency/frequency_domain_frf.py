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

"""Module providing the FrequencyDomainFrf class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_FREQUENCYDOMAINFRF_CARD0 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n1typ", int, 10, 10, 0),
    FieldSchema("dof1", int, 20, 10, 0),
    FieldSchema("vad1", int, 30, 10, 3),
    FieldSchema("vid1", int, 40, 10, 0),
    FieldSchema("fnmax", float, 50, 10, 0.0),
    FieldSchema("mdmin", int, 60, 10, 0),
    FieldSchema("mdmax", int, 70, 10, 0),
)

_FREQUENCYDOMAINFRF_CARD1 = (
    FieldSchema("dampf", float, 0, 10, 0.0),
    FieldSchema("lcdam", int, 10, 10, 0),
    FieldSchema("lctyp", int, 20, 10, 0),
    FieldSchema("dmpmas", float, 30, 10, 0.0),
    FieldSchema("dmpstf", float, 40, 10, 0.0),
)

_FREQUENCYDOMAINFRF_CARD2 = (
    FieldSchema("n2", int, 0, 10, None),
    FieldSchema("n2typ", int, 10, 10, 0),
    FieldSchema("dof2", int, 20, 10, 0),
    FieldSchema("vad2", int, 30, 10, 2),
    FieldSchema("vid2", int, 40, 10, 0),
    FieldSchema("relatv", int, 50, 10, 0),
)

_FREQUENCYDOMAINFRF_CARD3 = (
    FieldSchema("fmin", float, 0, 10, None),
    FieldSchema("fmax", float, 10, 10, None),
    FieldSchema("nfreq", int, 20, 10, 2),
    FieldSchema("fspace", int, 30, 10, 0),
    FieldSchema("lcfreq", int, 40, 10, None),
    FieldSchema("restrt", int, 50, 10, 0),
    FieldSchema("output", int, 60, 10, 0),
)

class FrequencyDomainFrf(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_FRF keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_FRF"
    _link_fields = {
        "lcdam": LinkType.DEFINE_CURVE,
        "lcfreq": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainFrf class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINFRF_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINFRF_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINFRF_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINFRF_CARD3,
                **kwargs,
            ),        ]
    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node / Node set/Segment set ID for excitation input.When VAD1,the excitation type, is set to 1, which is acceleration, this field is ignored.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n1typ(self) -> int:
        """Get or set the Type of N1:
        EQ.0: node ID,
        EQ.1: node set ID,
        EQ.2: segment set ID.
        When VAD1, the excitation type, is set to 1, which is acceleration,this field is ignored.
        """ # nopep8
        return self._cards[0].get_value("n1typ")

    @n1typ.setter
    def n1typ(self, value: int) -> None:
        """Set the n1typ property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""n1typ must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("n1typ", value)

    @property
    def dof1(self) -> int:
        """Get or set the Applicable degrees-of-freedom for excitation input (ignored if VAD1 = 4):
        EQ.0: translational movement in direction given by vector VID1,
        EQ.1: x-translational degree-of-freedom,or x-rotational degree-of-freedom (for torque excitation, VAD1 = 8)
        EQ.2: y-translational degree-of-freedom,or y-rotational degree-of-freedom (for torque excitation, VAD1 = 8),
        EQ.3: z-translational degree-of-freedom,or z-rotational degree-of-freedom (for torque excitation, VAD1 = 8).
        """ # nopep8
        return self._cards[0].get_value("dof1")

    @dof1.setter
    def dof1(self, value: int) -> None:
        """Set the dof1 property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""dof1 must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("dof1", value)

    @property
    def vad1(self) -> int:
        """Get or set the Excitation input type:
        EQ.0: base velocity,
        EQ.1: base acceleration,
        EQ.2: base displacement,
        EQ.3: nodal force,
        EQ.4: pressure.
        EQ.5: enforced velocity by large mass method.
        EQ.6: enforced acceleration by large mass method,
        EQ.7: enforced displacement by large mass method.
        EQ.8: torque.
        EQ.9: base angular velocity,
        EQ.10: base angular acceleration,
        EQ.11: base angular displacement
        """ # nopep8
        return self._cards[0].get_value("vad1")

    @vad1.setter
    def vad1(self, value: int) -> None:
        """Set the vad1 property."""
        if value not in [3, 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, None]:
            raise Exception("""vad1 must be `None` or one of {3,0,1,2,4,5,6,7,8,9,10,11}.""")
        self._cards[0].set_value("vad1", value)

    @property
    def vid1(self) -> int:
        """Get or set the Vector ID for DOF1=0 for excitation input, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        """Set the vid1 property."""
        self._cards[0].set_value("vid1", value)

    @property
    def fnmax(self) -> float:
        """Get or set the Optional maximum natural frequency employed in FRF computation.
        """ # nopep8
        return self._cards[0].get_value("fnmax")

    @fnmax.setter
    def fnmax(self, value: float) -> None:
        """Set the fnmax property."""
        self._cards[0].set_value("fnmax", value)

    @property
    def mdmin(self) -> int:
        """Get or set the The first mode employed in FRF computation (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmin")

    @mdmin.setter
    def mdmin(self, value: int) -> None:
        """Set the mdmin property."""
        self._cards[0].set_value("mdmin", value)

    @property
    def mdmax(self) -> int:
        """Get or set the The last mode employed in FRF computation (optional).It should be set as a positive integer in a restart run(RESTRT = 1or3) based	on the number of eigenmodes available in the existing d3eigv database.
        """ # nopep8
        return self._cards[0].get_value("mdmax")

    @mdmax.setter
    def mdmax(self, value: int) -> None:
        """Set the mdmax property."""
        self._cards[0].set_value("mdmax", value)

    @property
    def dampf(self) -> float:
        """Get or set the Modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        """Set the dampf property."""
        self._cards[1].set_value("dampf", value)

    @property
    def lcdam(self) -> int:
        """Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("lcdam")

    @lcdam.setter
    def lcdam(self, value: int) -> None:
        """Set the lcdam property."""
        self._cards[1].set_value("lcdam", value)

    @property
    def lctyp(self) -> int:
        """Get or set the Type of load curve defining modal damping coefficient:
        EQ.0: Abscissa value defines frequency,
        EQ.1: Abscissa value defines mode number.
        """ # nopep8
        return self._cards[1].get_value("lctyp")

    @lctyp.setter
    def lctyp(self, value: int) -> None:
        """Set the lctyp property."""
        if value not in [0, 1, None]:
            raise Exception("""lctyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("lctyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpmas")

    @dmpmas.setter
    def dmpmas(self, value: float) -> None:
        """Set the dmpmas property."""
        self._cards[1].set_value("dmpmas", value)

    @property
    def dmpstf(self) -> float:
        """Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpstf")

    @dmpstf.setter
    def dmpstf(self, value: float) -> None:
        """Set the dmpstf property."""
        self._cards[1].set_value("dmpstf", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node / Node set/Segment set ID for response output.
        """ # nopep8
        return self._cards[2].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[2].set_value("n2", value)

    @property
    def n2typ(self) -> int:
        """Get or set the Type of N2:
        EQ.0: node ID,
        EQ.1: node set ID,
        EQ.2: segment set ID.
        """ # nopep8
        return self._cards[2].get_value("n2typ")

    @n2typ.setter
    def n2typ(self, value: int) -> None:
        """Set the n2typ property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""n2typ must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("n2typ", value)

    @property
    def dof2(self) -> int:
        """Get or set the Applicable degrees-of-freedom for response output:
        EQ.0: direction given by vector VID2,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x-rotational degree-of-freedom,
        EQ.5: y-rotational degree-of-freedom,
        EQ.6: z-rotational degree-of-freedom,
        EQ.7: x, y and z-translational degrees-of-freedom,
        EQ.8: x, y and z-rotational degrees-of-freedom.
        """ # nopep8
        return self._cards[2].get_value("dof2")

    @dof2.setter
    def dof2(self, value: int) -> None:
        """Set the dof2 property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""dof2 must be `None` or one of {0,1,2,3,4,5,6,7,8}.""")
        self._cards[2].set_value("dof2", value)

    @property
    def vad2(self) -> int:
        """Get or set the Response output type:
        EQ.0: velocity,
        EQ.1: acceleration,
        EQ.2: displacement,
        EQ.3: nodal force.
        """ # nopep8
        return self._cards[2].get_value("vad2")

    @vad2.setter
    def vad2(self, value: int) -> None:
        """Set the vad2 property."""
        if value not in [2, 0, 1, 3, None]:
            raise Exception("""vad2 must be `None` or one of {2,0,1,3}.""")
        self._cards[2].set_value("vad2", value)

    @property
    def vid2(self) -> int:
        """Get or set the Vector ID for DOF2 = 0 for response direction, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[2].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
        """Set the vid2 property."""
        self._cards[2].set_value("vid2", value)

    @property
    def relatv(self) -> int:
        """Get or set the FLAG for displacement, velocity and acceleration results:
        EQ.0: absolute values are requested,
        EQ.1: relative values are requested (for VAD1=0,1,2 only).
        """ # nopep8
        return self._cards[2].get_value("relatv")

    @relatv.setter
    def relatv(self, value: int) -> None:
        """Set the relatv property."""
        if value not in [0, 1, None]:
            raise Exception("""relatv must be `None` or one of {0,1}.""")
        self._cards[2].set_value("relatv", value)

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum frequency for FRF output (cycles/time).
        """ # nopep8
        return self._cards[3].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        """Set the fmin property."""
        self._cards[3].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum frequency for FRF output (cycles/time).
        """ # nopep8
        return self._cards[3].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[3].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of frequencies for FRF output.
        """ # nopep8
        return self._cards[3].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[3].set_value("nfreq", value)

    @property
    def fspace(self) -> int:
        """Get or set the Frequency spacing option for FRF output:
        EQ.0: linear,
        EQ.1: logarithmic,
        EQ.2: biased.
        """ # nopep8
        return self._cards[3].get_value("fspace")

    @fspace.setter
    def fspace(self, value: int) -> None:
        """Set the fspace property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""fspace must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("fspace", value)

    @property
    def lcfreq(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining the frequencies for FRF output.
        """ # nopep8
        return self._cards[3].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
        """Set the lcfreq property."""
        self._cards[3].set_value("lcfreq", value)

    @property
    def restrt(self) -> int:
        """Get or set the Restart option:
        EQ.0: initial run,
        EQ.1: restart with d3eigv family files,
        EQ.2: restart with dumpfrf,
        EQ.3: restart with d3eigv family files and dumpfrf.
        """ # nopep8
        return self._cards[3].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        """Set the restrt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""restrt must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("restrt", value)

    @property
    def output(self) -> int:
        """Get or set the Output option:
        EQ.0: write amplitude and phase angle pairs,
        EQ.1: write real and imaginary pairs.
        """ # nopep8
        return self._cards[3].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        if value not in [0, 1, None]:
            raise Exception("""output must be `None` or one of {0,1}.""")
        self._cards[3].set_value("output", value)

    @property
    def lcdam_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdam."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdam:
                return kwd
        return None

    @lcdam_link.setter
    def lcdam_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdam."""
        self.lcdam = value.lcid

    @property
    def lcfreq_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcfreq."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfreq:
                return kwd
        return None

    @lcfreq_link.setter
    def lcfreq_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfreq."""
        self.lcfreq = value.lcid

