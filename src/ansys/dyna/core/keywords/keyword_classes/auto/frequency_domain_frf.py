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

class FrequencyDomainFrf(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_FRF keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_FRF"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n1typ",
                        int,
                        10,
                        10,
                        kwargs.get("n1typ", 0)
                    ),
                    Field(
                        "dof1",
                        int,
                        20,
                        10,
                        kwargs.get("dof1", 0)
                    ),
                    Field(
                        "vad1",
                        int,
                        30,
                        10,
                        kwargs.get("vad1", 3)
                    ),
                    Field(
                        "vid1",
                        int,
                        40,
                        10,
                        kwargs.get("vid1", 0)
                    ),
                    Field(
                        "fnmax",
                        float,
                        50,
                        10,
                        kwargs.get("fnmax", 0.0)
                    ),
                    Field(
                        "mdmin",
                        int,
                        60,
                        10,
                        kwargs.get("mdmin", 0)
                    ),
                    Field(
                        "mdmax",
                        int,
                        70,
                        10,
                        kwargs.get("mdmax", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dampf",
                        float,
                        0,
                        10,
                        kwargs.get("dampf", 0.0)
                    ),
                    Field(
                        "lcdam",
                        int,
                        10,
                        10,
                        kwargs.get("lcdam", 0)
                    ),
                    Field(
                        "lctyp",
                        int,
                        20,
                        10,
                        kwargs.get("lctyp", 0)
                    ),
                    Field(
                        "dmpmas",
                        float,
                        30,
                        10,
                        kwargs.get("dmpmas", 0.0)
                    ),
                    Field(
                        "dmpstf",
                        float,
                        40,
                        10,
                        kwargs.get("dmpstf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n2",
                        int,
                        0,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n2typ",
                        int,
                        10,
                        10,
                        kwargs.get("n2typ", 0)
                    ),
                    Field(
                        "dof2",
                        int,
                        20,
                        10,
                        kwargs.get("dof2", 0)
                    ),
                    Field(
                        "vad2",
                        int,
                        30,
                        10,
                        kwargs.get("vad2", 2)
                    ),
                    Field(
                        "vid2",
                        int,
                        40,
                        10,
                        kwargs.get("vid2", 0)
                    ),
                    Field(
                        "relatv",
                        int,
                        50,
                        10,
                        kwargs.get("relatv", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fmin",
                        float,
                        0,
                        10,
                        kwargs.get("fmin")
                    ),
                    Field(
                        "fmax",
                        float,
                        10,
                        10,
                        kwargs.get("fmax")
                    ),
                    Field(
                        "nfreq",
                        int,
                        20,
                        10,
                        kwargs.get("nfreq", 2)
                    ),
                    Field(
                        "fspace",
                        int,
                        30,
                        10,
                        kwargs.get("fspace", 0)
                    ),
                    Field(
                        "lcfreq",
                        int,
                        40,
                        10,
                        kwargs.get("lcfreq")
                    ),
                    Field(
                        "restrt",
                        int,
                        50,
                        10,
                        kwargs.get("restrt", 0)
                    ),
                    Field(
                        "output",
                        int,
                        60,
                        10,
                        kwargs.get("output", 0)
                    ),
                ],
            ),
        ]

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node / Node set/Segment set ID for excitation input.When VAD1,the excitation type, is set to 1, which is acceleration, this field is ignored.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
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
        if value not in [0, 1, 2]:
            raise Exception("""n1typ must be one of {0,1,2}""")
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
        if value not in [0, 1, 2, 3]:
            raise Exception("""dof1 must be one of {0,1,2,3}""")
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
        if value not in [3, 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11]:
            raise Exception("""vad1 must be one of {3,0,1,2,4,5,6,7,8,9,10,11}""")
        self._cards[0].set_value("vad1", value)

    @property
    def vid1(self) -> int:
        """Get or set the Vector ID for DOF1=0 for excitation input, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        self._cards[0].set_value("vid1", value)

    @property
    def fnmax(self) -> float:
        """Get or set the Optional maximum natural frequency employed in FRF computation.
        """ # nopep8
        return self._cards[0].get_value("fnmax")

    @fnmax.setter
    def fnmax(self, value: float) -> None:
        self._cards[0].set_value("fnmax", value)

    @property
    def mdmin(self) -> int:
        """Get or set the The first mode employed in FRF computation (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmin")

    @mdmin.setter
    def mdmin(self, value: int) -> None:
        self._cards[0].set_value("mdmin", value)

    @property
    def mdmax(self) -> int:
        """Get or set the The last mode employed in FRF computation (optional).It should be set as a positive integer in a restart run(RESTRT = 1or3) based	on the number of eigenmodes available in the existing d3eigv database.
        """ # nopep8
        return self._cards[0].get_value("mdmax")

    @mdmax.setter
    def mdmax(self, value: int) -> None:
        self._cards[0].set_value("mdmax", value)

    @property
    def dampf(self) -> float:
        """Get or set the Modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        self._cards[1].set_value("dampf", value)

    @property
    def lcdam(self) -> int:
        """Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("lcdam")

    @lcdam.setter
    def lcdam(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""lctyp must be one of {0,1}""")
        self._cards[1].set_value("lctyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpmas")

    @dmpmas.setter
    def dmpmas(self, value: float) -> None:
        self._cards[1].set_value("dmpmas", value)

    @property
    def dmpstf(self) -> float:
        """Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpstf")

    @dmpstf.setter
    def dmpstf(self, value: float) -> None:
        self._cards[1].set_value("dmpstf", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node / Node set/Segment set ID for response output.
        """ # nopep8
        return self._cards[2].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
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
        if value not in [0, 1, 2]:
            raise Exception("""n2typ must be one of {0,1,2}""")
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
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""dof2 must be one of {0,1,2,3,4,5,6,7,8}""")
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
        if value not in [2, 0, 1, 3]:
            raise Exception("""vad2 must be one of {2,0,1,3}""")
        self._cards[2].set_value("vad2", value)

    @property
    def vid2(self) -> int:
        """Get or set the Vector ID for DOF2 = 0 for response direction, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[2].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""relatv must be one of {0,1}""")
        self._cards[2].set_value("relatv", value)

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum frequency for FRF output (cycles/time).
        """ # nopep8
        return self._cards[3].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        self._cards[3].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum frequency for FRF output (cycles/time).
        """ # nopep8
        return self._cards[3].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        self._cards[3].set_value("fmax", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of frequencies for FRF output.
        """ # nopep8
        return self._cards[3].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
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
        if value not in [0, 1, 2]:
            raise Exception("""fspace must be one of {0,1,2}""")
        self._cards[3].set_value("fspace", value)

    @property
    def lcfreq(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining the frequencies for FRF output.
        """ # nopep8
        return self._cards[3].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
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
        if value not in [0, 1, 2, 3]:
            raise Exception("""restrt must be one of {0,1,2,3}""")
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
        if value not in [0, 1]:
            raise Exception("""output must be one of {0,1}""")
        self._cards[3].set_value("output", value)

