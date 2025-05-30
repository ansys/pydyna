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

"""Module providing the FrequencyDomainSeaSubsystem class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class FrequencyDomainSeaSubsystem(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_SEA_SUBSYSTEM keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_SEA_SUBSYSTEM"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainSeaSubsystem class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fmin",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fmax",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nfreq",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nfspace",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcfreq",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iread",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "subid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "subtyp",
                        int,
                        10,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "density",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "output",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "perim",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "thick",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "width",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "length",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dampb",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dampl",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "damps",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc1",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc2",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc3",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "perim",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "volume",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "width",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "length",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "height",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dampb",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc1",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iss",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "itt",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "j",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "length",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dampb",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dampl",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "damps",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dampt",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc1",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc2",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc3",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc4",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum frequency for SEA output (cycles/time).
        """ # nopep8
        return self._cards[0].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        """Set the fmin property."""
        self._cards[0].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum frequency for SEA output (cycles/time).
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[0].set_value("fmax", value)

    @property
    def nfreq(self) -> typing.Optional[int]:
        """Get or set the Number of frequencies for SEA output (cycles/time).
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[0].set_value("nfreq", value)

    @property
    def nfspace(self) -> int:
        """Get or set the Frequency spacing option for SEA output:
        EQ.0: linear
        EQ.1: logarithmic
        EQ.2: biased
        """ # nopep8
        return self._cards[0].get_value("nfspace")

    @nfspace.setter
    def nfspace(self, value: int) -> None:
        """Set the nfspace property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nfspace must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nfspace", value)

    @property
    def lcfreq(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining the frequencies for SEA output.
        """ # nopep8
        return self._cards[0].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
        """Set the lcfreq property."""
        self._cards[0].set_value("lcfreq", value)

    @property
    def iread(self) -> int:
        """Get or set the Type of SEA run:
        EQ.0:	run SEA analysis.
        EQ.1 : read FEM keyword input deck and create SEA model..
        """ # nopep8
        return self._cards[0].get_value("iread")

    @iread.setter
    def iread(self, value: int) -> None:
        """Set the iread property."""
        if value not in [0, 1, None]:
            raise Exception("""iread must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iread", value)

    @property
    def subid(self) -> typing.Optional[int]:
        """Get or set the ID of subsystem.
        """ # nopep8
        return self._cards[1].get_value("subid")

    @subid.setter
    def subid(self, value: int) -> None:
        """Set the subid property."""
        self._cards[1].set_value("subid", value)

    @property
    def subtyp(self) -> int:
        """Get or set the Type of subsystem:
        EQ.1: plate
        EQ.2: cavity
        EQ.3: beam.
        """ # nopep8
        return self._cards[1].get_value("subtyp")

    @subtyp.setter
    def subtyp(self, value: int) -> None:
        """Set the subtyp property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""subtyp must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("subtyp", value)

    @property
    def density(self) -> typing.Optional[float]:
        """Get or set the Mass density of subsystem.
        """ # nopep8
        return self._cards[1].get_value("density")

    @density.setter
    def density(self, value: float) -> None:
        """Set the density property."""
        self._cards[1].set_value("density", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of subsystem.
        """ # nopep8
        return self._cards[1].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[1].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio of subsystem.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[1].set_value("pr", value)

    @property
    def output(self) -> int:
        """Get or set the Include this subsystem in output:
        EQ.0:	no
        EQ.1 : yes.
        """ # nopep8
        return self._cards[1].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        if value not in [0, 1, None]:
            raise Exception("""output must be `None` or one of {0,1}.""")
        self._cards[1].set_value("output", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Plate Area.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[2].set_value("a", value)

    @property
    def perim(self) -> typing.Optional[float]:
        """Get or set the Plate Perimeter.
        """ # nopep8
        return self._cards[2].get_value("perim")

    @perim.setter
    def perim(self, value: float) -> None:
        """Set the perim property."""
        self._cards[2].set_value("perim", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Plate Thickness.
        """ # nopep8
        return self._cards[2].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[2].set_value("thick", value)

    @property
    def width(self) -> typing.Optional[float]:
        """Get or set the Plate width.
        """ # nopep8
        return self._cards[2].get_value("width")

    @width.setter
    def width(self, value: float) -> None:
        """Set the width property."""
        self._cards[2].set_value("width", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Plate length.
        """ # nopep8
        return self._cards[2].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[2].set_value("length", value)

    @property
    def dampb(self) -> typing.Optional[float]:
        """Get or set the Damping factor for bending wave.
        """ # nopep8
        return self._cards[3].get_value("dampb")

    @dampb.setter
    def dampb(self, value: float) -> None:
        """Set the dampb property."""
        self._cards[3].set_value("dampb", value)

    @property
    def dampl(self) -> typing.Optional[float]:
        """Get or set the Damping factor for longitudinal wave.
        """ # nopep8
        return self._cards[3].get_value("dampl")

    @dampl.setter
    def dampl(self, value: float) -> None:
        """Set the dampl property."""
        self._cards[3].set_value("dampl", value)

    @property
    def damps(self) -> typing.Optional[float]:
        """Get or set the Damping factor for shear wave.
        """ # nopep8
        return self._cards[3].get_value("damps")

    @damps.setter
    def damps(self, value: float) -> None:
        """Set the damps property."""
        self._cards[3].set_value("damps", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve for damping factor for bending wave.
        """ # nopep8
        return self._cards[3].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[3].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve for damping factor for longitudinal wave.
        """ # nopep8
        return self._cards[3].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[3].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve for damping factor for shear wave.
        """ # nopep8
        return self._cards[3].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[3].set_value("lc3", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Cavity area.
        """ # nopep8
        return self._cards[4].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[4].set_value("a", value)

    @property
    def perim(self) -> typing.Optional[float]:
        """Get or set the Cavity perimeter.
        """ # nopep8
        return self._cards[4].get_value("perim")

    @perim.setter
    def perim(self, value: float) -> None:
        """Set the perim property."""
        self._cards[4].set_value("perim", value)

    @property
    def volume(self) -> typing.Optional[float]:
        """Get or set the Cavity volume.
        """ # nopep8
        return self._cards[4].get_value("volume")

    @volume.setter
    def volume(self, value: float) -> None:
        """Set the volume property."""
        self._cards[4].set_value("volume", value)

    @property
    def width(self) -> typing.Optional[float]:
        """Get or set the Cavity width.
        """ # nopep8
        return self._cards[4].get_value("width")

    @width.setter
    def width(self, value: float) -> None:
        """Set the width property."""
        self._cards[4].set_value("width", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Cavity length.
        """ # nopep8
        return self._cards[4].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[4].set_value("length", value)

    @property
    def height(self) -> typing.Optional[int]:
        """Get or set the Cavity height.
        """ # nopep8
        return self._cards[4].get_value("height")

    @height.setter
    def height(self, value: int) -> None:
        """Set the height property."""
        self._cards[4].set_value("height", value)

    @property
    def dampb(self) -> typing.Optional[float]:
        """Get or set the Damping factor for bending wave.
        """ # nopep8
        return self._cards[5].get_value("dampb")

    @dampb.setter
    def dampb(self, value: float) -> None:
        """Set the dampb property."""
        self._cards[5].set_value("dampb", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve for damping factor for bending wave.
        """ # nopep8
        return self._cards[5].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[5].set_value("lc1", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Beam area.
        """ # nopep8
        return self._cards[6].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[6].set_value("a", value)

    @property
    def iss(self) -> typing.Optional[float]:
        """Get or set the area moment of inertia about local s-axis.
        """ # nopep8
        return self._cards[6].get_value("iss")

    @iss.setter
    def iss(self, value: float) -> None:
        """Set the iss property."""
        self._cards[6].set_value("iss", value)

    @property
    def itt(self) -> typing.Optional[float]:
        """Get or set the area moment of inertia about local t-axis.
        """ # nopep8
        return self._cards[6].get_value("itt")

    @itt.setter
    def itt(self, value: float) -> None:
        """Set the itt property."""
        self._cards[6].set_value("itt", value)

    @property
    def j(self) -> typing.Optional[float]:
        """Get or set the torsional constant.
        """ # nopep8
        return self._cards[6].get_value("j")

    @j.setter
    def j(self, value: float) -> None:
        """Set the j property."""
        self._cards[6].set_value("j", value)

    @property
    def length(self) -> typing.Optional[float]:
        """Get or set the Beam length.
        """ # nopep8
        return self._cards[6].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[6].set_value("length", value)

    @property
    def dampb(self) -> typing.Optional[float]:
        """Get or set the Damping factor for bending wave.
        """ # nopep8
        return self._cards[7].get_value("dampb")

    @dampb.setter
    def dampb(self, value: float) -> None:
        """Set the dampb property."""
        self._cards[7].set_value("dampb", value)

    @property
    def dampl(self) -> typing.Optional[float]:
        """Get or set the Damping factor for longitudinal wave.
        """ # nopep8
        return self._cards[7].get_value("dampl")

    @dampl.setter
    def dampl(self, value: float) -> None:
        """Set the dampl property."""
        self._cards[7].set_value("dampl", value)

    @property
    def damps(self) -> typing.Optional[float]:
        """Get or set the Damping factor for shear wave.
        """ # nopep8
        return self._cards[7].get_value("damps")

    @damps.setter
    def damps(self, value: float) -> None:
        """Set the damps property."""
        self._cards[7].set_value("damps", value)

    @property
    def dampt(self) -> typing.Optional[float]:
        """Get or set the Damping factor for torsional wave
        """ # nopep8
        return self._cards[7].get_value("dampt")

    @dampt.setter
    def dampt(self, value: float) -> None:
        """Set the dampt property."""
        self._cards[7].set_value("dampt", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve for damping factor for bending wave.
        """ # nopep8
        return self._cards[7].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[7].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve for damping factor for longitudinal wave.
        """ # nopep8
        return self._cards[7].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[7].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve for damping factor for shear wave.
        """ # nopep8
        return self._cards[7].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[7].set_value("lc3", value)

    @property
    def lc4(self) -> int:
        """Get or set the Load curve for damping factor for torsional wave
        """ # nopep8
        return self._cards[7].get_value("lc4")

    @lc4.setter
    def lc4(self, value: int) -> None:
        """Set the lc4 property."""
        self._cards[7].set_value("lc4", value)

