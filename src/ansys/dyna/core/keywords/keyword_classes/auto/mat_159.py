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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat159(KeywordBase):
    """DYNA MAT_159 keyword"""

    keyword = "MAT"
    subkeyword = "159"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "nplot",
                        int,
                        20,
                        10,
                        kwargs.get("nplot", 1)
                    ),
                    Field(
                        "incre",
                        float,
                        30,
                        10,
                        kwargs.get("incre")
                    ),
                    Field(
                        "irate",
                        int,
                        40,
                        10,
                        kwargs.get("irate", 0)
                    ),
                    Field(
                        "erode",
                        float,
                        50,
                        10,
                        kwargs.get("erode")
                    ),
                    Field(
                        "recov",
                        float,
                        60,
                        10,
                        kwargs.get("recov", 0)
                    ),
                    Field(
                        "itretrc",
                        int,
                        70,
                        10,
                        kwargs.get("itretrc", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pred",
                        float,
                        0,
                        10,
                        kwargs.get("pred")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fpc",
                        float,
                        0,
                        10,
                        kwargs.get("fpc")
                    ),
                    Field(
                        "dagg",
                        float,
                        10,
                        10,
                        kwargs.get("dagg")
                    ),
                    Field(
                        "units",
                        int,
                        20,
                        10,
                        kwargs.get("units", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat159.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification, a unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def nplot(self) -> int:
        """Get or set the Plotting options:
        EQ. 1: Maximum of brittle and ductile damage (default).
        EQ. 2: Maximum of brittle and ductile damage, with recovery of  brittle damage.
        EQ. 3:  Brittle damage.
        EQ. 4:  Ductile damage.
        EQ. 5:    (intersection of cap with shear surface).
        EQ. 6: X0 (intersection of cap with pressure axis).
        EQ. 7:   (plastic volume strain).
        """ # nopep8
        return self._cards[0].get_value("nplot")

    @nplot.setter
    def nplot(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""nplot must be one of {1,2,3,4,5,6,7}""")
        self._cards[0].set_value("nplot", value)

    @property
    def incre(self) -> typing.Optional[float]:
        """Get or set the Maximum strain increment for subincrementation.  If left blank, a default value is set during initialization based upon the shear strength and stiffness
        """ # nopep8
        return self._cards[0].get_value("incre")

    @incre.setter
    def incre(self, value: float) -> None:
        self._cards[0].set_value("incre", value)

    @property
    def irate(self) -> int:
        """Get or set the Rate effects options:
        EQ.   0: Rate effects model turned off (default).
        EQ.   1: Rate effects model turned on.
        """ # nopep8
        return self._cards[0].get_value("irate")

    @irate.setter
    def irate(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""irate must be one of {0,1}""")
        self._cards[0].set_value("irate", value)

    @property
    def erode(self) -> typing.Optional[float]:
        """Get or set the Elements erode when damage exceeds 0.99 and the maximum principal strain exceeds 1.-ERODE.   For erosion that is independent of strain, set ERODE equal to 1.0.   Erosion does not occur if ERODE is less than 1.0.
        """ # nopep8
        return self._cards[0].get_value("erode")

    @erode.setter
    def erode(self, value: float) -> None:
        self._cards[0].set_value("erode", value)

    @property
    def recov(self) -> float:
        """Get or set the The modulus is recovered in compression when RECOV is equal to 0 (default).  The modulus remains at the brittle damage level when RECOV is equal to 1.  Partial recovery is modeled for values of RECOV between 0 and 1.  Two options are available:
        Option 1:  Input a value between 0 and 1.  Recovery is based upon the sign of the pressure invariant only.
        Option 2:  Input a value between 10 and 11.  Recovery is based upon the sign of both the pressure and volumetric strain.    In this case, RECOV=RECOV-10, and a flag is set to request the volumetric strain check.
        """ # nopep8
        return self._cards[0].get_value("recov")

    @recov.setter
    def recov(self, value: float) -> None:
        self._cards[0].set_value("recov", value)

    @property
    def itretrc(self) -> int:
        """Get or set the Cap retraction option:
        EQ.0: Cap does not retract (default).
        EQ.1: Cap retracts.
        """ # nopep8
        return self._cards[0].get_value("itretrc")

    @itretrc.setter
    def itretrc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itretrc must be one of {0,1}""")
        self._cards[0].set_value("itretrc", value)

    @property
    def pred(self) -> typing.Optional[float]:
        """Get or set the Pre-existing damage (0   PreD < 1).  If left blank, the default is zero (no pre-existing damage).
        """ # nopep8
        return self._cards[1].get_value("pred")

    @pred.setter
    def pred(self, value: float) -> None:
        self._cards[1].set_value("pred", value)

    @property
    def fpc(self) -> typing.Optional[float]:
        """Get or set the Unconfined compression strength, f 'C.  If left blank, default is 30 MPa.
        """ # nopep8
        return self._cards[2].get_value("fpc")

    @fpc.setter
    def fpc(self, value: float) -> None:
        self._cards[2].set_value("fpc", value)

    @property
    def dagg(self) -> typing.Optional[float]:
        """Get or set the Maximum aggregate size, Dagg.   If left blank, default is 19 mm (3/4 inch).
        """ # nopep8
        return self._cards[2].get_value("dagg")

    @dagg.setter
    def dagg(self, value: float) -> None:
        self._cards[2].set_value("dagg", value)

    @property
    def units(self) -> int:
        """Get or set the Units options:
        EQ.0: GPa,  mm, msec,  Kg/mm3,  kN
        EQ.1: MPa, mm,  msec,     g/mm3,  Nt
        EQ.2: MPa, mm,    sec,  Mg/mm3,  Nt
        EQ.3: Psi,  inch,    sec,  lb-s2/inch4, lb
        EQ.4: Pa, m, sec, kg/m3, N.
        """ # nopep8
        return self._cards[2].get_value("units")

    @units.setter
    def units(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""units must be one of {0,1,2,3,4}""")
        self._cards[2].set_value("units", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

