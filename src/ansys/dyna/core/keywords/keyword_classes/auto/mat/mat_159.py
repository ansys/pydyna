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

"""Module providing the Mat159 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT159_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("nplot", int, 20, 10, 1),
    FieldSchema("incre", float, 30, 10, None),
    FieldSchema("irate", int, 40, 10, 0),
    FieldSchema("erode", float, 50, 10, None),
    FieldSchema("recov", float, 60, 10, 0.0),
    FieldSchema("itretrc", int, 70, 10, 0),
)

_MAT159_CARD1 = (
    FieldSchema("pred", float, 0, 10, None),
)

_MAT159_CARD2 = (
    FieldSchema("fpc", float, 0, 10, None),
    FieldSchema("dagg", float, 10, 10, None),
    FieldSchema("units", int, 20, 10, 0),
)

class Mat159(KeywordBase):
    """DYNA MAT_159 keyword"""

    keyword = "MAT"
    subkeyword = "159"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat159 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT159_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT159_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT159_CARD2,
                **kwargs,
            ),            OptionCardSet(
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
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
        """Set the nplot property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""nplot must be `None` or one of {1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("nplot", value)

    @property
    def incre(self) -> typing.Optional[float]:
        """Get or set the Maximum strain increment for subincrementation.  If left blank, a default value is set during initialization based upon the shear strength and stiffness
        """ # nopep8
        return self._cards[0].get_value("incre")

    @incre.setter
    def incre(self, value: float) -> None:
        """Set the incre property."""
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
        """Set the irate property."""
        if value not in [0, 1, None]:
            raise Exception("""irate must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irate", value)

    @property
    def erode(self) -> typing.Optional[float]:
        """Get or set the Elements erode when damage exceeds 0.99 and the maximum principal strain exceeds 1.-ERODE.   For erosion that is independent of strain, set ERODE equal to 1.0.   Erosion does not occur if ERODE is less than 1.0.
        """ # nopep8
        return self._cards[0].get_value("erode")

    @erode.setter
    def erode(self, value: float) -> None:
        """Set the erode property."""
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
        """Set the recov property."""
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
        """Set the itretrc property."""
        if value not in [0, 1, None]:
            raise Exception("""itretrc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("itretrc", value)

    @property
    def pred(self) -> typing.Optional[float]:
        """Get or set the Pre-existing damage (0   PreD < 1).  If left blank, the default is zero (no pre-existing damage).
        """ # nopep8
        return self._cards[1].get_value("pred")

    @pred.setter
    def pred(self, value: float) -> None:
        """Set the pred property."""
        self._cards[1].set_value("pred", value)

    @property
    def fpc(self) -> typing.Optional[float]:
        """Get or set the Unconfined compression strength, f 'C.  If left blank, default is 30 MPa.
        """ # nopep8
        return self._cards[2].get_value("fpc")

    @fpc.setter
    def fpc(self, value: float) -> None:
        """Set the fpc property."""
        self._cards[2].set_value("fpc", value)

    @property
    def dagg(self) -> typing.Optional[float]:
        """Get or set the Maximum aggregate size, Dagg.   If left blank, default is 19 mm (3/4 inch).
        """ # nopep8
        return self._cards[2].get_value("dagg")

    @dagg.setter
    def dagg(self, value: float) -> None:
        """Set the dagg property."""
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
        """Set the units property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""units must be `None` or one of {0,1,2,3,4}.""")
        self._cards[2].set_value("units", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

