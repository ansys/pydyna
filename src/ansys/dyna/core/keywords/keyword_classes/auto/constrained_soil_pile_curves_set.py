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

class ConstrainedSoilPileCurvesSet(KeywordBase):
    """DYNA CONSTRAINED_SOIL_PILE_CURVES_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SOIL_PILE_CURVES_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pbsid",
                        int,
                        0,
                        10,
                        kwargs.get("pbsid")
                    ),
                    Field(
                        "diam",
                        float,
                        10,
                        10,
                        kwargs.get("diam")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "pidns",
                        int,
                        30,
                        10,
                        kwargs.get("pidns")
                    ),
                    Field(
                        "pidnb",
                        int,
                        40,
                        10,
                        kwargs.get("pidnb")
                    ),
                    Field(
                        "error",
                        int,
                        50,
                        10,
                        kwargs.get("error", 0)
                    ),
                    Field(
                        "nring",
                        int,
                        60,
                        10,
                        kwargs.get("nring", 1)
                    ),
                    Field(
                        "nringb",
                        int,
                        70,
                        10,
                        kwargs.get("nringb")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "damp",
                        float,
                        0,
                        10,
                        kwargs.get("damp", 0.0)
                    ),
                    Field(
                        "local",
                        int,
                        10,
                        10,
                        kwargs.get("local", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "zref",
                        float,
                        10,
                        10,
                        kwargs.get("zref")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "blcz",
                        int,
                        0,
                        10,
                        kwargs.get("blcz")
                    ),
                    Field(
                        "blc",
                        int,
                        10,
                        10,
                        kwargs.get("blc")
                    ),
                    Field(
                        "blcsh",
                        int,
                        20,
                        10,
                        kwargs.get("blcsh")
                    ),
                    Field(
                        "blcsv",
                        int,
                        30,
                        10,
                        kwargs.get("blcsv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vlcz",
                        int,
                        0,
                        10,
                        kwargs.get("vlcz")
                    ),
                    Field(
                        "vlc",
                        int,
                        10,
                        10,
                        kwargs.get("vlc")
                    ),
                    Field(
                        "vlcsh",
                        int,
                        20,
                        10,
                        kwargs.get("vlcsh")
                    ),
                    Field(
                        "vlcsv",
                        int,
                        30,
                        10,
                        kwargs.get("vlcsv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hlcz",
                        int,
                        0,
                        10,
                        kwargs.get("hlcz")
                    ),
                    Field(
                        "hlc",
                        int,
                        10,
                        10,
                        kwargs.get("hlc")
                    ),
                    Field(
                        "hlcsh",
                        int,
                        20,
                        10,
                        kwargs.get("hlcsh")
                    ),
                    Field(
                        "hlcsv",
                        int,
                        30,
                        10,
                        kwargs.get("hlcsv")
                    ),
                ],
            ),
        ]

    @property
    def pbsid(self) -> typing.Optional[int]:
        """Get or set the Part set ID containing beam elements for coupling (the piles).
        """ # nopep8
        return self._cards[0].get_value("pbsid")

    @pbsid.setter
    def pbsid(self, value: int) -> None:
        self._cards[0].set_value("pbsid", value)

    @property
    def diam(self) -> typing.Optional[float]:
        """Get or set the Pile diameter (optional). If zero or blank, the pile diameter will be taken automatically from the section properties of the beam element.
        """ # nopep8
        return self._cards[0].get_value("diam")

    @diam.setter
    def diam(self, value: float) -> None:
        self._cards[0].set_value("diam", value)

    @property
    def pidns(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for perpendicular and axial coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidns")

    @pidns.setter
    def pidns(self, value: int) -> None:
        self._cards[0].set_value("pidns", value)

    @property
    def pidnb(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for base coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidnb")

    @pidnb.setter
    def pidnb(self, value: int) -> None:
        self._cards[0].set_value("pidnb", value)

    @property
    def error(self) -> int:
        """Get or set the Action taken if any coupling point is not constrained within a soil element:
        EQ.0:	Stop with an error message.
        EQ.1 : Warn and continue..
        """ # nopep8
        return self._cards[0].get_value("error")

    @error.setter
    def error(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""error must be one of {0,1}""")
        self._cards[0].set_value("error", value)

    @property
    def nring(self) -> int:
        """Get or set the Number of coupling points around circumference at each pile node:
        EQ.1:	One coupling point coincident with pile node
        GT.1 : NRING coupling points equally spaced around the circumference of the pile.
        """ # nopep8
        return self._cards[0].get_value("nring")

    @nring.setter
    def nring(self, value: int) -> None:
        self._cards[0].set_value("nring", value)

    @property
    def nringb(self) -> typing.Optional[int]:
        """Get or set the Number of extra rings of coupling points on base, in addition to those around the pile circumference. By default, NRINGB is chosen automatically to distribute the base stress as uniformly as possible .
        """ # nopep8
        return self._cards[0].get_value("nringb")

    @nringb.setter
    def nringb(self, value: int) -> None:
        self._cards[0].set_value("nringb", value)

    @property
    def damp(self) -> float:
        """Get or set the Optional damping coefficient for Axial coupling (stress/velocity units). An additional axial coupling shear stress equal to DAMP times the axial velocity of the pile relative to the soil will be generated.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[1].set_value("damp", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to identify which free end of a pile is treated as the Base:
        EQ.1:	End with the most negative global Z - coordinate
        EQ.2 : End which is Node 1 of the attached beam element topology.
        """ # nopep8
        return self._cards[1].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""local must be one of {1,2}""")
        self._cards[1].set_value("local", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID (depending on OPTION2) containing solid elements for coupling (the soil).
        """ # nopep8
        return self._cards[2].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[2].set_value("psid", value)

    @property
    def zref(self) -> typing.Optional[float]:
        """Get or set the Reference Z-coordinate, used in calculation of “relative z-coordinate”. For example, ZREF may be located at the soil surface.  .
        """ # nopep8
        return self._cards[2].get_value("zref")

    @zref.setter
    def zref(self, value: float) -> None:
        self._cards[2].set_value("zref", value)

    @property
    def blcz(self) -> typing.Optional[int]:
        """Get or set the For base coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[3].get_value("blcz")

    @blcz.setter
    def blcz(self, value: int) -> None:
        self._cards[3].set_value("blcz", value)

    @property
    def blc(self) -> typing.Optional[int]:
        """Get or set the For base coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement.
        """ # nopep8
        return self._cards[3].get_value("blc")

    @blc.setter
    def blc(self, value: int) -> None:
        self._cards[3].set_value("blc", value)

    @property
    def blcsh(self) -> typing.Optional[int]:
        """Get or set the For base coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate .
        """ # nopep8
        return self._cards[3].get_value("blcsh")

    @blcsh.setter
    def blcsh(self, value: int) -> None:
        self._cards[3].set_value("blcsh", value)

    @property
    def blcsv(self) -> typing.Optional[int]:
        """Get or set the For base coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate .
        """ # nopep8
        return self._cards[3].get_value("blcsv")

    @blcsv.setter
    def blcsv(self, value: int) -> None:
        self._cards[3].set_value("blcsv", value)

    @property
    def vlcz(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[4].get_value("vlcz")

    @vlcz.setter
    def vlcz(self, value: int) -> None:
        self._cards[4].set_value("vlcz", value)

    @property
    def vlc(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement .
        """ # nopep8
        return self._cards[4].get_value("vlc")

    @vlc.setter
    def vlc(self, value: int) -> None:
        self._cards[4].set_value("vlc", value)

    @property
    def vlcsh(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[4].get_value("vlcsh")

    @vlcsh.setter
    def vlcsh(self, value: int) -> None:
        self._cards[4].set_value("vlcsh", value)

    @property
    def vlcsv(self) -> typing.Optional[int]:
        """Get or set the For axial coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[4].get_value("vlcsv")

    @vlcsv.setter
    def vlcsv(self, value: int) -> None:
        self._cards[4].set_value("vlcsv", value)

    @property
    def hlcz(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
        """ # nopep8
        return self._cards[5].get_value("hlcz")

    @hlcz.setter
    def hlcz(self, value: int) -> None:
        self._cards[5].set_value("hlcz", value)

    @property
    def hlc(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement
        """ # nopep8
        return self._cards[5].get_value("hlc")

    @hlc.setter
    def hlc(self, value: int) -> None:
        self._cards[5].set_value("hlc", value)

    @property
    def hlcsh(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate
        """ # nopep8
        return self._cards[5].get_value("hlcsh")

    @hlcsh.setter
    def hlcsh(self, value: int) -> None:
        self._cards[5].set_value("hlcsh", value)

    @property
    def hlcsv(self) -> typing.Optional[int]:
        """Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
        """ # nopep8
        return self._cards[5].get_value("hlcsv")

    @hlcsv.setter
    def hlcsv(self, value: int) -> None:
        self._cards[5].set_value("hlcsv", value)

