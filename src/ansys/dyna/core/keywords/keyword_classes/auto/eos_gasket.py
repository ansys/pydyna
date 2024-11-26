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

class EosGasket(KeywordBase):
    """DYNA EOS_GASKET keyword"""

    keyword = "EOS"
    subkeyword = "GASKET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "lcid1",
                        int,
                        10,
                        10,
                        kwargs.get("lcid1")
                    ),
                    Field(
                        "lcid2",
                        int,
                        20,
                        10,
                        kwargs.get("lcid2")
                    ),
                    Field(
                        "lcid3",
                        int,
                        30,
                        10,
                        kwargs.get("lcid3")
                    ),
                    Field(
                        "lcid4",
                        int,
                        40,
                        10,
                        kwargs.get("lcid4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unload",
                        float,
                        0,
                        10,
                        kwargs.get("unload", 0.0)
                    ),
                    Field(
                        "et",
                        float,
                        10,
                        10,
                        kwargs.get("et")
                    ),
                    Field(
                        "dmpf",
                        float,
                        20,
                        10,
                        kwargs.get("dmpf")
                    ),
                    Field(
                        "tfs",
                        float,
                        30,
                        10,
                        kwargs.get("tfs")
                    ),
                    Field(
                        "cfs",
                        float,
                        40,
                        10,
                        kwargs.get("cfs")
                    ),
                    Field(
                        "loffset",
                        float,
                        50,
                        10,
                        kwargs.get("loffset")
                    ),
                    Field(
                        "ivs",
                        float,
                        60,
                        10,
                        kwargs.get("ivs")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Load curve for loading
        """ # nopep8
        return self._cards[0].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        self._cards[0].set_value("lcid1", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve for unloading
        """ # nopep8
        return self._cards[0].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[0].set_value("lcid2", value)

    @property
    def lcid3(self) -> typing.Optional[int]:
        """Get or set the Load curve for damping as a function of volumetric strain rate
        """ # nopep8
        return self._cards[0].get_value("lcid3")

    @lcid3.setter
    def lcid3(self, value: int) -> None:
        self._cards[0].set_value("lcid3", value)

    @property
    def lcid4(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling the damping as a function of the volumetric strain
        """ # nopep8
        return self._cards[0].get_value("lcid4")

    @lcid4.setter
    def lcid4(self, value: int) -> None:
        self._cards[0].set_value("lcid4", value)

    @property
    def unload(self) -> float:
        """Get or set the Unloading option (See Volume II, Figure 119.1.):
        EQ.0.0: Loading and unloading follow loading curve
        EQ.1.0: Loading follows loading curve, unloading follows unloading curve. The unloading curve ID if undefined is taken as the loading curve.
        EQ.2.0: Loading follows loading curve, unloading follows unloading stiffness, KT or KR, to the unloading curve.  The loading and unloading curves may only intersect at the origin of the axes.
        EQ.3.0: Quadratic unloading from peak displacement value to a permanent offset..

        """ # nopep8
        return self._cards[1].get_value("unload")

    @unload.setter
    def unload(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0]:
            raise Exception("""unload must be one of {0.0,1.0,2.0,3.0}""")
        self._cards[1].set_value("unload", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[1].set_value("et", value)

    @property
    def dmpf(self) -> typing.Optional[float]:
        """Get or set the Damping factor for stability. Values in the neighborhood of unity are recommended. The damping factor is properly scaled to eliminate time step size dependency. Also, it is active if and only if ET is defined.
        """ # nopep8
        return self._cards[1].get_value("dmpf")

    @dmpf.setter
    def dmpf(self, value: float) -> None:
        self._cards[1].set_value("dmpf", value)

    @property
    def tfs(self) -> typing.Optional[float]:
        """Get or set the Tensile failure strain
        """ # nopep8
        return self._cards[1].get_value("tfs")

    @tfs.setter
    def tfs(self, value: float) -> None:
        self._cards[1].set_value("tfs", value)

    @property
    def cfs(self) -> typing.Optional[float]:
        """Get or set the Compressive failure strain
        """ # nopep8
        return self._cards[1].get_value("cfs")

    @cfs.setter
    def cfs(self, value: float) -> None:
        self._cards[1].set_value("cfs", value)

    @property
    def loffset(self) -> typing.Optional[float]:
        """Get or set the Offset factor between 0 and 1.0 to determine permanent set upon unloading if the UNLOAD=3.0.  The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively
        """ # nopep8
        return self._cards[1].get_value("loffset")

    @loffset.setter
    def loffset(self, value: float) -> None:
        self._cards[1].set_value("loffset", value)

    @property
    def ivs(self) -> typing.Optional[float]:
        """Get or set the Initial volume strain
        """ # nopep8
        return self._cards[1].get_value("ivs")

    @ivs.setter
    def ivs(self, value: float) -> None:
        self._cards[1].set_value("ivs", value)

