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

class ElementLancing(KeywordBase):
    """DYNA ELEMENT_LANCING keyword"""

    keyword = "ELEMENT"
    subkeyword = "LANCING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "idpt",
                        int,
                        0,
                        10,
                        kwargs.get("idpt")
                    ),
                    Field(
                        "idcv",
                        int,
                        10,
                        10,
                        kwargs.get("idcv")
                    ),
                    Field(
                        "irefine",
                        int,
                        20,
                        10,
                        kwargs.get("irefine")
                    ),
                    Field(
                        "smin",
                        float,
                        30,
                        10,
                        kwargs.get("smin")
                    ),
                    Field(
                        "at",
                        float,
                        40,
                        10,
                        kwargs.get("at")
                    ),
                    Field(
                        "endt",
                        float,
                        50,
                        10,
                        kwargs.get("endt")
                    ),
                    Field(
                        "ntimes",
                        int,
                        60,
                        10,
                        kwargs.get("ntimes")
                    ),
                ],
            ),
        ]

    @property
    def idpt(self) -> typing.Optional[int]:
        """Get or set the PID of the sheet blank to be lanced, see *PART.
        """ # nopep8
        return self._cards[0].get_value("idpt")

    @idpt.setter
    def idpt(self, value: int) -> None:
        self._cards[0].set_value("idpt", value)

    @property
    def idcv(self) -> typing.Optional[int]:
        """Get or set the Curve ID (the variable TCID in *DEFINE_CURVE_TRIM_3D) defining a lancing route (see Remarks)..
        """ # nopep8
        return self._cards[0].get_value("idcv")

    @idcv.setter
    def idcv(self, value: int) -> None:
        self._cards[0].set_value("idcv", value)

    @property
    def irefine(self) -> typing.Optional[int]:
        """Get or set the Set IREFINE = 1 to refine elements along lancing route until no
        adapted nodes exist in the neighborhood. This feature result in a
        more robust lancing in the form of improved lancing boundary.
        Available starting in Revision 107708..
        """ # nopep8
        return self._cards[0].get_value("irefine")

    @irefine.setter
    def irefine(self, value: int) -> None:
        self._cards[0].set_value("irefine", value)

    @property
    def smin(self) -> typing.Optional[float]:
        """Get or set the Minimum element characteristic length to be refined to, to be
        supported in the future. Currently, no refinement will be made..
        """ # nopep8
        return self._cards[0].get_value("smin")

    @smin.setter
    def smin(self, value: float) -> None:
        self._cards[0].set_value("smin", value)

    @property
    def at(self) -> typing.Optional[float]:
        """Get or set the Activation time for lancing operation. This variable needs to be
        defined for both instant and progressive lancing types (see Remarks).
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[0].set_value("at", value)

    @property
    def endt(self) -> typing.Optional[float]:
        """Get or set the End time (for progressive lancing only).
        """ # nopep8
        return self._cards[0].get_value("endt")

    @endt.setter
    def endt(self, value: float) -> None:
        self._cards[0].set_value("endt", value)

    @property
    def ntimes(self) -> typing.Optional[int]:
        """Get or set the A progressive lancing operation is evenly divided into NTIMES
        segments between AT and ENDT; within each segment lancing is
        done instantly. Do not define for instant lancing.
        """ # nopep8
        return self._cards[0].get_value("ntimes")

    @ntimes.setter
    def ntimes(self, value: int) -> None:
        self._cards[0].set_value("ntimes", value)

