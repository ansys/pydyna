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

class InterfaceBlanksizeScaleFactor(KeywordBase):
    """DYNA INTERFACE_BLANKSIZE_SCALE_FACTOR keyword"""

    keyword = "INTERFACE"
    subkeyword = "BLANKSIZE_SCALE_FACTOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "idcrv",
                        int,
                        0,
                        10,
                        kwargs.get("idcrv", 1)
                    ),
                    Field(
                        "sf",
                        float,
                        10,
                        10,
                        kwargs.get("sf", 0.0)
                    ),
                    Field(
                        "offx",
                        float,
                        20,
                        10,
                        kwargs.get("offx", 0.0)
                    ),
                    Field(
                        "offy",
                        float,
                        30,
                        10,
                        kwargs.get("offy", 0.0)
                    ),
                    Field(
                        "offz",
                        float,
                        40,
                        10,
                        kwargs.get("offz", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def idcrv(self) -> int:
        """Get or set the Curve ID in the order of appearance as in FILENAME1 in the target card, as defined by *DEFINE_TARGET_BOUNDARY.
        """ # nopep8
        return self._cards[0].get_value("idcrv")

    @idcrv.setter
    def idcrv(self, value: int) -> None:
        self._cards[0].set_value("idcrv", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor for the IDCRV defined above.  It defines a fraction of the changes required for the predicted initial blank shape.
        For example, if SF is set to 0.0 � the corresponding IDCRV will be excluded from the calculation (although the original initial curve still will be output);
        on the other hand, if SF is set to  1.0 , full change will be applied to obtain the modified initial blank that reflects the forming process.
        A SF of 0.5 will apply 50% of the changes required to map the initial blank.  This feature is especially important for inner holes that are small and hole boundary expansions are large,
        so the predicted initial hole can avoid  crisscross  situation.  An example is provided in Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def offx(self) -> float:
        """Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
        Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
        """ # nopep8
        return self._cards[0].get_value("offx")

    @offx.setter
    def offx(self, value: float) -> None:
        self._cards[0].set_value("offx", value)

    @property
    def offy(self) -> float:
        """Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
        Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
        """ # nopep8
        return self._cards[0].get_value("offy")

    @offy.setter
    def offy(self, value: float) -> None:
        self._cards[0].set_value("offy", value)

    @property
    def offz(self) -> float:
        """Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
        Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
        """ # nopep8
        return self._cards[0].get_value("offz")

    @offz.setter
    def offz(self, value: float) -> None:
        self._cards[0].set_value("offz", value)

