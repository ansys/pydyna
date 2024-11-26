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

class DefineFpToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_FP_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "FP_TO_SURFACE_COUPLING"
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
                        "fp",
                        int,
                        0,
                        10,
                        kwargs.get("fp")
                    ),
                    Field(
                        "surf",
                        int,
                        10,
                        10,
                        kwargs.get("surf")
                    ),
                    Field(
                        "fptype",
                        int,
                        20,
                        10,
                        kwargs.get("fptype", 0)
                    ),
                    Field(
                        "surftype",
                        int,
                        30,
                        10,
                        kwargs.get("surftype", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sbc",
                        int,
                        0,
                        10,
                        kwargs.get("sbc")
                    ),
                    Field(
                        "sca",
                        int,
                        10,
                        10,
                        kwargs.get("sca", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "sfp",
                        int,
                        50,
                        10,
                        kwargs.get("sfp", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFpToSurfaceCoupling.option_specs[0],
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
    def fp(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in the coupling on the slave side.
        """ # nopep8
        return self._cards[0].get_value("fp")

    @fp.setter
    def fp(self, value: int) -> None:
        self._cards[0].set_value("fp", value)

    @property
    def surf(self) -> typing.Optional[int]:
        """Get or set the Segments set ID defined in the coupling on the master side. Currently the segments set should be generated from the 8-noded hexahedron elements.
        """ # nopep8
        return self._cards[0].get_value("surf")

    @surf.setter
    def surf(self, value: int) -> None:
        self._cards[0].set_value("surf", value)

    @property
    def fptype(self) -> int:
        """Get or set the Type for SLAVE:
        EQ.0: Part set ID
        EQ.1 : Part ID.
        """ # nopep8
        return self._cards[0].get_value("fptype")

    @fptype.setter
    def fptype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""fptype must be one of {0,1}""")
        self._cards[0].set_value("fptype", value)

    @property
    def surftype(self) -> int:
        """Get or set the Type for SURF:
        EQ.0:	Segment set ID
        """ # nopep8
        return self._cards[0].get_value("surftype")

    @surftype.setter
    def surftype(self, value: int) -> None:
        self._cards[0].set_value("surftype", value)

    @property
    def sbc(self) -> typing.Optional[int]:
        """Get or set the Type of boundary condition.
        EQ.0: free-slip boundary
        EQ.1: non - slip boundary
        """ # nopep8
        return self._cards[1].get_value("sbc")

    @sbc.setter
    def sbc(self, value: int) -> None:
        self._cards[1].set_value("sbc", value)

    @property
    def sca(self) -> int:
        """Get or set the Static (equilibrium) contact angle in radian
        """ # nopep8
        return self._cards[1].get_value("sca")

    @sca.setter
    def sca(self, value: int) -> None:
        self._cards[1].set_value("sca", value)

    @property
    def sfp(self) -> int:
        """Get or set the Stiffness coefficient along the normal direction of the contact interface. SFP should be less than 1.0. If SFPSFPN is too small, large penetrations can occur.
        """ # nopep8
        return self._cards[1].get_value("sfp")

    @sfp.setter
    def sfp(self, value: int) -> None:
        self._cards[1].set_value("sfp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

