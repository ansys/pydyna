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

class MatInelastic6DofSpringDiscreteBeam(KeywordBase):
    """DYNA MAT_INELASTIC_6DOF_SPRING_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "INELASTIC_6DOF_SPRING_DISCRETE_BEAM"
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
                        "tpidr",
                        int,
                        20,
                        10,
                        kwargs.get("tpidr")
                    ),
                    Field(
                        "tpids",
                        int,
                        30,
                        10,
                        kwargs.get("tpids")
                    ),
                    Field(
                        "tpidt",
                        int,
                        40,
                        10,
                        kwargs.get("tpidt")
                    ),
                    Field(
                        "rpidr",
                        int,
                        50,
                        10,
                        kwargs.get("rpidr")
                    ),
                    Field(
                        "rpids",
                        int,
                        60,
                        10,
                        kwargs.get("rpids")
                    ),
                    Field(
                        "rpidt",
                        int,
                        70,
                        10,
                        kwargs.get("rpidt")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatInelastic6DofSpringDiscreteBeam.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def tpidr(self) -> typing.Optional[int]:
        """Get or set the Translational motion in the local r-direction is governed by part ID TPIDR. If zero, no force is computed in this direction.
        """ # nopep8
        return self._cards[0].get_value("tpidr")

    @tpidr.setter
    def tpidr(self, value: int) -> None:
        self._cards[0].set_value("tpidr", value)

    @property
    def tpids(self) -> typing.Optional[int]:
        """Get or set the Translational motion in the local s-direction is governed by part ID TPIDS. If zero, no force is computed in this direction.
        """ # nopep8
        return self._cards[0].get_value("tpids")

    @tpids.setter
    def tpids(self, value: int) -> None:
        self._cards[0].set_value("tpids", value)

    @property
    def tpidt(self) -> typing.Optional[int]:
        """Get or set the Translational motion in the local t-direction is governed by part ID TPIDT. If zero, no force is computed in this direction.
        """ # nopep8
        return self._cards[0].get_value("tpidt")

    @tpidt.setter
    def tpidt(self, value: int) -> None:
        self._cards[0].set_value("tpidt", value)

    @property
    def rpidr(self) -> typing.Optional[int]:
        """Get or set the Rotational motion about the local r-axis is governed by part ID RPIDR. If zero, no moment is computed about this axis.
        """ # nopep8
        return self._cards[0].get_value("rpidr")

    @rpidr.setter
    def rpidr(self, value: int) -> None:
        self._cards[0].set_value("rpidr", value)

    @property
    def rpids(self) -> typing.Optional[int]:
        """Get or set the Rotational motion about the local s-axis is governed by part ID RPIDS. If zero, no moment is computed about this axis.
        """ # nopep8
        return self._cards[0].get_value("rpids")

    @rpids.setter
    def rpids(self, value: int) -> None:
        self._cards[0].set_value("rpids", value)

    @property
    def rpidt(self) -> typing.Optional[int]:
        """Get or set the Rotational motion about the local t-axis is governed by part ID RPIDT. If zero, no moment is computed about this axis.
        """ # nopep8
        return self._cards[0].get_value("rpidt")

    @rpidt.setter
    def rpidt(self, value: int) -> None:
        self._cards[0].set_value("rpidt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

