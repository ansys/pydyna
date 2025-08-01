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

"""Module providing the SectionDiscrete class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SectionDiscrete(KeywordBase):
    """DYNA SECTION_DISCRETE keyword"""

    keyword = "SECTION"
    subkeyword = "DISCRETE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionDiscrete class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "secid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dro",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "kd",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "v0",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cl",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "fd",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cdl",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tdl",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionDiscrete.option_specs[0],
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
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def dro(self) -> int:
        """Get or set the Displacement/Rotation Option:
        EQ.0: the material describes a translational spring/damper,
        EQ.1: the material describes a torsional spring/damper.
        """ # nopep8
        return self._cards[0].get_value("dro")

    @dro.setter
    def dro(self, value: int) -> None:
        """Set the dro property."""
        if value not in [0, 1, None]:
            raise Exception("""dro must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dro", value)

    @property
    def kd(self) -> float:
        """Get or set the Dynamic magnification factor.
        """ # nopep8
        return self._cards[0].get_value("kd")

    @kd.setter
    def kd(self, value: float) -> None:
        """Set the kd property."""
        self._cards[0].set_value("kd", value)

    @property
    def v0(self) -> float:
        """Get or set the Test velocity.
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

    @property
    def cl(self) -> float:
        """Get or set the Clearance.
        """ # nopep8
        return self._cards[0].get_value("cl")

    @cl.setter
    def cl(self, value: float) -> None:
        """Set the cl property."""
        self._cards[0].set_value("cl", value)

    @property
    def fd(self) -> float:
        """Get or set the Failure deflection (twist for DRO=1).
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[0].set_value("fd", value)

    @property
    def cdl(self) -> typing.Optional[float]:
        """Get or set the Deflection (twist for DRO=1)limit in compression.
        """ # nopep8
        return self._cards[1].get_value("cdl")

    @cdl.setter
    def cdl(self, value: float) -> None:
        """Set the cdl property."""
        self._cards[1].set_value("cdl", value)

    @property
    def tdl(self) -> typing.Optional[float]:
        """Get or set the Deflection (twist for DRO=1)limit in tension.
        """ # nopep8
        return self._cards[1].get_value("tdl")

    @tdl.setter
    def tdl(self, value: float) -> None:
        """Set the tdl property."""
        self._cards[1].set_value("tdl", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

