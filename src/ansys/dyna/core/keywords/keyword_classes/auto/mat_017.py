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
from ansys.dyna.core.lib.config import use_lspp_defaults
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat017(KeywordBase):
    """DYNA MAT_017 keyword"""

    keyword = "MAT"
    subkeyword = "017"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "etan",
                        float,
                        50,
                        10,
                        kwargs.get("etan")
                    ),
                    Field(
                        "fs",
                        float,
                        60,
                        10,
                        kwargs.get("fs")
                    ),
                    Field(
                        "prf",
                        float,
                        70,
                        10,
                        kwargs.get("prf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "soft",
                        float,
                        0,
                        10,
                        kwargs.get("soft", 1.0 if use_lspp_defaults() else None)
                    ),
                    Field(
                        "cvelo",
                        float,
                        10,
                        10,
                        kwargs.get("cvelo")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat017.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        self._cards[0].set_value("etan", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Fracture stress.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[0].set_value("fs", value)

    @property
    def prf(self) -> typing.Optional[float]:
        """Get or set the Failure or cutoff pressure(<= 0.0).
        """ # nopep8
        return self._cards[0].get_value("prf")

    @prf.setter
    def prf(self, value: float) -> None:
        self._cards[0].set_value("prf", value)

    @property
    def soft(self) -> float:
        """Get or set the Factor by which the fracture stress is reduced when a crack is coming from failed neighboring element.
        """ # nopep8
        return self._cards[1].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        self._cards[1].set_value("soft", value)

    @property
    def cvelo(self) -> typing.Optional[float]:
        """Get or set the Crack propagation velocity.
        """ # nopep8
        return self._cards[1].get_value("cvelo")

    @cvelo.setter
    def cvelo(self, value: float) -> None:
        self._cards[1].set_value("cvelo", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

