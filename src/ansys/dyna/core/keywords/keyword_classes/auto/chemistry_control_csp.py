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

class ChemistryControlCsp(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_CSP keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_CSP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "ierropt",
                        int,
                        10,
                        10,
                        kwargs.get("ierropt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ampl",
                        float,
                        0,
                        10,
                        kwargs.get("ampl")
                    ),
                    Field(
                        "ycut",
                        float,
                        10,
                        10,
                        kwargs.get("ycut")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this computational singular perturbation solver.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def ierropt(self) -> typing.Optional[int]:
        """Get or set the Selector:
        EQ.0: AMPL and YCUT values for all chemical species are required.
        EQ.1: One CSP Parameter Card should be provided, and it will be used for all species.
        """ # nopep8
        return self._cards[0].get_value("ierropt")

    @ierropt.setter
    def ierropt(self, value: int) -> None:
        self._cards[0].set_value("ierropt", value)

    @property
    def ampl(self) -> typing.Optional[float]:
        """Get or set the Relative accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[1].get_value("ampl")

    @ampl.setter
    def ampl(self, value: float) -> None:
        self._cards[1].set_value("ampl", value)

    @property
    def ycut(self) -> typing.Optional[float]:
        """Get or set the Absolute accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[1].get_value("ycut")

    @ycut.setter
    def ycut(self, value: float) -> None:
        self._cards[1].set_value("ycut", value)

