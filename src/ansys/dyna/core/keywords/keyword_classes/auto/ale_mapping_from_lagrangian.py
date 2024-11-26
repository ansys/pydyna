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

class AleMappingFromLagrangian(KeywordBase):
    """DYNA ALE_MAPPING_FROM_LAGRANGIAN keyword"""

    keyword = "ALE"
    subkeyword = "MAPPING_FROM_LAGRANGIAN"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lagpid",
                        int,
                        0,
                        10,
                        kwargs.get("lagpid")
                    ),
                    Field(
                        "lagpty",
                        int,
                        10,
                        10,
                        kwargs.get("lagpty", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nx",
                        int,
                        0,
                        10,
                        kwargs.get("nx")
                    ),
                    Field(
                        "ny",
                        int,
                        10,
                        10,
                        kwargs.get("ny")
                    ),
                    Field(
                        "nx",
                        int,
                        20,
                        10,
                        kwargs.get("nx")
                    ),
                    Field(
                        "npx",
                        int,
                        30,
                        10,
                        kwargs.get("npx")
                    ),
                    Field(
                        "npy",
                        int,
                        40,
                        10,
                        kwargs.get("npy")
                    ),
                    Field(
                        "npz",
                        int,
                        50,
                        10,
                        kwargs.get("npz")
                    ),
                    Field(
                        "aleid",
                        int,
                        60,
                        10,
                        kwargs.get("aleid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method")
                    ),
                    Field(
                        "div",
                        int,
                        0,
                        10,
                        kwargs.get("div")
                    ),
                ],
            ),
        ]

    @property
    def lagpid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID for Lagrangian parts involved in the mapping
        """ # nopep8
        return self._cards[0].get_value("lagpid")

    @lagpid.setter
    def lagpid(self, value: int) -> None:
        self._cards[0].set_value("lagpid", value)

    @property
    def lagpty(self) -> int:
        """Get or set the Type of LARGPID:
        EQ.0: ID is a part set id(see * SET_PART)
        EQ.1 : ID is a part id(see * PART)
        """ # nopep8
        return self._cards[0].get_value("lagpty")

    @lagpty.setter
    def lagpty(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lagpty must be one of {0,1}""")
        self._cards[0].set_value("lagpty", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        self._cards[1].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[int]:
        """Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
        """ # nopep8
        return self._cards[1].get_value("ny")

    @ny.setter
    def ny(self, value: int) -> None:
        self._cards[1].set_value("ny", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
        """ # nopep8
        return self._cards[1].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        self._cards[1].set_value("nx", value)

    @property
    def npx(self) -> typing.Optional[int]:
        """Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("npx")

    @npx.setter
    def npx(self, value: int) -> None:
        self._cards[1].set_value("npx", value)

    @property
    def npy(self) -> typing.Optional[int]:
        """Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("npy")

    @npy.setter
    def npy(self, value: int) -> None:
        self._cards[1].set_value("npy", value)

    @property
    def npz(self) -> typing.Optional[int]:
        """Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("npz")

    @npz.setter
    def npz(self, value: int) -> None:
        self._cards[1].set_value("npz", value)

    @property
    def aleid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the ALE mesh.
        """ # nopep8
        return self._cards[1].get_value("aleid")

    @aleid.setter
    def aleid(self, value: int) -> None:
        self._cards[1].set_value("aleid", value)

    @property
    def method(self) -> typing.Optional[int]:
        """Get or set the Method to compute volumes at the intersection of Lagrangian and ALE elements :
        EQ.0: Both METHOD = 1 and METHOD = 2 are applied by default.
        EQ.1 : The intersection volumes are exactly computed
        EQ.2 : The intersection volumes are evaluated with DIV.
        """ # nopep8
        return self._cards[2].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        self._cards[2].set_value("method", value)

    @property
    def div(self) -> typing.Optional[int]:
        """Get or set the Division of ALE element edges to create subcells, which volumes inside Lagrangian elements are added up by MTH=2 to approximate the intersection volumes at the intersection between ALE and Lagrangian elements
        """ # nopep8
        return self._cards[2].get_value("div")

    @div.setter
    def div(self, value: int) -> None:
        self._cards[2].set_value("div", value)

