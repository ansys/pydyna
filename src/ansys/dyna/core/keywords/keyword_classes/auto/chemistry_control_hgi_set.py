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

class ChemistryControlHgiSet(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_HGI_SET keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_HGI_SET"

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
                        "compid",
                        int,
                        10,
                        10,
                        kwargs.get("compid")
                    ),
                    Field(
                        "exit_bc",
                        int,
                        20,
                        10,
                        kwargs.get("exit_bc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file",
                        str,
                        0,
                        80,
                        kwargs.get("file")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this chemistry solver.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def compid(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of the initial composition
        """ # nopep8
        return self._cards[0].get_value("compid")

    @compid.setter
    def compid(self, value: int) -> None:
        self._cards[0].set_value("compid", value)

    @property
    def exit_bc(self) -> typing.Optional[int]:
        """Get or set the The exit boundary condition surface as a segment set.
        """ # nopep8
        return self._cards[0].get_value("exit_bc")

    @exit_bc.setter
    def exit_bc(self, value: int) -> None:
        self._cards[0].set_value("exit_bc", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the lsda file in which to write the results of the inflator simulation.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        self._cards[1].set_value("file", value)

