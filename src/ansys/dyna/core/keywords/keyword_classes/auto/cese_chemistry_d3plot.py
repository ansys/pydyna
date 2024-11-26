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

class CeseChemistryD3Plot(KeywordBase):
    """DYNA CESE_CHEMISTRY_D3PLOT keyword"""

    keyword = "CESE"
    subkeyword = "CHEMISTRY_D3PLOT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "modelid",
                        int,
                        0,
                        10,
                        kwargs.get("modelid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "species",
                        str,
                        0,
                        256,
                        kwargs.get("species")
                    ),
                ],
            ),
        ]

    @property
    def modelid(self) -> typing.Optional[int]:
        """Get or set the Identifier of a Chemkin-compatible chemistry model.
        """ # nopep8
        return self._cards[0].get_value("modelid")

    @modelid.setter
    def modelid(self, value: int) -> None:
        self._cards[0].set_value("modelid", value)

    @property
    def species(self) -> typing.Optional[str]:
        """Get or set the Name of a chemical species that is defined in the chemistry model identified by MODELID (see *CHEMISTRY_MODEL).
        """ # nopep8
        return self._cards[1].get_value("species")

    @species.setter
    def species(self, value: str) -> None:
        self._cards[1].set_value("species", value)

