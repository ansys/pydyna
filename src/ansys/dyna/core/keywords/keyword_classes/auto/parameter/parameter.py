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

"""Module providing the Parameter class."""
import dataclasses
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Parameter(KeywordBase):
    """DYNA PARAMETER keyword"""

    keyword = "PARAMETER"
    subkeyword = "PARAMETER"

    @dataclasses.dataclass
    class Parameter:
        """Dataclass for Parameter."""
        name: str = None
        val: str = None

    def __init__(self, **kwargs):
        """Initialize the Parameter class."""
        super().__init__(**kwargs)
        self._cards = [
            SeriesCard(
                "parameters",
                8,
                10,
                self.Parameter,
                None,
                data = kwargs.get("parameters")),        ]
    @property
    def parameters(self) -> SeriesCard:
        """Parameters.."""
        return self._cards[0]

    @parameters.setter
    def parameters(self, value: typing.List) -> None:
        self._cards[0].data = value

