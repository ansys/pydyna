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

"""Module providing the IgaIntegrationSolidReduce class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAINTEGRATIONSOLIDREDUCE_CARD0 = (
    FieldSchema("patchid", int, 0, 10, None),
    FieldSchema("nrdr", int, 10, 10, 0),
    FieldSchema("nrds", int, 20, 20, 0),
    FieldSchema("nrdt", int, 30, 20, 0),
)

class IgaIntegrationSolidReduce(KeywordBase):
    """DYNA IGA_INTEGRATION_SOLID_REDUCE keyword"""

    keyword = "IGA"
    subkeyword = "INTEGRATION_SOLID_REDUCE"

    def __init__(self, **kwargs):
        """Initialize the IgaIntegrationSolidReduce class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAINTEGRATIONSOLIDREDUCE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Patch ID defined in *IGA_SHELL:
        EQ.0: Apply to all IGA shell patches
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def nrdr(self) -> int:
        """Get or set the Reduced degree in the r-direction:
        EQ.0: Full integration
        EQ.1: Reduced by 1
        EQ.2: Reduced by 2
        """ # nopep8
        return self._cards[0].get_value("nrdr")

    @nrdr.setter
    def nrdr(self, value: int) -> None:
        """Set the nrdr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrdr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nrdr", value)

    @property
    def nrds(self) -> int:
        """Get or set the Reduced degree in the s-direction:
        EQ.0: Full integration
        EQ.1: Reduced by 1
        EQ.2: Reduced by 2
        """ # nopep8
        return self._cards[0].get_value("nrds")

    @nrds.setter
    def nrds(self, value: int) -> None:
        """Set the nrds property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrds must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nrds", value)

    @property
    def nrdt(self) -> int:
        """Get or set the Reduced degree in the t-direction:
        EQ.0: Full integration
        EQ.1: Reduced by 1
        EQ.2: Reduced by 2
        """ # nopep8
        return self._cards[0].get_value("nrdt")

    @nrdt.setter
    def nrdt(self, value: int) -> None:
        """Set the nrdt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrdt must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nrdt", value)

