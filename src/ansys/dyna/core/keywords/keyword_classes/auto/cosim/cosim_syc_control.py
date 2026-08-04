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

"""Module providing the CosimSycControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_COSIMSYCCONTROL_CARD0 = (
    FieldSchema("sycid", str, 0, 20, None),
    FieldSchema("opt", str, 20, 10, "G"),
)

class CosimSycControl(KeywordBase):
    """DYNA COSIM_SYC_CONTROL keyword"""

    keyword = "COSIM"
    subkeyword = "SYC_CONTROL"

    def __init__(self, **kwargs):
        """Initialize the CosimSycControl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _COSIMSYCCONTROL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def sycid(self) -> typing.Optional[str]:
        """Get or set the SYC identification.
        """ # nopep8
        return self._cards[0].get_value("sycid")

    @sycid.setter
    def sycid(self, value: str) -> None:
        """Set the sycid property."""
        self._cards[0].set_value("sycid", value)

    @property
    def opt(self) -> str:
        """Get or set the OPT Purpose of ls - dyna simulation :
        EQ.G : Generation mode. LS-DYNA will generate a file named SYCID.scp, which defines the variables LS-DYNA will exchange with other Ansys solvers.  Note that the scp file is not needed any more since SYC25.
        EQ.C:	Co-simulation mode. LS-DYNA will co-simulate with other Ansys solvers.
        """ # nopep8
        return self._cards[0].get_value("opt")

    @opt.setter
    def opt(self, value: str) -> None:
        """Set the opt property."""
        if value not in ["G", "C", None]:
            raise Exception("""opt must be `None` or one of {"G","C"}.""")
        self._cards[0].set_value("opt", value)

