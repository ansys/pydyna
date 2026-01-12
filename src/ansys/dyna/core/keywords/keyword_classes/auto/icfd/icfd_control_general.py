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

"""Module providing the IcfdControlGeneral class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLGENERAL_CARD0 = (
    FieldSchema("atype", int, 0, 10, 0),
    FieldSchema("mtype", int, 10, 10, 0),
    FieldSchema("dvcl", int, 20, 10, 0),
    FieldSchema("rdvcl", int, 30, 10, 0),
)

class IcfdControlGeneral(KeywordBase):
    """DYNA ICFD_CONTROL_GENERAL keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_GENERAL"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlGeneral class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLGENERAL_CARD0,
                **kwargs,
            ),        ]
    @property
    def atype(self) -> int:
        """Get or set the Analysis type:
        EQ.-1:Turns off the ICFD solver after initial keyword reading.
        EQ.0:Transient analysis
        EQ.1:Steady state analysis
        """ # nopep8
        return self._cards[0].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        """Set the atype property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""atype must be `None` or one of {0,-1,1}.""")
        self._cards[0].set_value("atype", value)

    @property
    def mtype(self) -> int:
        """Get or set the Solving Method type:
        EQ.0:Fractional Step Method
        EQ.1:Monolithic solve
        EQ.2:Potential flow solve (Steady state only)
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""mtype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def dvcl(self) -> int:
        """Get or set the Divergence Cleaning Flag:
        EQ.0: Default.Initialize the solution with divergence cleaning
        EQ.1 : No divergence cleaning
        EQ.2 : Initial divergence cleaning using potential flow
        EQ.3 : Divergence cleaning after each remeshing step
        """ # nopep8
        return self._cards[0].get_value("dvcl")

    @dvcl.setter
    def dvcl(self, value: int) -> None:
        """Set the dvcl property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""dvcl must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("dvcl", value)

    @property
    def rdvcl(self) -> int:
        """Get or set the Remeshing divergence cleaning :
        EQ.0:	 Default.No divergence cleaning after remesh(default)
        EQ.1 : Divergence cleaning after each remeshing step.
        """ # nopep8
        return self._cards[0].get_value("rdvcl")

    @rdvcl.setter
    def rdvcl(self, value: int) -> None:
        """Set the rdvcl property."""
        if value not in [0, 1, None]:
            raise Exception("""rdvcl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("rdvcl", value)

