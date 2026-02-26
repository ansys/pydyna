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

"""Module providing the DatabasePwpOutput class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEPWPOUTPUT_CARD0 = (
    FieldSchema("ivel", int, 0, 10, 0),
    FieldSchema("iaccx", int, 10, 10, 0),
    FieldSchema("iaccy", int, 20, 10, 0),
    FieldSchema("iaccz", int, 30, 10, 0),
    FieldSchema("ncyout", int, 40, 10, 100),
)

class DatabasePwpOutput(KeywordBase):
    """DYNA DATABASE_PWP_OUTPUT keyword"""

    keyword = "DATABASE"
    subkeyword = "PWP_OUTPUT"

    def __init__(self, **kwargs):
        """Initialize the DatabasePwpOutput class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEPWPOUTPUT_CARD0,
                **kwargs,
            ),        ]
    @property
    def ivel(self) -> int:
        """Get or set the Meaning of "velocity" in d3plot and d3thdt output files
        0:  Nodal velocity vector
        1:  Seepage velocity vector
        """ # nopep8
        return self._cards[0].get_value("ivel")

    @ivel.setter
    def ivel(self, value: int) -> None:
        """Set the ivel property."""
        if value not in [0, 1, None]:
            raise Exception("""ivel must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ivel", value)

    @property
    def iaccx(self) -> int:
        """Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        1:  Total pwp head
        2:  Excess pwp head (this is also written as temperature)
        3: Target rate of volume change
        4: Actual rate of volume change
        7:  Hydraulic pwp head
        8:  Error in rate of volume change (calculated from seepage minus actual)
        9:  Volume at node
        10:  Rate of volume change calculated from seepage
        14:  Void volume (generated at suction limit)
        17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
        """ # nopep8
        return self._cards[0].get_value("iaccx")

    @iaccx.setter
    def iaccx(self, value: int) -> None:
        """Set the iaccx property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 9, 10, 14, 17, None]:
            raise Exception("""iaccx must be `None` or one of {0,1,2,3,4,5,6,9,10,14,17}.""")
        self._cards[0].set_value("iaccx", value)

    @property
    def iaccy(self) -> int:
        """Get or set the Meaning of "Y-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        1:  Total pwp head
        2:  Excess pwp head (this is also written as temperature)
        3: Target rate of volume change
        4: Actual rate of volume change
        7:  Hydraulic pwp head
        8:  Error in rate of volume change (calculated from seepage minus actual)
        9:  Volume at node
        10:  Rate of volume change calculated from seepage
        14:  Void volume (generated at suction limit)
        17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
        """ # nopep8
        return self._cards[0].get_value("iaccy")

    @iaccy.setter
    def iaccy(self, value: int) -> None:
        """Set the iaccy property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 9, 10, 14, 17, None]:
            raise Exception("""iaccy must be `None` or one of {0,1,2,3,4,5,6,9,10,14,17}.""")
        self._cards[0].set_value("iaccy", value)

    @property
    def iaccz(self) -> int:
        """Get or set the Meaning of "Z-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        1:  Total pwp head
        2:  Excess pwp head (this is also written as temperature)
        3: Target rate of volume change
        4: Actual rate of volume change
        7:  Hydraulic pwp head
        8:  Error in rate of volume change (calculated from seepage minus actual)
        9:  Volume at node
        10:  Rate of volume change calculated from seepage
        14:  Void volume (generated at suction limit)
        17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
        """ # nopep8
        return self._cards[0].get_value("iaccz")

    @iaccz.setter
    def iaccz(self, value: int) -> None:
        """Set the iaccz property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 9, 10, 14, 17, None]:
            raise Exception("""iaccz must be `None` or one of {0,1,2,3,4,5,6,9,10,14,17}.""")
        self._cards[0].set_value("iaccz", value)

    @property
    def ncyout(self) -> int:
        """Get or set the Number of cycles between outputs of calculation status to d3hsp, log, and tdc_control_output.csv files (time-dependent and steady-state analysis types)
        """ # nopep8
        return self._cards[0].get_value("ncyout")

    @ncyout.setter
    def ncyout(self, value: int) -> None:
        """Set the ncyout property."""
        self._cards[0].set_value("ncyout", value)

