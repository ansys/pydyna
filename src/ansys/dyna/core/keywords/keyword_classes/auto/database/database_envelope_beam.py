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

"""Module providing the DatabaseEnvelopeBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DATABASEENVELOPEBEAM_CARD0 = (
    FieldSchema("tcheck", float, 0, 10, None),
    FieldSchema("tback", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("bsetid", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("output", int, 70, 10, 2),
)

class DatabaseEnvelopeBeam(KeywordBase):
    """DYNA DATABASE_ENVELOPE_BEAM keyword"""

    keyword = "DATABASE"
    subkeyword = "ENVELOPE_BEAM"
    _link_fields = {
        "bsetid": LinkType.SET_BEAM,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseEnvelopeBeam class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEENVELOPEBEAM_CARD0,
                **kwargs,
            ),
        ]
    @property
    def tcheck(self) -> typing.Optional[float]:
        """Get or set the Time step for checking beam data against the previous maxima (minima). and updating maximum (or minimum) values.
        EQ.0 : Data is checked every time step.
        """ # nopep8
        return self._cards[0].get_value("tcheck")

    @tcheck.setter
    def tcheck(self, value: float) -> None:
        """Set the tcheck property."""
        self._cards[0].set_value("tcheck", value)

    @property
    def tback(self) -> typing.Optional[float]:
        """Get or set the Time interval for writing output files.
        EQ.0 : Output files are written only at termination.
        """ # nopep8
        return self._cards[0].get_value("tback")

    @tback.setter
    def tback(self, value: float) -> None:
        """Set the tback property."""
        self._cards[0].set_value("tback", value)

    @property
    def bsetid(self) -> int:
        """Get or set the ID of a set containing the selected beam elements.
        EQ.0 : All beam elements in the model
        """ # nopep8
        return self._cards[0].get_value("bsetid")

    @bsetid.setter
    def bsetid(self, value: int) -> None:
        """Set the bsetid property."""
        if value not in [0, 1, None]:
            raise Exception("""bsetid must be `None` or one of {0,1}.""")
        self._cards[0].set_value("bsetid", value)

    @property
    def output(self) -> int:
        """Get or set the Output file control for the details file (see Remark 4).
        EQ.0 : Same as 2. EQ.1 : Human -readable text format.
        EQ.2 : CSV format.EQ.3 : No �details� file.
        """ # nopep8
        return self._cards[0].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        if value not in [2, 0, 1, 3, None]:
            raise Exception("""output must be `None` or one of {2,0,1,3}.""")
        self._cards[0].set_value("output", value)

    @property
    def bsetid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_BEAM_* keyword for bsetid."""
        return self._get_set_link("BEAM", self.bsetid)

    @bsetid_link.setter
    def bsetid_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsetid."""
        self.bsetid = value.sid

