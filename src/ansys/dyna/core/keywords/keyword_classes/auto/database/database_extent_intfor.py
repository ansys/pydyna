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

"""Module providing the DatabaseExtentIntfor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DATABASEEXTENTINTFOR_CARD0 = (
    FieldSchema("nglbv", int, 0, 10, 1),
    FieldSchema("nvelo", int, 10, 10, 1),
    FieldSchema("npresu", int, 20, 10, 1),
    FieldSchema("nshear", int, 30, 10, 1),
    FieldSchema("nforce", int, 40, 10, 1),
    FieldSchema("ngapc", int, 50, 10, 1),
    FieldSchema("nfail", int, 60, 10, 0),
    FieldSchema("ieverf", int, 70, 10, 0),
)

_DATABASEEXTENTINTFOR_CARD1 = (
    FieldSchema("nwear", int, 0, 10, 0),
    FieldSchema("nwusr", int, 10, 10, 0),
    FieldSchema("nhuf", int, 20, 10, 0),
    FieldSchema("ntied", int, 30, 10, 0),
    FieldSchema("neng", int, 40, 10, 0),
    FieldSchema("npen", int, 50, 10, 0),
)

class DatabaseExtentIntfor(KeywordBase):
    """DYNA DATABASE_EXTENT_INTFOR keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_INTFOR"

    def __init__(self, **kwargs):
        """Initialize the DatabaseExtentIntfor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEEXTENTINTFOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEEXTENTINTFOR_CARD1,
                **kwargs,
            ),        ]
    @property
    def nglbv(self) -> int:
        """Get or set the Output option for global variables
        EQ.-1: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("nglbv")

    @nglbv.setter
    def nglbv(self, value: int) -> None:
        """Set the nglbv property."""
        if value not in [1, -1, None]:
            raise Exception("""nglbv must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("nglbv", value)

    @property
    def nvelo(self) -> int:
        """Get or set the Output option for nodal velocity
        EQ.-1: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("nvelo")

    @nvelo.setter
    def nvelo(self, value: int) -> None:
        """Set the nvelo property."""
        if value not in [1, -1, None]:
            raise Exception("""nvelo must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("nvelo", value)

    @property
    def npresu(self) -> int:
        """Get or set the Output option for pressures
        EQ.-1: no
        EQ.1: output interface pressure only
        EQ.2: output interface press and peak pressure
        EQ.3: output interface pressure, peak pressure, and time to peak pressure.
        """ # nopep8
        return self._cards[0].get_value("npresu")

    @npresu.setter
    def npresu(self, value: int) -> None:
        """Set the npresu property."""
        if value not in [1, -1, 2, 3, None]:
            raise Exception("""npresu must be `None` or one of {1,-1,2,3}.""")
        self._cards[0].set_value("npresu", value)

    @property
    def nshear(self) -> int:
        """Get or set the Output option for shear stress in r and s-direction
        EQ.-1: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("nshear")

    @nshear.setter
    def nshear(self, value: int) -> None:
        """Set the nshear property."""
        if value not in [1, -1, None]:
            raise Exception("""nshear must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("nshear", value)

    @property
    def nforce(self) -> int:
        """Get or set the Output option for X, Y and Z-force at nodes
        EQ.-1: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("nforce")

    @nforce.setter
    def nforce(self, value: int) -> None:
        """Set the nforce property."""
        if value not in [1, -1, None]:
            raise Exception("""nforce must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("nforce", value)

    @property
    def ngapc(self) -> int:
        """Get or set the Output option for contact gap at nodes and surface energy density
        EQ.-1: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("ngapc")

    @ngapc.setter
    def ngapc(self, value: int) -> None:
        """Set the ngapc property."""
        if value not in [1, -1, None]:
            raise Exception("""ngapc must be `None` or one of {1,-1}.""")
        self._cards[0].set_value("ngapc", value)

    @property
    def nfail(self) -> int:
        """Get or set the Flag for display of deleted contact segments
        EQ.0: all segments are displayed,
        EQ.1: remove deleted contact segments from display.
        """ # nopep8
        return self._cards[0].get_value("nfail")

    @nfail.setter
    def nfail(self, value: int) -> None:
        """Set the nfail property."""
        if value not in [0, 1, None]:
            raise Exception("""nfail must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nfail", value)

    @property
    def ieverf(self) -> int:
        """Get or set the Every interface force state for the  intfor  database is written to a separate file:
        EQ.0: more than one interface force state can be on each intfor file,
        EQ.1: one interface force output state only on each intfor file..
        """ # nopep8
        return self._cards[0].get_value("ieverf")

    @ieverf.setter
    def ieverf(self, value: int) -> None:
        """Set the ieverf property."""
        if value not in [0, 1, None]:
            raise Exception("""ieverf must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ieverf", value)

    @property
    def nwear(self) -> int:
        """Get or set the Output contact wear data, see *CONTACT_ADD_WEAR
        EQ.0: No output.
        EQ.1: Output wear depth.
        EQ.2: Output wear depth and sliding distance.
        """ # nopep8
        return self._cards[1].get_value("nwear")

    @nwear.setter
    def nwear(self, value: int) -> None:
        """Set the nwear property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nwear must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("nwear", value)

    @property
    def nwusr(self) -> int:
        """Get or set the Number of user wear history variables to output from user defined wear routines, see *CONTACT_ADD_WEAR.
        """ # nopep8
        return self._cards[1].get_value("nwusr")

    @nwusr.setter
    def nwusr(self, value: int) -> None:
        """Set the nwusr property."""
        self._cards[1].set_value("nwusr", value)

    @property
    def nhuf(self) -> int:
        """Get or set the Number of user friction history variables to output from user
        defined friction routines; see *USER_INTERFACE_FRICTION
        (MPP only). See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("nhuf")

    @nhuf.setter
    def nhuf(self, value: int) -> None:
        """Set the nhuf property."""
        self._cards[1].set_value("nhuf", value)

    @property
    def ntied(self) -> int:
        """Get or set the Output tied segments for Mortar contact. See Remark 3.
        EQ.0: No output
        EQ.1: Output.
        """ # nopep8
        return self._cards[1].get_value("ntied")

    @ntied.setter
    def ntied(self, value: int) -> None:
        """Set the ntied property."""
        if value not in [0, 1, None]:
            raise Exception("""ntied must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ntied", value)

    @property
    def neng(self) -> int:
        """Get or set the Output (total) sliding interface energy density for Mortar contact, see also ENGOUT on * CONTROL_OUTPUT.
        EQ.0: No output
        EQ.1 : Output.
        """ # nopep8
        return self._cards[1].get_value("neng")

    @neng.setter
    def neng(self, value: int) -> None:
        """Set the neng property."""
        if value not in [0, 1, None]:
            raise Exception("""neng must be `None` or one of {0,1}.""")
        self._cards[1].set_value("neng", value)

    @property
    def npen(self) -> int:
        """Get or set the Output penetration info for Mortar contact. A nodal field gives the
        pentration for each node(magnitude and direction) in the sliding interface,
        see also PENOUT on* CONTROL_OUTPUT.
        EQ.0 : No output
        GE.1 : Output absolute penetration
        GE.2 : Output relative penetration.
        """ # nopep8
        return self._cards[1].get_value("npen")

    @npen.setter
    def npen(self, value: int) -> None:
        """Set the npen property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""npen must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("npen", value)

