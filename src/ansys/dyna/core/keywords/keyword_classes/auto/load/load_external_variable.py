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

"""Module providing the LoadExternalVariable class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADEXTERNALVARIABLE_CARD0 = (
    FieldSchema("vid", int, 0, 10, None),
    FieldSchema("dbas", float, 10, 10, 0.0),
    FieldSchema("dsca", float, 20, 10, 1.0),
    FieldSchema("dlcid", int, 30, 10, None),
    FieldSchema("nmp", int, 40, 10, None),
    FieldSchema("ntmp", int, 50, 10, None),
    FieldSchema("nemp", int, 60, 10, None),
)

_LOADEXTERNALVARIABLE_CARD1 = (
    FieldSchema("imp", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("ptype", int, 20, 10, 0),
)

_LOADEXTERNALVARIABLE_CARD2 = (
    FieldSchema("itmp", int, 0, 10, None),
    FieldSchema("tpid", int, 10, 10, None),
    FieldSchema("tptype", int, 20, 10, 0),
)

_LOADEXTERNALVARIABLE_CARD3 = (
    FieldSchema("iemp", int, 0, 10, None),
    FieldSchema("eid", int, 10, 10, None),
    FieldSchema("tptype", int, 20, 10, 0),
)

_LOADEXTERNALVARIABLE_CARD4 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtyp", int, 10, 10, 1),
    FieldSchema("bas", float, 20, 10, None),
    FieldSchema("sca", float, 30, 10, 1.0),
    FieldSchema("lcid", int, 40, 10, None),
)

_LOADEXTERNALVARIABLE_CARD5 = (
    FieldSchema("filename", str, 0, 80, None),
)

class LoadExternalVariable(KeywordBase):
    """DYNA LOAD_EXTERNAL_VARIABLE keyword"""

    keyword = "LOAD"
    subkeyword = "EXTERNAL_VARIABLE"

    def __init__(self, **kwargs):
        """Initialize the LoadExternalVariable class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADEXTERNALVARIABLE_CARD5,
                **kwargs,
            ),
        ]
    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Variable ID. If this keyword contains more than one *LOAD_EXTERNAL_VARIABLE instantiation, each variable must have a unique ID.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def dbas(self) -> float:
        """Get or set the Base value a_B. It is the default used for nodes that are not specified in a node or node set card (card 2)
        """ # nopep8
        return self._cards[0].get_value("dbas")

    @dbas.setter
    def dbas(self, value: float) -> None:
        """Set the dbas property."""
        self._cards[0].set_value("dbas", value)

    @property
    def dsca(self) -> float:
        """Get or set the Scaled value a_S. It is the default used for nodes that are not specified in a node or node set card (card 2)
        """ # nopep8
        return self._cards[0].get_value("dsca")

    @dsca.setter
    def dsca(self, value: float) -> None:
        """Set the dsca property."""
        self._cards[0].set_value("dsca", value)

    @property
    def dlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining a scaling f(t) as function of time. This is default for all nodes not specified in a node or node set card (card 2).
        """ # nopep8
        return self._cards[0].get_value("dlcid")

    @dlcid.setter
    def dlcid(self, value: int) -> None:
        """Set the dlcid property."""
        self._cards[0].set_value("dlcid", value)

    @property
    def nmp(self) -> typing.Optional[int]:
        """Get or set the Number of material properties to be defined.
        """ # nopep8
        return self._cards[0].get_value("nmp")

    @nmp.setter
    def nmp(self, value: int) -> None:
        """Set the nmp property."""
        self._cards[0].set_value("nmp", value)

    @property
    def ntmp(self) -> typing.Optional[int]:
        """Get or set the Number of thermal material properties to be defined.
        """ # nopep8
        return self._cards[0].get_value("ntmp")

    @ntmp.setter
    def ntmp(self, value: int) -> None:
        """Set the ntmp property."""
        self._cards[0].set_value("ntmp", value)

    @property
    def nemp(self) -> typing.Optional[int]:
        """Get or set the Number of electromagnetic properties to be defined
        """ # nopep8
        return self._cards[0].get_value("nemp")

    @nemp.setter
    def nemp(self, value: int) -> None:
        """Set the nemp property."""
        self._cards[0].set_value("nemp", value)

    @property
    def imp(self) -> typing.Optional[int]:
        """Get or set the Index of the material property. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("imp")

    @imp.setter
    def imp(self, value: int) -> None:
        """Set the imp property."""
        self._cards[1].set_value("imp", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID. For parts referenced, the material property associated with the material property index IMP will be modified by the state of the   external variable. See Remark 5
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def ptype(self) -> int:
        """Get or set the ID type of PID:
        EQ.0: Part ID.
        EQ.1: Part set ID.
        """ # nopep8
        return self._cards[1].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        if value not in [0, 1, None]:
            raise Exception("""ptype must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ptype", value)

    @property
    def itmp(self) -> typing.Optional[int]:
        """Get or set the Index of the thermal material property. See Remark 6.
        """ # nopep8
        return self._cards[2].get_value("itmp")

    @itmp.setter
    def itmp(self, value: int) -> None:
        """Set the itmp property."""
        self._cards[2].set_value("itmp", value)

    @property
    def tpid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID. For parts referenced here, the thermal  material property associated with the material property index ITMP will be modified by the state of the   external variable. See Remark 6
        """ # nopep8
        return self._cards[2].get_value("tpid")

    @tpid.setter
    def tpid(self, value: int) -> None:
        """Set the tpid property."""
        self._cards[2].set_value("tpid", value)

    @property
    def tptype(self) -> int:
        """Get or set the ID type of TPID:
        EQ.0: Part ID.
        EQ.1: Part set ID.
        """ # nopep8
        return self._cards[2].get_value("tptype")

    @tptype.setter
    def tptype(self, value: int) -> None:
        """Set the tptype property."""
        if value not in [0, 1, None]:
            raise Exception("""tptype must be `None` or one of {0,1}.""")
        self._cards[2].set_value("tptype", value)

    @property
    def iemp(self) -> typing.Optional[int]:
        """Get or set the Index of the electromagnetic property. See Remark 7.
        """ # nopep8
        return self._cards[3].get_value("iemp")

    @iemp.setter
    def iemp(self, value: int) -> None:
        """Set the iemp property."""
        self._cards[3].set_value("iemp", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Function ID. For entities referenced here, the electromagnetic property associated with property index IEMP will be modified by the state of the external variable. See Remark 7.
        """ # nopep8
        return self._cards[3].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[3].set_value("eid", value)

    @property
    def tptype(self) -> int:
        """Get or set the ID type of EID:
        EQ.2:Function ID
        """ # nopep8
        return self._cards[3].get_value("tptype")

    @tptype.setter
    def tptype(self, value: int) -> None:
        """Set the tptype property."""
        if value not in [0, 2, None]:
            raise Exception("""tptype must be `None` or one of {0,2}.""")
        self._cards[3].set_value("tptype", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Node or node set ID. Ignored for IDTYP = 3
        """ # nopep8
        return self._cards[4].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[4].set_value("id", value)

    @property
    def idtyp(self) -> int:
        """Get or set the ID type:
        EQ.1: Node ID
        EQ.2: Node set ID
        EQ.3: LSDA option(Rest of this Card 2 is ignored in that case)
        """ # nopep8
        return self._cards[4].get_value("idtyp")

    @idtyp.setter
    def idtyp(self, value: int) -> None:
        """Set the idtyp property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""idtyp must be `None` or one of {1,2,3}.""")
        self._cards[4].set_value("idtyp", value)

    @property
    def bas(self) -> typing.Optional[float]:
        """Get or set the Base value a_B used for nodes defined by ID and IDTYP.Ignored for IDTYP = 3.
        """ # nopep8
        return self._cards[4].get_value("bas")

    @bas.setter
    def bas(self, value: float) -> None:
        """Set the bas property."""
        self._cards[4].set_value("bas", value)

    @property
    def sca(self) -> float:
        """Get or set the Scaling parameter a_S used for nodes defined by ID and IDTYP.Ignored for IDTYP = 3.
        """ # nopep8
        return self._cards[4].get_value("sca")

    @sca.setter
    def sca(self, value: float) -> None:
        """Set the sca property."""
        self._cards[4].set_value("sca", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining a scaling f(t) as function of time used for nodes defined by ID and IDTYP.Ignored for IDTYP = 3.
        """ # nopep8
        return self._cards[4].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[4].set_value("lcid", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of a LSDA file that contains temperature information that are interpreted as value for the external variable.
        """ # nopep8
        return self._cards[5].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[5].set_value("filename", value)

