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

"""Module providing the LsoDomain class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LSODOMAIN_CARD0 = (
    FieldSchema("domain_type", str, 0, 20, "ROGO"),
)

_LSODOMAIN_CARD1 = (
    FieldSchema("solver_name", str, 0, 20, "MECH"),
)

_LSODOMAIN_CARD2 = (
    FieldSchema("outid", int, 0, 10, None),
    FieldSchema("refid", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("reduct", int, 30, 10, None),
)

_LSODOMAIN_CARD3 = (
    FieldSchema("outid", int, 0, 10, None),
    FieldSchema("refid", int, 10, 10, None),
    FieldSchema("override", int, 20, 10, 1),
    FieldSchema("reduct", int, 30, 10, None),
)

_LSODOMAIN_CARD4 = (
    FieldSchema("variable_name", str, 0, 80, None),
)

class LsoDomain(KeywordBase):
    """DYNA LSO_DOMAIN keyword"""

    keyword = "LSO"
    subkeyword = "DOMAIN"

    def __init__(self, **kwargs):
        """Initialize the LsoDomain class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LSODOMAIN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSODOMAIN_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSODOMAIN_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSODOMAIN_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSODOMAIN_CARD4,
                **kwargs,
            ),        ]
    @property
    def domain_type(self) -> str:
        """Get or set the The type of domain for which LSO output may be generated.Accepted entries so far are 'thist_point'
        """ # nopep8
        return self._cards[0].get_value("domain_type")

    @domain_type.setter
    def domain_type(self, value: str) -> None:
        """Set the domain_type property."""
        if value not in ["ROGO", "CIRCUIT", "THIST_POINT", "TRACER_POINT", "NODE", "PART", "SEGMENT", "SURF_NODE", "SURF_ELEMENT", "VOLUME_ELEMENT", "SURFACE_PART", "VOLUME_PART", None]:
            raise Exception("""domain_type must be `None` or one of {"ROGO","CIRCUIT","THIST_POINT","TRACER_POINT","NODE","PART","SEGMENT","SURF_NODE","SURF_ELEMENT","VOLUME_ELEMENT","SURFACE_PART","VOLUME_PART"}.""")
        self._cards[0].set_value("domain_type", value)

    @property
    def solver_name(self) -> str:
        """Get or set the Selects the solver from which data is output on this domain.
        Accepted entries so far are 'em', 'cese' and 'icfd'.
        """ # nopep8
        return self._cards[1].get_value("solver_name")

    @solver_name.setter
    def solver_name(self, value: str) -> None:
        """Set the solver_name property."""
        if value not in ["MECH", "EM", "CESE", "ICFD", None]:
            raise Exception("""solver_name must be `None` or one of {"MECH","EM","CESE","ICFD"}.""")
        self._cards[1].set_value("solver_name", value)

    @property
    def outid(self) -> typing.Optional[int]:
        """Get or set the LSO domain ID associated with this domain, and used by *LSO_TIME_SEQUENCE cards.
        """ # nopep8
        return self._cards[2].get_value("outid")

    @outid.setter
    def outid(self, value: int) -> None:
        """Set the outid property."""
        self._cards[2].set_value("outid", value)

    @property
    def refid(self) -> typing.Optional[int]:
        """Get or set the Support set ID. This can be a set defined by a *SET card, a *LSO_ID_SET, card, or a *LSO_POINT_SET card. Unless OVERRIDE is specified,this set must be of the same type as DOMAIN_TYPE.
        """ # nopep8
        return self._cards[2].get_value("refid")

    @refid.setter
    def refid(self, value: int) -> None:
        """Set the refid property."""
        self._cards[2].set_value("refid", value)

    @property
    def reduct(self) -> typing.Optional[int]:
        """Get or set the A function that operates on the entire domain and returns a single
        value for scalar variables, three values for vector variables, or 6
        values for symmetric tensor variables. For REDUCT=range, the
        number of returned values doubles. The following are the supported
        functions:
        EQ.BLANK: no reduction (default)
        EQ.none: Same as above
        EQ.avg: the average by component
        EQ.average: Same as above
        EQ.min: the minimum by component
        EQ.minimum: Same as above
        EQ.max: the maximum by component
        EQ.maximum: Same as above
        EQ.maximum: Same as above
        EQ.range: the minimum by component followed by the maximum by component.
        """ # nopep8
        return self._cards[2].get_value("reduct")

    @reduct.setter
    def reduct(self, value: int) -> None:
        """Set the reduct property."""
        self._cards[2].set_value("reduct", value)

    @property
    def outid(self) -> typing.Optional[int]:
        """Get or set the LSO domain ID associated with this domain, and used by *LSO_TIME_SEQUENCE cards.
        """ # nopep8
        return self._cards[3].get_value("outid")

    @outid.setter
    def outid(self, value: int) -> None:
        """Set the outid property."""
        self._cards[3].set_value("outid", value)

    @property
    def refid(self) -> typing.Optional[int]:
        """Get or set the Support set ID. This can be a set defined by a *SET card, a *LSO_ID_SET, card, or a *LSO_POINT_SET card. Unless OVERRIDE is specified,this set must be of the same type as DOMAIN_TYPE.
        """ # nopep8
        return self._cards[3].get_value("refid")

    @refid.setter
    def refid(self, value: int) -> None:
        """Set the refid property."""
        self._cards[3].set_value("refid", value)

    @property
    def override(self) -> int:
        """Get or set the If non-zero, then REFID is interpreted as:
        .EQ.1: a PART set for SOLVER_NAME
        EQ.2: a PART set of volume parts created with a *LSO_ID_SET card (volume parts are defined with *MESH_VOLUME cards).
        EQ.3: a PART set of surface parts created with a *LSO_ID_SET card (surface parts are defined with *MESH_SURFACE_ELEMENT cards).
        EQ.4: a set of segment sets created with a *LSO_ID_SET card.
        """ # nopep8
        return self._cards[3].get_value("override")

    @override.setter
    def override(self, value: int) -> None:
        """Set the override property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""override must be `None` or one of {1,2,3,4}.""")
        self._cards[3].set_value("override", value)

    @property
    def reduct(self) -> typing.Optional[int]:
        """Get or set the A function that operates on the entire domain and returns a single
        value for scalar variables, three values for vector variables, or 6
        values for symmetric tensor variables. For REDUCT=range, the
        number of returned values doubles. The following are the supported
        functions:
        EQ.BLANK: no reduction (default)
        EQ.none: Same as above
        EQ.avg: the average by component
        EQ.average: Same as above
        EQ.min: the minimum by component
        EQ.minimum: Same as above
        EQ.max: the maximum by component
        EQ.maximum: Same as above
        EQ.maximum: Same as above
        EQ.range: the minimum by component followed by the maximum by component.
        """ # nopep8
        return self._cards[3].get_value("reduct")

    @reduct.setter
    def reduct(self, value: int) -> None:
        """Set the reduct property."""
        self._cards[3].set_value("reduct", value)

    @property
    def variable_name(self) -> typing.Optional[str]:
        """Get or set the Either the name of a single output variable or a variable group. If no names are given, then no output occurs.
        """ # nopep8
        return self._cards[4].get_value("variable_name")

    @variable_name.setter
    def variable_name(self, value: str) -> None:
        """Set the variable_name property."""
        self._cards[4].set_value("variable_name", value)

