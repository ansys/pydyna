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

"""Module providing the DualceseEnsightDomain class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEENSIGHTDOMAIN_CARD0 = (
    FieldSchema("dom_type", str, 0, 80, " "),
)

_DUALCESEENSIGHTDOMAIN_CARD1 = (
    FieldSchema("domid", int, 0, 10, None),
    FieldSchema("refid", int, 10, 10, None),
    FieldSchema("reduct", str, 20, 10, None),
)

_DUALCESEENSIGHTDOMAIN_CARD2 = (
    FieldSchema("var_name", str, 0, 80, None),
)

class DualceseEnsightDomain(KeywordBase):
    """DYNA DUALCESE_ENSIGHT_DOMAIN keyword"""

    keyword = "DUALCESE"
    subkeyword = "ENSIGHT_DOMAIN"

    def __init__(self, **kwargs):
        """Initialize the DualceseEnsightDomain class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTDOMAIN_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTDOMAIN_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTDOMAIN_CARD2,
                **kwargs,
            ),
        ]
    @property
    def dom_type(self) -> str:
        """Get or set the The type of domain for which DualCESE EnSight output may be generated.  The currently supported types are: NODE, NODE_SET, ELEMENT, ELEMENT_SET, SEGMENT, SEGMENT_SET, POINT_SET, DUALCESE_FSI_INTERFACE (mechanics nodes), DUALCESE_FSI_FACES (mechanics FSI faces), MECHANICAL_PIECE, DUALCESE_GLOBAL,  ENSIGHT_REGION, or CHEMICAL_SPECIES.
        """ # nopep8
        return self._cards[0].get_value("dom_type")

    @dom_type.setter
    def dom_type(self, value: str) -> None:
        """Set the dom_type property."""
        if value not in ["NODE", "NODE_SET", "ELEMENT", "ELEMENT_SET", "SEGMENT", "SEGMENT_SET", "DUALCESE_FSI_INTERFACE", "DUALCESE_FSI_FACES", "DUALCESE_PIECE", "DUALCESE_GLOBAL", "ENSIGHT_REGION", "CHEMICAL_SPECIES", None]:
            raise Exception("""dom_type must be `None` or one of {"NODE","NODE_SET","ELEMENT","ELEMENT_SET","SEGMENT","SEGMENT_SET","DUALCESE_FSI_INTERFACE","DUALCESE_FSI_FACES","DUALCESE_PIECE","DUALCESE_GLOBAL","ENSIGHT_REGION","CHEMICAL_SPECIES"}.""")
        self._cards[0].set_value("dom_type", value)

    @property
    def domid(self) -> typing.Optional[int]:
        """Get or set the Dual CESE domain ID associated with this domain that is used by *DUALCESE_ENSIGHT_?TIME?SEQ.
        """ # nopep8
        return self._cards[1].get_value("domid")

    @domid.setter
    def domid(self, value: int) -> None:
        """Set the domid property."""
        self._cards[1].set_value("domid", value)

    @property
    def refid(self) -> typing.Optional[int]:
        """Get or set the ID for referring to dual CESE element, node, or segment sets to obtain a subset of the dual CESE mesh. The type of set referenced by REFID must match the DOMAIN_TYPE. For instance, for DOMAIN_TYPE = NODE or NODE_SET, this field refers to a *DUALCESE_NODESET. This field is not required and is not used by all values of DOMAIN_TYPE. See Table 0-1 for details.
        """ # nopep8
        return self._cards[1].get_value("refid")

    @refid.setter
    def refid(self, value: int) -> None:
        """Set the refid property."""
        self._cards[1].set_value("refid", value)

    @property
    def reduct(self) -> typing.Optional[str]:
        """Get or set the A function that operates on the entire domain and returns a single value for scalar variables, three values for vector variables, or 6 values for symmetric tensor variables. The following are the supported functions:
        EQ.<BLANK>:	No reduction(default)
        EQ.none : Same as <BLANK>
        EQ.avg : The average by component
        EQ.average : Same as avg
        EQ.min : The minimum by component
        EQ.minimum : Same as min
        EQ.max : The maximum by component
        EQ.maximum : Same as max
        EQ.sum : The sum by component
        """ # nopep8
        return self._cards[1].get_value("reduct")

    @reduct.setter
    def reduct(self, value: str) -> None:
        """Set the reduct property."""
        self._cards[1].set_value("reduct", value)

    @property
    def var_name(self) -> typing.Optional[str]:
        """Get or set the Name of a single output variable.
        """ # nopep8
        return self._cards[2].get_value("var_name")

    @var_name.setter
    def var_name(self, value: str) -> None:
        """Set the var_name property."""
        self._cards[2].set_value("var_name", value)

