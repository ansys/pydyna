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

"""Module providing the IcfdDefineHemolysisIndex class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDEFINEHEMOLYSISINDEX_CARD0 = (
    FieldSchema("massdif", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, 3.458e-06),
    FieldSchema("alpha", float, 20, 10, 0.2777),
    FieldSchema("beta", float, 30, 10, 2.0639),
    FieldSchema("pressto", int, 40, 10, 0),
)

class IcfdDefineHemolysisIndex(KeywordBase):
    """DYNA ICFD_DEFINE_HEMOLYSIS_INDEX keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_HEMOLYSIS_INDEX"

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineHemolysisIndex class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINEHEMOLYSISINDEX_CARD0,
                **kwargs,
            ),
        ]
    @property
    def massdif(self) -> typing.Optional[float]:
        """Get or set the Mass diffusion for the hemolysis index transport problem
        """ # nopep8
        return self._cards[0].get_value("massdif")

    @massdif.setter
    def massdif(self, value: float) -> None:
        """Set the massdif property."""
        self._cards[0].set_value("massdif", value)

    @property
    def c(self) -> float:
        """Get or set the Constant in the hemolysis index model.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def alpha(self) -> float:
        """Get or set the Exponent in the hemolysis index model.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def beta(self) -> float:
        """Get or set the Exponent in the hemolysis index model.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def pressto(self) -> int:
        """Get or set the Flag to impose a hemolysis index of 0 on boundaries with pressure boundary conditions:
        EQ.0: Interpolate the hemolysis index on the boundaries from the interior of the domain
        EQ.1: Set the hemolysis index to 0 on boundaries with pressure boundary conditions.
        """ # nopep8
        return self._cards[0].get_value("pressto")

    @pressto.setter
    def pressto(self, value: int) -> None:
        """Set the pressto property."""
        if value not in [0, 1, None]:
            raise Exception("""pressto must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pressto", value)

