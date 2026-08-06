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

"""Module providing the SectionIspg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONISPG_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("elform", int, 10, 10, 0),
)

_SECTIONISPG_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionIspg(KeywordBase):
    """DYNA SECTION_ISPG keyword"""

    keyword = "SECTION"
    subkeyword = "ISPG"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionIspg class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONISPG_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SectionIspg._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONISPG_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID.  SECID is referenced on the *PART card.  A unique number or label must be specified..
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation options.
        EQ.49: 3D and pseudo 2D ISPG formulation.For pseudo 2D simulations, *ISPG_BOUNDARY_SYMMETRY must specify the symmetric boundary condition.
        EQ.50: Planar 2D ISPG formulation.One layer of elements should be defined in the thickness direction. *ISPG_BOUNDARY_SYMMETRY must set the symmetric boundary condition.
        EQ.51: Axisymmetric 2D ISPG formulation.Notice that(r,z) for the axisymmetric definition must be(x,z).Because remeshing is based on 3D remeshing, the model needs one layer of elements in the y - direction with the same thickness. *ISPG_BOUNDARY_SYMMETRY defines the symmetric boundary condition.The surface with COORD1 defined in *ISPG_BOUNDARY_SYMMETRY gives the axisymmetric surface(see Figure 0 - 1).
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        self._cards[0].set_value("elform", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

