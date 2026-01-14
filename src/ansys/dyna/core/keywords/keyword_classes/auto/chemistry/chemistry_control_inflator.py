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

"""Module providing the ChemistryControlInflator class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CHEMISTRYCONTROLINFLATOR_CARD0 = (
    FieldSchema("model", int, 0, 10, 1),
    FieldSchema("out_type", int, 10, 10, 0),
    FieldSchema("truntim", float, 20, 10, None),
    FieldSchema("delt", float, 30, 10, None),
    FieldSchema("ptime", float, 40, 10, None),
)

_CHEMISTRYCONTROLINFLATOR_CARD1 = (
    FieldSchema("file", str, 0, 80, None),
)

_CHEMISTRYCONTROLINFLATOR_CARD2 = (
    FieldSchema("density", str, 0, 10, None),
    FieldSchema("species_name", str, 10, 70, None, "species name"),
)

class ChemistryControlInflator(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_INFLATOR keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_INFLATOR"

    def __init__(self, **kwargs):
        """Initialize the ChemistryControlInflator class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYCONTROLINFLATOR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CHEMISTRYCONTROLINFLATOR_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CHEMISTRYCONTROLINFLATOR_CARD2,
                **kwargs,
            ),        ]
    @property
    def model(self) -> int:
        """Get or set the Type of inflator model to compute.
        EQ.1:Pyrotechnic model.
        EQ.2:Hybrid model with cold flow option in the gas chamber.
        EQ.3:Hybrid model with heat flow in the gas chamber.
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        """Set the model property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""model must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("model", value)

    @property
    def out_type(self) -> int:
        """Get or set the Selects the output file format that will be used in an airbag simulation.EQ.0:Screen output.
        EQ.1:CESE compressible flow solver.
        EQ.2:ALE solver.
        EQ.3:CPM solver(with 2nd-order expansion of Cp)
        EQ.4:CPM solver(with 4th-order expansion of Cp)
        """ # nopep8
        return self._cards[0].get_value("out_type")

    @out_type.setter
    def out_type(self, value: int) -> None:
        """Set the out_type property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""out_type must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("out_type", value)

    @property
    def truntim(self) -> typing.Optional[float]:
        """Get or set the Total run time.
        """ # nopep8
        return self._cards[0].get_value("truntim")

    @truntim.setter
    def truntim(self, value: float) -> None:
        """Set the truntim property."""
        self._cards[0].set_value("truntim", value)

    @property
    def delt(self) -> typing.Optional[float]:
        """Get or set the Delta(t) to use in the model calculation.
        """ # nopep8
        return self._cards[0].get_value("delt")

    @delt.setter
    def delt(self, value: float) -> None:
        """Set the delt property."""
        self._cards[0].set_value("delt", value)

    @property
    def ptime(self) -> typing.Optional[float]:
        """Get or set the Time interval for output of time history data to FILE.
        """ # nopep8
        return self._cards[0].get_value("ptime")

    @ptime.setter
    def ptime(self, value: float) -> None:
        """Set the ptime property."""
        self._cards[0].set_value("ptime", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the ASCII file in which to write the time history data and other data output by the inflator simulation.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        """Set the file property."""
        self._cards[1].set_value("file", value)

    @property
    def density(self) -> typing.Optional[str]:
        """Get or set the Density of a condensed-phase species present in the inflator.
        """ # nopep8
        return self._cards[2].get_value("density")

    @density.setter
    def density(self, value: str) -> None:
        """Set the density property."""
        self._cards[2].set_value("density", value)

    @property
    def species_name(self) -> typing.Optional[str]:
        """Get or set the Chemkin-compatible name of a condensed-phase species.
        """ # nopep8
        return self._cards[2].get_value("species_name")

    @species_name.setter
    def species_name(self, value: str) -> None:
        """Set the species_name property."""
        self._cards[2].set_value("species_name", value)

