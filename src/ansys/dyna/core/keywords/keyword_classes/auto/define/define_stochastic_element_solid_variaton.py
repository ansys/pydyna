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

"""Module providing the DefineStochasticElementSolidVariaton class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESTOCHASTICELEMENTSOLIDVARIATON_CARD0 = (
    FieldSchema("ide", int, 0, 10, 0),
    FieldSchema("varsy", float, 10, 10, 0.0),
    FieldSchema("varf", float, 20, 10, 0.0),
    FieldSchema("varro", float, 30, 10, 0.0),
    FieldSchema("vare", float, 40, 10, 0.0),
)

_DEFINESTOCHASTICELEMENTSOLIDVARIATON_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineStochasticElementSolidVariaton(KeywordBase):
    """DYNA DEFINE_STOCHASTIC_ELEMENT_SOLID_VARIATON keyword"""

    keyword = "DEFINE"
    subkeyword = "STOCHASTIC_ELEMENT_SOLID_VARIATON"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "ide": LinkType.ELEMENT_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineStochasticElementSolidVariaton class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESTOCHASTICELEMENTSOLIDVARIATON_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineStochasticElementSolidVariaton.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESTOCHASTICELEMENTSOLIDVARIATON_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def ide(self) -> int:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("ide")

    @ide.setter
    def ide(self, value: int) -> None:
        """Set the ide property."""
        self._cards[0].set_value("ide", value)

    @property
    def varsy(self) -> float:
        """Get or set the The yield stress and its hardening function are scaled by 1.+VARSY.
        """ # nopep8
        return self._cards[0].get_value("varsy")

    @varsy.setter
    def varsy(self, value: float) -> None:
        """Set the varsy property."""
        self._cards[0].set_value("varsy", value)

    @property
    def varf(self) -> float:
        """Get or set the The failure criterion is scaled by 1+VARF.
        """ # nopep8
        return self._cards[0].get_value("varf")

    @varf.setter
    def varf(self, value: float) -> None:
        """Set the varf property."""
        self._cards[0].set_value("varf", value)

    @property
    def varro(self) -> float:
        """Get or set the The density is scaled by 1+VARRO. This is intended to be used with topology optimization. This option is not available for shell elements.
        """ # nopep8
        return self._cards[0].get_value("varro")

    @varro.setter
    def varro(self, value: float) -> None:
        """Set the varro property."""
        self._cards[0].set_value("varro", value)

    @property
    def vare(self) -> float:
        """Get or set the The elastic moduli are scaled by 1+VARE. This is intended to be used with topology optimization.
        """ # nopep8
        return self._cards[0].get_value("vare")

    @vare.setter
    def vare(self, value: float) -> None:
        """Set the vare property."""
        self._cards[0].set_value("vare", value)

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

    @property
    def ide_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given ide."""
        return self._get_link_by_attr("ELEMENT", "eid", self.ide, "parts")

