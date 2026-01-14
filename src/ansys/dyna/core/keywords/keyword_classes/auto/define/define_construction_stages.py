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

"""Module providing the DefineConstructionStages class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECONSTRUCTIONSTAGES_CARD0 = (
    FieldSchema("istage", int, 0, 10, None),
    FieldSchema("ats", float, 10, 10, 0.0),
    FieldSchema("ate", float, 20, 10, 0.0),
    FieldSchema("atr", float, 30, 10, None),
    FieldSchema("rts", float, 40, 10, 0.0),
    FieldSchema("rte", float, 50, 10, 0.0),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("idynain", int, 70, 10, 0),
)

_DEFINECONSTRUCTIONSTAGES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineConstructionStages(KeywordBase):
    """DYNA DEFINE_CONSTRUCTION_STAGES keyword"""

    keyword = "DEFINE"
    subkeyword = "CONSTRUCTION_STAGES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineConstructionStages class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONSTRUCTIONSTAGES_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineConstructionStages.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONSTRUCTIONSTAGES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def istage(self) -> typing.Optional[int]:
        """Get or set the Stage ID.
        """ # nopep8
        return self._cards[0].get_value("istage")

    @istage.setter
    def istage(self, value: int) -> None:
        """Set the istage property."""
        self._cards[0].set_value("istage", value)

    @property
    def ats(self) -> float:
        """Get or set the Analysis time at start of stage
        """ # nopep8
        return self._cards[0].get_value("ats")

    @ats.setter
    def ats(self, value: float) -> None:
        """Set the ats property."""
        self._cards[0].set_value("ats", value)

    @property
    def ate(self) -> float:
        """Get or set the Analysis time at end of stage.
        """ # nopep8
        return self._cards[0].get_value("ate")

    @ate.setter
    def ate(self, value: float) -> None:
        """Set the ate property."""
        self._cards[0].set_value("ate", value)

    @property
    def atr(self) -> typing.Optional[float]:
        """Get or set the Analysis time duration of ramp.
        """ # nopep8
        return self._cards[0].get_value("atr")

    @atr.setter
    def atr(self, value: float) -> None:
        """Set the atr property."""
        self._cards[0].set_value("atr", value)

    @property
    def rts(self) -> float:
        """Get or set the Real time at start of stage.
        """ # nopep8
        return self._cards[0].get_value("rts")

    @rts.setter
    def rts(self, value: float) -> None:
        """Set the rts property."""
        self._cards[0].set_value("rts", value)

    @property
    def rte(self) -> float:
        """Get or set the Real time at end of stage.
        """ # nopep8
        return self._cards[0].get_value("rte")

    @rte.setter
    def rte(self, value: float) -> None:
        """Set the rte property."""
        self._cards[0].set_value("rte", value)

    @property
    def idynain(self) -> int:
        """Get or set the Flag to control output of dynain file at the end of the stage
        EQ.0:	write dynain file
        EQ.1:	do not write dynain file .
        """ # nopep8
        return self._cards[0].get_value("idynain")

    @idynain.setter
    def idynain(self, value: int) -> None:
        """Set the idynain property."""
        if value not in [0, 1, None]:
            raise Exception("""idynain must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idynain", value)

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

