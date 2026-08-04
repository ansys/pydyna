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

"""Module providing the DefineDeToCpmCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDETOCPMCOUPLING_CARD0 = (
    FieldSchema("desid", int, 0, 10, 0),
    FieldSchema("bagid", int, 10, 10, 0),
    FieldSchema("destyp", int, 20, 10, 0),
)

_DEFINEDETOCPMCOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeToCpmCoupling(KeywordBase):
    """DYNA DEFINE_DE_TO_CPM_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_CPM_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeToCpmCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOCPMCOUPLING_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeToCpmCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDETOCPMCOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def desid(self) -> int:
        """Get or set the Part set ID or part ID specifying the DES in the coupling. DESTYP below indicates the ID type.
        """ # nopep8
        return self._cards[0].get_value("desid")

    @desid.setter
    def desid(self, value: int) -> None:
        """Set the desid property."""
        self._cards[0].set_value("desid", value)

    @property
    def bagid(self) -> int:
        """Get or set the *AIRBAG_PARTICLE ID specifying the CPM airbag in the coupling.
        """ # nopep8
        return self._cards[0].get_value("bagid")

    @bagid.setter
    def bagid(self, value: int) -> None:
        """Set the bagid property."""
        self._cards[0].set_value("bagid", value)

    @property
    def destyp(self) -> int:
        """Get or set the Type for DESID:
        EQ.0: Part set
        EQ.1: Part
        """ # nopep8
        return self._cards[0].get_value("destyp")

    @destyp.setter
    def destyp(self, value: int) -> None:
        """Set the destyp property."""
        if value not in [0, 1, None]:
            raise Exception("""destyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("destyp", value)

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

