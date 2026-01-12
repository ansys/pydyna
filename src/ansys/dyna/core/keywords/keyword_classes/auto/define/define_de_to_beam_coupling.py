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

"""Module providing the DefineDeToBeamCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDETOBEAMCOUPLING_CARD0 = (
    FieldSchema("slave", int, 0, 10, 0),
    FieldSchema("master", int, 10, 10, 0),
    FieldSchema("stype", int, 20, 10, 0),
    FieldSchema("mtype", int, 30, 10, 0),
)

_DEFINEDETOBEAMCOUPLING_CARD1 = (
    FieldSchema("frics", float, 0, 10, None),
    FieldSchema("fricd", float, 10, 10, 0.0),
    FieldSchema("damp", float, 20, 10, 0.0),
    FieldSchema("bsort", int, 30, 10, 100),
)

class DefineDeToBeamCoupling(KeywordBase):
    """DYNA DEFINE_DE_TO_BEAM_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_BEAM_COUPLING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeToBeamCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOBEAMCOUPLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDETOBEAMCOUPLING_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeToBeamCoupling.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def slave(self) -> int:
        """Get or set the DES nodes.
        """ # nopep8
        return self._cards[0].get_value("slave")

    @slave.setter
    def slave(self, value: int) -> None:
        """Set the slave property."""
        self._cards[0].set_value("slave", value)

    @property
    def master(self) -> int:
        """Get or set the Shell set.
        """ # nopep8
        return self._cards[0].get_value("master")

    @master.setter
    def master(self, value: int) -> None:
        """Set the master property."""
        self._cards[0].set_value("master", value)

    @property
    def stype(self) -> int:
        """Get or set the EQ.0: Slave node set
        EQ.1: Slave node
        EQ.2: Slave part set
        EQ.3: Slave part.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("stype", value)

    @property
    def mtype(self) -> int:
        """Get or set the EQ.0: Part set
        EQ.1: Part.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, None]:
            raise Exception("""mtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def frics(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient.
        """ # nopep8
        return self._cards[1].get_value("frics")

    @frics.setter
    def frics(self, value: float) -> None:
        """Set the frics property."""
        self._cards[1].set_value("frics", value)

    @property
    def fricd(self) -> float:
        """Get or set the Rolling friction coefficient.
        """ # nopep8
        return self._cards[1].get_value("fricd")

    @fricd.setter
    def fricd(self, value: float) -> None:
        """Set the fricd property."""
        self._cards[1].set_value("fricd", value)

    @property
    def damp(self) -> float:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def bsort(self) -> int:
        """Get or set the Number of cycle between bucket sortings. (Default = 100).
        """ # nopep8
        return self._cards[1].get_value("bsort")

    @bsort.setter
    def bsort(self, value: int) -> None:
        """Set the bsort property."""
        self._cards[1].set_value("bsort", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

