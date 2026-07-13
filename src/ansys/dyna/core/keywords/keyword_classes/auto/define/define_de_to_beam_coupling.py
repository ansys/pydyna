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
    FieldSchema("desid", int, 0, 10, 0),
    FieldSchema("beamid", int, 10, 10, 0),
    FieldSchema("destyp", int, 20, 10, 0),
    FieldSchema("beamtyp", int, 30, 10, 0),
)

_DEFINEDETOBEAMCOUPLING_CARD1 = (
    FieldSchema("frics", float, 0, 10, None),
    FieldSchema("fricd", float, 10, 10, 0.0),
    FieldSchema("damp", float, 20, 10, 0.0),
    FieldSchema("bsort", int, 30, 10, 100),
)

_DEFINEDETOBEAMCOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeToBeamCoupling(KeywordBase):
    """DYNA DEFINE_DE_TO_BEAM_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_BEAM_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeToBeamCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOBEAMCOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOBEAMCOUPLING_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeToBeamCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDETOBEAMCOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def desid(self) -> int:
        """Get or set the Node set ID, node ID, part set ID or part ID specifying the DES in the coupling. DESTYP below indicates the ID type
        """ # nopep8
        return self._cards[0].get_value("desid")

    @desid.setter
    def desid(self, value: int) -> None:
        """Set the desid property."""
        self._cards[0].set_value("desid", value)

    @property
    def beamid(self) -> int:
        """Get or set the Part set ID or part ID specifying the beams in the coupling. BEAMTYP below indicates the ID type.
        """ # nopep8
        return self._cards[0].get_value("beamid")

    @beamid.setter
    def beamid(self, value: int) -> None:
        """Set the beamid property."""
        self._cards[0].set_value("beamid", value)

    @property
    def destyp(self) -> int:
        """Get or set the Type for DESID:
        EQ.0: Node set
        EQ.1: Node
        EQ.2: Part set
        EQ.3: Part
        """ # nopep8
        return self._cards[0].get_value("destyp")

    @destyp.setter
    def destyp(self, value: int) -> None:
        """Set the destyp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""destyp must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("destyp", value)

    @property
    def beamtyp(self) -> int:
        """Get or set the Type for BEAMID:
        EQ.0: Part set
        EQ.1: Part
        """ # nopep8
        return self._cards[0].get_value("beamtyp")

    @beamtyp.setter
    def beamtyp(self, value: int) -> None:
        """Set the beamtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""beamtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("beamtyp", value)

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
        """Get or set the Number of cycle between bucket sortngs. (Default = 100).
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

