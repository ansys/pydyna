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

"""Module providing the DefineSaleabParameters class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINESALEABPARAMETERS_CARD0 = (
    FieldSchema("nabsale", int, 0, 10, None),
    FieldSchema("iunit", int, 10, 10, None),
    FieldSchema("patm", float, 20, 10, None),
)

_DEFINESALEABPARAMETERS_CARD1 = (
    FieldSchema("fsiext", int, 0, 10, None),
    FieldSchema("fsint", int, 10, 10, None),
    FieldSchema("fsilck", int, 20, 10, None),
    FieldSchema("cvid", int, 30, 10, None),
    FieldSchema("pair", float, 40, 10, None),
    FieldSchema("tswt", float, 50, 10, 1e+16),
)

_DEFINESALEABPARAMETERS_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSaleabParameters(KeywordBase):
    """DYNA DEFINE_SALEAB_PARAMETERS keyword"""

    keyword = "DEFINE"
    subkeyword = "SALEAB_PARAMETERS"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineSaleabParameters class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESALEABPARAMETERS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESALEABPARAMETERS_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSaleabParameters._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESALEABPARAMETERS_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def nabsale(self) -> typing.Optional[int]:
        """Get or set the Number of original *AIRBAG_SALE cards.
        """ # nopep8
        return self._cards[0].get_value("nabsale")

    @nabsale.setter
    def nabsale(self, value: int) -> None:
        """Set the nabsale property."""
        self._cards[0].set_value("nabsale", value)

    @property
    def iunit(self) -> typing.Optional[int]:
        """Get or set the IUNIT in original *AIRBAG_SALE cards.
        """ # nopep8
        return self._cards[0].get_value("iunit")

    @iunit.setter
    def iunit(self, value: int) -> None:
        """Set the iunit property."""
        self._cards[0].set_value("iunit", value)

    @property
    def patm(self) -> typing.Optional[float]:
        """Get or set the PATM in original *AIRBAG_SALE cards.
        """ # nopep8
        return self._cards[0].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[0].set_value("patm", value)

    @property
    def fsiext(self) -> typing.Optional[int]:
        """Get or set the FSI ID of *ALE_STRUCTURED_FSI_ABEXT card coupling to exterior parts. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("fsiext")

    @fsiext.setter
    def fsiext(self, value: int) -> None:
        """Set the fsiext property."""
        self._cards[1].set_value("fsiext", value)

    @property
    def fsint(self) -> typing.Optional[int]:
        """Get or set the FSI ID of * ALE_STRUCTURED_FSI_ABINT card coupling to interior parts. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("fsint")

    @fsint.setter
    def fsint(self, value: int) -> None:
        """Set the fsint property."""
        self._cards[1].set_value("fsint", value)

    @property
    def fsilck(self) -> typing.Optional[int]:
        """Get or set the FSI ID of * ALE_STRUCTURED_FSI card performing anti contact locking. See Remark
        """ # nopep8
        return self._cards[1].get_value("fsilck")

    @fsilck.setter
    def fsilck(self, value: int) -> None:
        """Set the fsilck property."""
        self._cards[1].set_value("fsilck", value)

    @property
    def cvid(self) -> typing.Optional[int]:
        """Get or set the ID of *AIRBAG_HYBRID to perform switch if prescribed in the original *AIRBAG_SALE card.
        """ # nopep8
        return self._cards[1].get_value("cvid")

    @cvid.setter
    def cvid(self, value: int) -> None:
        """Set the cvid property."""
        self._cards[1].set_value("cvid", value)

    @property
    def pair(self) -> typing.Optional[float]:
        """Get or set the PAIR in the original *AIRBAG_SALE card.
        """ # nopep8
        return self._cards[1].get_value("pair")

    @pair.setter
    def pair(self, value: float) -> None:
        """Set the pair property."""
        self._cards[1].set_value("pair", value)

    @property
    def tswt(self) -> float:
        """Get or set the TSWT in the original *AIRBAG_SALE card.
        """ # nopep8
        return self._cards[1].get_value("tswt")

    @tswt.setter
    def tswt(self, value: float) -> None:
        """Set the tswt property."""
        self._cards[1].set_value("tswt", value)

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

