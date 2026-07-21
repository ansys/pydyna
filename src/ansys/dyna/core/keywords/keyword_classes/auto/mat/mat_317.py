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

"""Module providing the Mat317 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT317_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
)

_MAT317_CARD1 = (
    FieldSchema("estr1", float, 0, 10, None),
    FieldSchema("eend1", float, 10, 10, None),
    FieldSchema("eelim1", float, 20, 10, None),
    FieldSchema("pr1", float, 30, 10, None),
)

_MAT317_CARD2 = (
    FieldSchema("estr2", float, 0, 10, None),
    FieldSchema("eend2", float, 10, 10, None),
    FieldSchema("eelim2", float, 20, 10, None),
    FieldSchema("pr2", float, 30, 10, None),
)

_MAT317_CARD3 = (
    FieldSchema("estr3", float, 0, 10, None),
    FieldSchema("eend3", float, 10, 10, None),
    FieldSchema("eelim3", float, 20, 10, None),
    FieldSchema("pr3", float, 30, 10, None),
)

_MAT317_CARD4 = (
    FieldSchema("mstr1", float, 0, 10, None),
    FieldSchema("mend1", float, 10, 10, None),
    FieldSchema("eclim1", float, 20, 10, None),
    FieldSchema("sglim1", float, 30, 10, None),
    FieldSchema("a1", float, 40, 10, None),
    FieldSchema("prv1", float, 50, 10, None),
)

_MAT317_CARD5 = (
    FieldSchema("mstr2", float, 0, 10, None),
    FieldSchema("mend2", float, 10, 10, None),
    FieldSchema("eclim2", float, 20, 10, None),
    FieldSchema("sglim2", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("prv2", float, 50, 10, 0.5),
)

_MAT317_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat317(KeywordBase):
    """DYNA MAT_317 keyword"""

    keyword = "MAT"
    subkeyword = "317"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat317 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT317_CARD5,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat317._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT317_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART)..
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def estr1(self) -> typing.Optional[float]:
        """Get or set the Starting Young's modulus Esi in link 1.
        """ # nopep8
        return self._cards[1].get_value("estr1")

    @estr1.setter
    def estr1(self, value: float) -> None:
        """Set the estr1 property."""
        self._cards[1].set_value("estr1", value)

    @property
    def eend1(self) -> typing.Optional[float]:
        """Get or set the Ending Young's modulus Eei in link 1.
        """ # nopep8
        return self._cards[1].get_value("eend1")

    @eend1.setter
    def eend1(self, value: float) -> None:
        """Set the eend1 property."""
        self._cards[1].set_value("eend1", value)

    @property
    def eelim1(self) -> typing.Optional[float]:
        """Get or set the Elastic limit in link 1.
        """ # nopep8
        return self._cards[1].get_value("eelim1")

    @eelim1.setter
    def eelim1(self, value: float) -> None:
        """Set the eelim1 property."""
        self._cards[1].set_value("eelim1", value)

    @property
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Poisson ratio in link 1.
        """ # nopep8
        return self._cards[1].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        """Set the pr1 property."""
        self._cards[1].set_value("pr1", value)

    @property
    def estr2(self) -> typing.Optional[float]:
        """Get or set the Starting Young's modulus Esi in link 2.
        """ # nopep8
        return self._cards[2].get_value("estr2")

    @estr2.setter
    def estr2(self, value: float) -> None:
        """Set the estr2 property."""
        self._cards[2].set_value("estr2", value)

    @property
    def eend2(self) -> typing.Optional[float]:
        """Get or set the Ending Young's modulus Eei in link 2.
        """ # nopep8
        return self._cards[2].get_value("eend2")

    @eend2.setter
    def eend2(self, value: float) -> None:
        """Set the eend2 property."""
        self._cards[2].set_value("eend2", value)

    @property
    def eelim2(self) -> typing.Optional[float]:
        """Get or set the Elastic limit in link 2.
        """ # nopep8
        return self._cards[2].get_value("eelim2")

    @eelim2.setter
    def eelim2(self, value: float) -> None:
        """Set the eelim2 property."""
        self._cards[2].set_value("eelim2", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Poisson ratio in link 2.
        """ # nopep8
        return self._cards[2].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        """Set the pr2 property."""
        self._cards[2].set_value("pr2", value)

    @property
    def estr3(self) -> typing.Optional[float]:
        """Get or set the Starting Young's modulus Esi in link 3.
        """ # nopep8
        return self._cards[3].get_value("estr3")

    @estr3.setter
    def estr3(self, value: float) -> None:
        """Set the estr3 property."""
        self._cards[3].set_value("estr3", value)

    @property
    def eend3(self) -> typing.Optional[float]:
        """Get or set the Ending Young's modulus Eei in link 3.
        """ # nopep8
        return self._cards[3].get_value("eend3")

    @eend3.setter
    def eend3(self, value: float) -> None:
        """Set the eend3 property."""
        self._cards[3].set_value("eend3", value)

    @property
    def eelim3(self) -> typing.Optional[float]:
        """Get or set the Elastic limit in link 3.
        """ # nopep8
        return self._cards[3].get_value("eelim3")

    @eelim3.setter
    def eelim3(self, value: float) -> None:
        """Set the eelim3 property."""
        self._cards[3].set_value("eelim3", value)

    @property
    def pr3(self) -> typing.Optional[float]:
        """Get or set the Poisson ratio in link 3.
        """ # nopep8
        return self._cards[3].get_value("pr3")

    @pr3.setter
    def pr3(self, value: float) -> None:
        """Set the pr3 property."""
        self._cards[3].set_value("pr3", value)

    @property
    def mstr1(self) -> typing.Optional[float]:
        """Get or set the Starting exponent in link 1.
        """ # nopep8
        return self._cards[4].get_value("mstr1")

    @mstr1.setter
    def mstr1(self, value: float) -> None:
        """Set the mstr1 property."""
        self._cards[4].set_value("mstr1", value)

    @property
    def mend1(self) -> typing.Optional[float]:
        """Get or set the Ending exponent in link 1.
        """ # nopep8
        return self._cards[4].get_value("mend1")

    @mend1.setter
    def mend1(self, value: float) -> None:
        """Set the mend1 property."""
        self._cards[4].set_value("mend1", value)

    @property
    def eclim1(self) -> typing.Optional[float]:
        """Get or set the Creep strain limit in link 1.
        """ # nopep8
        return self._cards[4].get_value("eclim1")

    @eclim1.setter
    def eclim1(self, value: float) -> None:
        """Set the eclim1 property."""
        self._cards[4].set_value("eclim1", value)

    @property
    def sglim1(self) -> typing.Optional[float]:
        """Get or set the Effective stress limit in link 1.
        """ # nopep8
        return self._cards[4].get_value("sglim1")

    @sglim1.setter
    def sglim1(self, value: float) -> None:
        """Set the sglim1 property."""
        self._cards[4].set_value("sglim1", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Reference creep strain rate in link 1.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def prv1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("prv1")

    @prv1.setter
    def prv1(self, value: float) -> None:
        """Set the prv1 property."""
        self._cards[4].set_value("prv1", value)

    @property
    def mstr2(self) -> typing.Optional[float]:
        """Get or set the Starting exponent in link 2.
        """ # nopep8
        return self._cards[5].get_value("mstr2")

    @mstr2.setter
    def mstr2(self, value: float) -> None:
        """Set the mstr2 property."""
        self._cards[5].set_value("mstr2", value)

    @property
    def mend2(self) -> typing.Optional[float]:
        """Get or set the Ending exponent in link 2.
        """ # nopep8
        return self._cards[5].get_value("mend2")

    @mend2.setter
    def mend2(self, value: float) -> None:
        """Set the mend2 property."""
        self._cards[5].set_value("mend2", value)

    @property
    def eclim2(self) -> typing.Optional[float]:
        """Get or set the Creep strain limit in link 2.
        """ # nopep8
        return self._cards[5].get_value("eclim2")

    @eclim2.setter
    def eclim2(self, value: float) -> None:
        """Set the eclim2 property."""
        self._cards[5].set_value("eclim2", value)

    @property
    def sglim2(self) -> typing.Optional[float]:
        """Get or set the Effective stress limit in link 2.
        """ # nopep8
        return self._cards[5].get_value("sglim2")

    @sglim2.setter
    def sglim2(self, value: float) -> None:
        """Set the sglim2 property."""
        self._cards[5].set_value("sglim2", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Reference creep strain rate in link 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[5].set_value("a2", value)

    @property
    def prv2(self) -> float:
        """Get or set the Viscous Poisson ratio, u_i, in link i,i=1,2.Default: 0.5
        """ # nopep8
        return self._cards[5].get_value("prv2")

    @prv2.setter
    def prv2(self, value: float) -> None:
        """Set the prv2 property."""
        self._cards[5].set_value("prv2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

