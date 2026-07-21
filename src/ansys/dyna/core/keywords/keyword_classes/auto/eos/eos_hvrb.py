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

"""Module providing the EosHvrb class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSHVRB_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eosurid", int, 10, 10, None),
    FieldSchema("eosrpid", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("tao0", float, 70, 10, None),
)

_EOSHVRB_CARD1 = (
    FieldSchema("pr", float, 0, 10, None),
    FieldSchema("z", float, 10, 10, None),
    FieldSchema("m", float, 20, 10, None),
    FieldSchema("pi", float, 30, 10, None),
    FieldSchema("rmax", float, 40, 10, None),
    FieldSchema("rmin", float, 50, 10, None),
)

_EOSHVRB_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class EosHvrb(KeywordBase):
    """DYNA EOS_HVRB keyword"""

    keyword = "EOS"
    subkeyword = "HVRB"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the EosHvrb class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSHVRB_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOSHVRB_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = EosHvrb._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _EOSHVRB_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def eosurid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID of the unreacted explosive (UR). This field refers to a separately defined *EOS_GRUNEISEN card that specifies the shock behavior of the unreacted explosive.
        """ # nopep8
        return self._cards[0].get_value("eosurid")

    @eosurid.setter
    def eosurid(self, value: int) -> None:
        """Set the eosurid property."""
        self._cards[0].set_value("eosurid", value)

    @property
    def eosrpid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID of the reaction products (RP). This field refers to a separately defined *EOS_JWL card that specifies the afterburn behavior of the reaction products.
        """ # nopep8
        return self._cards[0].get_value("eosrpid")

    @eosrpid.setter
    def eosrpid(self, value: int) -> None:
        """Set the eosrpid property."""
        self._cards[0].set_value("eosrpid", value)

    @property
    def tao0(self) -> typing.Optional[float]:
        """Get or set the Parameter ?_0 in the reaction fraction calculation (see Remarks). We recommend setting ?_0 to 1 �s
        """ # nopep8
        return self._cards[0].get_value("tao0")

    @tao0.setter
    def tao0(self, value: float) -> None:
        """Set the tao0 property."""
        self._cards[0].set_value("tao0", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Parameter P_R in the reaction fraction calculation (see Remarks)
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[1].set_value("pr", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Parameter Z in the reaction fraction calculation (see Remarks)
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Parameter M in the reaction fraction calculation (see Remarks)
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def pi(self) -> typing.Optional[float]:
        """Get or set the Parameter P_I in the reaction fraction calculation (see Remarks)
        """ # nopep8
        return self._cards[1].get_value("pi")

    @pi.setter
    def pi(self, value: float) -> None:
        """Set the pi property."""
        self._cards[1].set_value("pi", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum density of the unreacted explosive. The reaction is complete for densities > RMAX.
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[1].set_value("rmax", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the Minimum density of the unreacted explosive. The reaction is complete for densities < RMIN.
        """ # nopep8
        return self._cards[1].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        """Set the rmin property."""
        self._cards[1].set_value("rmin", value)

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

