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

"""Module providing the DefineFpToSurfaceCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFPTOSURFACECOUPLING_CARD0 = (
    FieldSchema("fp", int, 0, 10, None),
    FieldSchema("surf", int, 10, 10, None),
    FieldSchema("fptype", int, 20, 10, 0),
    FieldSchema("surftype", int, 30, 10, 0),
)

_DEFINEFPTOSURFACECOUPLING_CARD1 = (
    FieldSchema("sbc", int, 0, 10, None),
    FieldSchema("sca", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("sfp", int, 50, 10, 0),
)

_DEFINEFPTOSURFACECOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFpToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_FP_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "FP_TO_SURFACE_COUPLING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFpToSurfaceCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFPTOSURFACECOUPLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFPTOSURFACECOUPLING_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFpToSurfaceCoupling.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFPTOSURFACECOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fp(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in the coupling on the slave side.
        """ # nopep8
        return self._cards[0].get_value("fp")

    @fp.setter
    def fp(self, value: int) -> None:
        """Set the fp property."""
        self._cards[0].set_value("fp", value)

    @property
    def surf(self) -> typing.Optional[int]:
        """Get or set the Segments set ID defined in the coupling on the master side. Currently the segments set should be generated from the 8-noded hexahedron elements.
        """ # nopep8
        return self._cards[0].get_value("surf")

    @surf.setter
    def surf(self, value: int) -> None:
        """Set the surf property."""
        self._cards[0].set_value("surf", value)

    @property
    def fptype(self) -> int:
        """Get or set the Type for SLAVE:
        EQ.0: Part set ID
        EQ.1 : Part ID.
        """ # nopep8
        return self._cards[0].get_value("fptype")

    @fptype.setter
    def fptype(self, value: int) -> None:
        """Set the fptype property."""
        if value not in [0, 1, None]:
            raise Exception("""fptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fptype", value)

    @property
    def surftype(self) -> int:
        """Get or set the Type for SURF:
        EQ.0:	Segment set ID
        """ # nopep8
        return self._cards[0].get_value("surftype")

    @surftype.setter
    def surftype(self, value: int) -> None:
        """Set the surftype property."""
        self._cards[0].set_value("surftype", value)

    @property
    def sbc(self) -> typing.Optional[int]:
        """Get or set the Type of boundary condition.
        EQ.0: free-slip boundary
        EQ.1: non - slip boundary
        """ # nopep8
        return self._cards[1].get_value("sbc")

    @sbc.setter
    def sbc(self, value: int) -> None:
        """Set the sbc property."""
        self._cards[1].set_value("sbc", value)

    @property
    def sca(self) -> int:
        """Get or set the Static (equilibrium) contact angle in radian
        """ # nopep8
        return self._cards[1].get_value("sca")

    @sca.setter
    def sca(self, value: int) -> None:
        """Set the sca property."""
        self._cards[1].set_value("sca", value)

    @property
    def sfp(self) -> int:
        """Get or set the Stiffness coefficient along the normal direction of the contact interface. SFP should be less than 1.0. If SFPSFPN is too small, large penetrations can occur.
        """ # nopep8
        return self._cards[1].get_value("sfp")

    @sfp.setter
    def sfp(self, value: int) -> None:
        """Set the sfp property."""
        self._cards[1].set_value("sfp", value)

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

