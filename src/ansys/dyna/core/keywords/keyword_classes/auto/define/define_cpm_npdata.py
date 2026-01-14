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

"""Module providing the DefineCpmNpdata class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECPMNPDATA_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("hconv", float, 10, 10, None),
    FieldSchema("pfric", float, 20, 10, None),
    FieldSchema("sdfblk", float, 30, 10, None),
    FieldSchema("kp", float, 40, 10, None),
    FieldSchema("inip", int, 50, 10, None),
    FieldSchema("cp", float, 60, 10, None),
    FieldSchema("psfdcf", float, 70, 10, None),
)

_DEFINECPMNPDATA_CARD1 = (
    FieldSchema("id", int, 0, 10, None),
)

_DEFINECPMNPDATA_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpmNpdata(KeywordBase):
    """DYNA DEFINE_CPM_NPDATA keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_NPDATA"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCpmNpdata class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPMNPDATA_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECPMNPDATA_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineCpmNpdata.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPMNPDATA_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def hconv(self) -> typing.Optional[float]:
        """Get or set the Convective heat transfer coefficient used to calculate heat loss from the airbag external surface to ambient. See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
        LT.0: | HCONV | is a load curve ID defines heat convection coefficient as a function of time.
        """ # nopep8
        return self._cards[0].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        """Set the hconv property."""
        self._cards[0].set_value("hconv", value)

    @property
    def pfric(self) -> typing.Optional[float]:
        """Get or set the Friction factor F_r if -1.0 < PFRIC ≤ 1.0.  Defaults to FRIC from Card 1 if undefined.  Otherwise,
        LE. - 1.0: | PFRIC | is the curve ID which defines F_r as a function of the part pressure.
        GT.1.0 : PFRIC is the * DEFINE_FUNCTION ID that defines F_r.
        """ # nopep8
        return self._cards[0].get_value("pfric")

    @pfric.setter
    def pfric(self, value: float) -> None:
        """Set the pfric property."""
        self._cards[0].set_value("pfric", value)

    @property
    def sdfblk(self) -> typing.Optional[float]:
        """Get or set the Scaling down factor for blockage factor (Default = 1.0, no scaling down).  The valid factor will be (0.0,1.0]. If 0.0, it will set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sdfblk")

    @sdfblk.setter
    def sdfblk(self, value: float) -> None:
        """Set the sdfblk property."""
        self._cards[0].set_value("sdfblk", value)

    @property
    def kp(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity of the part.
        """ # nopep8
        return self._cards[0].get_value("kp")

    @kp.setter
    def kp(self, value: float) -> None:
        """Set the kp property."""
        self._cards[0].set_value("kp", value)

    @property
    def inip(self) -> typing.Optional[int]:
        """Get or set the Place initial air particles on surface.
        EQ.0:	yes(default)
        EQ.1 : no
        This feature excludes surfaces from initial particle placement.This option is useful for preventing particles from being trapped between adjacent fabric layers.
        """ # nopep8
        return self._cards[0].get_value("inip")

    @inip.setter
    def inip(self, value: int) -> None:
        """Set the inip property."""
        self._cards[0].set_value("inip", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def psfdcf(self) -> typing.Optional[float]:
        """Get or set the Additional scale factor for force decay constant.
        """ # nopep8
        return self._cards[0].get_value("psfdcf")

    @psfdcf.setter
    def psfdcf(self, value: float) -> None:
        """Set the psfdcf property."""
        self._cards[0].set_value("psfdcf", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].set_value("id", value)

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

