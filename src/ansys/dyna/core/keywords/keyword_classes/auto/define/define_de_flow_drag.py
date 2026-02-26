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

"""Module providing the DefineDeFlowDrag class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDEFLOWDRAG_CARD0 = (
    FieldSchema("cd", float, 0, 10, 0.0),
    FieldSchema("rho", float, 10, 10, 0.0),
    FieldSchema("mu", float, 20, 10, 0.0),
    FieldSchema("vx", float, 30, 10, 0.0),
    FieldSchema("vy", float, 40, 10, 0.0),
    FieldSchema("vz", float, 50, 10, 0.0),
    FieldSchema("tbirth", float, 60, 10, 0.0),
    FieldSchema("tdeath", float, 70, 10, 1e+20),
)

_DEFINEDEFLOWDRAG_CARD1 = (
    FieldSchema("vs", float, 0, 10, 0.0),
    FieldSchema("dflag", int, 10, 10, 1),
    FieldSchema("sfn", float, 20, 10, 1.0),
    FieldSchema("sfs", float, 30, 10, 1.0),
)

_DEFINEDEFLOWDRAG_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeFlowDrag(KeywordBase):
    """DYNA DEFINE_DE_FLOW_DRAG keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_FLOW_DRAG"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeFlowDrag class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEFLOWDRAG_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEFLOWDRAG_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeFlowDrag.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEFLOWDRAG_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def cd(self) -> float:
        """Get or set the Drag coefficient
        EQ.-1:CD is evaluated based on remark 2.
        EQ.-2: Cd determined based on Allen 2018. See Remark 2. Card 1.1 must be included
        """ # nopep8
        return self._cards[0].get_value("cd")

    @cd.setter
    def cd(self, value: float) -> None:
        """Set the cd property."""
        self._cards[0].set_value("cd", value)

    @property
    def rho(self) -> float:
        """Get or set the Density.
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def mu(self) -> float:
        """Get or set the Dynamic viscosity.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[0].set_value("mu", value)

    @property
    def vx(self) -> float:
        """Get or set the Velocity.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Velocity.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Velocity.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def vs(self) -> float:
        """Get or set the Sound speed, Vs for CD=-2
        """ # nopep8
        return self._cards[1].get_value("vs")

    @vs.setter
    def vs(self, value: float) -> None:
        """Set the vs property."""
        self._cards[1].set_value("vs", value)

    @property
    def dflag(self) -> int:
        """Get or set the Influence of neighbors on drag treatment:
        EQ.1:	Consider only shadowing effect(default)
        EQ.2 : See Remark 3.
        EQ.3 : See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("dflag")

    @dflag.setter
    def dflag(self, value: int) -> None:
        """Set the dflag property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dflag must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("dflag", value)

    @property
    def sfn(self) -> float:
        """Get or set the Scale factors for Cd1_eff respectively when DFLAG=2.
        """ # nopep8
        return self._cards[1].get_value("sfn")

    @sfn.setter
    def sfn(self, value: float) -> None:
        """Set the sfn property."""
        self._cards[1].set_value("sfn", value)

    @property
    def sfs(self) -> float:
        """Get or set the Scale factors for Cd2_eff respectively when DFLAG=2.
        """ # nopep8
        return self._cards[1].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        """Set the sfs property."""
        self._cards[1].set_value("sfs", value)

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

