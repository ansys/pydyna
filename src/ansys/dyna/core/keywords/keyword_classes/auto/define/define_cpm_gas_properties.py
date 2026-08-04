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

"""Module providing the DefineCpmGasProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINECPMGASPROPERTIES_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("xmm", float, 10, 10, None),
    FieldSchema("cp0", float, 20, 10, None),
    FieldSchema("cp1", float, 30, 10, None),
    FieldSchema("cp2", float, 40, 10, None),
    FieldSchema("cp3", float, 50, 10, None),
    FieldSchema("cp4", float, 60, 10, None),
)

_DEFINECPMGASPROPERTIES_CARD1 = (
    FieldSchema("mut0", float, 0, 10, None),
    FieldSchema("mut1", float, 10, 10, None),
    FieldSchema("mut2", float, 20, 10, None),
    FieldSchema("mut3", float, 30, 10, None),
    FieldSchema("mut4", float, 40, 10, None),
    FieldSchema("chm_id", int, 50, 10, None),
    FieldSchema("vini", float, 60, 10, 0.0),
    FieldSchema("lcmcf", int, 70, 10, 0),
)

_DEFINECPMGASPROPERTIES_CARD2 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("pcut", float, 10, 10, 1e+20),
    FieldSchema("mcut", float, 20, 10, -1e+20),
    FieldSchema("ttime", float, 30, 10, None),
)

_DEFINECPMGASPROPERTIES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpmGasProperties(KeywordBase):
    """DYNA DEFINE_CPM_GAS_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_GAS_PROPERTIES"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCpmGasProperties class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPMGASPROPERTIES_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPMGASPROPERTIES_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPMGASPROPERTIES_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpmGasProperties._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPMGASPROPERTIES_OPTION0_CARD0,
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
    def xmm(self) -> typing.Optional[float]:
        """Get or set the Molar mass
        """ # nopep8
        return self._cards[0].get_value("xmm")

    @xmm.setter
    def xmm(self, value: float) -> None:
        """Set the xmm property."""
        self._cards[0].set_value("xmm", value)

    @property
    def cp0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp0")

    @cp0.setter
    def cp0(self, value: float) -> None:
        """Set the cp0 property."""
        self._cards[0].set_value("cp0", value)

    @property
    def cp1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp1")

    @cp1.setter
    def cp1(self, value: float) -> None:
        """Set the cp1 property."""
        self._cards[0].set_value("cp1", value)

    @property
    def cp2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp2")

    @cp2.setter
    def cp2(self, value: float) -> None:
        """Set the cp2 property."""
        self._cards[0].set_value("cp2", value)

    @property
    def cp3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp3")

    @cp3.setter
    def cp3(self, value: float) -> None:
        """Set the cp3 property."""
        self._cards[0].set_value("cp3", value)

    @property
    def cp4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent specific heat with constant pressure
        """ # nopep8
        return self._cards[0].get_value("cp4")

    @cp4.setter
    def cp4(self, value: float) -> None:
        """Set the cp4 property."""
        self._cards[0].set_value("cp4", value)

    @property
    def mut0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut0")

    @mut0.setter
    def mut0(self, value: float) -> None:
        """Set the mut0 property."""
        self._cards[1].set_value("mut0", value)

    @property
    def mut1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut1")

    @mut1.setter
    def mut1(self, value: float) -> None:
        """Set the mut1 property."""
        self._cards[1].set_value("mut1", value)

    @property
    def mut2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut2")

    @mut2.setter
    def mut2(self, value: float) -> None:
        """Set the mut2 property."""
        self._cards[1].set_value("mut2", value)

    @property
    def mut3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut3")

    @mut3.setter
    def mut3(self, value: float) -> None:
        """Set the mut3 property."""
        self._cards[1].set_value("mut3", value)

    @property
    def mut4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of temperature dependent Joule-Thomson effect
        """ # nopep8
        return self._cards[1].get_value("mut4")

    @mut4.setter
    def mut4(self, value: float) -> None:
        """Set the mut4 property."""
        self._cards[1].set_value("mut4", value)

    @property
    def chm_id(self) -> typing.Optional[int]:
        """Get or set the Chamber ID (see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("chm_id")

    @chm_id.setter
    def chm_id(self, value: int) -> None:
        """Set the chm_id property."""
        self._cards[1].set_value("chm_id", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial volume for user defined inflator (see Remark 1):
        EQ.0.0: User defined inflator disabled
        GT.0.0: Initial volume
        LT.0.0: Calculate volume based on chamber geometry.
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        """Set the vini property."""
        self._cards[1].set_value("vini", value)

    @property
    def lcmcf(self) -> int:
        """Get or set the Optional load curve ID referring to a *DEFINE_CURVE_FUNCTION. If specified, this load curve gives the total mass flow rate of the gas. Each LCMi on Card 13 of *AIRBAG_PARTICLE is then not used to give the mass flow rate for component i. However, LCMi must still be provided because it is used to determine the mass of gas component i in the system at time t by integrating the curve from 0 to t. LCMCF is not supported when using the MOLEFRACTION keyword option with *AIRBAG_PARTICLE.
        """ # nopep8
        return self._cards[1].get_value("lcmcf")

    @lcmcf.setter
    def lcmcf(self, value: int) -> None:
        """Set the lcmcf property."""
        self._cards[1].set_value("lcmcf", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID to get part pressure)
        """ # nopep8
        return self._cards[2].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[2].set_value("pid", value)

    @property
    def pcut(self) -> float:
        """Get or set the Cutoff pressure. If the part pressure found for PID is greater than PCUT, the mass flow rate of this gas is restricted.The pressure compared to PCUT is determined with moving average over the number of cycles specified with ISMTH.
        """ # nopep8
        return self._cards[2].get_value("pcut")

    @pcut.setter
    def pcut(self, value: float) -> None:
        """Set the pcut property."""
        self._cards[2].set_value("pcut", value)

    @property
    def mcut(self) -> float:
        """Get or set the Cutoff mass flow rate. If the total mass flow rate excluding this gas is declining and below MCUT, the mass flow rate of this gas is restricted.
        """ # nopep8
        return self._cards[2].get_value("mcut")

    @mcut.setter
    def mcut(self, value: float) -> None:
        """Set the mcut property."""
        self._cards[2].set_value("mcut", value)

    @property
    def ttime(self) -> typing.Optional[float]:
        """Get or set the Taper down duration. After satisfying one of the above cutoff conditions (MCUT or PCUT), a linear factor from 1.0 to 0.0 is applied to this mass flow rate. If TTIME equals zero, the flow from this gas is stopped after the cutoff.
        """ # nopep8
        return self._cards[2].get_value("ttime")

    @ttime.setter
    def ttime(self, value: float) -> None:
        """Set the ttime property."""
        self._cards[2].set_value("ttime", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

