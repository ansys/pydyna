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

"""Module providing the MatT10 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATT10_CARD0 = (
    FieldSchema("tmid", int, 0, 10, None),
    FieldSchema("tro", float, 10, 10, None),
    FieldSchema("tgrlc", int, 20, 10, None),
    FieldSchema("tgmult", float, 30, 10, None),
    FieldSchema("tlat", float, 40, 10, None),
    FieldSchema("hlat", float, 50, 10, None),
)

_MATT10_CARD1 = (
    FieldSchema("hclc", int, 0, 10, None),
    FieldSchema("tclc", int, 10, 10, None),
    FieldSchema("hchsv", float, 20, 10, None),
    FieldSchema("tchsv", float, 30, 10, None),
    FieldSchema("tghsv", float, 40, 10, None),
)

_MATT10_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatT10(KeywordBase):
    """DYNA MAT_T10 keyword"""

    keyword = "MAT"
    subkeyword = "T10"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatT10 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATT10_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATT10_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatT10.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATT10_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal conductivity at T1al material identification, a unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        """Set the tmid property."""
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:
        EQ 0.0 structural density(default).
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        """Set the tro property."""
        self._cards[0].set_value("tro", value)

    @property
    def tgrlc(self) -> typing.Optional[int]:
        """Get or set the Thermal generation rate (see *DEFINE_CURVE).
        GT.0:	Load curve ID defining thermal generation rate as a function of time
        EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
        LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("tgrlc")

    @tgrlc.setter
    def tgrlc(self, value: int) -> None:
        """Set the tgrlc property."""
        self._cards[0].set_value("tgrlc", value)

    @property
    def tgmult(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
        EQ.0.0: no heat generation.
        """ # nopep8
        return self._cards[0].get_value("tgmult")

    @tgmult.setter
    def tgmult(self, value: float) -> None:
        """Set the tgmult property."""
        self._cards[0].set_value("tgmult", value)

    @property
    def tlat(self) -> typing.Optional[float]:
        """Get or set the Phase change temperature.
        """ # nopep8
        return self._cards[0].get_value("tlat")

    @tlat.setter
    def tlat(self, value: float) -> None:
        """Set the tlat property."""
        self._cards[0].set_value("tlat", value)

    @property
    def hlat(self) -> typing.Optional[float]:
        """Get or set the Latent heat.
        """ # nopep8
        return self._cards[0].get_value("hlat")

    @hlat.setter
    def hlat(self, value: float) -> None:
        """Set the hlat property."""
        self._cards[0].set_value("hlat", value)

    @property
    def hclc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying specific heat as a function of temperature, or, if |HCHSV| > 0:
        GT.0:	Load curve specifying the specific heat as a function of the mechanical history variable specified by HCHSV.
        LT.0 : Table of load curves for different temperatures.Each curve specifies the specific heat as a function of the mechanical history variable specified by HCHSV
        """ # nopep8
        return self._cards[1].get_value("hclc")

    @hclc.setter
    def hclc(self, value: int) -> None:
        """Set the hclc property."""
        self._cards[1].set_value("hclc", value)

    @property
    def tclc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying thermal conductivity as a function of temperature, or if |TCHSV| > 0:
        GT.0:	Load curve giving thermal conductivity as a function of the mechanical history variable specified by TCHSV.
        LT.0 : Table of load curves for different temperatures.Each curve is a function of the mechanical history variable specified by TCHSV
        """ # nopep8
        return self._cards[1].get_value("tclc")

    @tclc.setter
    def tclc(self, value: int) -> None:
        """Set the tclc property."""
        self._cards[1].set_value("tclc", value)

    @property
    def hchsv(self) -> typing.Optional[float]:
        """Get or set the Optional:
        GT.0.0:	mechanical history variable # used by HCL
        LT.0.0:	as above but | HCHSV |= 1 - 6 points to the six stress components, | HCHSV |= 7 to plastic strain,and | HCHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("hchsv")

    @hchsv.setter
    def hchsv(self, value: float) -> None:
        """Set the hchsv property."""
        self._cards[1].set_value("hchsv", value)

    @property
    def tchsv(self) -> typing.Optional[float]:
        """Get or set the Optional:
        GT.0.0:	mechanical history variable # used by TCLC
        LT.0.0:	as above but | TCHSV |= 1 - 6 points to the six stress components, | TCHSV |= 7 to plastic strain,and | TCHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("tchsv")

    @tchsv.setter
    def tchsv(self, value: float) -> None:
        """Set the tchsv property."""
        self._cards[1].set_value("tchsv", value)

    @property
    def tghsv(self) -> typing.Optional[float]:
        """Get or set the Optional:
        GT.0.0:	mechanical history variable # used by TGRLC
        LT.0.0:	as above but | TGHSV |= 1 - 6 points to the six stress components, | TGHSV |= 7 to plastic strain,and | TGHSV |= 7 + k points to history variable k
        """ # nopep8
        return self._cards[1].get_value("tghsv")

    @tghsv.setter
    def tghsv(self, value: float) -> None:
        """Set the tghsv property."""
        self._cards[1].set_value("tghsv", value)

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

