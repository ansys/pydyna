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

"""Module providing the DefineStochasticVariationProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINESTOCHASTICVARIATIONPROPERTIES_CARD0 = (
    FieldSchema("id_sv", int, 0, 10, None),
    FieldSchema("mtype", int, 10, 10, None),
    FieldSchema("pid", int, 20, 10, None),
    FieldSchema("pid_typ", int, 30, 10, 0),
    FieldSchema("irng", int, 40, 10, 0),
    FieldSchema("numv", int, 50, 10, 0),
    FieldSchema("num_beg", int, 60, 10, 0),
    FieldSchema("unused", int, 70, 10, None),
)

_DEFINESTOCHASTICVARIATIONPROPERTIES_CARD1 = (
    FieldSchema("vartyp", int, 0, 10, 0),
    FieldSchema("corlgr", int, 10, 10, None),
    FieldSchema("r1", float, 20, 10, None),
    FieldSchema("r2", float, 30, 10, None),
    FieldSchema("r3", float, 40, 10, None),
)

_DEFINESTOCHASTICVARIATIONPROPERTIES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineStochasticVariationProperties(KeywordBase):
    """DYNA DEFINE_STOCHASTIC_VARIATION_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "STOCHASTIC_VARIATION_PROPERTIES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineStochasticVariationProperties class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESTOCHASTICVARIATIONPROPERTIES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESTOCHASTICVARIATIONPROPERTIES_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineStochasticVariationProperties.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESTOCHASTICVARIATIONPROPERTIES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id_sv(self) -> typing.Optional[int]:
        """Get or set the Stochastic variation ID. A unique ID number must be used
        """ # nopep8
        return self._cards[0].get_value("id_sv")

    @id_sv.setter
    def id_sv(self, value: int) -> None:
        """Set the id_sv property."""
        self._cards[0].set_value("id_sv", value)

    @property
    def mtype(self) -> typing.Optional[int]:
        """Get or set the Material type.  The available types are 10, 15, 24, 81, 98, and 213.  This variation only works for this material.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        self._cards[0].set_value("mtype", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the *PART ID or *SET_PART ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def pid_typ(self) -> int:
        """Get or set the Flag for PID type. If PID and PID_TYP are both 0, then the
        properties defined here apply to all shell and solid parts using
        materials with the STOCHASTIC option.
        EQ.0: PID is a *PART ID.
        EQ.1: PID is a *SET_PART ID
        """ # nopep8
        return self._cards[0].get_value("pid_typ")

    @pid_typ.setter
    def pid_typ(self, value: int) -> None:
        """Set the pid_typ property."""
        if value not in [0, 1, None]:
            raise Exception("""pid_typ must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pid_typ", value)

    @property
    def irng(self) -> int:
        """Get or set the 
        Flag for random number generation.
        EQ.0:	Use deterministic(pseudo - ) random number generator.The same input always leads to the same distribution.
        EQ.1 : Use non - deterministic(true) random number generator.With the same input, a different distribution is achieved in each run
        """ # nopep8
        return self._cards[0].get_value("irng")

    @irng.setter
    def irng(self, value: int) -> None:
        """Set the irng property."""
        if value not in [0, 1, None]:
            raise Exception("""irng must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irng", value)

    @property
    def numv(self) -> int:
        """Get or set the Number of variations for a user material
        """ # nopep8
        return self._cards[0].get_value("numv")

    @numv.setter
    def numv(self, value: int) -> None:
        """Set the numv property."""
        self._cards[0].set_value("numv", value)

    @property
    def num_beg(self) -> int:
        """Get or set the The location of the first variation in the history variables for a user material. The remaining variations are added sequentially
        """ # nopep8
        return self._cards[0].get_value("num_beg")

    @num_beg.setter
    def num_beg(self, value: int) -> None:
        """Set the num_beg property."""
        self._cards[0].set_value("num_beg", value)

    @property
    def vartyp(self) -> int:
        """Get or set the Variation type for scaling the material property:
        EQ.0:	The scale factor is 1.0 everywhere.
        EQ.1 : The scale factor is a random number in the uniform random distribution in the interval defined by R1 and R2.
        EQ.2 : The scale factor is a random number obeying the Gaussian distribution defined by R1, R2,and R3.
        EQ.3 : The scale factor is defined by the probability distribution function defined by curve LCID.
        EQ.4 : The scale factor is defined by the cumulative distribution function defined by curve LCID.
        """ # nopep8
        return self._cards[1].get_value("vartyp")

    @vartyp.setter
    def vartyp(self, value: int) -> None:
        """Set the vartyp property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""vartyp must be `None` or one of {0,1,2,3,4}.""")
        self._cards[1].set_value("vartyp", value)

    @property
    def corlgr(self) -> typing.Optional[int]:
        """Get or set the Correlation group number. If CORLGRP is 0, then the random number for the distribution is uncorrelated with all the other distributions. The same random number is used for evaluating all the distributions having the same positive integer value for CORLGRP
        """ # nopep8
        return self._cards[1].get_value("corlgr")

    @corlgr.setter
    def corlgr(self, value: int) -> None:
        """Set the corlgr property."""
        self._cards[1].set_value("corlgr", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[1].set_value("r2", value)

    @property
    def r3(self) -> typing.Optional[float]:
        """Get or set the Real values to define the stochastic distribution
        """ # nopep8
        return self._cards[1].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        """Set the r3 property."""
        self._cards[1].set_value("r3", value)

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

