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

"""Module providing the Mat280Sto class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT280STO_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("imod", float, 60, 10, 0.0),
    FieldSchema("ilaw", float, 70, 10, 0.0),
)

_MAT280STO_CARD1 = (
    FieldSchema("fmod", float, 0, 10, 0.0),
    FieldSchema("ft", float, 10, 10, None),
    FieldSchema("fc", float, 20, 10, None),
    FieldSchema("at", float, 30, 10, None),
    FieldSchema("bt", int, 40, 10, None),
    FieldSchema("ac", int, 50, 10, None),
    FieldSchema("bc", float, 60, 10, None),
    FieldSchema("ftscl", float, 70, 10, 1.0),
)

_MAT280STO_CARD2 = (
    FieldSchema("sfsti", float, 0, 10, None),
    FieldSchema("sfstr", float, 10, 10, None),
    FieldSchema("crin", float, 20, 10, 0.0),
    FieldSchema("ecrcl", float, 30, 10, None),
    FieldSchema("ncycr", float, 40, 10, None),
    FieldSchema("nipf", float, 50, 10, None),
)

_MAT280STO_CARD3 = (
    FieldSchema("epscr", float, 0, 10, None),
    FieldSchema("engcrt", float, 10, 10, None),
    FieldSchema("radcrt", float, 20, 10, None),
    FieldSchema("ratenl", float, 30, 10, None),
    FieldSchema("rfiltf", float, 40, 10, None),
    FieldSchema("fracen", float, 50, 10, None),
    FieldSchema("ctrack", int, 60, 10, 0),
    FieldSchema("grpft", float, 70, 10, None),
)

_MAT280STO_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat280Sto(KeywordBase):
    """DYNA MAT_280_STO keyword"""

    keyword = "MAT"
    subkeyword = "280_STO"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat280Sto class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT280STO_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT280STO_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT280STO_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT280STO_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat280Sto._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT280STO_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the the material density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def imod(self) -> float:
        """Get or set the Flag to choose degradation procedure, when critical stress is reached.
        EQ.0.0: Softening in NCYCR load steps. Define SFSTI, SFSTR, and NCYCR (default).
        EQ.1.0: Damage model for softening. Define ILAW, AT, BT, AC, and BC.
        EQ.2.0: Drucker - Prager
        EQ.10.0: Rankine with modified compressive failure
        EQ.11.0: Mohr - Coulomb with modified compressive failure
        EQ.12.0: Drucker - Prager with modified compressive failure
        """ # nopep8
        return self._cards[0].get_value("imod")

    @imod.setter
    def imod(self, value: float) -> None:
        """Set the imod property."""
        if value not in [0.0, 1.0, 2.0, 10.0, 11.0, 12.0, None]:
            raise Exception("""imod must be `None` or one of {0.0,1.0,2.0,10.0,11.0,12.0}.""")
        self._cards[0].set_value("imod", value)

    @property
    def ilaw(self) -> float:
        """Get or set the Flag to choose damage evolution law if IMOD=1.0, see Remarks.
        EQ.0.0: Same damage evolution for tensile and compressive failure (default).
        EQ.1.0: Different damage evolution for tensile failure and compressive failure.
        """ # nopep8
        return self._cards[0].get_value("ilaw")

    @ilaw.setter
    def ilaw(self, value: float) -> None:
        """Set the ilaw property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ilaw must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ilaw", value)

    @property
    def fmod(self) -> float:
        """Get or set the Flag to choose between failure criteria, see Remarks.
        EQ.0.0: Rankine maximum stress (default),
        EQ.1.0: Mohr-Coulomb,
        EQ.2.0: Drucker-Prager.
        """ # nopep8
        return self._cards[1].get_value("fmod")

    @fmod.setter
    def fmod(self, value: float) -> None:
        """Set the fmod property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""fmod must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[1].set_value("fmod", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Tensile strength, f_t.
        GT.0.0: constant value
        LT.0.0: Load curve ID = |FT|, which gives tensile strength as a function of effective strain rate (RFILTF is recommended). If used with FTSCL > 0, |FT| specifies a curve for tensile strength vs. strain rate, and FTSCL scales the strength values from that curve as long as the material is intact. If cracked, neighbors get non-scaled values from that curve. RATENL is set to zero in that case. Logarithmic interpolation between strain rates is assumed if the first abscissa value in the curve is negative; in this case, all the abscissa values are assumed to represent the natural logarithm of a strain rate.
        Note that a spatially - varying scale factor can be applied to FT through * INITIAL_STRESS_SHELL by setting history variable #13 or through the STOCHASTIC keyword option by using* DEFINE_STOCHASTIC_VARIATION.This scale factor scales FT regardless of RATENL.As a result, depending on the value of RATENL, both this scale factor and FTSCL may scale FT.
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        """Set the ft property."""
        self._cards[1].set_value("ft", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Compressive strength.
        """ # nopep8
        return self._cards[1].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[1].set_value("fc", value)

    @property
    def at(self) -> typing.Optional[float]:
        """Get or set the Tensile damage evolution parameter alpha_t. Can be interpreted as the residual load carrying capacity ratio for tensile failure ranging from 0 to 1..
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[1].set_value("at", value)

    @property
    def bt(self) -> typing.Optional[int]:
        """Get or set the Tensile damage evolution parameter beta_t. It controls the softening velocity for tensile failure.
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: int) -> None:
        """Set the bt property."""
        self._cards[1].set_value("bt", value)

    @property
    def ac(self) -> typing.Optional[int]:
        """Get or set the Compressive damage evolution parameter alpha_c. Can be interpreted as the residual load carrying capacity ratio for compressive failure ranging from 0 to 1.
        """ # nopep8
        return self._cards[1].get_value("ac")

    @ac.setter
    def ac(self, value: int) -> None:
        """Set the ac property."""
        self._cards[1].set_value("ac", value)

    @property
    def bc(self) -> typing.Optional[float]:
        """Get or set the Compressive damage evolution parameter beta_c. It controls the softening velocity for compressive failure.
        """ # nopep8
        return self._cards[1].get_value("bc")

    @bc.setter
    def bc(self, value: float) -> None:
        """Set the bc property."""
        self._cards[1].set_value("bc", value)

    @property
    def ftscl(self) -> float:
        """Get or set the Scale factor for the tensile strength (default = 1.0):
        FT _mod = FTSCLxFT
        If RATENL = 0.0 (see Card 4), then the tensile strength drops to its original value, FT, as soon as the first crack happens in the associated part.In this case, FTSCL > 1.0 can be helpful in modeling high - force peaks in impact events.
        If RATENL != 0.0, the tensile strength of an element is evaluated depending on the smoothed effective strain rate when a crack forms in a neighboring element(see Remark 7).
        """ # nopep8
        return self._cards[1].get_value("ftscl")

    @ftscl.setter
    def ftscl(self, value: float) -> None:
        """Set the ftscl property."""
        self._cards[1].set_value("ftscl", value)

    @property
    def sfsti(self) -> typing.Optional[float]:
        """Get or set the Scale factor for stiffness in case of failure, e.g. SFSTI = 0.1 means
        that stiffness is reduced to 10% of the stiffness at failure.
        """ # nopep8
        return self._cards[2].get_value("sfsti")

    @sfsti.setter
    def sfsti(self, value: float) -> None:
        """Set the sfsti property."""
        self._cards[2].set_value("sfsti", value)

    @property
    def sfstr(self) -> typing.Optional[float]:
        """Get or set the Scale factor for stress in case of failure, e.g. SFSTR = 0.1 means that
        stress is reduced to 10% of the stress at failure.
        """ # nopep8
        return self._cards[2].get_value("sfstr")

    @sfstr.setter
    def sfstr(self, value: float) -> None:
        """Set the sfstr property."""
        self._cards[2].set_value("sfstr", value)

    @property
    def crin(self) -> float:
        """Get or set the Flag for crack strain initialization
        EQ.0.0: initial crack strain is the strain at failure (default),
        EQ.1.0: initial crack strain is zero.
        """ # nopep8
        return self._cards[2].get_value("crin")

    @crin.setter
    def crin(self, value: float) -> None:
        """Set the crin property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""crin must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("crin", value)

    @property
    def ecrcl(self) -> typing.Optional[float]:
        """Get or set the Crack strain necessary to reactivate certain stress components after crack closure..
        """ # nopep8
        return self._cards[2].get_value("ecrcl")

    @ecrcl.setter
    def ecrcl(self, value: float) -> None:
        """Set the ecrcl property."""
        self._cards[2].set_value("ecrcl", value)

    @property
    def ncycr(self) -> typing.Optional[float]:
        """Get or set the Number of cycles in which the stress is reduced to SFSTR*failure stress.
        """ # nopep8
        return self._cards[2].get_value("ncycr")

    @ncycr.setter
    def ncycr(self, value: float) -> None:
        """Set the ncycr property."""
        self._cards[2].set_value("ncycr", value)

    @property
    def nipf(self) -> typing.Optional[float]:
        """Get or set the Number of failed through thickness integration points to fail all through thickness integration points.
        """ # nopep8
        return self._cards[2].get_value("nipf")

    @nipf.setter
    def nipf(self, value: float) -> None:
        """Set the nipf property."""
        self._cards[2].set_value("nipf", value)

    @property
    def epscr(self) -> typing.Optional[float]:
        """Get or set the Effective critical strain to trigger element deletion. This can be useful to get rid of highly distorted elements.
        """ # nopep8
        return self._cards[3].get_value("epscr")

    @epscr.setter
    def epscr(self, value: float) -> None:
        """Set the epscr property."""
        self._cards[3].set_value("epscr", value)

    @property
    def engcrt(self) -> typing.Optional[float]:
        """Get or set the Critical energy for nonlocal failure criterion; see Remark 6.
        GT.0.0: Constant value.
        LT.0.0 : | ENGCRT | refers to a * DEFINE_FUNCTION giving the critical energy as a function of the minimum distance of the center of impact to the edge of the windshield. Thus the critical energy value can now depend on this distance, for example, larger in the middle area and smaller toward the edge.
        """ # nopep8
        return self._cards[3].get_value("engcrt")

    @engcrt.setter
    def engcrt(self, value: float) -> None:
        """Set the engcrt property."""
        self._cards[3].set_value("engcrt", value)

    @property
    def radcrt(self) -> typing.Optional[float]:
        """Get or set the Critical radius for nonlocal failure criterion; see Remark 6.
        """ # nopep8
        return self._cards[3].get_value("radcrt")

    @radcrt.setter
    def radcrt(self, value: float) -> None:
        """Set the radcrt property."""
        self._cards[3].set_value("radcrt", value)

    @property
    def ratenl(self) -> typing.Optional[float]:
        """Get or set the Quasi-static strain rate threshold variable which activates a nonlocal, strain rate dependent tensile strength adaption; see Remark 7.
        """ # nopep8
        return self._cards[3].get_value("ratenl")

    @ratenl.setter
    def ratenl(self, value: float) -> None:
        """Set the ratenl property."""
        self._cards[3].set_value("ratenl", value)

    @property
    def rfiltf(self) -> typing.Optional[float]:
        """Get or set the Smoothing factor on the effective strain rate for the evaluation of the current tensile strength if RATENL > 0.0; see Remark 7.
        """ # nopep8
        return self._cards[3].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        """Set the rfiltf property."""
        self._cards[3].set_value("rfiltf", value)

    @property
    def fracen(self) -> typing.Optional[float]:
        """Get or set the Fracture energy (units of stress * length). An alternative orthotropic damage model with linear softening is invoked with this option. Values smaller than 0.5*FT*FT/E*l_e (element size) lead to immediate failure. This is the area under the elastic stress-displacement line until FT is reached. Only larger values result in actual residual energy after crack initiation. Variables SFSTI, SFSTR, and NCYCR are ignored with this option.You can specify a spatially-varying scale factor for FRACEN by setting history variable #14 with *INITIAL_STRESS_SHELL.
        """ # nopep8
        return self._cards[3].get_value("fracen")

    @fracen.setter
    def fracen(self, value: float) -> None:
        """Set the fracen property."""
        self._cards[3].set_value("fracen", value)

    @property
    def ctrack(self) -> int:
        """Get or set the Flag for optional crack tracking algorithm (see Remark 10):
        EQ.0.0: Inactive
        EQ.1.0: Active
        """ # nopep8
        return self._cards[3].get_value("ctrack")

    @ctrack.setter
    def ctrack(self, value: int) -> None:
        """Set the ctrack property."""
        if value not in [0, 1, None]:
            raise Exception("""ctrack must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ctrack", value)

    @property
    def grpft(self) -> typing.Optional[float]:
        """Get or set the Optional group number for strength reduction. If several parts use *MAT_280 with potentially different material parameters, giving them the same value of GRPT causes them to experience tensile strength reduction by FTSCL at the same time (RATENL = 0).
        """ # nopep8
        return self._cards[3].get_value("grpft")

    @grpft.setter
    def grpft(self, value: float) -> None:
        """Set the grpft property."""
        self._cards[3].set_value("grpft", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

