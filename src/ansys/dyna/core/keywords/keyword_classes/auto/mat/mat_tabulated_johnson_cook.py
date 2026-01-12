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

"""Module providing the MatTabulatedJohnsonCook class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATTABULATEDJOHNSONCOOK_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("cp", float, 40, 10, None),
    FieldSchema("tr", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, 1.0),
    FieldSchema("numint", float, 70, 10, 1.0),
)

_MATTABULATEDJOHNSONCOOK_CARD1 = (
    FieldSchema("tabk1", int, 0, 10, 0),
    FieldSchema("tabkt", int, 10, 10, 0),
    FieldSchema("lcf", int, 20, 10, 0),
    FieldSchema("lcg", int, 30, 10, 0),
    FieldSchema("lch", int, 40, 10, 0),
    FieldSchema("lci", int, 50, 10, 0),
)

_MATTABULATEDJOHNSONCOOK_CARD2 = (
    FieldSchema("failopt", int, 0, 10, 0),
    FieldSchema("numavg", int, 10, 10, 1),
    FieldSchema("ncyfail", int, 20, 10, 1),
    FieldSchema("erode", int, 30, 10, 0),
    FieldSchema("lcps", int, 40, 10, None),
)

_MATTABULATEDJOHNSONCOOK_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatTabulatedJohnsonCook(KeywordBase):
    """DYNA MAT_TABULATED_JOHNSON_COOK keyword"""

    keyword = "MAT"
    subkeyword = "TABULATED_JOHNSON_COOK"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatTabulatedJohnsonCook class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATTABULATEDJOHNSONCOOK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTABULATEDJOHNSONCOOK_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTABULATEDJOHNSONCOOK_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatTabulatedJohnsonCook.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATTABULATEDJOHNSONCOOK_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        GT.0.0:	constant value is used
        LT.0.0:	-E gives curve ID for temperature dependence
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
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[0].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        """Set the tr property."""
        self._cards[0].set_value("tr", value)

    @property
    def beta(self) -> float:
        """Get or set the Fraction of plastic work converted into heat(supersedes FWORK in *CONTROL_THERMAL_SOLVER if a coupled thermal/structural analysis):
        GT.0.0:	constant value is used
        LT.0.0:	-BETA gives either a curve ID for strain rate dependence, or a table ID for strain rate and temperature dependence,
        or a 3-dimensional table ID for temperature (TABLE_3D), strain rate (TABLE) and plastic strain (CURVE) dependence.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of integration points which must fail before the element is deleted. Available for shells and solids.
        LT.0.0: |NUMINT| is percentage of integration points/layers which must fail before element fails. For fully integrated shells, a methodology is used where a layer fails if one integrationpoint fails and then the given percentage of layers must fail before the element fails.
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        """Set the numint property."""
        self._cards[0].set_value("numint", value)

    @property
    def tabk1(self) -> int:
        """Get or set the Load curve ID or Table ID. The load curve ID defines effective stress as a function of effective plastic strain. The table ID defines for each plastic strain rate value a load curve ID giving the (isothermal) effective stress versus effective plastic strain for that rate.
        """ # nopep8
        return self._cards[1].get_value("tabk1")

    @tabk1.setter
    def tabk1(self, value: int) -> None:
        """Set the tabk1 property."""
        self._cards[1].set_value("tabk1", value)

    @property
    def tabkt(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) effective stress versus effective plastic strain for that temperature.
        """ # nopep8
        return self._cards[1].get_value("tabkt")

    @tabkt.setter
    def tabkt(self, value: int) -> None:
        """Set the tabkt property."""
        self._cards[1].set_value("tabkt", value)

    @property
    def lcf(self) -> int:
        """Get or set the Load curve ID or Table ID. The load curve ID defines plastic failure strain as a function of triaxiality.
        The table ID defines for each Lode angle a load curve ID giving the plastic failure strain versus triaxiality for that Lode angle.
        See Remarks for a description of the combination of LCF, LCG, LCH, and LCI.
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        """Set the lcf property."""
        self._cards[1].set_value("lcf", value)

    @property
    def lcg(self) -> int:
        """Get or set the Load curve ID defining plastic failure strain as a function of strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcg")

    @lcg.setter
    def lcg(self, value: int) -> None:
        """Set the lcg property."""
        self._cards[1].set_value("lcg", value)

    @property
    def lch(self) -> int:
        """Get or set the Load curve ID defining plastic failure strain as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        """Set the lch property."""
        self._cards[1].set_value("lch", value)

    @property
    def lci(self) -> int:
        """Get or set the Load curve ID, Table ID, or Table_3D ID. The load curve ID defines plastic failure strain (or scale factor – see Remarks) as a function of element size. The table ID defines for each triaxiality a load curve ID giving the plastic failure strain versus element size for that triaxiality. If a three dimensional table ID is referred, plastic failure strain can be a function of Lode parameter (TABLE_3D), triaxiality (TABLE), and element size (CURVE).
        """ # nopep8
        return self._cards[1].get_value("lci")

    @lci.setter
    def lci(self, value: int) -> None:
        """Set the lci property."""
        self._cards[1].set_value("lci", value)

    @property
    def failopt(self) -> int:
        """Get or set the Flag for additional failure criterion F_2,
        EQ.0.0:	off (default)
        EQ.1.0:	on.
        """ # nopep8
        return self._cards[2].get_value("failopt")

    @failopt.setter
    def failopt(self, value: int) -> None:
        """Set the failopt property."""
        if value not in [0, 1, None]:
            raise Exception("""failopt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("failopt", value)

    @property
    def numavg(self) -> int:
        """Get or set the Number of time steps for running average of plastic failure strain in the additional failure criterion.Default is 1 (no averaging).
        """ # nopep8
        return self._cards[2].get_value("numavg")

    @numavg.setter
    def numavg(self, value: int) -> None:
        """Set the numavg property."""
        self._cards[2].set_value("numavg", value)

    @property
    def ncyfail(self) -> int:
        """Get or set the Number of time steps that the additional failure criterion must be met before element deletion.Default is 1.
        """ # nopep8
        return self._cards[2].get_value("ncyfail")

    @ncyfail.setter
    def ncyfail(self, value: int) -> None:
        """Set the ncyfail property."""
        self._cards[2].set_value("ncyfail", value)

    @property
    def erode(self) -> int:
        """Get or set the Erosion flag (only for solid elements):
        EQ.0.0:	default, element erosion is allowed.
        EQ.1.0:	element does not erode; deviatoric stresses set to zero when element fails..
        """ # nopep8
        return self._cards[2].get_value("erode")

    @erode.setter
    def erode(self, value: int) -> None:
        """Set the erode property."""
        if value not in [0, 1, None]:
            raise Exception("""erode must be `None` or one of {0,1}.""")
        self._cards[2].set_value("erode", value)

    @property
    def lcps(self) -> typing.Optional[int]:
        """Get or set the Table ID with first principal stress limit as a function of plastic strain (curves) and plastic strain rate (table). This option is for post-processing purposes only and gives an indication of areas in the structure where failure is likely to occur. History variable #17 shows a value of 1.0 for integration points that exceeded the limit, else a value of 0.0.
        """ # nopep8
        return self._cards[2].get_value("lcps")

    @lcps.setter
    def lcps(self, value: int) -> None:
        """Set the lcps property."""
        self._cards[2].set_value("lcps", value)

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

