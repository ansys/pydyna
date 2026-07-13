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

"""Module providing the Mat307 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT307_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("gasc", float, 20, 10, None),
    FieldSchema("idoc", float, 30, 10, None),
    FieldSchema("incr", float, 40, 10, None),
    FieldSchema("qcure", float, 50, 10, None),
    FieldSchema("tzero", float, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_MAT307_CARD1 = (
    FieldSchema("ckopt", int, 0, 10, 0),
    FieldSchema("ck1", float, 10, 10, None),
    FieldSchema("ck2", float, 20, 10, None),
    FieldSchema("ck3", float, 30, 10, None),
    FieldSchema("ck4", float, 40, 10, None),
    FieldSchema("ck5", float, 50, 10, None),
    FieldSchema("ck6", float, 60, 10, None),
    FieldSchema("ck7", float, 70, 10, None),
)

_MAT307_CARD2 = (
    FieldSchema("ck8", float, 0, 10, None),
    FieldSchema("ck9", float, 10, 10, None),
    FieldSchema("ck10", float, 20, 10, None),
    FieldSchema("ck11", float, 30, 10, None),
    FieldSchema("ck12", float, 40, 10, None),
    FieldSchema("ck13", float, 50, 10, None),
    FieldSchema("ck14", float, 60, 10, None),
    FieldSchema("ck15", float, 70, 10, None),
)

_MAT307_CARD3 = (
    FieldSchema("ck16", float, 0, 10, None),
    FieldSchema("ck17", float, 10, 10, None),
    FieldSchema("ck18", float, 20, 10, None),
    FieldSchema("ck19", float, 30, 10, None),
    FieldSchema("ck20", float, 40, 10, None),
    FieldSchema("ck21", float, 50, 10, None),
    FieldSchema("ck22", float, 60, 10, None),
    FieldSchema("ck23", float, 70, 10, None),
)

_MAT307_CARD4 = (
    FieldSchema("cdopt", int, 0, 10, 1),
    FieldSchema("cd1", float, 10, 10, None),
    FieldSchema("cd2", float, 20, 10, None),
    FieldSchema("cd3", float, 30, 10, None),
)

_MAT307_CARD5 = (
    FieldSchema("ctgopt", int, 0, 10, 1),
    FieldSchema("ctg1", float, 10, 10, None),
    FieldSchema("ctg2", float, 20, 10, None),
    FieldSchema("ctg3", float, 30, 10, None),
)

_MAT307_CARD6 = (
    FieldSchema("ceopt", int, 0, 10, 0),
    FieldSchema("ce1", float, 10, 10, None),
    FieldSchema("ce2", float, 20, 10, None),
    FieldSchema("ce3", float, 30, 10, None),
    FieldSchema("ce4", float, 40, 10, None),
)

_MAT307_CARD7 = (
    FieldSchema("teopt", int, 0, 10, 0),
    FieldSchema("te1", float, 10, 10, None),
    FieldSchema("te2", float, 20, 10, None),
)

_MAT307_CARD8 = (
    FieldSchema("thopt", int, 0, 10, 0),
    FieldSchema("th1", float, 10, 10, None),
    FieldSchema("th2", float, 20, 10, None),
    FieldSchema("th3", float, 30, 10, None),
    FieldSchema("th4", float, 40, 10, None),
    FieldSchema("th5", float, 50, 10, None),
    FieldSchema("th6", float, 60, 10, None),
    FieldSchema("th7", float, 70, 10, None),
)

_MAT307_CARD9 = (
    FieldSchema("tvopt", int, 0, 10, 0),
    FieldSchema("tv1", float, 10, 10, None),
    FieldSchema("tv2", float, 20, 10, None),
)

_MAT307_CARD10 = (
    FieldSchema("phopt", int, 0, 10, 0),
    FieldSchema("ph1", float, 10, 10, None),
    FieldSchema("ph2", float, 20, 10, None),
    FieldSchema("ph3", float, 30, 10, None),
    FieldSchema("ph4", float, 40, 10, None),
    FieldSchema("ph5", float, 50, 10, None),
    FieldSchema("ph6", float, 60, 10, None),
)

_MAT307_CARD11 = (
    FieldSchema("pvopt", int, 0, 10, 0),
    FieldSchema("pv1", float, 10, 10, None),
    FieldSchema("pv2", float, 20, 10, None),
    FieldSchema("pv3", float, 30, 10, None),
    FieldSchema("pv4", float, 40, 10, None),
)

_MAT307_CARD12 = (
    FieldSchema("pl1opt", int, 0, 10, 0),
    FieldSchema("pl11", float, 10, 10, None),
    FieldSchema("pl12", float, 20, 10, None),
    FieldSchema("pl13", float, 30, 10, None),
    FieldSchema("pl14", float, 40, 10, None),
    FieldSchema("pl15", float, 50, 10, None),
)

_MAT307_CARD13 = (
    FieldSchema("pl2opt", int, 0, 10, 0),
    FieldSchema("pl21", float, 10, 10, None),
    FieldSchema("pl22", float, 20, 10, None),
    FieldSchema("pl23", float, 30, 10, None),
    FieldSchema("pl24", float, 40, 10, None),
    FieldSchema("pl25", float, 50, 10, None),
    FieldSchema("pl26", float, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_MAT307_CARD14 = (
    FieldSchema("daopt", int, 0, 10, 0),
    FieldSchema("daevo", float, 10, 10, 0.0),
    FieldSchema("datria", float, 20, 10, 0.0),
    FieldSchema("da1", float, 30, 10, None),
    FieldSchema("da2", float, 40, 10, None),
    FieldSchema("da3", float, 50, 10, None),
    FieldSchema("da4", float, 60, 10, None),
    FieldSchema("da5", float, 70, 10, None),
)

_MAT307_CARD15 = (
    FieldSchema("da6", float, 0, 10, None),
    FieldSchema("da7", float, 10, 10, None),
    FieldSchema("da8", float, 20, 10, None),
    FieldSchema("da9", float, 30, 10, None),
    FieldSchema("da10", float, 40, 10, None),
    FieldSchema("da11", float, 50, 10, None),
    FieldSchema("pda1", float, 60, 10, None),
    FieldSchema("pgel", float, 70, 10, None),
)

_MAT307_CARD16 = (
    FieldSchema("gi", float, 0, 10, None),
    FieldSchema("betagi", float, 10, 10, None),
    FieldSchema("ki", float, 20, 10, None),
    FieldSchema("betaki", float, 30, 10, None),
)

_MAT307_CARD17 = (
    FieldSchema("viopt", float, 0, 10, None),
    FieldSchema("nue", float, 10, 10, None),
)

_MAT307_CARD18 = (
    FieldSchema("ei", float, 0, 10, None),
    FieldSchema("betai", float, 10, 10, None),
    FieldSchema("ei", float, 20, 10, None),
    FieldSchema("betai", float, 30, 10, None),
)

_MAT307_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat307(KeywordBase):
    """DYNA MAT_307 keyword"""

    keyword = "MAT"
    subkeyword = "307"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat307 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD7,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD8,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD9,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD10,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD11,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD12,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD13,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD14,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD15,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD16,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD17,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT307_CARD18,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat307._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT307_OPTION0_CARD0,
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def gasc(self) -> typing.Optional[float]:
        """Get or set the Gas constant R
        """ # nopep8
        return self._cards[0].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        """Set the gasc property."""
        self._cards[0].set_value("gasc", value)

    @property
    def idoc(self) -> typing.Optional[float]:
        """Get or set the Definition of the initial degree of cure p_I:
        EQ.0:Uncured initial state,P_i = 0;
        GT.0:Uniformly distributed initial state of cure with p_i = IDOC
        LT.0:Use *INITIAL_STRESS_OPTION cards to define a potentially on -uniform distribution for the initial state of cure(history variable 2).
        """ # nopep8
        return self._cards[0].get_value("idoc")

    @idoc.setter
    def idoc(self, value: float) -> None:
        """Set the idoc property."""
        self._cards[0].set_value("idoc", value)

    @property
    def incr(self) -> typing.Optional[float]:
        """Get or set the Switch between incremental and total stress formulation:
        EQ.1: Incremental form(default, recommended).
        EQ.2: Total form.
        """ # nopep8
        return self._cards[0].get_value("incr")

    @incr.setter
    def incr(self, value: float) -> None:
        """Set the incr property."""
        self._cards[0].set_value("incr", value)

    @property
    def qcure(self) -> typing.Optional[float]:
        """Get or set the Heat generation factor, relating the heat generated in one time step with the increment of the degree of cure in that step
        """ # nopep8
        return self._cards[0].get_value("qcure")

    @qcure.setter
    def qcure(self, value: float) -> None:
        """Set the qcure property."""
        self._cards[0].set_value("qcure", value)

    @property
    def tzero(self) -> typing.Optional[float]:
        """Get or set the Temperature value with respect to the temperature scale used in the input deck for a temperature of 0K..
        """ # nopep8
        return self._cards[0].get_value("tzero")

    @tzero.setter
    def tzero(self, value: float) -> None:
        """Set the tzero property."""
        self._cards[0].set_value("tzero", value)

    @property
    def ckopt(self) -> int:
        """Get or set the Curing kinetics option. See Remark 1 or details.
        EQ.0: No curing kinetics
        EQ.1: Generalized model, with load curves for pre - exponential factors
        EQ.2: Extended Kamal model
        EQ.-2: Extended Kamal model with diffusion control
        EQ.3: Kamal model
        EQ.-3: Kamal model with diffusion control
        EQ.4: Three species reaction kinetics model
        EQ.-4: Three-species reaction kinetics model with diffusion control
        EQ.5: Five species reaction kinetics model
        EQ.6: Five species reaction kinetics model
        EQ.7: Three species reaction kinetics model
        EQ.8: Three species reaction kinetics model
        EQ.9: Four species reaction kinetics model
        EQ.10: Five species reaction kinetics model
        EQ.11. Model-free kinetics
        """ # nopep8
        return self._cards[1].get_value("ckopt")

    @ckopt.setter
    def ckopt(self, value: int) -> None:
        """Set the ckopt property."""
        if value not in [0, 1, 2, -2, 3, -3, 4, -4, 5, 6, 7, 8, 9, 10, 11, None]:
            raise Exception("""ckopt must be `None` or one of {0,1,2,-2,3,-3,4,-4,5,6,7,8,9,10,11}.""")
        self._cards[1].set_value("ckopt", value)

    @property
    def ck1(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck1")

    @ck1.setter
    def ck1(self, value: float) -> None:
        """Set the ck1 property."""
        self._cards[1].set_value("ck1", value)

    @property
    def ck2(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck2")

    @ck2.setter
    def ck2(self, value: float) -> None:
        """Set the ck2 property."""
        self._cards[1].set_value("ck2", value)

    @property
    def ck3(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck3")

    @ck3.setter
    def ck3(self, value: float) -> None:
        """Set the ck3 property."""
        self._cards[1].set_value("ck3", value)

    @property
    def ck4(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck4")

    @ck4.setter
    def ck4(self, value: float) -> None:
        """Set the ck4 property."""
        self._cards[1].set_value("ck4", value)

    @property
    def ck5(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck5")

    @ck5.setter
    def ck5(self, value: float) -> None:
        """Set the ck5 property."""
        self._cards[1].set_value("ck5", value)

    @property
    def ck6(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck6")

    @ck6.setter
    def ck6(self, value: float) -> None:
        """Set the ck6 property."""
        self._cards[1].set_value("ck6", value)

    @property
    def ck7(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[1].get_value("ck7")

    @ck7.setter
    def ck7(self, value: float) -> None:
        """Set the ck7 property."""
        self._cards[1].set_value("ck7", value)

    @property
    def ck8(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck8")

    @ck8.setter
    def ck8(self, value: float) -> None:
        """Set the ck8 property."""
        self._cards[2].set_value("ck8", value)

    @property
    def ck9(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck9")

    @ck9.setter
    def ck9(self, value: float) -> None:
        """Set the ck9 property."""
        self._cards[2].set_value("ck9", value)

    @property
    def ck10(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck10")

    @ck10.setter
    def ck10(self, value: float) -> None:
        """Set the ck10 property."""
        self._cards[2].set_value("ck10", value)

    @property
    def ck11(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck11")

    @ck11.setter
    def ck11(self, value: float) -> None:
        """Set the ck11 property."""
        self._cards[2].set_value("ck11", value)

    @property
    def ck12(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck12")

    @ck12.setter
    def ck12(self, value: float) -> None:
        """Set the ck12 property."""
        self._cards[2].set_value("ck12", value)

    @property
    def ck13(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck13")

    @ck13.setter
    def ck13(self, value: float) -> None:
        """Set the ck13 property."""
        self._cards[2].set_value("ck13", value)

    @property
    def ck14(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck14")

    @ck14.setter
    def ck14(self, value: float) -> None:
        """Set the ck14 property."""
        self._cards[2].set_value("ck14", value)

    @property
    def ck15(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[2].get_value("ck15")

    @ck15.setter
    def ck15(self, value: float) -> None:
        """Set the ck15 property."""
        self._cards[2].set_value("ck15", value)

    @property
    def ck16(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck16")

    @ck16.setter
    def ck16(self, value: float) -> None:
        """Set the ck16 property."""
        self._cards[3].set_value("ck16", value)

    @property
    def ck17(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck17")

    @ck17.setter
    def ck17(self, value: float) -> None:
        """Set the ck17 property."""
        self._cards[3].set_value("ck17", value)

    @property
    def ck18(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck18")

    @ck18.setter
    def ck18(self, value: float) -> None:
        """Set the ck18 property."""
        self._cards[3].set_value("ck18", value)

    @property
    def ck19(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck19")

    @ck19.setter
    def ck19(self, value: float) -> None:
        """Set the ck19 property."""
        self._cards[3].set_value("ck19", value)

    @property
    def ck20(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck20")

    @ck20.setter
    def ck20(self, value: float) -> None:
        """Set the ck20 property."""
        self._cards[3].set_value("ck20", value)

    @property
    def ck21(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck21")

    @ck21.setter
    def ck21(self, value: float) -> None:
        """Set the ck21 property."""
        self._cards[3].set_value("ck21", value)

    @property
    def ck22(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck22")

    @ck22.setter
    def ck22(self, value: float) -> None:
        """Set the ck22 property."""
        self._cards[3].set_value("ck22", value)

    @property
    def ck23(self) -> typing.Optional[float]:
        """Get or set the ith kinetic parameter. The meaning of the parameter depends on the choice of CKOPT. For details, see Remark1.
        """ # nopep8
        return self._cards[3].get_value("ck23")

    @ck23.setter
    def ck23(self, value: float) -> None:
        """Set the ck23 property."""
        self._cards[3].set_value("ck23", value)

    @property
    def cdopt(self) -> int:
        """Get or set the Diffusion control mechanism option (see Remark 16):
        EQ.1: Williams-Landel-Ferry (WLF) type
        EQ.2: Arrhenius shift-like mechanism
        EQ.3: Combined Williams-Landel-Ferry (WLF) and Arrhenius shift-like mechanism
        """ # nopep8
        return self._cards[4].get_value("cdopt")

    @cdopt.setter
    def cdopt(self, value: int) -> None:
        """Set the cdopt property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""cdopt must be `None` or one of {1,2,3}.""")
        self._cards[4].set_value("cdopt", value)

    @property
    def cd1(self) -> typing.Optional[float]:
        """Get or set the ith diffusion control model parameter. The meaning of the parameter depends on the choice of CDOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[4].get_value("cd1")

    @cd1.setter
    def cd1(self, value: float) -> None:
        """Set the cd1 property."""
        self._cards[4].set_value("cd1", value)

    @property
    def cd2(self) -> typing.Optional[float]:
        """Get or set the ith diffusion control model parameter. The meaning of the parameter depends on the choice of CDOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[4].get_value("cd2")

    @cd2.setter
    def cd2(self, value: float) -> None:
        """Set the cd2 property."""
        self._cards[4].set_value("cd2", value)

    @property
    def cd3(self) -> typing.Optional[float]:
        """Get or set the ith diffusion control model parameter. The meaning of the parameter depends on the choice of CDOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[4].get_value("cd3")

    @cd3.setter
    def cd3(self, value: float) -> None:
        """Set the cd3 property."""
        self._cards[4].set_value("cd3", value)

    @property
    def ctgopt(self) -> int:
        """Get or set the Glass transition temperature for diffusion control (see Remark 16):
        EQ.1: DiBenedetto equation
        EQ.2: Hesekamp equation
        """ # nopep8
        return self._cards[5].get_value("ctgopt")

    @ctgopt.setter
    def ctgopt(self, value: int) -> None:
        """Set the ctgopt property."""
        if value not in [1, 2, None]:
            raise Exception("""ctgopt must be `None` or one of {1,2}.""")
        self._cards[5].set_value("ctgopt", value)

    @property
    def ctg1(self) -> typing.Optional[float]:
        """Get or set the ith parameter for calculating glass transition temperature. The meaning of the parameter depends on the choice of CTOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[5].get_value("ctg1")

    @ctg1.setter
    def ctg1(self, value: float) -> None:
        """Set the ctg1 property."""
        self._cards[5].set_value("ctg1", value)

    @property
    def ctg2(self) -> typing.Optional[float]:
        """Get or set the ith parameter for calculating glass transition temperature. The meaning of the parameter depends on the choice of CTOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[5].get_value("ctg2")

    @ctg2.setter
    def ctg2(self, value: float) -> None:
        """Set the ctg2 property."""
        self._cards[5].set_value("ctg2", value)

    @property
    def ctg3(self) -> typing.Optional[float]:
        """Get or set the ith parameter for calculating glass transition temperature. The meaning of the parameter depends on the choice of CTOPT. For details, see Remark 16.
        """ # nopep8
        return self._cards[5].get_value("ctg3")

    @ctg3.setter
    def ctg3(self, value: float) -> None:
        """Set the ctg3 property."""
        self._cards[5].set_value("ctg3", value)

    @property
    def ceopt(self) -> int:
        """Get or set the Chemical expansion option. See Remark 2 for details.
        EQ.0: No chemical expansion
        EQ.1: Differential form with load curve input
        EQ.2: Secant form with load curve input
        EQ.3: Secant form with polynomial expression
        """ # nopep8
        return self._cards[6].get_value("ceopt")

    @ceopt.setter
    def ceopt(self, value: int) -> None:
        """Set the ceopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ceopt must be `None` or one of {0,1,2,3}.""")
        self._cards[6].set_value("ceopt", value)

    @property
    def ce1(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of CEOPT. For details, see Remark 2
        """ # nopep8
        return self._cards[6].get_value("ce1")

    @ce1.setter
    def ce1(self, value: float) -> None:
        """Set the ce1 property."""
        self._cards[6].set_value("ce1", value)

    @property
    def ce2(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of CEOPT. For details, see Remark 2
        """ # nopep8
        return self._cards[6].get_value("ce2")

    @ce2.setter
    def ce2(self, value: float) -> None:
        """Set the ce2 property."""
        self._cards[6].set_value("ce2", value)

    @property
    def ce3(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of CEOPT. For details, see Remark 2
        """ # nopep8
        return self._cards[6].get_value("ce3")

    @ce3.setter
    def ce3(self, value: float) -> None:
        """Set the ce3 property."""
        self._cards[6].set_value("ce3", value)

    @property
    def ce4(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of CEOPT. For details, see Remark 2
        """ # nopep8
        return self._cards[6].get_value("ce4")

    @ce4.setter
    def ce4(self, value: float) -> None:
        """Set the ce4 property."""
        self._cards[6].set_value("ce4", value)

    @property
    def teopt(self) -> int:
        """Get or set the Thermal expansion option
        EQ.0: No thermal expansion
        EQ.1: Differential form with load curve input
        EQ.2: Secant form with load curve input
        For details on the different options, see Remark 3
        """ # nopep8
        return self._cards[7].get_value("teopt")

    @teopt.setter
    def teopt(self, value: int) -> None:
        """Set the teopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""teopt must be `None` or one of {0,1,2}.""")
        self._cards[7].set_value("teopt", value)

    @property
    def te1(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of TEOPT. For details, see Remark 3
        """ # nopep8
        return self._cards[7].get_value("te1")

    @te1.setter
    def te1(self, value: float) -> None:
        """Set the te1 property."""
        self._cards[7].set_value("te1", value)

    @property
    def te2(self) -> typing.Optional[float]:
        """Get or set the ith expansion parameter. The meaning of the parameter depends on the choice of TEOPT. For details, see Remark 3
        """ # nopep8
        return self._cards[7].get_value("te2")

    @te2.setter
    def te2(self, value: float) -> None:
        """Set the te2 property."""
        self._cards[7].set_value("te2", value)

    @property
    def thopt(self) -> int:
        """Get or set the Option for horizontal temperature shift of the viscoelastic master curve as given by the Prony series expansion. See Remarks 4 and 5 for details.
        EQ.0: No temperature shift
        EQ.1: Williams - Landel - Ferry(WLF) shift function
        EQ.2: Arrhenius shift function
        EQ.3: Combined WLF(above glass transition temperature T_g) and Arrhenius(below T_g) shift function
        EQ.4: As 3, but with glass transition temperature T_g(p) as function of degree of cure p
        EQ.5: Direct input of shift factors as function of temperature
        """ # nopep8
        return self._cards[8].get_value("thopt")

    @thopt.setter
    def thopt(self, value: int) -> None:
        """Set the thopt property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""thopt must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[8].set_value("thopt", value)

    @property
    def th1(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th1")

    @th1.setter
    def th1(self, value: float) -> None:
        """Set the th1 property."""
        self._cards[8].set_value("th1", value)

    @property
    def th2(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th2")

    @th2.setter
    def th2(self, value: float) -> None:
        """Set the th2 property."""
        self._cards[8].set_value("th2", value)

    @property
    def th3(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th3")

    @th3.setter
    def th3(self, value: float) -> None:
        """Set the th3 property."""
        self._cards[8].set_value("th3", value)

    @property
    def th4(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th4")

    @th4.setter
    def th4(self, value: float) -> None:
        """Set the th4 property."""
        self._cards[8].set_value("th4", value)

    @property
    def th5(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th5")

    @th5.setter
    def th5(self, value: float) -> None:
        """Set the th5 property."""
        self._cards[8].set_value("th5", value)

    @property
    def th6(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th6")

    @th6.setter
    def th6(self, value: float) -> None:
        """Set the th6 property."""
        self._cards[8].set_value("th6", value)

    @property
    def th7(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of THOPT. For details, see Remark 5
        """ # nopep8
        return self._cards[8].get_value("th7")

    @th7.setter
    def th7(self, value: float) -> None:
        """Set the th7 property."""
        self._cards[8].set_value("th7", value)

    @property
    def tvopt(self) -> int:
        """Get or set the Option for vertical temperature shift of the master viscoelastic curve as given by the Prony series expansion. See Remark 4 and 6 for details.
        EQ.0: No temperature shift
        EQ.1: Shifting of the complete master curves G(t)
        EQ.2: Shifting of all terms G_i and K_i, but not G_infinity and K_infinity
        """ # nopep8
        return self._cards[9].get_value("tvopt")

    @tvopt.setter
    def tvopt(self, value: int) -> None:
        """Set the tvopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""tvopt must be `None` or one of {0,1,2}.""")
        self._cards[9].set_value("tvopt", value)

    @property
    def tv1(self) -> typing.Optional[float]:
        """Get or set the The meaning of the shifting parameter depends on the choice of TVOPT. For details, see Remark 6
        """ # nopep8
        return self._cards[9].get_value("tv1")

    @tv1.setter
    def tv1(self, value: float) -> None:
        """Set the tv1 property."""
        self._cards[9].set_value("tv1", value)

    @property
    def tv2(self) -> typing.Optional[float]:
        """Get or set the The meaning of the shifting parameter depends on the choice of TVOPT. For details, see Remark 6
        """ # nopep8
        return self._cards[9].get_value("tv2")

    @tv2.setter
    def tv2(self, value: float) -> None:
        """Set the tv2 property."""
        self._cards[9].set_value("tv2", value)

    @property
    def phopt(self) -> int:
        """Get or set the Option for horizontal shift due to curing effects of the viscoelastic master curve as given by the Prony series expansion. See Remark 4 and 7 for details.
        EQ.0: No shift
        EQ.1: Eom model
        EQ.2: Direct input of shift factors as function of degree of cure
        """ # nopep8
        return self._cards[10].get_value("phopt")

    @phopt.setter
    def phopt(self, value: int) -> None:
        """Set the phopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""phopt must be `None` or one of {0,1,2}.""")
        self._cards[10].set_value("phopt", value)

    @property
    def ph1(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph1")

    @ph1.setter
    def ph1(self, value: float) -> None:
        """Set the ph1 property."""
        self._cards[10].set_value("ph1", value)

    @property
    def ph2(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph2")

    @ph2.setter
    def ph2(self, value: float) -> None:
        """Set the ph2 property."""
        self._cards[10].set_value("ph2", value)

    @property
    def ph3(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph3")

    @ph3.setter
    def ph3(self, value: float) -> None:
        """Set the ph3 property."""
        self._cards[10].set_value("ph3", value)

    @property
    def ph4(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph4")

    @ph4.setter
    def ph4(self, value: float) -> None:
        """Set the ph4 property."""
        self._cards[10].set_value("ph4", value)

    @property
    def ph5(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph5")

    @ph5.setter
    def ph5(self, value: float) -> None:
        """Set the ph5 property."""
        self._cards[10].set_value("ph5", value)

    @property
    def ph6(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PHOPT. For details, see Remark 7
        """ # nopep8
        return self._cards[10].get_value("ph6")

    @ph6.setter
    def ph6(self, value: float) -> None:
        """Set the ph6 property."""
        self._cards[10].set_value("ph6", value)

    @property
    def pvopt(self) -> int:
        """Get or set the Option for vertical shift of the master viscoelastic curve due to curing effects as given by the Prony series expansion. See Remark 4 and 8 for details.
        EQ.0: No shift
        EQ.1: Input of instantaneous moduli G_0 and K_0 as function of degree of cure p.Assumption of constant ratios(G_i(p))/(G_0(p)) and (K_i(p))/(K_0(p)).
        EQ.2: Input of long - term moduli G_(p) and K_(p) as functions of degree of cure p and of scaling functions for other moduli G_iand K_i.
        """ # nopep8
        return self._cards[11].get_value("pvopt")

    @pvopt.setter
    def pvopt(self, value: int) -> None:
        """Set the pvopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""pvopt must be `None` or one of {0,1,2}.""")
        self._cards[11].set_value("pvopt", value)

    @property
    def pv1(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PVOPT. For details, see Remark 8
        """ # nopep8
        return self._cards[11].get_value("pv1")

    @pv1.setter
    def pv1(self, value: float) -> None:
        """Set the pv1 property."""
        self._cards[11].set_value("pv1", value)

    @property
    def pv2(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PVOPT. For details, see Remark 8
        """ # nopep8
        return self._cards[11].get_value("pv2")

    @pv2.setter
    def pv2(self, value: float) -> None:
        """Set the pv2 property."""
        self._cards[11].set_value("pv2", value)

    @property
    def pv3(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PVOPT. For details, see Remark 8
        """ # nopep8
        return self._cards[11].get_value("pv3")

    @pv3.setter
    def pv3(self, value: float) -> None:
        """Set the pv3 property."""
        self._cards[11].set_value("pv3", value)

    @property
    def pv4(self) -> typing.Optional[float]:
        """Get or set the ith shifting parameter. The meaning of the parameter depends on the choice of PVOPT. For details, see Remark 8
        """ # nopep8
        return self._cards[11].get_value("pv4")

    @pv4.setter
    def pv4(self, value: float) -> None:
        """Set the pv4 property."""
        self._cards[11].set_value("pv4", value)

    @property
    def pl1opt(self) -> int:
        """Get or set the Option for yield function description, see Remark 9 and 10.
        EQ.0: No plasticity
        EQ.1: Version of Toughened Adhesive Polymer model(TAPO) with cap in tension and Drucker & Prager in compression with distortional hardening under plastic flow.
        EQ.2: Version of Toughened Adhesive Polymer model(TAPO) with cap in tension and von Mises in compression.
        EQ.3: Version of Toughened Adhesive Polymer model(TAPO) with cap in tension and Drucker & Prager in compression with distortional hardening under temperature change.
        """ # nopep8
        return self._cards[12].get_value("pl1opt")

    @pl1opt.setter
    def pl1opt(self, value: int) -> None:
        """Set the pl1opt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""pl1opt must be `None` or one of {0,1,2,3}.""")
        self._cards[12].set_value("pl1opt", value)

    @property
    def pl11(self) -> typing.Optional[float]:
        """Get or set the ith yield surface parameter. The meaning of the parameter depends on the choice of PL1OPT. For details, see Remark 10
        """ # nopep8
        return self._cards[12].get_value("pl11")

    @pl11.setter
    def pl11(self, value: float) -> None:
        """Set the pl11 property."""
        self._cards[12].set_value("pl11", value)

    @property
    def pl12(self) -> typing.Optional[float]:
        """Get or set the ith yield surface parameter. The meaning of the parameter depends on the choice of PL1OPT. For details, see Remark 10
        """ # nopep8
        return self._cards[12].get_value("pl12")

    @pl12.setter
    def pl12(self, value: float) -> None:
        """Set the pl12 property."""
        self._cards[12].set_value("pl12", value)

    @property
    def pl13(self) -> typing.Optional[float]:
        """Get or set the ith yield surface parameter. The meaning of the parameter depends on the choice of PL1OPT. For details, see Remark 10
        """ # nopep8
        return self._cards[12].get_value("pl13")

    @pl13.setter
    def pl13(self, value: float) -> None:
        """Set the pl13 property."""
        self._cards[12].set_value("pl13", value)

    @property
    def pl14(self) -> typing.Optional[float]:
        """Get or set the ith yield surface parameter. The meaning of the parameter depends on the choice of PL1OPT. For details, see Remark 10
        """ # nopep8
        return self._cards[12].get_value("pl14")

    @pl14.setter
    def pl14(self, value: float) -> None:
        """Set the pl14 property."""
        self._cards[12].set_value("pl14", value)

    @property
    def pl15(self) -> typing.Optional[float]:
        """Get or set the ith yield surface parameter. The meaning of the parameter depends on the choice of PL1OPT. For details, see Remark 10
        """ # nopep8
        return self._cards[12].get_value("pl15")

    @pl15.setter
    def pl15(self, value: float) -> None:
        """Set the pl15 property."""
        self._cards[12].set_value("pl15", value)

    @property
    def pl2opt(self) -> int:
        """Get or set the Option for yield stress description. See Remark 9 and 11 for details.
        EQ.0: No plasticity
        EQ.1: Tabular input for yield stress as function of curing, temperature,and plastic strains.
        EQ.2: Tabular input for initial yield stress as function of curingand temperatureand for hardening as function of curing, temperature,and plastic strains.
        EQ.3: Load curve inputs for effects of curing on initial yield stressand on hardening.Load curve input for temperature dependence of initial yield stress.Tabular input for hardening as function of temperatureand strain
        EQ.4: Load curve inputs for effects of curingand temperature on the parameters for the yield stress definitions in the Toughened Adhesive Polymer model(TAPO)
        EQ.5: Yield stress definitions in the Toughened Adhesive Polymer model(TAPO).No influence of temperature or curing.
        """ # nopep8
        return self._cards[13].get_value("pl2opt")

    @pl2opt.setter
    def pl2opt(self, value: int) -> None:
        """Set the pl2opt property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""pl2opt must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[13].set_value("pl2opt", value)

    @property
    def pl21(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl21")

    @pl21.setter
    def pl21(self, value: float) -> None:
        """Set the pl21 property."""
        self._cards[13].set_value("pl21", value)

    @property
    def pl22(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl22")

    @pl22.setter
    def pl22(self, value: float) -> None:
        """Set the pl22 property."""
        self._cards[13].set_value("pl22", value)

    @property
    def pl23(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl23")

    @pl23.setter
    def pl23(self, value: float) -> None:
        """Set the pl23 property."""
        self._cards[13].set_value("pl23", value)

    @property
    def pl24(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl24")

    @pl24.setter
    def pl24(self, value: float) -> None:
        """Set the pl24 property."""
        self._cards[13].set_value("pl24", value)

    @property
    def pl25(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl25")

    @pl25.setter
    def pl25(self, value: float) -> None:
        """Set the pl25 property."""
        self._cards[13].set_value("pl25", value)

    @property
    def pl26(self) -> typing.Optional[float]:
        """Get or set the ith yield stress parameter.The meaning of the parameter depends on the choice of PL2OPT.For details, see Remark 11
        """ # nopep8
        return self._cards[13].get_value("pl26")

    @pl26.setter
    def pl26(self, value: float) -> None:
        """Set the pl26 property."""
        self._cards[13].set_value("pl26", value)

    @property
    def daopt(self) -> int:
        """Get or set the Material damaging option (damage parameter D_1), defines the strain thresholds gammma_c and gamma_f for damage initiation and rupture, see Remark 12 for details.
        EQ.0: No material damage
        EQ.1: Version of Toughened Adhesive Polymer model(TAPO):Strain threshold exponential function of triaxiality.Load curve inputs for temperature and cure dependence.
        EQ.2: Version of Toughened Adhesive Polymer model(TAPO):Same as 1, but without temperatureand cure dependence
        EQ.3: Version of Toughened Adhesive Polymer model(TAPO):Same as 1, but with additional strain rate dependency.
        """ # nopep8
        return self._cards[14].get_value("daopt")

    @daopt.setter
    def daopt(self, value: int) -> None:
        """Set the daopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""daopt must be `None` or one of {0,1,2,3}.""")
        self._cards[14].set_value("daopt", value)

    @property
    def daevo(self) -> float:
        """Get or set the Effective strain measure used for material damage evolution, see Remark 12.
        EQ.0: Arc length of damage plastic strain
        EQ.1: Arc length of plastic strain.
        EQ.2: Arc length of viscoelastic - plastic strain rate.
        """ # nopep8
        return self._cards[14].get_value("daevo")

    @daevo.setter
    def daevo(self, value: float) -> None:
        """Set the daevo property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""daevo must be `None` or one of {0,1,2}.""")
        self._cards[14].set_value("daevo", value)

    @property
    def datria(self) -> float:
        """Get or set the Triaxility flag for calculation of strain thresholds gamma_c and gamma_f for damage initiation and rupture of material damage option, see Remark 12.
        EQ.0: Use triaxiality factor only in tension,
        EQ.1: Use triaxiality factor in tension and compression.
        """ # nopep8
        return self._cards[14].get_value("datria")

    @datria.setter
    def datria(self, value: float) -> None:
        """Set the datria property."""
        if value not in [0, 1, None]:
            raise Exception("""datria must be `None` or one of {0,1}.""")
        self._cards[14].set_value("datria", value)

    @property
    def da1(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[14].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[14].set_value("da1", value)

    @property
    def da2(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[14].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[14].set_value("da2", value)

    @property
    def da3(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[14].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[14].set_value("da3", value)

    @property
    def da4(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[14].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[14].set_value("da4", value)

    @property
    def da5(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[14].get_value("da5")

    @da5.setter
    def da5(self, value: float) -> None:
        """Set the da5 property."""
        self._cards[14].set_value("da5", value)

    @property
    def da6(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da6")

    @da6.setter
    def da6(self, value: float) -> None:
        """Set the da6 property."""
        self._cards[15].set_value("da6", value)

    @property
    def da7(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da7")

    @da7.setter
    def da7(self, value: float) -> None:
        """Set the da7 property."""
        self._cards[15].set_value("da7", value)

    @property
    def da8(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da8")

    @da8.setter
    def da8(self, value: float) -> None:
        """Set the da8 property."""
        self._cards[15].set_value("da8", value)

    @property
    def da9(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da9")

    @da9.setter
    def da9(self, value: float) -> None:
        """Set the da9 property."""
        self._cards[15].set_value("da9", value)

    @property
    def da10(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da10")

    @da10.setter
    def da10(self, value: float) -> None:
        """Set the da10 property."""
        self._cards[15].set_value("da10", value)

    @property
    def da11(self) -> typing.Optional[float]:
        """Get or set the ith material damage parameter. The meaning of the parameter depends on the choice of DAOPT for the evolution of damage parameter D_1. For details, see Remark 12
        """ # nopep8
        return self._cards[15].get_value("da11")

    @da11.setter
    def da11(self, value: float) -> None:
        """Set the da11 property."""
        self._cards[15].set_value("da11", value)

    @property
    def pda1(self) -> typing.Optional[float]:
        """Get or set the Parameter for the (pre-) damage formulation due to for example viscous fingering. It defines the damage parameter D_2 as function of the thickness strain eps_33 and the degree of cure p. For details, see Remark 13.
        EQ.0: No damage
        GT.0: Use exponential approach.
        LT.0: Load curve input with id | PDA1 | input for D_2(eps_33).
        """ # nopep8
        return self._cards[15].get_value("pda1")

    @pda1.setter
    def pda1(self, value: float) -> None:
        """Set the pda1 property."""
        self._cards[15].set_value("pda1", value)

    @property
    def pgel(self) -> typing.Optional[float]:
        """Get or set the Gelation point p_gel as needed to switch between evolution of damage parameters D_1 and D_2. For details, see Remark 12 and Remark 13.
        """ # nopep8
        return self._cards[15].get_value("pgel")

    @pgel.setter
    def pgel(self, value: float) -> None:
        """Set the pgel property."""
        self._cards[15].set_value("pgel", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Shear relaxation modulus for the ith term
        """ # nopep8
        return self._cards[16].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        """Set the gi property."""
        self._cards[16].set_value("gi", value)

    @property
    def betagi(self) -> typing.Optional[float]:
        """Get or set the Shear decay constant for the ith term
        """ # nopep8
        return self._cards[16].get_value("betagi")

    @betagi.setter
    def betagi(self, value: float) -> None:
        """Set the betagi property."""
        self._cards[16].set_value("betagi", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Bulk relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[16].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        """Set the ki property."""
        self._cards[16].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Bulk decay constant for the ith term.
        """ # nopep8
        return self._cards[16].get_value("betaki")

    @betaki.setter
    def betaki(self, value: float) -> None:
        """Set the betaki property."""
        self._cards[16].set_value("betaki", value)

    @property
    def viopt(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[17].get_value("viopt")

    @viopt.setter
    def viopt(self, value: float) -> None:
        """Set the viopt property."""
        self._cards[17].set_value("viopt", value)

    @property
    def nue(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[17].get_value("nue")

    @nue.setter
    def nue(self, value: float) -> None:
        """Set the nue property."""
        self._cards[17].set_value("nue", value)

    @property
    def ei(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[18].get_value("ei")

    @ei.setter
    def ei(self, value: float) -> None:
        """Set the ei property."""
        self._cards[18].set_value("ei", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[18].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[18].set_value("betai", value)

    @property
    def ei(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[18].get_value("ei")

    @ei.setter
    def ei(self, value: float) -> None:
        """Set the ei property."""
        self._cards[18].set_value("ei", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[18].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[18].set_value("betai", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[19].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[19].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

