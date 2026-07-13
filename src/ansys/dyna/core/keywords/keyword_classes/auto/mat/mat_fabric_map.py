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

"""Module providing the MatFabricMap class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATFABRICMAP_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pxx", float, 20, 10, None),
    FieldSchema("pyy", float, 30, 10, None),
    FieldSchema("sxy", float, 40, 10, None),
    FieldSchema("damp", float, 50, 10, None),
    FieldSchema("th", float, 60, 10, None),
    FieldSchema("t0", float, 70, 10, None),
)

_MATFABRICMAP_CARD1 = (
    FieldSchema("t1", float, 0, 10, None),
    FieldSchema("t2", float, 10, 10, None),
    FieldSchema("t3", float, 20, 10, None),
    FieldSchema("t4", float, 30, 10, None),
    FieldSchema("t5", float, 40, 10, None),
    FieldSchema("t6", float, 50, 10, None),
    FieldSchema("t7", float, 60, 10, None),
    FieldSchema("t8", float, 70, 10, None),
)

_MATFABRICMAP_CARD2 = (
    FieldSchema("pxx1", float, 0, 10, None),
    FieldSchema("pxx2", float, 10, 10, None),
    FieldSchema("pxx3", float, 20, 10, None),
    FieldSchema("pxx4", float, 30, 10, None),
    FieldSchema("pxx5", float, 40, 10, None),
    FieldSchema("pxx6", float, 50, 10, None),
    FieldSchema("pxx7", float, 60, 10, None),
    FieldSchema("pxx8", float, 70, 10, None),
)

_MATFABRICMAP_CARD3 = (
    FieldSchema("pyy1", float, 0, 10, None),
    FieldSchema("pyy2", float, 10, 10, None),
    FieldSchema("pyy3", float, 20, 10, None),
    FieldSchema("pyy4", float, 30, 10, None),
    FieldSchema("pyy5", float, 40, 10, None),
    FieldSchema("pyy6", float, 50, 10, None),
    FieldSchema("pyy7", float, 60, 10, None),
    FieldSchema("pyy8", float, 70, 10, None),
)

_MATFABRICMAP_CARD4 = (
    FieldSchema("sxy1", float, 0, 10, None),
    FieldSchema("sxy2", float, 10, 10, None),
    FieldSchema("sxy3", float, 20, 10, None),
    FieldSchema("sxy4", float, 30, 10, None),
    FieldSchema("sxy5", float, 40, 10, None),
    FieldSchema("sxy6", float, 50, 10, None),
    FieldSchema("sxy7", float, 60, 10, None),
    FieldSchema("sxy8", float, 70, 10, None),
)

_MATFABRICMAP_CARD5 = (
    FieldSchema("fvopt", float, 0, 10, None),
    FieldSchema("x0", float, 10, 10, None),
    FieldSchema("x1", float, 20, 10, None),
    FieldSchema("flc_x2", float, 30, 10, None, "flc/x2"),
    FieldSchema("fac_x3", float, 40, 10, None, "fac/x3"),
)

_MATFABRICMAP_CARD6 = (
    FieldSchema("isrefg", float, 0, 10, 0.0),
    FieldSchema("cse", float, 10, 10, 0.0),
    FieldSchema("srfac", int, 20, 10, None),
    FieldSchema("bulkc", float, 30, 10, None),
    FieldSchema("jacc", float, 40, 10, None),
    FieldSchema("fxx", int, 50, 10, None),
    FieldSchema("fyy", int, 60, 10, None),
    FieldSchema("dt", float, 70, 10, None),
)

_MATFABRICMAP_CARD7 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("ecoat", float, 10, 10, None),
    FieldSchema("scoat", float, 20, 10, None),
    FieldSchema("tcoat", float, 30, 10, None),
)

_MATFABRICMAP_CARD8 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATFABRICMAP_CARD9 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATFABRICMAP_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatFabricMap(KeywordBase):
    """DYNA MAT_FABRIC_MAP keyword"""

    keyword = "MAT"
    subkeyword = "FABRIC_MAP"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "sxy": LinkType.DEFINE_CURVE,
        "th": LinkType.DEFINE_CURVE,
        "srfac": LinkType.DEFINE_CURVE,
        "fxx": LinkType.DEFINE_CURVE,
        "fyy": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatFabricMap class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD7,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD8,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATFABRICMAP_CARD9,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatFabricMap._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATFABRICMAP_OPTION0_CARD0,
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
    def pxx(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as a function of engineering local XX-strain and YY-strain.
        """ # nopep8
        return self._cards[0].get_value("pxx")

    @pxx.setter
    def pxx(self, value: float) -> None:
        """Set the pxx property."""
        self._cards[0].set_value("pxx", value)

    @property
    def pyy(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as a function of engineering local YY-strain and XX-strain.
        """ # nopep8
        return self._cards[0].get_value("pyy")

    @pyy.setter
    def pyy(self, value: float) -> None:
        """Set the pyy property."""
        self._cards[0].set_value("pyy", value)

    @property
    def sxy(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as a function of local Green XY-strain.
        """ # nopep8
        return self._cards[0].get_value("sxy")

    @sxy.setter
    def sxy(self, value: float) -> None:
        """Set the sxy property."""
        self._cards[0].set_value("sxy", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient for numerical stability.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def th(self) -> typing.Optional[float]:
        """Get or set the Table giving hysteresis factor 0 <= H < 1 as a function of engineering local XX-strain and YY-strain.
        GT.0.0: TH is table ID
        LE.0.0: -TH is used as constant value for hysteresis factor
        """ # nopep8
        return self._cards[0].get_value("th")

    @th.setter
    def th(self, value: float) -> None:
        """Set the th property."""
        self._cards[0].set_value("th", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[1].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[1].set_value("t2", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        """Set the t3 property."""
        self._cards[1].set_value("t3", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        """Set the t4 property."""
        self._cards[1].set_value("t4", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        """Set the t5 property."""
        self._cards[1].set_value("t5", value)

    @property
    def t6(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t6")

    @t6.setter
    def t6(self, value: float) -> None:
        """Set the t6 property."""
        self._cards[1].set_value("t6", value)

    @property
    def t7(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t7")

    @t7.setter
    def t7(self, value: float) -> None:
        """Set the t7 property."""
        self._cards[1].set_value("t7", value)

    @property
    def t8(self) -> typing.Optional[float]:
        """Get or set the Temperature values for which tables and curve apply. Temperature values must be increasing and positive; T1>0, T2>T1, T3>T2, etc. If fewer than 8 temperature points are needed, then set the first unused temperature value to zero. T0 may not take any of the positive Ti values, but will be properly inserted into the range so that all positive temperatures defined are in increasing order.
        """ # nopep8
        return self._cards[1].get_value("t8")

    @t8.setter
    def t8(self, value: float) -> None:
        """Set the t8 property."""
        self._cards[1].set_value("t8", value)

    @property
    def pxx1(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx1")

    @pxx1.setter
    def pxx1(self, value: float) -> None:
        """Set the pxx1 property."""
        self._cards[2].set_value("pxx1", value)

    @property
    def pxx2(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx2")

    @pxx2.setter
    def pxx2(self, value: float) -> None:
        """Set the pxx2 property."""
        self._cards[2].set_value("pxx2", value)

    @property
    def pxx3(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx3")

    @pxx3.setter
    def pxx3(self, value: float) -> None:
        """Set the pxx3 property."""
        self._cards[2].set_value("pxx3", value)

    @property
    def pxx4(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx4")

    @pxx4.setter
    def pxx4(self, value: float) -> None:
        """Set the pxx4 property."""
        self._cards[2].set_value("pxx4", value)

    @property
    def pxx5(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx5")

    @pxx5.setter
    def pxx5(self, value: float) -> None:
        """Set the pxx5 property."""
        self._cards[2].set_value("pxx5", value)

    @property
    def pxx6(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx6")

    @pxx6.setter
    def pxx6(self, value: float) -> None:
        """Set the pxx6 property."""
        self._cards[2].set_value("pxx6", value)

    @property
    def pxx7(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx7")

    @pxx7.setter
    def pxx7(self, value: float) -> None:
        """Set the pxx7 property."""
        self._cards[2].set_value("pxx7", value)

    @property
    def pxx8(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain for temperature Ti
        """ # nopep8
        return self._cards[2].get_value("pxx8")

    @pxx8.setter
    def pxx8(self, value: float) -> None:
        """Set the pxx8 property."""
        self._cards[2].set_value("pxx8", value)

    @property
    def pyy1(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy1")

    @pyy1.setter
    def pyy1(self, value: float) -> None:
        """Set the pyy1 property."""
        self._cards[3].set_value("pyy1", value)

    @property
    def pyy2(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy2")

    @pyy2.setter
    def pyy2(self, value: float) -> None:
        """Set the pyy2 property."""
        self._cards[3].set_value("pyy2", value)

    @property
    def pyy3(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy3")

    @pyy3.setter
    def pyy3(self, value: float) -> None:
        """Set the pyy3 property."""
        self._cards[3].set_value("pyy3", value)

    @property
    def pyy4(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy4")

    @pyy4.setter
    def pyy4(self, value: float) -> None:
        """Set the pyy4 property."""
        self._cards[3].set_value("pyy4", value)

    @property
    def pyy5(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy5")

    @pyy5.setter
    def pyy5(self, value: float) -> None:
        """Set the pyy5 property."""
        self._cards[3].set_value("pyy5", value)

    @property
    def pyy6(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy6")

    @pyy6.setter
    def pyy6(self, value: float) -> None:
        """Set the pyy6 property."""
        self._cards[3].set_value("pyy6", value)

    @property
    def pyy7(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy7")

    @pyy7.setter
    def pyy7(self, value: float) -> None:
        """Set the pyy7 property."""
        self._cards[3].set_value("pyy7", value)

    @property
    def pyy8(self) -> typing.Optional[float]:
        """Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain for temperature Ti.
        """ # nopep8
        return self._cards[3].get_value("pyy8")

    @pyy8.setter
    def pyy8(self, value: float) -> None:
        """Set the pyy8 property."""
        self._cards[3].set_value("pyy8", value)

    @property
    def sxy1(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy1")

    @sxy1.setter
    def sxy1(self, value: float) -> None:
        """Set the sxy1 property."""
        self._cards[4].set_value("sxy1", value)

    @property
    def sxy2(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy2")

    @sxy2.setter
    def sxy2(self, value: float) -> None:
        """Set the sxy2 property."""
        self._cards[4].set_value("sxy2", value)

    @property
    def sxy3(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy3")

    @sxy3.setter
    def sxy3(self, value: float) -> None:
        """Set the sxy3 property."""
        self._cards[4].set_value("sxy3", value)

    @property
    def sxy4(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy4")

    @sxy4.setter
    def sxy4(self, value: float) -> None:
        """Set the sxy4 property."""
        self._cards[4].set_value("sxy4", value)

    @property
    def sxy5(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy5")

    @sxy5.setter
    def sxy5(self, value: float) -> None:
        """Set the sxy5 property."""
        self._cards[4].set_value("sxy5", value)

    @property
    def sxy6(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy6")

    @sxy6.setter
    def sxy6(self, value: float) -> None:
        """Set the sxy6 property."""
        self._cards[4].set_value("sxy6", value)

    @property
    def sxy7(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy7")

    @sxy7.setter
    def sxy7(self, value: float) -> None:
        """Set the sxy7 property."""
        self._cards[4].set_value("sxy7", value)

    @property
    def sxy8(self) -> typing.Optional[float]:
        """Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain for temperature Ti.
        """ # nopep8
        return self._cards[4].get_value("sxy8")

    @sxy8.setter
    def sxy8(self, value: float) -> None:
        """Set the sxy8 property."""
        self._cards[4].set_value("sxy8", value)

    @property
    def fvopt(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option, see *MAT_FABRIC.
        EQ.1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered.
        EQ.2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
        EQ.3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
        EQ.4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
        EQ.5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
        EQ.6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
        EQ.7: Leakage is based on gas volume outflow versus pressure load curve [Lian, 2000]. Blockage is not considered.
        Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC.
        EQ.8: Leakage is based on gas volume outflow versus pressure load curve [Lian 2000]. Blockage of venting or porous area due to contact is considered.
        Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC.
        LT.0: |"FVOPT"| defines the same fabric venting options as above, but a new formula for the leakage area is used to replace the element area.
        """ # nopep8
        return self._cards[5].get_value("fvopt")

    @fvopt.setter
    def fvopt(self, value: float) -> None:
        """Set the fvopt property."""
        self._cards[5].set_value("fvopt", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[5].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[5].set_value("x0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option parameters, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[5].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[5].set_value("x1", value)

    @property
    def flc_x2(self) -> typing.Optional[float]:
        """Get or set the FLC: Optional porous leakage flow coefficient (see the theory manual and the remarks section of *AIRBAG_WANG_NEFSKE):
        GE.0.0: Porous leakage flow coefficient.
        LT.0.0: | FLC | is a load curve ID defining FLC as a function of time.
        X0: Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area: A_leak =A_0 (X_0+X_1 r_s+X_2 r_p+X_3 r_s r_p ); see *MAT_FABRIC.
        """ # nopep8
        return self._cards[5].get_value("flc_x2")

    @flc_x2.setter
    def flc_x2(self, value: float) -> None:
        """Set the flc_x2 property."""
        self._cards[5].set_value("flc_x2", value)

    @property
    def fac_x3(self) -> typing.Optional[float]:
        """Get or set the FAC: Optional characteristic fabric parameter (see the theory manual and the remarks section of *AIRBAG_WANG_NEFSKE):
        GE.0.0: Characteristic fabric parameter
        LT.0.0: | FAC | is a load curve ID defining FAC as a function of absolute pressure.
        X1:Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area: A_leak =A_0 (X_0+X_1 r_s+X_2 r_p+X_3 r_s r_p ); see *MAT_FABRIC.
        """ # nopep8
        return self._cards[5].get_value("fac_x3")

    @fac_x3.setter
    def fac_x3(self, value: float) -> None:
        """Set the fac_x3 property."""
        self._cards[5].set_value("fac_x3", value)

    @property
    def isrefg(self) -> float:
        """Get or set the Initial stress by reference geometry.
        EQ.0.0: Not active.
        EQ.1.0: Active
        """ # nopep8
        return self._cards[6].get_value("isrefg")

    @isrefg.setter
    def isrefg(self, value: float) -> None:
        """Set the isrefg property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""isrefg must be `None` or one of {0.0,1.0}.""")
        self._cards[6].set_value("isrefg", value)

    @property
    def cse(self) -> float:
        """Get or set the Compressive stress elimination option.
        EQ.0.0: Don't eliminate compressive stresses,
        EQ.1.0: Eliminate compressive stresses.
        """ # nopep8
        return self._cards[6].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        """Set the cse property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""cse must be `None` or one of {0.0,1.0}.""")
        self._cards[6].set_value("cse", value)

    @property
    def srfac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for smooth stress initialization when using a reference geometry.
        """ # nopep8
        return self._cards[6].get_value("srfac")

    @srfac.setter
    def srfac(self, value: int) -> None:
        """Set the srfac property."""
        self._cards[6].set_value("srfac", value)

    @property
    def bulkc(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus for fabric compaction.
        """ # nopep8
        return self._cards[6].get_value("bulkc")

    @bulkc.setter
    def bulkc(self, value: float) -> None:
        """Set the bulkc property."""
        self._cards[6].set_value("bulkc", value)

    @property
    def jacc(self) -> typing.Optional[float]:
        """Get or set the Jacobian for the onset of fabric compaction.
        """ # nopep8
        return self._cards[6].get_value("jacc")

    @jacc.setter
    def jacc(self, value: float) -> None:
        """Set the jacc property."""
        self._cards[6].set_value("jacc", value)

    @property
    def fxx(self) -> typing.Optional[int]:
        """Get or set the Load curve giving scale factor of uniaxial stress in first material direction as function of engineering strain rate.
        """ # nopep8
        return self._cards[6].get_value("fxx")

    @fxx.setter
    def fxx(self, value: int) -> None:
        """Set the fxx property."""
        self._cards[6].set_value("fxx", value)

    @property
    def fyy(self) -> typing.Optional[int]:
        """Get or set the Load curve giving scale factor of uniaxial stress in second material direction as function of engineering strain rate.
        """ # nopep8
        return self._cards[6].get_value("fyy")

    @fyy.setter
    def fyy(self, value: int) -> None:
        """Set the fyy property."""
        self._cards[6].set_value("fyy", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time window for smoothing strain rates used for FXX and FYY.
        """ # nopep8
        return self._cards[6].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[6].set_value("dt", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description). Also, please refer to Remark of *MAT_FABRIC for additional information specific to fiber directions for fabrics:
        EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES,and then rotated about the element normal by an angle BETA
        EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR
        EQ.3.0: Locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.
        LT.0.0: The absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR).
        """ # nopep8
        return self._cards[7].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[7].set_value("aopt", value)

    @property
    def ecoat(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of coat material to include bending properties. This together with the following two parameters (SCOAT and TCOAT) encompass the same coating/bending feature as in *MAT_FABRIC. Please refer to these manual pages and associated remarks..
        """ # nopep8
        return self._cards[7].get_value("ecoat")

    @ecoat.setter
    def ecoat(self, value: float) -> None:
        """Set the ecoat property."""
        self._cards[7].set_value("ecoat", value)

    @property
    def scoat(self) -> typing.Optional[float]:
        """Get or set the Yield stress of coat material, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[7].get_value("scoat")

    @scoat.setter
    def scoat(self, value: float) -> None:
        """Set the scoat property."""
        self._cards[7].set_value("scoat", value)

    @property
    def tcoat(self) -> typing.Optional[float]:
        """Get or set the Thickness of coat material, may be positive or negative, see *MAT_FABRIC.
        """ # nopep8
        return self._cards[7].get_value("tcoat")

    @tcoat.setter
    def tcoat(self, value: float) -> None:
        """Set the tcoat property."""
        self._cards[7].set_value("tcoat", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[8].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[8].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[8].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[8].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[8].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[8].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[8].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[8].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[8].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[9].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[9].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[9].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[9].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[9].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[9].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[9].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[9].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[9].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[9].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[9].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[9].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[9].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[9].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[10].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def sxy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for sxy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.sxy:
                return kwd
        return None

    @sxy_link.setter
    def sxy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for sxy."""
        self.sxy = value.lcid

    @property
    def th_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for th."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.th:
                return kwd
        return None

    @th_link.setter
    def th_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for th."""
        self.th = value.lcid

    @property
    def srfac_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for srfac."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.srfac:
                return kwd
        return None

    @srfac_link.setter
    def srfac_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for srfac."""
        self.srfac = value.lcid

    @property
    def fxx_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fxx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fxx:
                return kwd
        return None

    @fxx_link.setter
    def fxx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fxx."""
        self.fxx = value.lcid

    @property
    def fyy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for fyy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fyy:
                return kwd
        return None

    @fyy_link.setter
    def fyy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fyy."""
        self.fyy = value.lcid

