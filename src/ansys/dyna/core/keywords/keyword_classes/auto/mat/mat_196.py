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

"""Module providing the Mat196 class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT196_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("mdfail", int, 60, 10, 0),
    FieldSchema("dospot", int, 70, 10, 0),
)

_MAT196_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat196(KeywordBase):
    """DYNA MAT_196 keyword"""

    keyword = "MAT"
    subkeyword = "196"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "glcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat196 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT196_CARD0,
                **kwargs,
            ),
            TableCardGroup(
                [
                    (
                        FieldSchema("dof", int, 0, 10, None),
                        FieldSchema("type", int, 10, 10, 0),
                        FieldSchema("k", float, 20, 10, None),
                        FieldSchema("d", float, 30, 10, None),
                        FieldSchema("cdf", float, 40, 10, None),
                        FieldSchema("tdf", float, 50, 10, None),
                    ),
                    (
                        FieldSchema("flcid", int, 0, 10, None),
                        FieldSchema("hlcid", int, 10, 10, None),
                        FieldSchema("c1", float, 20, 10, None),
                        FieldSchema("c2", float, 30, 10, None),
                        FieldSchema("dle", float, 40, 10, None),
                        FieldSchema("glcid", int, 50, 10, None),
                    ),
                ],
                None,
                None,
                "springs",
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat196._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT196_OPTION0_CARD0,
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
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def mdfail(self) -> int:
        """Get or set the Multidirectional failure calculation method. See Remark 5.
        EQ.0: Default failure calculation method.It calculates the ratio of actual displacement(or rotation) to its corresponding CDF(or TDF) for each degree of freedom and then uses the maximum of these ratios to evaluate failure.
        EQ.1 : Two separate criteria, one for compressionand the other for tension.If any criterion is fulfilled, failure occurs.
        EQ.2 : One criterion that considers the combined effect of all displacements and rotations in tension and compression.
        """ # nopep8
        return self._cards[0].get_value("mdfail")

    @mdfail.setter
    def mdfail(self, value: int) -> None:
        """Set the mdfail property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""mdfail must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("mdfail", value)

    @property
    def dospot(self) -> int:
        """Get or set the Activate thinning of tied shell elements when SPOTHIN>0 on *CONTROL_CONTACT.
        EQ.0: Spot weld thinning is inactive for shells tied to discrete beams that use this material(default)
        EQ.1: Spot weld thinning is active for shells tied to discrete beams that use this material
        """ # nopep8
        return self._cards[0].get_value("dospot")

    @dospot.setter
    def dospot(self, value: int) -> None:
        """Set the dospot property."""
        if value not in [0, 1, None]:
            raise Exception("""dospot must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dospot", value)

    @property
    def springs(self) -> pd.DataFrame:
        """Gets the full table of springs."""
        return self._cards[1].table

    @springs.setter
    def springs(self, df: pd.DataFrame):
        """sets springs from the dataframe df."""
        self._cards[1].table = df

    @property
    def dof(self) -> pd.Series:
        """Get or set the Active degree-of-freedom, a number between 1 and 6 inclusive. Each value of DOF can only be used once. The active degree-of-freedom is measured in the local coordinate system for the discrete beam element.
        """ # nopep8
        return self._cards[1].get_column("dof")

    @dof.setter
    def dof(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the dof column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("dof", value)

    @property
    def type(self) -> pd.Series:
        """Get or set the Material behavior:
        EQ.0: Elastic(default)
        EQ.1 : Inelastic
        """ # nopep8
        return self._cards[1].get_column("type")

    @type.setter
    def type(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the type column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("type", value)

    @property
    def k(self) -> pd.Series:
        """Get or set the Elastic loading/unloading stiffness. This is required input for inelastic behavior.
        """ # nopep8
        return self._cards[1].get_column("k")

    @k.setter
    def k(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the k column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("k", value)

    @property
    def d(self) -> pd.Series:
        """Get or set the Optional viscous damping coefficient.
        """ # nopep8
        return self._cards[1].get_column("d")

    @d.setter
    def d(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the d column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("d", value)

    @property
    def cdf(self) -> pd.Series:
        """Get or set the Compressive displacement at failure. Input as a positive number. After failure, no forces are carried. This option does not apply to zero length springs.
        EQ.0.0: Inactive.
        """ # nopep8
        return self._cards[1].get_column("cdf")

    @cdf.setter
    def cdf(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the cdf column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("cdf", value)

    @property
    def tdf(self) -> pd.Series:
        """Get or set the Tensile displacement at failure. After failure, no forces are carried.
        EQ.0.0: Inactive.
        """ # nopep8
        return self._cards[1].get_column("tdf")

    @tdf.setter
    def tdf(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the tdf column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("tdf", value)

    @property
    def flcid(self) -> pd.Series:
        """Get or set the Load curve ID (see *DEFINE_CURVE).
        TYPE.EQ.0: This curve defines force or moment as a function of deflection for nonlinear elastic behavior.
        TYPE.EQ.1 : This curve defines the yield force as a function of plastic deflection.If the abscissa of the first point of the curve is 0. the force magnitude is identical in tension and compression, that is, only the sign changes.If not, the yield stress in the compression is used when the spring force is negative.The plastic displacement increases monotonically in this implementation.The load curve is required input.
        """ # nopep8
        return self._cards[1].get_column("flcid")

    @flcid.setter
    def flcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the flcid column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("flcid", value)

    @property
    def hlcid(self) -> pd.Series:
        """Get or set the Optional load curve ID (see *DEFINE_CURVE) defining force as a function of relative velocity.  If the origin of the curve is at (0,0), the force magnitude is identical for a given magnitude of the relative velocity, meaning only the sign changes.
        """ # nopep8
        return self._cards[1].get_column("hlcid")

    @hlcid.setter
    def hlcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the hlcid column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("hlcid", value)

    @property
    def c1(self) -> pd.Series:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[1].get_column("c1")

    @c1.setter
    def c1(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the c1 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("c1", value)

    @property
    def c2(self) -> pd.Series:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[1].get_column("c2")

    @c2.setter
    def c2(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the c2 column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("c2", value)

    @property
    def dle(self) -> pd.Series:
        """Get or set the Factor to scale time units.
        """ # nopep8
        return self._cards[1].get_column("dle")

    @dle.setter
    def dle(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the dle column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("dle", value)

    @property
    def glcid(self) -> pd.Series:
        """Get or set the Optional load curve ID, see *DEFINE_CURVE, defining a scale factor versus deflection for load curve ID, GLCID.
        If zero, a scale factor of unity is assumed.
        """ # nopep8
        return self._cards[1].get_column("glcid")

    @glcid.setter
    def glcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the glcid column from a scalar, a Series, or an array-like."""
        self._cards[1].set_column("glcid", value)

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

    @property
    def glcid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the glcid column, keyed by glcid value."""
        return self._get_column_links(self.glcid, lambda value: self._find_keyword("DEFINE", "CURVE", "lcid", value))


class MatGeneralSpringDiscreteBeam(Mat196):
    """Alias for MAT keyword."""
    subkeyword = "GENERAL_SPRING_DISCRETE_BEAM"
