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

"""Module providing the Mat124 class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT124_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("c", float, 40, 10, None),
    FieldSchema("p", float, 50, 10, None),
    FieldSchema("fail", float, 60, 10, 1e+20),
    FieldSchema("tdel", float, 70, 10, None),
)

_MAT124_CARD1 = (
    FieldSchema("lcidc", int, 0, 10, 0),
    FieldSchema("lcidt", int, 10, 10, 0),
    FieldSchema("lcsrc", int, 20, 10, None),
    FieldSchema("lcsrt", int, 30, 10, None),
    FieldSchema("srflag", float, 40, 10, None),
    FieldSchema("lcfail", int, 50, 10, 0),
    FieldSchema("ec", float, 60, 10, None),
    FieldSchema("rpct", float, 70, 10, None),
)

_MAT124_CARD2 = (
    FieldSchema("pc", float, 0, 10, None),
    FieldSchema("pt", float, 10, 10, None),
    FieldSchema("pcutc", float, 20, 10, None),
    FieldSchema("pcutt", float, 30, 10, None),
    FieldSchema("pcutf", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("srfilt", float, 70, 10, None),
)

_MAT124_CARD3 = (
    FieldSchema("k", float, 0, 10, None),
)

_MAT124_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat124(KeywordBase):
    """DYNA MAT_124 keyword"""

    keyword = "MAT"
    subkeyword = "124"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcidc": LinkType.DEFINE_CURVE,
        "lcidt": LinkType.DEFINE_CURVE,
        "lcsrc": LinkType.DEFINE_CURVE,
        "lcsrt": LinkType.DEFINE_CURVE,
        "lcfail": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat124 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT124_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT124_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT124_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT124_CARD3,
                **kwargs,
            ),            TableCard(
                [
                    Field("gi", float, 0, 10, None),
                    Field("betai", float, 10, 10, None),
                ],
                None,
                name="constants",
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat124.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT124_OPTION0_CARD0,
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
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[0].set_value("p", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure flag:
        LT.0.0: User defined failure subroutine is called to determine failure.
        EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many caluculations will be saved.
        GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion.
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        """Set the tdel property."""
        self._cards[0].set_value("tdel", value)

    @property
    def lcidc(self) -> int:
        """Get or set the Load curve ID defining effective yield stress versus effective plastic strain in compression.
        """ # nopep8
        return self._cards[1].get_value("lcidc")

    @lcidc.setter
    def lcidc(self, value: int) -> None:
        """Set the lcidc property."""
        self._cards[1].set_value("lcidc", value)

    @property
    def lcidt(self) -> int:
        """Get or set the Load curve ID defining effective yield stress versus effective plastic strain in tension.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[1].set_value("lcidt", value)

    @property
    def lcsrc(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID defining strain rate scaling effect on yield stress
        when the material is in compression (compressive yield stress scaling factor vs. strain rate)
        """ # nopep8
        return self._cards[1].get_value("lcsrc")

    @lcsrc.setter
    def lcsrc(self, value: int) -> None:
        """Set the lcsrc property."""
        self._cards[1].set_value("lcsrc", value)

    @property
    def lcsrt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID defining strain rate scaling effect on yield stress
        when the material is in tension (tensile yield stress scaling factor vs. strain rate).
        """ # nopep8
        return self._cards[1].get_value("lcsrt")

    @lcsrt.setter
    def lcsrt(self, value: int) -> None:
        """Set the lcsrt property."""
        self._cards[1].set_value("lcsrt", value)

    @property
    def srflag(self) -> typing.Optional[float]:
        """Get or set the Formulation for rate effects:
        EQ.0.0:  Total strain rate,
        EQ.1.0:  Deviatoric strain rate
        """ # nopep8
        return self._cards[1].get_value("srflag")

    @srflag.setter
    def srflag(self, value: float) -> None:
        """Set the srflag property."""
        self._cards[1].set_value("srflag", value)

    @property
    def lcfail(self) -> int:
        """Get or set the Load curve ID defining failure strain versus strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcfail")

    @lcfail.setter
    def lcfail(self, value: int) -> None:
        """Set the lcfail property."""
        self._cards[1].set_value("lcfail", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Optional Young's modulus for compression, >0.
        """ # nopep8
        return self._cards[1].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[1].set_value("ec", value)

    @property
    def rpct(self) -> typing.Optional[float]:
        """Get or set the Ratio of PC and PT, used to define mean stress at which Young's modulus is E or EC. Young's modulus is E when mean stress >RPCT*PT, and EC when mean stress <-RPCT*PC. If the mean stress falls between - RPCT*PC and RPCT*PT, a linearly interpolated value is used.
        """ # nopep8
        return self._cards[1].get_value("rpct")

    @rpct.setter
    def rpct(self, value: float) -> None:
        """Set the rpct property."""
        self._cards[1].set_value("rpct", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Compressive mean stress at which the yield stress follows load curve ID, LCIDC. If the pressure falls between PC and PT a weighted average of the two load curves is used.
        """ # nopep8
        return self._cards[2].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[2].set_value("pc", value)

    @property
    def pt(self) -> typing.Optional[float]:
        """Get or set the Tensile mean stress at which the yield stress follows load curve ID, LCIDT.
        """ # nopep8
        return self._cards[2].get_value("pt")

    @pt.setter
    def pt(self, value: float) -> None:
        """Set the pt property."""
        self._cards[2].set_value("pt", value)

    @property
    def pcutc(self) -> typing.Optional[float]:
        """Get or set the Pressure cut-off in compression
        """ # nopep8
        return self._cards[2].get_value("pcutc")

    @pcutc.setter
    def pcutc(self, value: float) -> None:
        """Set the pcutc property."""
        self._cards[2].set_value("pcutc", value)

    @property
    def pcutt(self) -> typing.Optional[float]:
        """Get or set the Pressure cut-off in tension.
        """ # nopep8
        return self._cards[2].get_value("pcutt")

    @pcutt.setter
    def pcutt(self, value: float) -> None:
        """Set the pcutt property."""
        self._cards[2].set_value("pcutt", value)

    @property
    def pcutf(self) -> typing.Optional[float]:
        """Get or set the Pressure cut-off flag.
        EQ.0.0:  Inactive,
        EQ.1.0:  Active
        """ # nopep8
        return self._cards[2].get_value("pcutf")

    @pcutf.setter
    def pcutf(self, value: float) -> None:
        """Set the pcutf property."""
        self._cards[2].set_value("pcutf", value)

    @property
    def srfilt(self) -> typing.Optional[float]:
        """Get or set the Strain rate filtering parameter in exponential moving average with admissible values ranging from 0 to 1 (available for LCSRC≠0 or LCSRT≠0 and SRFLAG = 0 or 1)
        """ # nopep8
        return self._cards[2].get_value("srfilt")

    @srfilt.setter
    def srfilt(self, value: float) -> None:
        """Set the srfilt property."""
        self._cards[2].set_value("srfilt", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Optional bulk modulus for the viscoelastic material.  If nonzero a Kelvin type behavior will be obtained.  Generally, K is set to zero.
        """ # nopep8
        return self._cards[3].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[3].set_value("k", value)

    @property
    def constants(self) -> pd.DataFrame:
        """Get the table of constants."""
        return self._cards[4].table

    @constants.setter
    def constants(self, df: pd.DataFrame):
        """Set constants from the dataframe df"""
        self._cards[4].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcidc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc:
                return kwd
        return None

    @lcidc_link.setter
    def lcidc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc."""
        self.lcidc = value.lcid

    @property
    def lcidt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidt:
                return kwd
        return None

    @lcidt_link.setter
    def lcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidt."""
        self.lcidt = value.lcid

    @property
    def lcsrc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsrc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsrc:
                return kwd
        return None

    @lcsrc_link.setter
    def lcsrc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsrc."""
        self.lcsrc = value.lcid

    @property
    def lcsrt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsrt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsrt:
                return kwd
        return None

    @lcsrt_link.setter
    def lcsrt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsrt."""
        self.lcsrt = value.lcid

    @property
    def lcfail_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfail."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfail:
                return kwd
        return None

    @lcfail_link.setter
    def lcfail_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfail."""
        self.lcfail = value.lcid


class MatPlasticityCompressionTension(Mat124):
    """Alias for MAT keyword."""
    subkeyword = "PLASTICITY_COMPRESSION_TENSION"
