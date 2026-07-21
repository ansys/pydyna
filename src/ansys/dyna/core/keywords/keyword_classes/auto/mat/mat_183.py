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

"""Module providing the Mat183 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT183_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
    FieldSchema("mu", float, 30, 10, None),
    FieldSchema("g", float, 40, 10, None),
    FieldSchema("sigf", float, 50, 10, None),
)

_MAT183_CARD1 = (
    FieldSchema("sgl", float, 0, 10, None),
    FieldSchema("sw", float, 10, 10, None),
    FieldSchema("st", float, 20, 10, None),
    FieldSchema("lc_tbid", float, 30, 10, None, "lc/tbid"),
    FieldSchema("tension", float, 40, 10, -1.0),
    FieldSchema("rtype", float, 50, 10, 0.0),
    FieldSchema("avgopt", float, 60, 10, 0.0),
)

_MAT183_CARD2 = (
    FieldSchema("lcunld", int, 0, 10, None),
    FieldSchema("ref", float, 10, 10, None),
    FieldSchema("stol", float, 20, 10, None),
)

_MAT183_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat183(KeywordBase):
    """DYNA MAT_183 keyword"""

    keyword = "MAT"
    subkeyword = "183"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lc_tbid": LinkType.DEFINE_CURVE,
        "lcunld": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat183 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT183_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT183_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT183_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat183._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT183_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Linear bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[0].set_value("mu", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based on a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Limit stress for frequency independent, frictional, damping.
        """ # nopep8
        return self._cards[0].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[0].set_value("sigf", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[1].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        """Set the sgl property."""
        self._cards[1].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[1].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        """Set the sw property."""
        self._cards[1].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[1].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        """Set the st property."""
        self._cards[1].set_value("st", value)

    @property
    def lc_tbid(self) -> typing.Optional[float]:
        """Get or set the Load curve ID or table ID (see *DEFINE_TABLE) defining the force as a function of actual change in the gauge length.
        If SGL, SW, and ST are set to unity (1.0), curve LC is also engineering stress as a function of engineering strain.
        If the table definition is used, a family of curves is defined for discrete strain rates. The curves should cover the complete
        range of expected responses, including both compressive (negative values) and tensile (positive values) regimes. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("lc_tbid")

    @lc_tbid.setter
    def lc_tbid(self, value: float) -> None:
        """Set the lc_tbid property."""
        self._cards[1].set_value("lc_tbid", value)

    @property
    def tension(self) -> float:
        """Get or set the Parameter that controls how the rate effects are treated. It is applicable to the table definition.
        EQ.-1.0: Rate effects are considered for loading either in tension or compression, but not for unloading,
        EQ.0.0: Rate effects are considered for compressive loading only,
        EQ.1.0: Rate effects are treated identically in tension and compression.
        """ # nopep8
        return self._cards[1].get_value("tension")

    @tension.setter
    def tension(self, value: float) -> None:
        """Set the tension property."""
        if value not in [-1.0, 0.0, 1.0, None]:
            raise Exception("""tension must be `None` or one of {-1.0,0.0,1.0}.""")
        self._cards[1].set_value("tension", value)

    @property
    def rtype(self) -> float:
        """Get or set the Strain rate type if a table is defined:
        EQ.0.0: True strain rate,
        EQ.1.0: Engineering strain rate.
        """ # nopep8
        return self._cards[1].get_value("rtype")

    @rtype.setter
    def rtype(self, value: float) -> None:
        """Set the rtype property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""rtype must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("rtype", value)

    @property
    def avgopt(self) -> float:
        """Get or set the Averaging option for determining strain rate to reduce numerical noise.
        EQ.0.0: Simple average of twelve time steps
        EQ.1.0: Running 12 point average.
        """ # nopep8
        return self._cards[1].get_value("avgopt")

    @avgopt.setter
    def avgopt(self, value: float) -> None:
        """Set the avgopt property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""avgopt must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("avgopt", value)

    @property
    def lcunld(self) -> typing.Optional[int]:
        """Get or set the Load curve (see *DEFINE_CURVE) defining the force as a function of actual change in the gauge length during unloading.
        The unloading curve should cover exactly the same range as LC (or as the first curve of table TBID) and its endpoints should
        have identical values, meaning the combination of LC (or the first curve of table TBID) and LCUNLD describes a complete cycle of loading and unloading.
        """ # nopep8
        return self._cards[2].get_value("lcunld")

    @lcunld.setter
    def lcunld(self, value: int) -> None:
        """Set the lcunld property."""
        self._cards[2].set_value("lcunld", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY.
        EQ.0.0: Off
        EQ.1.0: On.
        """ # nopep8
        return self._cards[2].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[2].set_value("ref", value)

    @property
    def stol(self) -> typing.Optional[float]:
        """Get or set the Tolerance in stability check. See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("stol")

    @stol.setter
    def stol(self, value: float) -> None:
        """Set the stol property."""
        self._cards[2].set_value("stol", value)

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
    def lc_tbid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc_tbid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc_tbid:
                return kwd
        return None

    @lc_tbid_link.setter
    def lc_tbid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc_tbid."""
        self.lc_tbid = value.lcid

    @property
    def lcunld_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcunld."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcunld:
                return kwd
        return None

    @lcunld_link.setter
    def lcunld_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcunld."""
        self.lcunld = value.lcid

