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

"""Module providing the MatSeatbelt class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATSEATBELT_CARD0 = (
    FieldSchema("mid", int, 0, 10, 0),
    FieldSchema("mpul", float, 10, 10, None),
    FieldSchema("llcid", int, 20, 10, 0),
    FieldSchema("ulcid", int, 30, 10, 0),
    FieldSchema("lmin", float, 40, 10, None),
    FieldSchema("cse", float, 50, 10, 0.0),
    FieldSchema("damp", float, 60, 10, None),
    FieldSchema("e", float, 70, 10, None),
)

_MATSEATBELT_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("i", float, 10, 10, None),
    FieldSchema("j", float, 20, 10, None),
    FieldSchema("as_", float, 30, 10, None, "as"),
    FieldSchema("f", float, 40, 10, 1e+20),
    FieldSchema("m", float, 50, 10, 1e+20),
    FieldSchema("r", float, 60, 10, 0.05),
)

_MATSEATBELT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSeatbelt(KeywordBase):
    """DYNA MAT_SEATBELT keyword"""

    keyword = "MAT"
    subkeyword = "SEATBELT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "llcid": LinkType.DEFINE_CURVE,
        "ulcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatSeatbelt class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSEATBELT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSEATBELT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSeatbelt.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSEATBELT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> int:
        """Get or set the Belt material number. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mpul(self) -> typing.Optional[float]:
        """Get or set the Mass per unit length.
        """ # nopep8
        return self._cards[0].get_value("mpul")

    @mpul.setter
    def mpul(self, value: float) -> None:
        """Set the mpul property."""
        self._cards[0].set_value("mpul", value)

    @property
    def llcid(self) -> int:
        """Get or set the Load curve identification for loading (strain/force with engineering strain).
        """ # nopep8
        return self._cards[0].get_value("llcid")

    @llcid.setter
    def llcid(self, value: int) -> None:
        """Set the llcid property."""
        self._cards[0].set_value("llcid", value)

    @property
    def ulcid(self) -> int:
        """Get or set the Load curve identification for unloading (strain/force with engineering strain).
        """ # nopep8
        return self._cards[0].get_value("ulcid")

    @ulcid.setter
    def ulcid(self, value: int) -> None:
        """Set the ulcid property."""
        self._cards[0].set_value("ulcid", value)

    @property
    def lmin(self) -> typing.Optional[float]:
        """Get or set the Minimum length (for elements connected to slip rings and retractors).
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        """Set the lmin property."""
        self._cards[0].set_value("lmin", value)

    @property
    def cse(self) -> float:
        """Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
        EQ.0.0:	eliminate compressive stresses in shell fabric
        EQ.1.0:	don't eliminate compressive stresses.  This option should not be used if retractors and sliprings are present in the model.
        EQ.2.0:	whether or not compressive stress is eliminated is decided by ls-dyna automatically, recommended for shell belt.
        """ # nopep8
        return self._cards[0].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        """Set the cse property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""cse must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[0].set_value("cse", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Cross sectional area for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def i(self) -> typing.Optional[float]:
        """Get or set the Area moment of inertia for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("i")

    @i.setter
    def i(self, value: float) -> None:
        """Set the i property."""
        self._cards[1].set_value("i", value)

    @property
    def j(self) -> typing.Optional[float]:
        """Get or set the Torsional constant for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("j")

    @j.setter
    def j(self, value: float) -> None:
        """Set the j property."""
        self._cards[1].set_value("j", value)

    @property
    def as_(self) -> typing.Optional[float]:
        """Get or set the Shear area for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("as_")

    @as_.setter
    def as_(self, value: float) -> None:
        """Set the as_ property."""
        self._cards[1].set_value("as_", value)

    @property
    def f(self) -> float:
        """Get or set the Maximum force in compression/tension.
        """ # nopep8
        return self._cards[1].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[1].set_value("f", value)

    @property
    def m(self) -> float:
        """Get or set the Maximum torque
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def r(self) -> float:
        """Get or set the Rotational mass scaling factor
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[1].set_value("r", value)

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
    def llcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for llcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.llcid:
                return kwd
        return None

    @llcid_link.setter
    def llcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for llcid."""
        self.llcid = value.lcid

    @property
    def ulcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for ulcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ulcid:
                return kwd
        return None

    @ulcid_link.setter
    def ulcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ulcid."""
        self.ulcid = value.lcid

