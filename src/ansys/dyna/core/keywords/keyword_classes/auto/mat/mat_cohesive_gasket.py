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

"""Module providing the MatCohesiveGasket class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATCOHESIVEGASKET_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("roflg", int, 20, 10, 0),
    FieldSchema("intfail", int, 30, 10, None),
)

_MATCOHESIVEGASKET_CARD1 = (
    FieldSchema("lc", int, 0, 10, None),
    FieldSchema("uc", int, 10, 10, None),
    FieldSchema("eten", float, 20, 10, None),
)

_MATCOHESIVEGASKET_CARD2 = (
    FieldSchema("etsr", float, 0, 10, None),
)

_MATCOHESIVEGASKET_CARD3 = (
    FieldSchema("emem", float, 0, 10, None),
    FieldSchema("pr", float, 10, 10, None),
    FieldSchema("ps", int, 20, 10, 0),
)

_MATCOHESIVEGASKET_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCohesiveGasket(KeywordBase):
    """DYNA MAT_COHESIVE_GASKET keyword"""

    keyword = "MAT"
    subkeyword = "COHESIVE_GASKET"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lc": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatCohesiveGasket class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEGASKET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEGASKET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEGASKET_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCOHESIVEGASKET_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCohesiveGasket.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCOHESIVEGASKET_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def roflg(self) -> int:
        """Get or set the Flag for whether density is specified per unit area or volume:
        EQ.0:	Density is per unit volume(default).
        EQ.1 : Density is per unit area for controlling the mass of cohesive elements with an initial volume of zero
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: int) -> None:
        """Set the roflg property."""
        if value not in [0, 1, None]:
            raise Exception("""roflg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[int]:
        """Get or set the Quadrature rule. Note that this material has no failure
        LE.0.0:	2 x 2 Newton - Cotes quadrature.
        GT.0.0 : 2 x 2 Gaussian quadrature
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: int) -> None:
        """Set the intfail property."""
        self._cards[0].set_value("intfail", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Main load curve ID defining the pressure as function of closure, p = p(c)
        """ # nopep8
        return self._cards[1].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        """Set the lc property."""
        self._cards[1].set_value("lc", value)

    @property
    def uc(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the unloading curves
        """ # nopep8
        return self._cards[1].get_value("uc")

    @uc.setter
    def uc(self, value: int) -> None:
        """Set the uc property."""
        self._cards[1].set_value("uc", value)

    @property
    def eten(self) -> typing.Optional[float]:
        """Get or set the Tensile stiffness
        """ # nopep8
        return self._cards[1].get_value("eten")

    @eten.setter
    def eten(self, value: float) -> None:
        """Set the eten property."""
        self._cards[1].set_value("eten", value)

    @property
    def etsr(self) -> typing.Optional[float]:
        """Get or set the Transverse shear stiffness
        """ # nopep8
        return self._cards[2].get_value("etsr")

    @etsr.setter
    def etsr(self, value: float) -> None:
        """Set the etsr property."""
        self._cards[2].set_value("etsr", value)

    @property
    def emem(self) -> typing.Optional[float]:
        """Get or set the Membrane stiffness
        """ # nopep8
        return self._cards[3].get_value("emem")

    @emem.setter
    def emem(self, value: float) -> None:
        """Set the emem property."""
        self._cards[3].set_value("emem", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Membrane Poisson ratio
        """ # nopep8
        return self._cards[3].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[3].set_value("pr", value)

    @property
    def ps(self) -> int:
        """Get or set the Membrane plane stress or plain strain assumption:
        EQ.0:	Plane stress(default)
        EQ.1 : Plane strain
        """ # nopep8
        return self._cards[3].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        """Set the ps property."""
        if value not in [0, 1, None]:
            raise Exception("""ps must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ps", value)

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

    @property
    def lc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc:
                return kwd
        return None

    @lc_link.setter
    def lc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc."""
        self.lc = value.lcid

