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

"""Module providing the SetPorousLagrangian class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_SETPOROUSLAGRANGIAN_CARD0 = (
    FieldSchema("eidbeg", int, 0, 10, None),
    FieldSchema("eidend", int, 10, 10, 0),
    FieldSchema("local", int, 20, 10, 0),
    FieldSchema("veccid1", int, 30, 10, 0),
    FieldSchema("veccid2", int, 40, 10, 0),
    FieldSchema("userdef", int, 50, 10, 0),
)

_SETPOROUSLAGRANGIAN_CARD1 = (
    FieldSchema("axx", float, 0, 10, 0.0),
    FieldSchema("axy", float, 10, 10, 0.0),
    FieldSchema("axz", float, 20, 10, 0.0),
    FieldSchema("bxx", float, 30, 10, 0.0),
    FieldSchema("bxy", float, 40, 10, 0.0),
    FieldSchema("bxz", float, 50, 10, 0.0),
)

_SETPOROUSLAGRANGIAN_CARD2 = (
    FieldSchema("ayx", float, 0, 10, 0.0),
    FieldSchema("ayy", float, 10, 10, 0.0),
    FieldSchema("ayz", float, 20, 10, 0.0),
    FieldSchema("byx", float, 30, 10, 0.0),
    FieldSchema("byy", float, 40, 10, 0.0),
    FieldSchema("byz", float, 50, 10, 0.0),
)

_SETPOROUSLAGRANGIAN_CARD3 = (
    FieldSchema("azx", float, 0, 10, 0.0),
    FieldSchema("azy", float, 10, 10, 0.0),
    FieldSchema("azz", float, 20, 10, 0.0),
    FieldSchema("bzx", float, 30, 10, 0.0),
    FieldSchema("bzy", float, 40, 10, 0.0),
    FieldSchema("bzz", float, 50, 10, 0.0),
)

_SETPOROUSLAGRANGIAN_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetPorousLagrangian(KeywordBase):
    """DYNA SET_POROUS_LAGRANGIAN keyword"""

    keyword = "SET"
    subkeyword = "POROUS_LAGRANGIAN"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "veccid1": LinkType.DEFINE_VECTOR,
        "veccid2": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the SetPorousLagrangian class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETPOROUSLAGRANGIAN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETPOROUSLAGRANGIAN_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETPOROUSLAGRANGIAN_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETPOROUSLAGRANGIAN_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetPorousLagrangian.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETPOROUSLAGRANGIAN_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eidbeg(self) -> typing.Optional[int]:
        """Get or set the EIDBEG is a set of thick porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidbeg")

    @eidbeg.setter
    def eidbeg(self, value: int) -> None:
        """Set the eidbeg property."""
        self._cards[0].set_value("eidbeg", value)

    @property
    def eidend(self) -> int:
        """Get or set the EIDEND is a set of thin porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidend")

    @eidend.setter
    def eidend(self, value: int) -> None:
        """Set the eidend property."""
        self._cards[0].set_value("eidend", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to activate an element coordinate system:
        EQ.0:The forces are applied in the global directions.
        EQ.1:The forces are applied in a local system attached to the element. The system is consistent with DIREC=1 and CTYPE=12 in *CONSTRAINED_LAGRANGE_IN_SOLID.
        For CTYPE=11, LOCAL is always 1 and the -axis is aligned with the element normal while the -axis passes through the element center and the first node in the element connectivity
        (*ELEMENT_BEAM in 2D or *ELEMENT_SHELL in 3D)
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        """Set the local property."""
        if value not in [0, 1, None]:
            raise Exception("""local must be `None` or one of {0,1}.""")
        self._cards[0].set_value("local", value)

    @property
    def veccid1(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1 and VECID2 give the x- and y-direction respectively.
        The z-vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a cross-	product of z- and x-vectors. The vectors are stored as isoparametric.
        """ # nopep8
        return self._cards[0].get_value("veccid1")

    @veccid1.setter
    def veccid1(self, value: int) -> None:
        """Set the veccid1 property."""
        self._cards[0].set_value("veccid1", value)

    @property
    def veccid2(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1 and VECID2 give the x- and y-direction respectively.
        The z-vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a cross-	product of z- and x-vectors. The vectors are stored as isoparametric.
        """ # nopep8
        return self._cards[0].get_value("veccid2")

    @veccid2.setter
    def veccid2(self, value: int) -> None:
        """Set the veccid2 property."""
        self._cards[0].set_value("veccid2", value)

    @property
    def userdef(self) -> int:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("userdef")

    @userdef.setter
    def userdef(self, value: int) -> None:
        """Set the userdef property."""
        self._cards[0].set_value("userdef", value)

    @property
    def axx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axx")

    @axx.setter
    def axx(self, value: float) -> None:
        """Set the axx property."""
        self._cards[1].set_value("axx", value)

    @property
    def axy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axy")

    @axy.setter
    def axy(self, value: float) -> None:
        """Set the axy property."""
        self._cards[1].set_value("axy", value)

    @property
    def axz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("axz")

    @axz.setter
    def axz(self, value: float) -> None:
        """Set the axz property."""
        self._cards[1].set_value("axz", value)

    @property
    def bxx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxx")

    @bxx.setter
    def bxx(self, value: float) -> None:
        """Set the bxx property."""
        self._cards[1].set_value("bxx", value)

    @property
    def bxy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxy")

    @bxy.setter
    def bxy(self, value: float) -> None:
        """Set the bxy property."""
        self._cards[1].set_value("bxy", value)

    @property
    def bxz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[1].get_value("bxz")

    @bxz.setter
    def bxz(self, value: float) -> None:
        """Set the bxz property."""
        self._cards[1].set_value("bxz", value)

    @property
    def ayx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayx")

    @ayx.setter
    def ayx(self, value: float) -> None:
        """Set the ayx property."""
        self._cards[2].set_value("ayx", value)

    @property
    def ayy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayy")

    @ayy.setter
    def ayy(self, value: float) -> None:
        """Set the ayy property."""
        self._cards[2].set_value("ayy", value)

    @property
    def ayz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("ayz")

    @ayz.setter
    def ayz(self, value: float) -> None:
        """Set the ayz property."""
        self._cards[2].set_value("ayz", value)

    @property
    def byx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byx")

    @byx.setter
    def byx(self, value: float) -> None:
        """Set the byx property."""
        self._cards[2].set_value("byx", value)

    @property
    def byy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byy")

    @byy.setter
    def byy(self, value: float) -> None:
        """Set the byy property."""
        self._cards[2].set_value("byy", value)

    @property
    def byz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[2].get_value("byz")

    @byz.setter
    def byz(self, value: float) -> None:
        """Set the byz property."""
        self._cards[2].set_value("byz", value)

    @property
    def azx(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azx")

    @azx.setter
    def azx(self, value: float) -> None:
        """Set the azx property."""
        self._cards[3].set_value("azx", value)

    @property
    def azy(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azy")

    @azy.setter
    def azy(self, value: float) -> None:
        """Set the azy property."""
        self._cards[3].set_value("azy", value)

    @property
    def azz(self) -> float:
        """Get or set the Viscous matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("azz")

    @azz.setter
    def azz(self, value: float) -> None:
        """Set the azz property."""
        self._cards[3].set_value("azz", value)

    @property
    def bzx(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzx")

    @bzx.setter
    def bzx(self, value: float) -> None:
        """Set the bzx property."""
        self._cards[3].set_value("bzx", value)

    @property
    def bzy(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzy")

    @bzy.setter
    def bzy(self, value: float) -> None:
        """Set the bzy property."""
        self._cards[3].set_value("bzy", value)

    @property
    def bzz(self) -> float:
        """Get or set the Inertial matrix for the porous flow Ergun equation.
        """ # nopep8
        return self._cards[3].get_value("bzz")

    @bzz.setter
    def bzz(self, value: float) -> None:
        """Set the bzz property."""
        self._cards[3].set_value("bzz", value)

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
    def veccid1_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for veccid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.veccid1:
                return kwd
        return None

    @veccid1_link.setter
    def veccid1_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for veccid1."""
        self.veccid1 = value.vid

    @property
    def veccid2_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for veccid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.veccid2:
                return kwd
        return None

    @veccid2_link.setter
    def veccid2_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for veccid2."""
        self.veccid2 = value.vid

