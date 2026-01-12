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

"""Module providing the DefinePorousLagrangian class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEPOROUSLAGRANGIAN_CARD0 = (
    FieldSchema("eidbeg", int, 0, 10, None),
    FieldSchema("eidend", int, 10, 10, 0),
    FieldSchema("local", int, 20, 10, 0),
    FieldSchema("vecid1", int, 30, 10, 0),
    FieldSchema("vecid2", int, 40, 10, 0),
    FieldSchema("userdef", int, 50, 10, 0),
)

_DEFINEPOROUSLAGRANGIAN_CARD1 = (
    FieldSchema("axx", float, 0, 10, 0.0),
    FieldSchema("axy", float, 10, 10, 0.0),
    FieldSchema("axz", float, 20, 10, 0.0),
    FieldSchema("bxx", float, 30, 10, 0.0),
    FieldSchema("bxy", float, 40, 10, 0.0),
    FieldSchema("bxz", float, 50, 10, 0.0),
)

_DEFINEPOROUSLAGRANGIAN_CARD2 = (
    FieldSchema("ayx", float, 0, 10, 0.0),
    FieldSchema("ayy", float, 10, 10, 0.0),
    FieldSchema("ayz", float, 20, 10, 0.0),
    FieldSchema("byx", float, 30, 10, 0.0),
    FieldSchema("byy", float, 40, 10, 0.0),
    FieldSchema("byz", float, 50, 10, 0.0),
)

_DEFINEPOROUSLAGRANGIAN_CARD3 = (
    FieldSchema("azx", float, 0, 10, 0.0),
    FieldSchema("azy", float, 10, 10, 0.0),
    FieldSchema("azz", float, 20, 10, 0.0),
    FieldSchema("bzx", float, 30, 10, 0.0),
    FieldSchema("bzy", float, 40, 10, 0.0),
    FieldSchema("bzz", float, 50, 10, 0.0),
)

class DefinePorousLagrangian(KeywordBase):
    """DYNA DEFINE_POROUS_LAGRANGIAN keyword"""

    keyword = "DEFINE"
    subkeyword = "POROUS_LAGRANGIAN"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefinePorousLagrangian class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEPOROUSLAGRANGIAN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPOROUSLAGRANGIAN_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPOROUSLAGRANGIAN_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPOROUSLAGRANGIAN_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefinePorousLagrangian.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eidbeg(self) -> typing.Optional[int]:
        """Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
        EIDBEG, EIDEND < 0: Range of thin porous element IDs.	These are shells in 3D and beams in 2D.
        The ALE option does not support thin porous elements.
        EIDBEG > 0, EIDEND = 0: EIDBEG is a set of thick porous
        elements
        EIDBEG > 0, EIDEND < 0: EIDBEG is a set of thin porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidbeg")

    @eidbeg.setter
    def eidbeg(self, value: int) -> None:
        """Set the eidbeg property."""
        self._cards[0].set_value("eidbeg", value)

    @property
    def eidend(self) -> int:
        """Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
        EIDBEG, EIDEND < 0: Range of thin porous element IDs.	These are shells in 3D and beams in 2D.
        The ALE option does not support thin porous elements.
        EIDBEG > 0, EIDEND = 0: EIDBEG is a set of thick porous
        elements
        EIDBEG > 0, EIDEND < 0: EIDBEG is a set of thin porous elements.
        """ # nopep8
        return self._cards[0].get_value("eidend")

    @eidend.setter
    def eidend(self, value: int) -> None:
        """Set the eidend property."""
        self._cards[0].set_value("eidend", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to activate an element coordinate system:
        EQ.0: The forces are applied in the global directions.
        EQ.1: The forces are applied in a local system attached to the
        element. The system is consistent with DIREC = 1 and
        CTYPE = 12 in *CONSTRAINED_LAGRANGE_IN_SOLID.
        For CTYPE = 11, LOCAL is always 1 and the 𝑥-axis is
        aligned with the element normal while the 𝑦-axis passes
        through the element center and the first node in the element
        connectivity (*ELEMENT_BEAM in 2D or *ELEMENT_SHELL in 3D).
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        """Set the local property."""
        if value not in [0, 1, None]:
            raise Exception("""local must be `None` or one of {0,1}.""")
        self._cards[0].set_value("local", value)

    @property
    def vecid1(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
        and VECID2 give the 𝑥- and 𝑦-direction respectively. The 𝑧-
        vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a crossproduct
        of 𝑧- and 𝑥-vectors. The vectors are stored as isoparametric
        locations to update their directions if the element deforms or rotates.
        """ # nopep8
        return self._cards[0].get_value("vecid1")

    @vecid1.setter
    def vecid1(self, value: int) -> None:
        """Set the vecid1 property."""
        self._cards[0].set_value("vecid1", value)

    @property
    def vecid2(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
        and VECID2 give the 𝑥- and 𝑦-direction respectively. The 𝑧-
        vector is a cross product of VECID1 and VECID2. If this latter is not
        orthogonal to VECID1, its direction will be corrected with a crossproduct
        of 𝑧- and 𝑥-vectors. The vectors are stored as isoparametric
        locations to update their directions if the element deforms or rotates.
        """ # nopep8
        return self._cards[0].get_value("vecid2")

    @vecid2.setter
    def vecid2(self, value: int) -> None:
        """Set the vecid2 property."""
        self._cards[0].set_value("vecid2", value)

    @property
    def userdef(self) -> int:
        """Get or set the Flag to compute Aij and Bij with a user defined routine in the file
        dyn21.F called lagpor_getab_userdef. The file is part of the general package usermat..
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

