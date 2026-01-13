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

"""Module providing the MatMooney_RivlinPhaseChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATMOONEY_RIVLINPHASECHANGE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pr1", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("b1", float, 40, 10, None),
    FieldSchema("ref", float, 50, 10, 0.0),
)

_MATMOONEY_RIVLINPHASECHANGE_CARD1 = (
    FieldSchema("sgl1", float, 0, 10, None),
    FieldSchema("sw1", float, 10, 10, None),
    FieldSchema("st1", float, 20, 10, None),
    FieldSchema("lcid1", float, 30, 10, None),
)

_MATMOONEY_RIVLINPHASECHANGE_CARD2 = (
    FieldSchema("unused", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("pr2", float, 20, 10, None),
    FieldSchema("a2", float, 30, 10, None),
    FieldSchema("b2", float, 40, 10, None),
)

_MATMOONEY_RIVLINPHASECHANGE_CARD3 = (
    FieldSchema("sgl1", float, 0, 10, None),
    FieldSchema("sw1", float, 10, 10, None),
    FieldSchema("st1", float, 20, 10, None),
    FieldSchema("lcid1", float, 30, 10, None),
)

_MATMOONEY_RIVLINPHASECHANGE_CARD4 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
    FieldSchema("x2", float, 30, 10, None),
    FieldSchema("y2", float, 40, 10, None),
    FieldSchema("z2", float, 50, 10, None),
    FieldSchema("thkfac", float, 60, 10, 1.0),
)

_MATMOONEY_RIVLINPHASECHANGE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatMooney_RivlinPhaseChange(KeywordBase):
    """DYNA MAT_MOONEY-RIVLIN_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "MOONEY-RIVLIN_PHASE_CHANGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatMooney_RivlinPhaseChange class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATMOONEY_RIVLINPHASECHANGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMOONEY_RIVLINPHASECHANGE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMOONEY_RIVLINPHASECHANGE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMOONEY_RIVLINPHASECHANGE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMOONEY_RIVLINPHASECHANGE_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatMooney_RivlinPhaseChange.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATMOONEY_RIVLINPHASECHANGE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label not exceeding 8	characters must be specified.
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
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (value between 0.49 and 0.5 is recommended, smaller values may not work) where i indicates the phase.
        """ # nopep8
        return self._cards[0].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        """Set the pr1 property."""
        self._cards[0].set_value("pr1", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined below.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined	below.
        """ # nopep8
        return self._cards[0].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[0].set_value("b1", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference
        geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ref", value)

    @property
    def sgl1(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0 for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("sgl1")

    @sgl1.setter
    def sgl1(self, value: float) -> None:
        """Set the sgl1 property."""
        self._cards[1].set_value("sgl1", value)

    @property
    def sw1(self) -> typing.Optional[float]:
        """Get or set the Specimen width for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("sw1")

    @sw1.setter
    def sw1(self, value: float) -> None:
        """Set the sw1 property."""
        self._cards[1].set_value("sw1", value)

    @property
    def st1(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("st1")

    @st1.setter
    def st1(self, value: float) -> None:
        """Set the st1 property."""
        self._cards[1].set_value("st1", value)

    @property
    def lcid1(self) -> typing.Optional[float]:
        """Get or set the Curve ID for the i th phase, see *DEFINE_CURVE, giving the force versus actual change delta L in the gauge length. See also Figure M218-2
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: float) -> None:
        """Set the lcid1 property."""
        self._cards[1].set_value("lcid1", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (value between 0.49 and 0.5 is recommended, smaller values may not work) where i indicates the phase
        """ # nopep8
        return self._cards[2].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        """Set the pr2 property."""
        self._cards[2].set_value("pr2", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined below
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined	below
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[2].set_value("b2", value)

    @property
    def sgl1(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0 for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("sgl1")

    @sgl1.setter
    def sgl1(self, value: float) -> None:
        """Set the sgl1 property."""
        self._cards[3].set_value("sgl1", value)

    @property
    def sw1(self) -> typing.Optional[float]:
        """Get or set the Specimen width for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("sw1")

    @sw1.setter
    def sw1(self, value: float) -> None:
        """Set the sw1 property."""
        self._cards[3].set_value("sw1", value)

    @property
    def st1(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("st1")

    @st1.setter
    def st1(self, value: float) -> None:
        """Set the st1 property."""
        self._cards[3].set_value("st1", value)

    @property
    def lcid1(self) -> typing.Optional[float]:
        """Get or set the Curve ID for the i th phase, see *DEFINE_CURVE, giving the force versus actual change delta L in the gauge length. See also Figure M218-2
        """ # nopep8
        return self._cards[3].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: float) -> None:
        """Set the lcid1 property."""
        self._cards[3].set_value("lcid1", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[4].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[4].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[4].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[4].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[4].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[4].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[4].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[4].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[4].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[4].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        """Set the thkfac property."""
        self._cards[4].set_value("thkfac", value)

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

