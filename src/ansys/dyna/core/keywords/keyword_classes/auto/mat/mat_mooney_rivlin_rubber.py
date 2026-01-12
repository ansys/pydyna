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

"""Module providing the MatMooneyRivlinRubber class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATMOONEYRIVLINRUBBER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pr", float, 20, 10, None),
    FieldSchema("a", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("ref", float, 50, 10, 0.0),
)

_MATMOONEYRIVLINRUBBER_CARD1 = (
    FieldSchema("sgl", float, 0, 10, None),
    FieldSchema("sw", float, 10, 10, None),
    FieldSchema("st", float, 20, 10, None),
    FieldSchema("lcid", int, 30, 10, None),
)

class MatMooneyRivlinRubber(KeywordBase):
    """DYNA MAT_MOONEY-RIVLIN_RUBBER keyword"""

    keyword = "MAT"
    subkeyword = "MOONEY-RIVLIN_RUBBER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatMooneyRivlinRubber class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATMOONEYRIVLINRUBBER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMOONEYRIVLINRUBBER_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatMooneyRivlinRubber.option_specs[0],
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (> 0.49 is recommended, smaller values may not work).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor, see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded solid elements with one point integration):
        EQ.0.0: off (default),
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
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0.
        """ # nopep8
        return self._cards[1].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        """Set the sgl property."""
        self._cards[1].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen  gauge width.
        """ # nopep8
        return self._cards[1].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        """Set the sw property."""
        self._cards[1].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge thickness.
        """ # nopep8
        return self._cards[1].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        """Set the st property."""
        self._cards[1].set_value("st", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, giving the force versus actual change dL in the gauge length.
        For stress versus strain curve definition set SGL, SW, ST to 1.0..
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

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

