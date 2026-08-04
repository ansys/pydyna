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

"""Module providing the DefineSphAmbientDrag class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINESPHAMBIENTDRAG_CARD0 = (
    FieldSchema("icpl", int, 0, 10, 0),
    FieldSchema("vx", float, 10, 10, 0.0),
    FieldSchema("vy", float, 20, 10, 0.0),
    FieldSchema("vz", float, 30, 10, 0.0),
    FieldSchema("rhoa", float, 40, 10, None),
    FieldSchema("mua", float, 50, 10, None),
    FieldSchema("sftens", float, 60, 10, None),
)

_DEFINESPHAMBIENTDRAG_CARD1 = (
    FieldSchema("filename", str, 0, 60, None),
)

_DEFINESPHAMBIENTDRAG_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphAmbientDrag(KeywordBase):
    """DYNA DEFINE_SPH_AMBIENT_DRAG keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_AMBIENT_DRAG"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineSphAmbientDrag class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHAMBIENTDRAG_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPHAMBIENTDRAG_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSphAmbientDrag._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHAMBIENTDRAG_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def icpl(self) -> int:
        """Get or set the Coupling with ICFD:
        EQ.0: No coupling.
        EQ.1: Import ambient velocity field from a steady-state CFD analysis in a profile file format. Provide the file name in Card 2. See Example 1 below for a sample profile file.
        """ # nopep8
        return self._cards[0].get_value("icpl")

    @icpl.setter
    def icpl(self, value: int) -> None:
        """Set the icpl property."""
        if value not in [0, 1, None]:
            raise Exception("""icpl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("icpl", value)

    @property
    def vx(self) -> float:
        """Get or set the X-velocity of the ambient material
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Y-velocity of the ambient material
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Z-velocity of the ambient material
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def rhoa(self) -> typing.Optional[float]:
        """Get or set the Density of the ambient material
        """ # nopep8
        return self._cards[0].get_value("rhoa")

    @rhoa.setter
    def rhoa(self, value: float) -> None:
        """Set the rhoa property."""
        self._cards[0].set_value("rhoa", value)

    @property
    def mua(self) -> typing.Optional[float]:
        """Get or set the Viscosity of the ambient material
        """ # nopep8
        return self._cards[0].get_value("mua")

    @mua.setter
    def mua(self, value: float) -> None:
        """Set the mua property."""
        self._cards[0].set_value("mua", value)

    @property
    def sftens(self) -> typing.Optional[float]:
        """Get or set the Surface tension coefficient for the interface between the SPH fluid and ambient materials
        """ # nopep8
        return self._cards[0].get_value("sftens")

    @sftens.setter
    def sftens(self, value: float) -> None:
        """Set the sftens property."""
        self._cards[0].set_value("sftens", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing steady-state CFD data for the ambient air velocity field.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

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

