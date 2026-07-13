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

"""Module providing the DefineCpgRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECPGREGION_CARD0 = (
    FieldSchema("drid", int, 0, 10, None),
    FieldSchema("drtyp", int, 10, 10, None),
)

_DEFINECPGREGION_CARD1 = (
    FieldSchema("p1x", float, 0, 10, 0.0),
    FieldSchema("p1y", float, 10, 10, 0.0),
    FieldSchema("p1z", float, 20, 10, 0.0),
    FieldSchema("p2x", float, 30, 10, 0.0),
    FieldSchema("p2y", float, 40, 10, 0.0),
    FieldSchema("p2z", float, 50, 10, 0.0),
    FieldSchema("rad", float, 60, 10, 0.0),
)

_DEFINECPGREGION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCpgRegion(KeywordBase):
    """DYNA DEFINE_CPG_REGION keyword"""

    keyword = "DEFINE"
    subkeyword = "CPG_REGION"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCpgRegion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECPGREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINECPGREGION_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineCpgRegion._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECPGREGION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def drid(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this region
        """ # nopep8
        return self._cards[0].get_value("drid")

    @drid.setter
    def drid(self, value: int) -> None:
        """Set the drid property."""
        self._cards[0].set_value("drid", value)

    @property
    def drtyp(self) -> typing.Optional[int]:
        """Get or set the Region geometry type:
        EQ.1: Domain divided by a section plane that acts as a cut - off.The plane�s normal vector points outwards from the region.
        EQ.2: Box
        EQ.3: Cylinder
        EQ.4: Sphere
        """ # nopep8
        return self._cards[0].get_value("drtyp")

    @drtyp.setter
    def drtyp(self, value: int) -> None:
        """Set the drtyp property."""
        self._cards[0].set_value("drtyp", value)

    @property
    def p1x(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p1x")

    @p1x.setter
    def p1x(self, value: float) -> None:
        """Set the p1x property."""
        self._cards[1].set_value("p1x", value)

    @property
    def p1y(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p1y")

    @p1y.setter
    def p1y(self, value: float) -> None:
        """Set the p1y property."""
        self._cards[1].set_value("p1y", value)

    @property
    def p1z(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p1z")

    @p1z.setter
    def p1z(self, value: float) -> None:
        """Set the p1z property."""
        self._cards[1].set_value("p1z", value)

    @property
    def p2x(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p2x")

    @p2x.setter
    def p2x(self, value: float) -> None:
        """Set the p2x property."""
        self._cards[1].set_value("p2x", value)

    @property
    def p2y(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p2y")

    @p2y.setter
    def p2y(self, value: float) -> None:
        """Set the p2y property."""
        self._cards[1].set_value("p2y", value)

    @property
    def p2z(self) -> float:
        """Get or set the Points with meaning that depend on DRTYP:
        DRTYP.EQ.1: Plane normal and origin point
        DRTYP.EQ.2: Minimum and maximum coordinates of the box
        DRTYP.EQ.3: Points defining the center axis
        DRTYP.EQ.4: Origin of the sphere (second point not used)

        """ # nopep8
        return self._cards[1].get_value("p2z")

    @p2z.setter
    def p2z(self, value: float) -> None:
        """Set the p2z property."""
        self._cards[1].set_value("p2z", value)

    @property
    def rad(self) -> float:
        """Get or set the Cylinder or sphere radius if DRTYP = 3 or 4, respectively
        """ # nopep8
        return self._cards[1].get_value("rad")

    @rad.setter
    def rad(self, value: float) -> None:
        """Set the rad property."""
        self._cards[1].set_value("rad", value)

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

