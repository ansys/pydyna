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

"""Module providing the DefineFibers class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DEFINEFIBERS_CARD0 = (
    FieldSchema("idf", int, 0, 10, None),
    FieldSchema("idp", int, 10, 10, None),
    FieldSchema("numf", int, 20, 10, None),
    FieldSchema("n1", int, 30, 10, None),
    FieldSchema("n2", int, 40, 10, None),
    FieldSchema("efb", float, 50, 10, None),
    FieldSchema("shr", float, 50, 10, None),
    FieldSchema("hrgls", float, 50, 10, 1.0),
)

_DEFINEFIBERS_CARD1 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
)

_DEFINEFIBERS_CARD2 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("z1", float, 20, 10, None),
    FieldSchema("x2", float, 30, 10, None),
    FieldSchema("y2", float, 40, 10, None),
    FieldSchema("z2", float, 50, 10, None),
)

_DEFINEFIBERS_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFibers(KeywordBase):
    """DYNA DEFINE_FIBERS keyword"""

    keyword = "DEFINE"
    subkeyword = "FIBERS"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "idp": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineFibers class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFIBERS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFIBERS_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEFIBERS_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFibers.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFIBERS_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def idf(self) -> typing.Optional[int]:
        """Get or set the ID of a fiber set to be defined, must be unique number.
        """ # nopep8
        return self._cards[0].get_value("idf")

    @idf.setter
    def idf(self, value: int) -> None:
        """Set the idf property."""
        self._cards[0].set_value("idf", value)

    @property
    def idp(self) -> typing.Optional[int]:
        """Get or set the Part ID of the matrix material associated with the fiber set.
        """ # nopep8
        return self._cards[0].get_value("idp")

    @idp.setter
    def idp(self, value: int) -> None:
        """Set the idp property."""
        self._cards[0].set_value("idp", value)

    @property
    def numf(self) -> typing.Optional[int]:
        """Get or set the Number of fiber orientations.
        """ # nopep8
        return self._cards[0].get_value("numf")

    @numf.setter
    def numf(self, value: int) -> None:
        """Set the numf property."""
        self._cards[0].set_value("numf", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def efb(self) -> typing.Optional[float]:
        """Get or set the Effective stiffness of the fiber in its orientation, which typically equals to
        Young’s Modulus times fiber cross sectional area fraction.
        Fiber cross sectional area fraction is typically between 0.25 and 0.5.
        """ # nopep8
        return self._cards[0].get_value("efb")

    @efb.setter
    def efb(self, value: float) -> None:
        """Set the efb property."""
        self._cards[0].set_value("efb", value)

    @property
    def shr(self) -> typing.Optional[float]:
        """Get or set the Shear stiffness of the fiber:
        GT.0:	shear stiffness,
        LT.0:	|SHR| is theload curve ID defining shear stiffness vs. shear strain.
        """ # nopep8
        return self._cards[0].get_value("shr")

    @shr.setter
    def shr(self, value: float) -> None:
        """Set the shr property."""
        self._cards[0].set_value("shr", value)

    @property
    def hrgls(self) -> float:
        """Get or set the Hourglass coefficient for stiffness type hourglass control. Default=1.0.
        """ # nopep8
        return self._cards[0].get_value("hrgls")

    @hrgls.setter
    def hrgls(self, value: float) -> None:
        """Set the hrgls property."""
        self._cards[0].set_value("hrgls", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[1].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[1].set_value("alpha3", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[2].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[2].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[2].set_value("z2", value)

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
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def idp_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given idp."""
        return self._get_link_by_attr("PART", "pid", self.idp, "parts")

