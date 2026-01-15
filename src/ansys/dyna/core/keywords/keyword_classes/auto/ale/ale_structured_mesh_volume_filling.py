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

"""Module providing the AleStructuredMeshVolumeFilling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALESTRUCTUREDMESHVOLUMEFILLING_CARD0 = (
    FieldSchema("mshid", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("ammgto", str, 20, 10, "0"),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("nsample", int, 40, 10, 3),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused_", int, 60, 10, None, "unused-"),
    FieldSchema("vid", int, 70, 10, 0),
)

_ALESTRUCTUREDMESHVOLUMEFILLING_CARD1 = (
    FieldSchema("geom", str, 0, 10, "ALL"),
    FieldSchema("in_out", int, 10, 10, 0, "in/out"),
    FieldSchema("e1", float, 20, 10, None),
    FieldSchema("e2", float, 30, 10, None),
    FieldSchema("e3", float, 40, 10, None),
    FieldSchema("e4", float, 50, 10, None),
    FieldSchema("e5", float, 60, 10, None),
)

class AleStructuredMeshVolumeFilling(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_VOLUME_FILLING keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_VOLUME_FILLING"

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMeshVolumeFilling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHVOLUMEFILLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESHVOLUMEFILLING_CARD1,
                **kwargs,
            ),        ]
    @property
    def mshid(self) -> int:
        """Get or set the S-ALE Mesh ID. A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def ammgto(self) -> str:
        """Get or set the The ID of AMMG filling the geometry. See *ALE_MULTI-MATERIAL_GROUP for reference.
        """ # nopep8
        return self._cards[0].get_value("ammgto")

    @ammgto.setter
    def ammgto(self, value: str) -> None:
        """Set the ammgto property."""
        self._cards[0].set_value("ammgto", value)

    @property
    def nsample(self) -> int:
        """Get or set the Number of sampling points.  In case an element is partially filled, in each direction, 2 * NSAMPLE + 1 points are generated.
        These (2*"NSAMPLE" +1)^3 points, each representing a volume, are used to determine if its volume is in or out.
        """ # nopep8
        return self._cards[0].get_value("nsample")

    @nsample.setter
    def nsample(self, value: int) -> None:
        """Set the nsample property."""
        self._cards[0].set_value("nsample", value)

    @property
    def unused_(self) -> typing.Optional[int]:
        """Get or set the -.
        """ # nopep8
        return self._cards[0].get_value("unused_")

    @unused_.setter
    def unused_(self, value: int) -> None:
        """Set the unused_ property."""
        self._cards[0].set_value("unused_", value)

    @property
    def vid(self) -> int:
        """Get or set the ID of *DEFINE_VECTOR card.  This flag is used to assign initial velocity to material filling the domain.
        Field 2 to 5 (XT, YT, ZT) of the *DEFINE_VECTOR card are used to define the initial translational velocities.  Please refer to Example 1 below for usage.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def geom(self) -> str:
        """Get or set the Geometry types. They are: PARTSET, PART, SEGSET, PLANE, CYLINDER, BOXCOR, BOXCPT and SPHERE.
        See the table below for more details.
        """ # nopep8
        return self._cards[1].get_value("geom")

    @geom.setter
    def geom(self, value: str) -> None:
        """Set the geom property."""
        if value not in ["ALL", "PARTSET", "PART", "SEGSET", "PLANE", "CYLINDER", "BOXCOR", "BOXCPT", "ELLIPSOID", None]:
            raise Exception("""geom must be `None` or one of {"ALL","PARTSET","PART","SEGSET","PLANE","CYLINDER","BOXCOR","BOXCPT","ELLIPSOID"}.""")
        self._cards[1].set_value("geom", value)

    @property
    def in_out(self) -> int:
        """Get or set the To fill inside or outside of the geometry.  For PARTSET‌ / PART / SEGSET options, inside is taken as in the normal direction of the container’s segments (see Remark 1).
        EQ.0:	Inside(default)
        EQ.1 : Outside
        """ # nopep8
        return self._cards[1].get_value("in_out")

    @in_out.setter
    def in_out(self, value: int) -> None:
        """Set the in_out property."""
        if value not in [0, 1, None]:
            raise Exception("""in_out must be `None` or one of {0,1}.""")
        self._cards[1].set_value("in_out", value)

    @property
    def e1(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: float) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: float) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[float]:
        """Get or set the These values have different definitions for different options.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: float) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

