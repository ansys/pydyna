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

"""Module providing the EfvMappingFct class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVMAPPINGFCT_CARD0 = (
    FieldSchema("setid", int, 0, 10, None),
    FieldSchema("rw", int, 10, 10, None),
    FieldSchema("rkfct", int, 20, 10, 1),
    FieldSchema("dt", float, 30, 10, None),
)

_EFVMAPPINGFCT_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
    FieldSchema("t", float, 30, 10, None),
)

_EFVMAPPINGFCT_CARD2 = (
    FieldSchema("axi", int, 0, 10, 1),
)

class EfvMappingFct(KeywordBase):
    """DYNA EFV_MAPPING_FCT keyword"""

    keyword = "EFV"
    subkeyword = "MAPPING_FCT"

    def __init__(self, **kwargs):
        """Initialize the EfvMappingFct class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVMAPPINGFCT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVMAPPINGFCT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVMAPPINGFCT_CARD2,
                **kwargs,
            ),
        ]
    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_STRUCTURED_MESH.  This field only applies to writing the mapping file (RW = 1). If *EFV_MAPPING is invoked in an ALE input deck, SETID is the ID of a *SET_MULTI-MATERIAL_GROUP_LIST that lists the ALE groups (materials) written to a mapping file to be read in a subsequent Efv FCT or Eulerian run. Note that the multi-material groups are mapped to the FCT or Eulerian parts in the Finite Volume Euler model such that the nth multi-material group in the set is mapped to the nth Efv FCT or Eulerian as it appears in the input deck reading the mapping file.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def rw(self) -> typing.Optional[int]:
        """Get or set the Read or Write flag:
        EQ.1: Write the mapping file defined by the user after efvmapw = on the command line
        EQ.2: Read the mapping file defined by the user after efvnmapr = on the command line
        """ # nopep8
        return self._cards[0].get_value("rw")

    @rw.setter
    def rw(self, value: int) -> None:
        """Set the rw property."""
        self._cards[0].set_value("rw", value)

    @property
    def rkfct(self) -> int:
        """Get or set the Position of the Finite Volume Euler FCT part with respect to other Efv parts in the input deck reading the mapping file. This field only applies when writing the mapping file (RW = 1) while using the FCT keyword option for this keyword in the writing input deck. It is only useful when mapping to an RCT part in a subsequent run. For example, if the FCT part is the fifth Efv part by location in the reading input deck, RKFCT = 5 for this keyword in the writing input deck. This subsequent model has most likely only 1 Efv FCT part. Thus, RKFCT = 1 should work in most cases.
        """ # nopep8
        return self._cards[0].get_value("rkfct")

    @rkfct.setter
    def rkfct(self, value: int) -> None:
        """Set the rkfct property."""
        self._cards[0].set_value("rkfct", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval between outputs of the mapping files (only for RW=1):
        GT.0: Time interval
        LT.0: |DT| is the number of cycles between outputs
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of the origin of the previous run that created the mapping file (only for RW=2)
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of the origin of the previous run that created the mapping file (only for RW=2)
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of the origin of the previous run that created the mapping file (only for RW=2)
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Initial time of the current run(only for RW = 2)
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def axi(self) -> int:
        """Get or set the Flag defining the symmetry axis if the previous run was 2D axisymmetric (only for RW=2). AXI selects one of the 3 global axis:
        EQ.1: x - axis
        EQ.2: y - axis
        EQ.3: z - axis
        """ # nopep8
        return self._cards[2].get_value("axi")

    @axi.setter
    def axi(self, value: int) -> None:
        """Set the axi property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""axi must be `None` or one of {1,2,3}.""")
        self._cards[2].set_value("axi", value)

