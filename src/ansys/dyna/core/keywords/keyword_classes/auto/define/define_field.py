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

"""Module providing the DefineField class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFIELD_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("pcid", int, 10, 10, None),
    FieldSchema("nv", int, 20, 10, None),
    FieldSchema("lmtsrc", int, 30, 10, 0),
    FieldSchema("srcrad", float, 40, 10, 0.0),
    FieldSchema("reflen", float, 50, 10, 0.0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DEFINEFIELD_CARD1 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("v4", float, 30, 10, None),
    FieldSchema("v5", float, 40, 10, None),
    FieldSchema("v6", float, 50, 10, None),
    FieldSchema("v7", float, 60, 10, None),
    FieldSchema("v8", float, 70, 10, None),
)

_DEFINEFIELD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineField(KeywordBase):
    """DYNA DEFINE_FIELD keyword"""

    keyword = "DEFINE"
    subkeyword = "FIELD"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineField class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFIELD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEFIELD_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineField._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFIELD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Field ID. A unique ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def pcid(self) -> typing.Optional[int]:
        """Get or set the ID of the point cloud referenced by this field. See keyword *DEFINE_POINT_CLOUD.
        """ # nopep8
        return self._cards[0].get_value("pcid")

    @pcid.setter
    def pcid(self, value: int) -> None:
        """Set the pcid property."""
        self._cards[0].set_value("pcid", value)

    @property
    def nv(self) -> typing.Optional[int]:
        """Get or set the Number of data specified for each point of the point cloud PCID. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("nv")

    @nv.setter
    def nv(self, value: int) -> None:
        """Set the nv property."""
        self._cards[0].set_value("nv", value)

    @property
    def lmtsrc(self) -> int:
        """Get or set the Flag to restrict the interpolation domain to a subset of data points of the point cloud PCID. This flag activate a search algorithm that restricts the interpolation domain based on a point-to-element distance criterion. For any element associated with a *PART_FIELD that references this field FID:
        EQ.0: Perform global interpolation by interpolating field data from all data points of point cloud PCID.
        EQ.1: Restrict the interpolation domain and search for all data points, of point cloud PCID, whose distance from centroid of the element is <= a distance determined by SRCRAD
        """ # nopep8
        return self._cards[0].get_value("lmtsrc")

    @lmtsrc.setter
    def lmtsrc(self, value: int) -> None:
        """Set the lmtsrc property."""
        if value not in [0, 1, None]:
            raise Exception("""lmtsrc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("lmtsrc", value)

    @property
    def srcrad(self) -> float:
        """Get or set the Radius of the interpolation domain. If LMTSRC = 0, this parameter is disregarded. If LMTSRC = 1, this parameter allows to restrict the interpolation domain. Based on the value of SRCRAD, the interpolation domain is restricted to all data points whose distance from the element centroid is:
        LT. 0.0: less than or equal to |SRCRAD| * REFLEN.
        EQ.0.0: Less than or equal to REFLEN.
        GT.0.0: Less than or equal to SRCRAD.
        """ # nopep8
        return self._cards[0].get_value("srcrad")

    @srcrad.setter
    def srcrad(self, value: float) -> None:
        """Set the srcrad property."""
        self._cards[0].set_value("srcrad", value)

    @property
    def reflen(self) -> float:
        """Get or set the Reference length used for scaling of radial basis functions and, if required, to restrict the interpolation domain:
        EQ.0.0: For any element, the solver automatically computes a reference length..
        GT.0.0: User - defined reference length.Any value can be specified.
        """ # nopep8
        return self._cards[0].get_value("reflen")

    @reflen.setter
    def reflen(self, value: float) -> None:
        """Set the reflen property."""
        self._cards[0].set_value("reflen", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[1].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[1].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[1].set_value("v3", value)

    @property
    def v4(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v4")

    @v4.setter
    def v4(self, value: float) -> None:
        """Set the v4 property."""
        self._cards[1].set_value("v4", value)

    @property
    def v5(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v5")

    @v5.setter
    def v5(self, value: float) -> None:
        """Set the v5 property."""
        self._cards[1].set_value("v5", value)

    @property
    def v6(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v6")

    @v6.setter
    def v6(self, value: float) -> None:
        """Set the v6 property."""
        self._cards[1].set_value("v6", value)

    @property
    def v7(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v7")

    @v7.setter
    def v7(self, value: float) -> None:
        """Set the v7 property."""
        self._cards[1].set_value("v7", value)

    @property
    def v8(self) -> typing.Optional[float]:
        """Get or set the Field data specified at points of the point cloud PCID. See Remark 2. NV  number of points defined in PCID should be defined. The data should be input such that the first NV data are for the first point, the second NV data are for the second point, etc.
        """ # nopep8
        return self._cards[1].get_value("v8")

    @v8.setter
    def v8(self, value: float) -> None:
        """Set the v8 property."""
        self._cards[1].set_value("v8", value)

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

