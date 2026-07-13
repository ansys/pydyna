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

"""Module providing the DefinePointCloud class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEPOINTCLOUD_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
)

_DEFINEPOINTCLOUD_CARD1 = (
    FieldSchema("x", float, 0, 20, None),
    FieldSchema("y", float, 20, 20, None),
    FieldSchema("z", float, 40, 20, None),
)

_DEFINEPOINTCLOUD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefinePointCloud(KeywordBase):
    """DYNA DEFINE_POINT_CLOUD keyword"""

    keyword = "DEFINE"
    subkeyword = "POINT_CLOUD"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefinePointCloud class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEPOINTCLOUD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEPOINTCLOUD_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefinePointCloud._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEPOINTCLOUD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Point cloud ID. A unique ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x - coordinate of the i-th point in the point cloud in the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the i-th point in the point cloud in the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of the i-th point in the point cloud in the global coordinate system
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

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

