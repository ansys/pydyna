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

"""Module providing the Mat084 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT084_CARD0 = (
    FieldSchema("eid1_unused", int, 0, 10, None, "eid1/unused"),
    FieldSchema("eid2_pid", int, 10, 10, None, "eid2/pid"),
    FieldSchema("inc_axis", int, 20, 10, None, "inc/axis"),
    FieldSchema("xr_coor", float, 30, 10, None, "xr/coor"),
    FieldSchema("yr_rqa", float, 40, 10, None, "yr/rqa"),
    FieldSchema("zr_rqb", float, 50, 10, None, "zr/rqb"),
)

_MAT084_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat084(KeywordBase):
    """DYNA MAT_084 keyword"""

    keyword = "MAT"
    subkeyword = "084"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat084 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT084_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat084.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT084_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eid1_unused(self) -> typing.Optional[int]:
        """Get or set the First element ID in group. Left blank to active card of option 2.
        """ # nopep8
        return self._cards[0].get_value("eid1_unused")

    @eid1_unused.setter
    def eid1_unused(self, value: int) -> None:
        """Set the eid1_unused property."""
        self._cards[0].set_value("eid1_unused", value)

    @property
    def eid2_pid(self) -> typing.Optional[int]:
        """Get or set the Last element ID in group.
        PID: Part ID of reinforced elements
        """ # nopep8
        return self._cards[0].get_value("eid2_pid")

    @eid2_pid.setter
    def eid2_pid(self, value: int) -> None:
        """Set the eid2_pid property."""
        self._cards[0].set_value("eid2_pid", value)

    @property
    def inc_axis(self) -> typing.Optional[int]:
        """Get or set the Element increment for generation.
        AXIS: Axis normal to layer:
        EQ.1: A and B are parallel to global Y and Z, respectively (default),
        EQ.2: A and B are parallel to global Z and X, respectively,
        EQ.3: A and B are parallel to global X and Y, respectively.
        """ # nopep8
        return self._cards[0].get_value("inc_axis")

    @inc_axis.setter
    def inc_axis(self, value: int) -> None:
        """Set the inc_axis property."""
        self._cards[0].set_value("inc_axis", value)

    @property
    def xr_coor(self) -> typing.Optional[float]:
        """Get or set the X-reinforcement quantity (for bars running parallel to global x-axis).
        COOR: Coordinate location of layer
        If AXIS.EQ.1: X-coordinate ,
        If AXIS.EQ.2: Y-coordinate,
        If AXIS.EQ.3: Z-coordinate
        """ # nopep8
        return self._cards[0].get_value("xr_coor")

    @xr_coor.setter
    def xr_coor(self, value: float) -> None:
        """Set the xr_coor property."""
        self._cards[0].set_value("xr_coor", value)

    @property
    def yr_rqa(self) -> typing.Optional[float]:
        """Get or set the Y-reinforcement quantity (for bars running parallel to global y-axis).
        RQA: Reinforcement quantity (A).
        """ # nopep8
        return self._cards[0].get_value("yr_rqa")

    @yr_rqa.setter
    def yr_rqa(self, value: float) -> None:
        """Set the yr_rqa property."""
        self._cards[0].set_value("yr_rqa", value)

    @property
    def zr_rqb(self) -> typing.Optional[float]:
        """Get or set the Z-reinforcement quantity (for bars running parallel to global z-axis).
        RQB: Reinforcement quantity (B).
        """ # nopep8
        return self._cards[0].get_value("zr_rqb")

    @zr_rqb.setter
    def zr_rqb(self, value: float) -> None:
        """Set the zr_rqb property."""
        self._cards[0].set_value("zr_rqb", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

