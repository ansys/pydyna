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

"""Module providing the DefineCurveFeedback class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_DEFINECURVEFEEDBACK_CARD0 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("boxid", int, 20, 10, 0),
    FieldSchema("fldid", int, 30, 10, None),
)

_DEFINECURVEFEEDBACK_CARD1 = (
    FieldSchema("fsl", float, 0, 10, None),
    FieldSchema("tsl", float, 10, 10, None),
    FieldSchema("sff", float, 20, 10, 1.0),
    FieldSchema("sft", float, 30, 10, 1.0),
    FieldSchema("bias", float, 40, 10, 0.0),
)

_DEFINECURVEFEEDBACK_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineCurveFeedback(KeywordBase):
    """DYNA DEFINE_CURVE_FEEDBACK keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_FEEDBACK"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "fldid": LinkType.DEFINE_CURVE,
        "boxid": LinkType.DEFINE_BOX,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineCurveFeedback class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFEEDBACK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECURVEFEEDBACK_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineCurveFeedback.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECURVEFEEDBACK_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the ID number for load curve to be scaled.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Active part ID for load curve control
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Box ID. Elements of specified part ID contained in box are checked. If the box ID is set to zero then all elements of the active part are checked.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def fldid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID which defines the flow limit. If the product of FSL and the ordinate value of the maximum principal strain is exceeded the scale factor for flow, SF, is active.
        """ # nopep8
        return self._cards[0].get_value("fldid")

    @fldid.setter
    def fldid(self, value: int) -> None:
        """Set the fldid property."""
        self._cards[0].set_value("fldid", value)

    @property
    def fsl(self) -> typing.Optional[float]:
        """Get or set the If the strain ratio, epsilon-major-workpiece to epsilon-major-fld, is exceeded the scale factor for flow, SF, is active.
        """ # nopep8
        return self._cards[1].get_value("fsl")

    @fsl.setter
    def fsl(self, value: float) -> None:
        """Set the fsl property."""
        self._cards[1].set_value("fsl", value)

    @property
    def tsl(self) -> typing.Optional[float]:
        """Get or set the Thickness strain limit. If the through thickness strain is exceeded the scale factor for thickening, ST, is active.
        """ # nopep8
        return self._cards[1].get_value("tsl")

    @tsl.setter
    def tsl(self, value: float) -> None:
        """Set the tsl property."""
        self._cards[1].set_value("tsl", value)

    @property
    def sff(self) -> float:
        """Get or set the Scale factor for the flow limit diagram, SF (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("sff")

    @sff.setter
    def sff(self, value: float) -> None:
        """Set the sff property."""
        self._cards[1].set_value("sff", value)

    @property
    def sft(self) -> float:
        """Get or set the Scale factor for thickening, ST (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[1].set_value("sft", value)

    @property
    def bias(self) -> float:
        """Get or set the Bias for combined flow and thickening, S, -1.0 <= S <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("bias")

    @bias.setter
    def bias(self, value: float) -> None:
        """Set the bias property."""
        self._cards[1].set_value("bias", value)

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

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def fldid_link(self) -> DefineCurve:
        """Get the DefineCurve object for fldid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.fldid:
                return kwd
        return None

    @fldid_link.setter
    def fldid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for fldid."""
        self.fldid = value.lcid

    @property
    def boxid_link(self) -> DefineBox:
        """Get the DefineBox object for boxid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxid:
                return kwd
        return None

    @boxid_link.setter
    def boxid_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxid."""
        self.boxid = value.boxid

