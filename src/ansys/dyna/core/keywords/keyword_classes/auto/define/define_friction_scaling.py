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

"""Module providing the DefineFrictionScaling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFRICTIONSCALING_CARD0 = (
    FieldSchema("fsid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, 0),
    FieldSchema("psid", int, 20, 10, 0),
    FieldSchema("scale1", float, 30, 10, 1.0),
    FieldSchema("scaleo", float, 40, 10, 1.0),
)

_DEFINEFRICTIONSCALING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFrictionScaling(KeywordBase):
    """DYNA DEFINE_FRICTION_SCALING keyword"""

    keyword = "DEFINE"
    subkeyword = "FRICTION_SCALING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFrictionScaling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFRICTIONSCALING_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFrictionScaling.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFRICTIONSCALING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fsid(self) -> typing.Optional[int]:
        """Get or set the Friction scaling ID number.  Each friction scaling definition should have a unique ID which is used for output messages only.
        """ # nopep8
        return self._cards[0].get_value("fsid")

    @fsid.setter
    def fsid(self, value: int) -> None:
        """Set the fsid property."""
        self._cards[0].set_value("fsid", value)

    @property
    def cid(self) -> int:
        """Get or set the Contact ID.  Optional input to limit friction scaling to one contact interface with this ID number.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID.  Optional input to limit friction scaling to parts in the set
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def scale1(self) -> float:
        """Get or set the Friction scale factor for the inner surface of shell segments
        """ # nopep8
        return self._cards[0].get_value("scale1")

    @scale1.setter
    def scale1(self, value: float) -> None:
        """Set the scale1 property."""
        self._cards[0].set_value("scale1", value)

    @property
    def scaleo(self) -> float:
        """Get or set the Friction scale factor for the outer surface of shell segments
        """ # nopep8
        return self._cards[0].get_value("scaleo")

    @scaleo.setter
    def scaleo(self, value: float) -> None:
        """Set the scaleo property."""
        self._cards[0].set_value("scaleo", value)

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

