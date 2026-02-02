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

"""Module providing the DefineFormingOnestepPrimary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEFORMINGONESTEPPRIMARY_CARD0 = (
    FieldSchema("slpid", int, 0, 10, None),
)

_DEFINEFORMINGONESTEPPRIMARY_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFormingOnestepPrimary(KeywordBase):
    """DYNA DEFINE_FORMING_ONESTEP_PRIMARY keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_ONESTEP_PRIMARY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "slpid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineFormingOnestepPrimary class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFORMINGONESTEPPRIMARY_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFormingOnestepPrimary.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFORMINGONESTEPPRIMARY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def slpid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the primary blank to which a constrained blank is welded using* CONSTRAINED_SPOTWELD
        """ # nopep8
        return self._cards[0].get_value("slpid")

    @slpid.setter
    def slpid(self, value: int) -> None:
        """Set the slpid property."""
        self._cards[0].set_value("slpid", value)

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

    @property
    def slpid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given slpid."""
        return self._get_link_by_attr("PART", "pid", self.slpid, "parts")

