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

"""Module providing the DefineShellSwitch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_DEFINESHELLSWITCH_CARD0 = (
    FieldSchema("boxid", int, 0, 10, None),
    FieldSchema("psid", int, 10, 10, None),
    FieldSchema("shlform", int, 20, 10, 0),
    FieldSchema("nip", int, 30, 10, 0),
    FieldSchema("percelin", int, 40, 10, 100),
)

_DEFINESHELLSWITCH_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineShellSwitch(KeywordBase):
    """DYNA DEFINE_SHELL_SWITCH keyword"""

    keyword = "DEFINE"
    subkeyword = "SHELL_SWITCH"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "boxid": LinkType.DEFINE_BOX,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineShellSwitch class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESHELLSWITCH_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineShellSwitch._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESHELLSWITCH_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Box ID (see *DEFINE_BOX) of a box region in which the shell formulation and number of through-thickness integration points are switched. The change applies only to elements within the box at initialization.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Optional part set ID (see *SET_PART). If defined, only shell elements belonging to this part set are switched if the shell elements are in the box. If zero or undefined, all shell elements in the box are switched.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def shlform(self) -> int:
        """Get or set the Shell formulation to switch to. See ELFORM on *SECTION_SHELL for a list of available shell formulations. See Remark 1 for a list of switching restrictions. If set to 0 or left undefined, the element formulation remains unchanged.
        """ # nopep8
        return self._cards[0].get_value("shlform")

    @shlform.setter
    def shlform(self, value: int) -> None:
        """Set the shlform property."""
        self._cards[0].set_value("shlform", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through-thickness integration points to switch to. If set to 0 or left undefined, the number of integration points remains unchanged.
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[0].set_value("nip", value)

    @property
    def percelin(self) -> int:
        """Get or set the Percentage of elements belonging to a part that must be inside the box for the switch to occur for that part. By default, the whole part must be inside the box
        """ # nopep8
        return self._cards[0].get_value("percelin")

    @percelin.setter
    def percelin(self, value: int) -> None:
        """Set the percelin property."""
        self._cards[0].set_value("percelin", value)

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
    def boxid_link(self) -> typing.Optional[DefineBox]:
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

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

