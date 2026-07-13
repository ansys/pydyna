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

"""Module providing the DefineBirthPart class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEBIRTHPART_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("birth", float, 10, 10, None),
    FieldSchema("drtime", int, 20, 10, 0),
)

_DEFINEBIRTHPART_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineBirthPart(KeywordBase):
    """DYNA DEFINE_BIRTH_PART keyword"""

    keyword = "DEFINE"
    subkeyword = "BIRTH_PART"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineBirthPart class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEBIRTHPART_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineBirthPart._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEBIRTHPART_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID to which the birth time applies.
        LT.0.0: |PID| is the ID of a part set containing the affected parts.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[0].set_value("birth", value)

    @property
    def drtime(self) -> int:
        """Get or set the Controls whether dynamic relaxation pseudo time is to be accounted for or not.
        EQ.0 : The birth time is in relation to the transient time.In other words,the dynamic relaxation time is ignored.For example,if BIRTH = 0.0 and DRTIME = 0,the feature is activated at time zero of the transient run.This is the default.
        EQ.1 : The birth time is in relation to dynamic relaxation pseudo time.If the dynamic relaxation ends before BIRTH is reached,the feature is activated at time zero of the transient run.
        """ # nopep8
        return self._cards[0].get_value("drtime")

    @drtime.setter
    def drtime(self, value: int) -> None:
        """Set the drtime property."""
        if value not in [0, 1, None]:
            raise Exception("""drtime must be `None` or one of {0,1}.""")
        self._cards[0].set_value("drtime", value)

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
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

