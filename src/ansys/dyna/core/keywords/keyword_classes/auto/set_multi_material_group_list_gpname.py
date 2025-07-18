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

"""Module providing the SetMultiMaterialGroupListGpname class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SetMultiMaterialGroupListGpname(KeywordBase):
    """DYNA SET_MULTI_MATERIAL_GROUP_LIST_GPNAME keyword"""

    keyword = "SET"
    subkeyword = "MULTI_MATERIAL_GROUP_LIST_GPNAME"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetMultiMaterialGroupListGpname class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "ammsid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ammgid1",
                        str,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid2",
                        str,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid3",
                        str,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid4",
                        str,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid5",
                        str,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid6",
                        str,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid7",
                        str,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ammgid8",
                        str,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetMultiMaterialGroupListGpname.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def ammsid(self) -> int:
        """Get or set the An ALE multi-material set ID (AMMSID) which contains a collection of one or more ALE multi-material group ID(s) (AMMGID).
        """ # nopep8
        return self._cards[0].get_value("ammsid")

    @ammsid.setter
    def ammsid(self, value: int) -> None:
        """Set the ammsid property."""
        self._cards[0].set_value("ammsid", value)

    @property
    def ammgid1(self) -> typing.Optional[str]:
        """Get or set the The 1st ALE multi-material group ID (AMMGID=1) defined by the 1st line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid1")

    @ammgid1.setter
    def ammgid1(self, value: str) -> None:
        """Set the ammgid1 property."""
        self._cards[1].set_value("ammgid1", value)

    @property
    def ammgid2(self) -> typing.Optional[str]:
        """Get or set the The 2nd ALE multi-material group ID (AMMGID=1) defined by the 2nd line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid2")

    @ammgid2.setter
    def ammgid2(self, value: str) -> None:
        """Set the ammgid2 property."""
        self._cards[1].set_value("ammgid2", value)

    @property
    def ammgid3(self) -> typing.Optional[str]:
        """Get or set the The 3rd ALE multi-material group ID (AMMGID=1) defined by the 3rd line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid3")

    @ammgid3.setter
    def ammgid3(self, value: str) -> None:
        """Set the ammgid3 property."""
        self._cards[1].set_value("ammgid3", value)

    @property
    def ammgid4(self) -> typing.Optional[str]:
        """Get or set the The 4th ALE multi-material group ID (AMMGID=1) defined by the 4th line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid4")

    @ammgid4.setter
    def ammgid4(self, value: str) -> None:
        """Set the ammgid4 property."""
        self._cards[1].set_value("ammgid4", value)

    @property
    def ammgid5(self) -> typing.Optional[str]:
        """Get or set the The 5th ALE multi-material group ID (AMMGID=1) defined by the 5th line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid5")

    @ammgid5.setter
    def ammgid5(self, value: str) -> None:
        """Set the ammgid5 property."""
        self._cards[1].set_value("ammgid5", value)

    @property
    def ammgid6(self) -> typing.Optional[str]:
        """Get or set the The 6th ALE multi-material group ID (AMMGID=1) defined by the 6th line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid6")

    @ammgid6.setter
    def ammgid6(self, value: str) -> None:
        """Set the ammgid6 property."""
        self._cards[1].set_value("ammgid6", value)

    @property
    def ammgid7(self) -> typing.Optional[str]:
        """Get or set the The 7th ALE multi-material group ID (AMMGID=1) defined by the 7th line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid7")

    @ammgid7.setter
    def ammgid7(self, value: str) -> None:
        """Set the ammgid7 property."""
        self._cards[1].set_value("ammgid7", value)

    @property
    def ammgid8(self) -> typing.Optional[str]:
        """Get or set the The 8th ALE multi-material group ID (AMMGID=1) defined by the 8th line of the *ALE_MULTI-MATERIAL_GROUP card.
        """ # nopep8
        return self._cards[1].get_value("ammgid8")

    @ammgid8.setter
    def ammgid8(self, value: str) -> None:
        """Set the ammgid8 property."""
        self._cards[1].set_value("ammgid8", value)

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

