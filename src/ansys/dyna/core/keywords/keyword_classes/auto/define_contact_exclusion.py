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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineContactExclusion(KeywordBase):
    """DYNA DEFINE_CONTACT_EXCLUSION keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTACT_EXCLUSION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "target",
                        int,
                        0,
                        10,
                        kwargs.get("target")
                    ),
                    Field(
                        "c1",
                        int,
                        10,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        int,
                        20,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        int,
                        30,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        int,
                        40,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        int,
                        50,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        int,
                        60,
                        10,
                        kwargs.get("c6")
                    ),
                    Field(
                        "c7",
                        int,
                        70,
                        10,
                        kwargs.get("c7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c8",
                        int,
                        0,
                        10,
                        kwargs.get("c8")
                    ),
                    Field(
                        "c9",
                        int,
                        10,
                        10,
                        kwargs.get("c9")
                    ),
                    Field(
                        "c10",
                        int,
                        20,
                        10,
                        kwargs.get("c10")
                    ),
                    Field(
                        "c11",
                        int,
                        30,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c12",
                        int,
                        40,
                        10,
                        kwargs.get("c12")
                    ),
                    Field(
                        "c13",
                        int,
                        50,
                        10,
                        kwargs.get("c13")
                    ),
                    Field(
                        "c14",
                        int,
                        60,
                        10,
                        kwargs.get("c14")
                    ),
                    Field(
                        "c15",
                        int,
                        70,
                        10,
                        kwargs.get("c15")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineContactExclusion.option_specs[0],
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
    def eid(self) -> typing.Optional[int]:
        """Get or set the Exclusion ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Exclusion Title.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def target(self) -> typing.Optional[int]:
        """Get or set the Contact interface from which tied nodes are to be excluded. This
        must be the ID of a SINGLE_SURFACE, NODE_TO_SURFACE, or
        SURFACE_TO_SURFACE contact with SOFT 2..
        """ # nopep8
        return self._cards[1].get_value("target")

    @target.setter
    def target(self, value: int) -> None:
        self._cards[1].set_value("target", value)

    @property
    def c1(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: int) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: int) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: int) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c4")

    @c4.setter
    def c4(self, value: int) -> None:
        self._cards[1].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c5")

    @c5.setter
    def c5(self, value: int) -> None:
        self._cards[1].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c6")

    @c6.setter
    def c6(self, value: int) -> None:
        self._cards[1].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[1].get_value("c7")

    @c7.setter
    def c7(self, value: int) -> None:
        self._cards[1].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c8")

    @c8.setter
    def c8(self, value: int) -> None:
        self._cards[2].set_value("c8", value)

    @property
    def c9(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c9")

    @c9.setter
    def c9(self, value: int) -> None:
        self._cards[2].set_value("c9", value)

    @property
    def c10(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c10")

    @c10.setter
    def c10(self, value: int) -> None:
        self._cards[2].set_value("c10", value)

    @property
    def c11(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c11")

    @c11.setter
    def c11(self, value: int) -> None:
        self._cards[2].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c12")

    @c12.setter
    def c12(self, value: int) -> None:
        self._cards[2].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c13")

    @c13.setter
    def c13(self, value: int) -> None:
        self._cards[2].set_value("c13", value)

    @property
    def c14(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c14")

    @c14.setter
    def c14(self, value: int) -> None:
        self._cards[2].set_value("c14", value)

    @property
    def c15(self) -> typing.Optional[int]:
        """Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
        card for as many cards as necessary.
        Any node which is a slave node in one of these interfaces, and is in
        fact tied, will not be processed (as a slave node) in the Target interface.
        Note that if a node is excluded from the Target by this mechanism,
        contact forces may still be applied to the node due to any slave or
        master nodes impacting the contact segments of which it is a part
        (no contact SEGMENTS are deleted, only contact NODES).
        If the Target contact is of type SURFACE_TO_SURFACE, any tied
        slave nodes are deleted from both the slave side (for the normal
        treatment) and the master side (for the symmetric treatment).
        """ # nopep8
        return self._cards[2].get_value("c15")

    @c15.setter
    def c15(self, value: int) -> None:
        self._cards[2].set_value("c15", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

