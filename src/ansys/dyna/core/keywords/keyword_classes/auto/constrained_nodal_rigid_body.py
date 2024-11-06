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
from ansys.dyna.core.lib.duplicate_card_group import DuplicateCardGroup
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ConstrainedNodalRigidBody(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            DuplicateCardGroup(
                [
                    Card(
                            [
                                Field(
                                    "pid",
                                    int,
                                    0,
                                    10,
                                ),
                                Field(
                                    "cid",
                                    int,
                                    10,
                                    10,
                                ),
                                Field(
                                    "nsid",
                                    int,
                                    20,
                                    10,
                                ),
                                Field(
                                    "pnode",
                                    int,
                                    30,
                                    10,
                                ),
                                Field(
                                    "iprt",
                                    int,
                                    40,
                                    10,
                                ),
                                Field(
                                    "drflag",
                                    int,
                                    50,
                                    10,
                                ),
                                Field(
                                    "rrflag",
                                    int,
                                    60,
                                    10,
                                ),
                            ],
                    ),
                ],
                None,
                data = kwargs.get("constrained_nodal_rigid_bodies")),
            OptionCardSet(
                option_spec = ConstrainedNodalRigidBody.option_specs[0],
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
    def constrained_nodal_rigid_bodies(self):
        '''Gets the full table of constrained_nodal_rigid_bodies'''
        return self._cards[0].table

    @constrained_nodal_rigid_bodies.setter
    def constrained_nodal_rigid_bodies(self, df):
        '''sets constrained_nodal_rigid_bodies from the dataframe df'''
        self._cards[0].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

