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

"""Module providing the IgaTiedEdgeToEdge class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IgaTiedEdgeToEdge(KeywordBase):
    """DYNA IGA_TIED_EDGE_TO_EDGE keyword"""

    keyword = "IGA"
    subkeyword = "TIED_EDGE_TO_EDGE"

    def __init__(self, **kwargs):
        """Initialize the IgaTiedEdgeToEdge class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "form",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sfd",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sfr",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sft",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Apply coupling to entities referenced by the ID field along topologically connected edges. The next field, TYPE, specifies the type of entity to which ID refers because entities of different kinds, such as parts and part sets, are not uniquely numbered.  Currently (as of June 2020), Currently, no types requiring an ID are supported. This field is reserved for future enhancements
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the Type of ID:
        EQ.0:	Include all topological connections in the model.No ID required
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def form(self) -> int:
        """Get or set the Coupling formulation:
        EQ.0:	Penalty - based tied contact
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        self._cards[0].set_value("form", value)

    @property
    def sfd(self) -> float:
        """Get or set the Scaling factor for displacement penalty stiffness
        """ # nopep8
        return self._cards[0].get_value("sfd")

    @sfd.setter
    def sfd(self, value: float) -> None:
        """Set the sfd property."""
        self._cards[0].set_value("sfd", value)

    @property
    def sfr(self) -> float:
        """Get or set the Scaling factor for rotational penalty stiffness
        """ # nopep8
        return self._cards[0].get_value("sfr")

    @sfr.setter
    def sfr(self, value: float) -> None:
        """Set the sfr property."""
        self._cards[0].set_value("sfr", value)

    @property
    def sft(self) -> float:
        """Get or set the Scaling factor for thin constraint penalty stiffness (rotation free elements)
        """ # nopep8
        return self._cards[0].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[0].set_value("sft", value)

