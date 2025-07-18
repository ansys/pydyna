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

"""Module providing the BoundarySpcSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundarySpcSet(KeywordBase):
    """DYNA BOUNDARY_SPC_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_SET"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the BoundarySpcSet class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofx",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofy",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofz",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofrx",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofry",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dofrz",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = BoundarySpcSet.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "id",
                                int,
                                0,
                                10,
                                kwargs.get("id")
                            ),
                            Field(
                                "heading",
                                str,
                                10,
                                70,
                                kwargs.get("heading")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID, see also *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def dofx(self) -> int:
        """Get or set the EQ.0: no translational constraint in local x-direction,
        EQ.1: translational constraint in local x-direction.
        """ # nopep8
        return self._cards[0].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        """Set the dofx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the EQ.0: no translational constraint in local y-direction,
        EQ.1: translational constraint in local y-direction.
        """ # nopep8
        return self._cards[0].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        """Set the dofy property."""
        if value not in [0, 1, None]:
            raise Exception("""dofy must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the EQ.0: no translational constraint in local z-direction,
        EQ.1: translational constraint in local z-direction.
        """ # nopep8
        return self._cards[0].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        """Set the dofz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofz", value)

    @property
    def dofrx(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local x-axis,
        EQ.1: rotational constraint about local x-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrx")

    @dofrx.setter
    def dofrx(self, value: int) -> None:
        """Set the dofrx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofrx", value)

    @property
    def dofry(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local y-axis,
        EQ.1: rotational constraint about local y-axis.
        """ # nopep8
        return self._cards[0].get_value("dofry")

    @dofry.setter
    def dofry(self, value: int) -> None:
        """Set the dofry property."""
        if value not in [0, 1, None]:
            raise Exception("""dofry must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofry", value)

    @property
    def dofrz(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local z-axiis
        EQ.1: rotational constraint about local z-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrz")

    @dofrz.setter
    def dofrz(self, value: int) -> None:
        """Set the dofrz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofrz", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[1].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[1].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[1].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

