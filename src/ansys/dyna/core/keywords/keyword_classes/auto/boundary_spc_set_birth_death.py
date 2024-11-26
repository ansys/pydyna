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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundarySpcSetBirthDeath(KeywordBase):
    """DYNA BOUNDARY_SPC_SET_BIRTH_DEATH keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_SET_BIRTH_DEATH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid", 0)
                    ),
                    Field(
                        "dofx",
                        int,
                        20,
                        10,
                        kwargs.get("dofx", 0)
                    ),
                    Field(
                        "dofy",
                        int,
                        30,
                        10,
                        kwargs.get("dofy", 0)
                    ),
                    Field(
                        "dofz",
                        int,
                        40,
                        10,
                        kwargs.get("dofz", 0)
                    ),
                    Field(
                        "dofrx",
                        int,
                        50,
                        10,
                        kwargs.get("dofrx", 0)
                    ),
                    Field(
                        "dofry",
                        int,
                        60,
                        10,
                        kwargs.get("dofry", 0)
                    ),
                    Field(
                        "dofrz",
                        int,
                        70,
                        10,
                        kwargs.get("dofrz", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "birth",
                        float,
                        0,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        10,
                        10,
                        kwargs.get("death", 1.0E+20)
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID, see also *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def dofx(self) -> int:
        """Get or set the EQ.0: no translational constraint in local x-direction,
        EQ.1: translational constraint in local x-direction.
        """ # nopep8
        return self._cards[0].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofx must be one of {0,1}""")
        self._cards[0].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the EQ.0: no translational constraint in local y-direction,
        EQ.1: translational constraint in local y-direction.
        """ # nopep8
        return self._cards[0].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofy must be one of {0,1}""")
        self._cards[0].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the EQ.0: no translational constraint in local z-direction,
        EQ.1: translational constraint in local z-direction.
        """ # nopep8
        return self._cards[0].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofz must be one of {0,1}""")
        self._cards[0].set_value("dofz", value)

    @property
    def dofrx(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local x-axis,
        EQ.1: rotational constraint about local x-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrx")

    @dofrx.setter
    def dofrx(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofrx must be one of {0,1}""")
        self._cards[0].set_value("dofrx", value)

    @property
    def dofry(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local y-axis,
        EQ.1: rotational constraint about local y-axis.
        """ # nopep8
        return self._cards[0].get_value("dofry")

    @dofry.setter
    def dofry(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofry must be one of {0,1}""")
        self._cards[0].set_value("dofry", value)

    @property
    def dofrz(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local z-axiis
        EQ.1: rotational constraint about local z-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrz")

    @dofrz.setter
    def dofrz(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dofrz must be one of {0,1}""")
        self._cards[0].set_value("dofrz", value)

    @property
    def birth(self) -> float:
        """Get or set the Activation time for constraint
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Deactivation time for constraint.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

