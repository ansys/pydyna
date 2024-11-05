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

class AleReferenceSystemGroup(KeywordBase):
    """DYNA ALE_REFERENCE_SYSTEM_GROUP keyword"""

    keyword = "ALE"
    subkeyword = "REFERENCE_SYSTEM_GROUP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "prtype",
                        int,
                        20,
                        10,
                        kwargs.get("prtype", 0)
                    ),
                    Field(
                        "prid",
                        int,
                        30,
                        10,
                        kwargs.get("prid")
                    ),
                    Field(
                        "bctran",
                        int,
                        40,
                        10,
                        kwargs.get("bctran", 0)
                    ),
                    Field(
                        "bcexp",
                        int,
                        50,
                        10,
                        kwargs.get("bcexp", 0)
                    ),
                    Field(
                        "bcrot",
                        int,
                        60,
                        10,
                        kwargs.get("bcrot", 0)
                    ),
                    Field(
                        "icoord",
                        int,
                        70,
                        10,
                        kwargs.get("icoord", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "prtype",
                        int,
                        20,
                        10,
                        kwargs.get("prtype", 0)
                    ),
                    Field(
                        "prid",
                        int,
                        30,
                        10,
                        kwargs.get("prid")
                    ),
                    Field(
                        "bctran",
                        int,
                        40,
                        10,
                        kwargs.get("bctran", 0)
                    ),
                    Field(
                        "bcexp",
                        int,
                        50,
                        10,
                        kwargs.get("bcexp", 0)
                    ),
                    Field(
                        "bcrot",
                        int,
                        60,
                        10,
                        kwargs.get("bcrot", 0)
                    ),
                    Field(
                        "icoord",
                        int,
                        70,
                        10,
                        kwargs.get("icoord", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "prtype",
                        int,
                        20,
                        10,
                        kwargs.get("prtype", 0)
                    ),
                    Field(
                        "prid",
                        int,
                        30,
                        10,
                        kwargs.get("prid")
                    ),
                    Field(
                        "bctran",
                        int,
                        40,
                        10,
                        kwargs.get("bctran", 0)
                    ),
                    Field(
                        "bcexp",
                        int,
                        50,
                        10,
                        kwargs.get("bcexp", 0)
                    ),
                    Field(
                        "bcrot",
                        int,
                        60,
                        10,
                        kwargs.get("bcrot", 0)
                    ),
                    Field(
                        "icoord",
                        int,
                        70,
                        10,
                        kwargs.get("icoord", 0)
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.0: part set (default),
        EQ.1: part,
        EQ.2: node set,
        EQ.3: segment set.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""stype must be one of {0,1,2,3}""")
        self._cards[0].set_value("stype", value)

    @property
    def prtype(self) -> int:
        """Get or set the Reference system type :
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_ SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following a local coordinate system defined by three user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.6: Switching in time between different reference system types, see *ALE_REFERENCE_SYSTEM_SWITCH,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        EQ.8: Mesh smoothing option for shock waves, where the element grid contracts in the vicinity of the shock front.  This may be referred to as the Delayed-ALE option.  It controls how much the mesh is to be moved during the remap step.  This option requires the definition of the 5th parameter in the 2nd card, EFAC; see below for definition.
        EQ.9: Allowing the ALE mesh(es) to:
        -Translate and/or rotate to follow a local Lagrangian reference coordinate system (whose *ALE_REFERENCE_SYSTEM_NODE card ID is defined by the BCTRAN parameter)
        -Expand or contract to enclose a Lagrangian part-set ID defined by the PRID parameter.
        -Has a Lagrangian node ID be defined by the ICR/NID parameter to be the center of the ALE mesh expansion.
        """ # nopep8
        return self._cards[0].get_value("prtype")

    @prtype.setter
    def prtype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""prtype must be one of {0,1,2,3,4,5,6,7,8,9}""")
        self._cards[0].set_value("prtype", value)

    @property
    def prid(self) -> typing.Optional[int]:
        """Get or set the A parameter giving additional information depending on the reference system (PRTYPE) choice:
        PRTYPE.EQ.3:	PRID defines a load curve group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌CURVE card for mesh translation.This defines up to 12 curves which prescribe the motion of the system.
        PRTYPE.EQ.4 : PRID defines a node set ID(*SET_‌NODE), for which a mass average velocity is computed.This velocity controls the mesh motion.
        PRTYPE.EQ.5 : PRID defines a node group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌NODE card, via which, three nodes forming a local coordinate system are defined.
        PRTYPE.EQ.6 : PRID defines a switch list ID specifying an* ALE_‌REFERENCE_‌SYSTEM_‌SWITCH card.This defines the switch timesand the reference system choices for each time interval between the switches.
        PRTYPE.EQ.7 : PRID defines a node group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌NODE card.Up to 12 nodes in space forming a region to be enveloped by the ALE mesh are defined.
        PRTYPE.EQ.9 : PRID defines a Lagrangian part set ID(PSID) defining the Lagrangian part(s) whose range of motion is to be enveloped by the ALE mesh(es).This is useful for airbag modeling.
        """ # nopep8
        return self._cards[0].get_value("prid")

    @prid.setter
    def prid(self, value: int) -> None:
        self._cards[0].set_value("prid", value)

    @property
    def bctran(self) -> int:
        """Get or set the For PRTYPE 4 & 5:  BCTRAN is a translational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x translation,
        EQ.2: constrained y translation,
        EQ.3: constrained z translation,
        EQ.4: constrained x and y translation,
        EQ.5: constrained y and z translation,
        EQ.6: constrained z and x translation,
        EQ.7: constrained x, y, and z translation

        """ # nopep8
        return self._cards[0].get_value("bctran")

    @bctran.setter
    def bctran(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bctran must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("bctran", value)

    @property
    def bcexp(self) -> int:
        """Get or set the For PRTYPE= 4 & 7:  BCTRAN is an expansion constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x expansion,
        EQ.2: constrained y expansion,
        EQ.3: constrained z expansion,
        EQ.4: constrained x and y expansion,
        EQ.5: constrained y and z expansion,
        EQ.6: constrained z and x expansion,
        EQ.7: constrained x, y, and z expansion

        """ # nopep8
        return self._cards[0].get_value("bcexp")

    @bcexp.setter
    def bcexp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcexp must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("bcexp", value)

    @property
    def bcrot(self) -> int:
        """Get or set the For PRTYPE= 4:  BCROT is a rotational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotation,
        EQ.5: constrained y and z rotation,
        EQ.6: constrained z and x rotation,
        EQ.7: constrained x, y, and z rotation

        """ # nopep8
        return self._cards[0].get_value("bcrot")

    @bcrot.setter
    def bcrot(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcrot must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("bcrot", value)

    @property
    def icoord(self) -> int:
        """Get or set the PRTYPE=4: ICR is a center of mesh expansion and rotation flag,
        EQ.0:  The center is at center of gravity of the ALE mesh.
        EQ.1:  The center is at (XC, YC, ZC), just a point in space (it does not have to be a defined node)

        """ # nopep8
        return self._cards[0].get_value("icoord")

    @icoord.setter
    def icoord(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icoord must be one of {0,1}""")
        self._cards[0].set_value("icoord", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[1].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.0: part set (default),
        EQ.1: part,
        EQ.2: node set,
        EQ.3: segment set.
        """ # nopep8
        return self._cards[1].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""stype must be one of {0,1,2,3}""")
        self._cards[1].set_value("stype", value)

    @property
    def prtype(self) -> int:
        """Get or set the Reference system type :
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_ SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following a local coordinate system defined by three user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.6: Switching in time between different reference system types, see *ALE_REFERENCE_SYSTEM_SWITCH,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        EQ.8: Mesh smoothing option for shock waves, where the element grid contracts in the vicinity of the shock front.  This may be referred to as the Delayed-ALE option.  It controls how much the mesh is to be moved during the remap step.  This option requires the definition of the 5th parameter in the 2nd card, EFAC; see below for definition.
        EQ.9: Allowing the ALE mesh(es) to:
        -Translate and/or rotate to follow a local Lagrangian reference coordinate system (whose *ALE_REFERENCE_SYSTEM_NODE card ID is defined by the BCTRAN parameter)
        -Expand or contract to enclose a Lagrangian part-set ID defined by the PRID parameter.
        -Has a Lagrangian node ID be defined by the ICR/NID parameter to be the center of the ALE mesh expansion.
        """ # nopep8
        return self._cards[1].get_value("prtype")

    @prtype.setter
    def prtype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""prtype must be one of {0,1,2,3,4,5,6,7,8,9}""")
        self._cards[1].set_value("prtype", value)

    @property
    def prid(self) -> typing.Optional[int]:
        """Get or set the PRTYPE= 3:  PRID defines a load curve group ID specifying an *ALE_REFERENCE_SYSTEM_CURVE card for mesh translation.  This defines up to 12 curves which prescribe the motion of the system.
        """ # nopep8
        return self._cards[1].get_value("prid")

    @prid.setter
    def prid(self, value: int) -> None:
        self._cards[1].set_value("prid", value)

    @property
    def bctran(self) -> int:
        """Get or set the For PRTYPE 4 & 5:  BCTRAN is a translational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x translation,
        EQ.2: constrained y translation,
        EQ.3: constrained z translation,
        EQ.4: constrained x and y translation,
        EQ.5: constrained y and z translation,
        EQ.6: constrained z and x translation,
        EQ.7: constrained x, y, and z translation

        """ # nopep8
        return self._cards[1].get_value("bctran")

    @bctran.setter
    def bctran(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bctran must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[1].set_value("bctran", value)

    @property
    def bcexp(self) -> int:
        """Get or set the For PRTYPE= 4 & 7:  BCTRAN is an expansion constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x expansion,
        EQ.2: constrained y expansion,
        EQ.3: constrained z expansion,
        EQ.4: constrained x and y expansion,
        EQ.5: constrained y and z expansion,
        EQ.6: constrained z and x expansion,
        EQ.7: constrained x, y, and z expansion

        """ # nopep8
        return self._cards[1].get_value("bcexp")

    @bcexp.setter
    def bcexp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcexp must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[1].set_value("bcexp", value)

    @property
    def bcrot(self) -> int:
        """Get or set the For PRTYPE= 4:  BCROT is a rotational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotation,
        EQ.5: constrained y and z rotation,
        EQ.6: constrained z and x rotation,
        EQ.7: constrained x, y, and z rotation

        """ # nopep8
        return self._cards[1].get_value("bcrot")

    @bcrot.setter
    def bcrot(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcrot must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[1].set_value("bcrot", value)

    @property
    def icoord(self) -> int:
        """Get or set the PRTYPE=4: ICR is a center of mesh expansion and rotation flag,
        EQ.0:  The center is at center of gravity of the ALE mesh.
        EQ.1:  The center is at (XC, YC, ZC), just a point in space (it does not have to be a defined node)

        """ # nopep8
        return self._cards[1].get_value("icoord")

    @icoord.setter
    def icoord(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icoord must be one of {0,1}""")
        self._cards[1].set_value("icoord", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[2].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[2].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.0: part set (default),
        EQ.1: part,
        EQ.2: node set,
        EQ.3: segment set.
        """ # nopep8
        return self._cards[2].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""stype must be one of {0,1,2,3}""")
        self._cards[2].set_value("stype", value)

    @property
    def prtype(self) -> int:
        """Get or set the Reference system type :
        EQ.0: Eulerian,
        EQ.1: Lagrangian,
        EQ.2: Normal ALE mesh smoothing,
        EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_ SYSTEM_CURVE,
        EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
        EQ.5: Automatic mesh motion following a local coordinate system defined by three user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
        EQ.6: Switching in time between different reference system types, see *ALE_REFERENCE_SYSTEM_SWITCH,
        EQ.7: Automatic mesh expansion in order to enclose up to twelve user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
        EQ.8: Mesh smoothing option for shock waves, where the element grid contracts in the vicinity of the shock front.  This may be referred to as the Delayed-ALE option.  It controls how much the mesh is to be moved during the remap step.  This option requires the definition of the 5th parameter in the 2nd card, EFAC; see below for definition.
        EQ.9: Allowing the ALE mesh(es) to:
        -Translate and/or rotate to follow a local Lagrangian reference coordinate system (whose *ALE_REFERENCE_SYSTEM_NODE card ID is defined by the BCTRAN parameter)
        -Expand or contract to enclose a Lagrangian part-set ID defined by the PRID parameter.
        -Has a Lagrangian node ID be defined by the ICR/NID parameter to be the center of the ALE mesh expansion.
        """ # nopep8
        return self._cards[2].get_value("prtype")

    @prtype.setter
    def prtype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
            raise Exception("""prtype must be one of {0,1,2,3,4,5,6,7,8,9}""")
        self._cards[2].set_value("prtype", value)

    @property
    def prid(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("prid")

    @prid.setter
    def prid(self, value: int) -> None:
        self._cards[2].set_value("prid", value)

    @property
    def bctran(self) -> int:
        """Get or set the For PRTYPE 4 & 5:  BCTRAN is a translational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x translation,
        EQ.2: constrained y translation,
        EQ.3: constrained z translation,
        EQ.4: constrained x and y translation,
        EQ.5: constrained y and z translation,
        EQ.6: constrained z and x translation,
        EQ.7: constrained x, y, and z translation

        """ # nopep8
        return self._cards[2].get_value("bctran")

    @bctran.setter
    def bctran(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bctran must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[2].set_value("bctran", value)

    @property
    def bcexp(self) -> int:
        """Get or set the For PRTYPE= 4 & 7:  BCTRAN is an expansion constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x expansion,
        EQ.2: constrained y expansion,
        EQ.3: constrained z expansion,
        EQ.4: constrained x and y expansion,
        EQ.5: constrained y and z expansion,
        EQ.6: constrained z and x expansion,
        EQ.7: constrained x, y, and z expansion

        """ # nopep8
        return self._cards[2].get_value("bcexp")

    @bcexp.setter
    def bcexp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcexp must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[2].set_value("bcexp", value)

    @property
    def bcrot(self) -> int:
        """Get or set the For PRTYPE= 4:  BCROT is a rotational constraint (remark 3).
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotation,
        EQ.5: constrained y and z rotation,
        EQ.6: constrained z and x rotation,
        EQ.7: constrained x, y, and z rotation

        """ # nopep8
        return self._cards[2].get_value("bcrot")

    @bcrot.setter
    def bcrot(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""bcrot must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[2].set_value("bcrot", value)

    @property
    def icoord(self) -> int:
        """Get or set the PRTYPE=4: ICR is a center of mesh expansion and rotation flag,
        EQ.0:  The center is at center of gravity of the ALE mesh.
        EQ.1:  The center is at (XC, YC, ZC), just a point in space (it does not have to be a defined node)

        """ # nopep8
        return self._cards[2].get_value("icoord")

    @icoord.setter
    def icoord(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icoord must be one of {0,1}""")
        self._cards[2].set_value("icoord", value)

