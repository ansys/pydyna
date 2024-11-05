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

class ElementBeamSectionPid(KeywordBase):
    """DYNA ELEMENT_BEAM_SECTION_PID keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_SECTION_PID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "n1",
                        int,
                        16,
                        8,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        24,
                        8,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        32,
                        8,
                        kwargs.get("n3")
                    ),
                    Field(
                        "rt1",
                        int,
                        40,
                        8,
                        kwargs.get("rt1", 0)
                    ),
                    Field(
                        "rr1",
                        int,
                        48,
                        8,
                        kwargs.get("rr1", 0)
                    ),
                    Field(
                        "rt2",
                        int,
                        56,
                        8,
                        kwargs.get("rt2", 0)
                    ),
                    Field(
                        "rr2",
                        int,
                        64,
                        8,
                        kwargs.get("rr2", 0)
                    ),
                    Field(
                        "local",
                        int,
                        72,
                        8,
                        kwargs.get("local", 2)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "stype",
                        str,
                        0,
                        10,
                        kwargs.get("stype", "SECTION_01")
                    ),
                    Field(
                        "d1",
                        float,
                        10,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        20,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        30,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "d4",
                        float,
                        40,
                        10,
                        kwargs.get("d4")
                    ),
                    Field(
                        "d5",
                        float,
                        50,
                        10,
                        kwargs.get("d5")
                    ),
                    Field(
                        "d6",
                        float,
                        60,
                        10,
                        kwargs.get("d6")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid1",
                        int,
                        0,
                        8,
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        8,
                        8,
                        kwargs.get("pid2")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2. This node is optional for the spot weld, beam type 9, since if it not defined it will be created automatically and given a nonconfliciting nodal point ID. Nodes N1 and N2 are automatically positioned for the spot weld beam element.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3, the third node, N3, is optional for beam type 3,6,7,8,and 9, if the latter, type 9, has a non-circular cross section. The third node is used for the discrete beam, type 6, if and only if SCOOR is set to 2.0 in the *SECTION_BEAM input, but even in this case it is optional.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def rt1(self) -> int:
        """Get or set the Release conditions for translations at node N1.
        EQ.0: no translational degrees-of-freedom are released,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x and y-translational degrees-of-freedom,
        EQ.5: y and z-translational degrees-of-freedom,
        EQ.6: z and x-translational degrees-of-freedom,
        EQ.7: x, y, and z-translational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_value("rt1")

    @rt1.setter
    def rt1(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rt1 must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rt1", value)

    @property
    def rr1(self) -> int:
        """Get or set the Release conditions for rotations at node N1.
        EQ.0: no rotational degrees-of-freedom are released,
        EQ.1: x-rotational degree-of-freedom,
        EQ.2: y-rotational degree-of-freedom,
        EQ.3: z-rotational degree-of-freedom,
        EQ.4: x and y-rotational degrees-of-freedom,
        EQ.5: y and z-rotational degrees-of-freedom,
        EQ.6: z and x-rotational degrees-of-freedom,
        EQ.7: x, y, and z-rotational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_value("rr1")

    @rr1.setter
    def rr1(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rr1 must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rr1", value)

    @property
    def rt2(self) -> int:
        """Get or set the Release conditions for translations at node N2.
        EQ.0: no translational degrees-of-freedom are released,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x and y-translational degrees-of-freedom,
        EQ.5: y and z-translational degrees-of-freedom,
        EQ.6: z and x-translational degrees-of-freedom,
        EQ.7: x, y, and z-translational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_value("rt2")

    @rt2.setter
    def rt2(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rt2 must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rt2", value)

    @property
    def rr2(self) -> int:
        """Get or set the Release conditions for rotations at node N2.
        EQ.0: no rotational degrees-of-freedom are released,
        EQ.1: x-rotational degree-of-freedom,
        EQ.2: y-rotational degree-of-freedom,
        EQ.3: z-rotational degree-of-freedom,
        EQ.4: x and y-rotational degrees-of-freedom,
        EQ.5: y and z-rotational degrees-of-freedom,
        EQ.6: z and x-rotational degrees-of-freedom,
        EQ.7: x, y, and z-rotational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_value("rr2")

    @rr2.setter
    def rr2(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""rr2 must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("rr2", value)

    @property
    def local(self) -> int:
        """Get or set the Coordinate system
        EQ.1-global system
        EQ.2-Local system (default).
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""local must be one of {2,1}""")
        self._cards[0].set_value("local", value)

    @property
    def stype(self) -> str:
        """Get or set the Section type (A format):
        EQ.SECTION_01: I-shape		EQ.SECTION_12: Cross
        EQ.SECTION_02: Channel	EQ.SECTION_13: H-shape
        EQ.SECTION_03: L-shape		EQ.SECTION_14: T-shape2
        EQ.SECTION_04: T-shape		EQ.SECTION_15: I-shape3
        EQ.SECTION_05: Tubular box	EQ.SECTION_16: Channel2
        EQ.SECTION_06: Z-shape		EQ.SECTION_17: Channel3
        EQ.SECTION_07: Trapezoidal	EQ.SECTION_18: T-shape3
        EQ.SECTION_08: Circular		EQ.SECTION_19: Box-shape2
        EQ.SECTION_09: Tubular		EQ.SECTION_20: Hexagon
        EQ.SECTION_10: I-shape2	EQ.SECTION_21: Hat-shape
        EQ.SECTION_11: Solid box	EQ.SECTION_22: Hat-shape2
        """ # nopep8
        return self._cards[1].get_value("stype")

    @stype.setter
    def stype(self, value: str) -> None:
        if value not in ["SECTION_01", "SECTION_02", "SECTION_03", "SECTION_04", "SECTION_05", "SECTION_06", "SECTION_07", "SECTION_08", "SECTION_09", "SECTION_10", "SECTION_11", "SECTION_12", "SECTION_13", "SECTION_14", "SECTION_15", "SECTION_16", "SECTION_17", "SECTION_18", "SECTION_19", "SECTION_20", "SECTION_21", "SECTION_22"]:
            raise Exception("""stype must be one of {"SECTION_01","SECTION_02","SECTION_03","SECTION_04","SECTION_05","SECTION_06","SECTION_07","SECTION_08","SECTION_09","SECTION_10","SECTION_11","SECTION_12","SECTION_13","SECTION_14","SECTION_15","SECTION_16","SECTION_17","SECTION_18","SECTION_19","SECTION_20","SECTION_21","SECTION_22"}""")
        self._cards[1].set_value("stype", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[1].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[1].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[1].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        self._cards[1].set_value("d4", value)

    @property
    def d5(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d5")

    @d5.setter
    def d5(self, value: float) -> None:
        self._cards[1].set_value("d5", value)

    @property
    def d6(self) -> typing.Optional[float]:
        """Get or set the Input parameters for section option using STYPE above
        """ # nopep8
        return self._cards[1].get_value("d6")

    @d6.setter
    def d6(self, value: float) -> None:
        self._cards[1].set_value("d6", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Optional part ID for spot weld element type 9.
        """ # nopep8
        return self._cards[2].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[2].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Optional part ID for spot weld element type 9.
        """ # nopep8
        return self._cards[2].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[2].set_value("pid2", value)

