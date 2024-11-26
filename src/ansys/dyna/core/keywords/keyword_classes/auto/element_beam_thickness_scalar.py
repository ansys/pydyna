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

class ElementBeamThicknessScalar(KeywordBase):
    """DYNA ELEMENT_BEAM_THICKNESS_SCALAR keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_THICKNESS_SCALAR"

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
                        "parm1",
                        float,
                        0,
                        16,
                        kwargs.get("parm1")
                    ),
                    Field(
                        "parm2",
                        float,
                        16,
                        16,
                        kwargs.get("parm2")
                    ),
                    Field(
                        "parm3",
                        float,
                        32,
                        16,
                        kwargs.get("parm3")
                    ),
                    Field(
                        "parm4",
                        float,
                        48,
                        16,
                        kwargs.get("parm4")
                    ),
                    Field(
                        "parm5",
                        float,
                        64,
                        16,
                        kwargs.get("parm5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vol",
                        float,
                        0,
                        16,
                        kwargs.get("vol")
                    ),
                    Field(
                        "iner",
                        float,
                        16,
                        16,
                        kwargs.get("iner")
                    ),
                    Field(
                        "cid",
                        int,
                        32,
                        16,
                        kwargs.get("cid")
                    ),
                    Field(
                        "dofn1",
                        float,
                        48,
                        16,
                        kwargs.get("dofn1", 1)
                    ),
                    Field(
                        "dofn2",
                        float,
                        64,
                        16,
                        kwargs.get("dofn2", 1)
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
    def parm1(self) -> typing.Optional[float]:
        """Get or set the Based on beam type:
        Type.EQ.1: beam thickness, s direction at node 1
        Type.EQ.2: area
        Type.EQ.3: area
        Type.EQ.4: beam thickness, s direction at node 1
        Type.EQ.5: beam thickness, s direction at node 1
        Type.EQ.6: volume
        Type.EQ.7: beam thickness, s direction at node 1
        Type.EQ.8: beam thickness, s direction at node 1
        Type.EQ.9:beam thickness, s direction at node 1
        """ # nopep8
        return self._cards[1].get_value("parm1")

    @parm1.setter
    def parm1(self, value: float) -> None:
        self._cards[1].set_value("parm1", value)

    @property
    def parm2(self) -> typing.Optional[float]:
        """Get or set the Based on beam type:
        Type.EQ.1: beam thickness, s direction at node 2
        Type.EQ.2: Iss
        Type.EQ.3: not used
        Type.EQ.4: beam thickness, s direction at node 2
        Type.EQ.5: beam thickness, s direction at node 2
        Type.EQ.6: geometric inertia
        Type.EQ.6: volume
        Type.EQ.7: beam thickness, s direction at node 2
        Type.EQ.8: beam thickness, s direction at node 2
        Type.EQ.9: beam thickness, s direction at node 2
        """ # nopep8
        return self._cards[1].get_value("parm2")

    @parm2.setter
    def parm2(self, value: float) -> None:
        self._cards[1].set_value("parm2", value)

    @property
    def parm3(self) -> typing.Optional[float]:
        """Get or set the Based on beam type:
        Type.EQ.1: beam thickness, t direction at node 1
        Type.EQ.2: Itt
        Type.EQ.3: not used
        Type.EQ.4: beam thickness, t direction at node 1
        Type.EQ.5: beam thickness, t direction at node 1
        Type.EQ.6: local coordinate ID
        Type.EQ.7: not used.
        Type.EQ.8: not used.
        Type.EQ.9: beam thickness, t direction at node 1
        """ # nopep8
        return self._cards[1].get_value("parm3")

    @parm3.setter
    def parm3(self, value: float) -> None:
        self._cards[1].set_value("parm3", value)

    @property
    def parm4(self) -> typing.Optional[float]:
        """Get or set the Based on beam type:
        Type.EQ.1: beam thickness, t direction at node 2
        Type.EQ.2: Irr
        Type.EQ.3: not used
        Type.EQ.4: beam thickness, t direction at node 2
        Type.EQ.5: beam thickness, t direction at node 2
        Type.EQ.6: area
        Type.EQ.7: not used.
        Type.EQ.8: not used.
        Type.EQ.9: beam thickness, t direction at node 2
        """ # nopep8
        return self._cards[1].get_value("parm4")

    @parm4.setter
    def parm4(self, value: float) -> None:
        self._cards[1].set_value("parm4", value)

    @property
    def parm5(self) -> typing.Optional[float]:
        """Get or set the Based on beam type:
        Type.EQ.1: not used
        Type.EQ.2: shear area
        Type.EQ.3: not used
        Type.EQ.4: not used
        Type.EQ.5: not used
        Type.EQ.6: offset
        Type.EQ.7: not used
        Type.EQ.8: not used
        Type.EQ.9: not used
        """ # nopep8
        return self._cards[1].get_value("parm5")

    @parm5.setter
    def parm5(self, value: float) -> None:
        self._cards[1].set_value("parm5", value)

    @property
    def vol(self) -> typing.Optional[float]:
        """Get or set the Volume of discrete beam and scalar beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
        """ # nopep8
        return self._cards[2].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        self._cards[2].set_value("vol", value)

    @property
    def iner(self) -> typing.Optional[float]:
        """Get or set the Mass moment of inertia for the six degree of freedom discrete beam and scalar beam. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
        """ # nopep8
        return self._cards[2].get_value("iner")

    @iner.setter
    def iner(self, value: float) -> None:
        self._cards[2].set_value("iner", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93, 95, 97, 121, 146), see *DEFINE_COORDINATE_SYSTEM. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This option is not defined for material types than act between two nodal points, such as cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
        """ # nopep8
        return self._cards[2].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[2].set_value("cid", value)

    @property
    def dofn1(self) -> float:
        """Get or set the Active degree-of-freedom at node 1, a number between 1 to 6 where 1 in x-translation and 4 is x-rotation.
        """ # nopep8
        return self._cards[2].get_value("dofn1")

    @dofn1.setter
    def dofn1(self, value: float) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""dofn1 must be one of {1,2,3,4,5,6}""")
        self._cards[2].set_value("dofn1", value)

    @property
    def dofn2(self) -> float:
        """Get or set the Active degree-of-freedom at node 2, a number between 1 to 6.
        """ # nopep8
        return self._cards[2].get_value("dofn2")

    @dofn2.setter
    def dofn2(self, value: float) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""dofn2 must be one of {1,2,3,4,5,6}""")
        self._cards[2].set_value("dofn2", value)

