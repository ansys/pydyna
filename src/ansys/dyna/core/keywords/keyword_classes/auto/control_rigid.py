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

class ControlRigid(KeywordBase):
    """DYNA CONTROL_RIGID keyword"""

    keyword = "CONTROL"
    subkeyword = "RIGID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lmf",
                        int,
                        0,
                        10,
                        kwargs.get("lmf", 0)
                    ),
                    Field(
                        "jntf",
                        int,
                        10,
                        10,
                        kwargs.get("jntf", 0)
                    ),
                    Field(
                        "orthmd",
                        int,
                        20,
                        10,
                        kwargs.get("orthmd", 0)
                    ),
                    Field(
                        "partm",
                        int,
                        30,
                        10,
                        kwargs.get("partm", 0)
                    ),
                    Field(
                        "sparse",
                        int,
                        40,
                        10,
                        kwargs.get("sparse", 0)
                    ),
                    Field(
                        "metalf",
                        int,
                        50,
                        10,
                        kwargs.get("metalf", 0)
                    ),
                    Field(
                        "plotel",
                        int,
                        60,
                        10,
                        kwargs.get("plotel", 0)
                    ),
                    Field(
                        "rbsms",
                        int,
                        70,
                        10,
                        kwargs.get("rbsms", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "norbic",
                        int,
                        0,
                        10,
                        kwargs.get("norbic", 0)
                    ),
                    Field(
                        "gjadstf",
                        float,
                        10,
                        10,
                        kwargs.get("gjadstf", 0.0)
                    ),
                    Field(
                        "gjadvsc",
                        float,
                        20,
                        10,
                        kwargs.get("gjadvsc", 0.0)
                    ),
                    Field(
                        "tjastf",
                        float,
                        30,
                        10,
                        kwargs.get("tjastf", 0.0)
                    ),
                    Field(
                        "tjadvsc",
                        float,
                        40,
                        10,
                        kwargs.get("tjadvsc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def lmf(self) -> int:
        """Get or set the Joint formulation flag for explicit analysis.  This flag can be used to switch to an implicit formulation for joints (*CONSTRAINED_JOINT_Option) which uses Lagrange multipliers to impose prescribed kinematic boundary conditions and joint constraints.  There is a slight cost overhead due to the assembly of sparse matrix equations which are solved using standard procedures for nonlinear problems in rigid multi-body dynamics.
        EQ.0:	penalty formulation for joints(default)
        EQ.1 : Lagrange - multiplier - based formulation for joints
        """ # nopep8
        return self._cards[0].get_value("lmf")

    @lmf.setter
    def lmf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lmf must be one of {0,1}""")
        self._cards[0].set_value("lmf", value)

    @property
    def jntf(self) -> int:
        """Get or set the Generalized joint stiffness formulation:
        EQ.0: incremental update,
        EQ.1: total formulation (exact).
        EQ.2: total formulation intended for implicit analysis.
        """ # nopep8
        return self._cards[0].get_value("jntf")

    @jntf.setter
    def jntf(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""jntf must be one of {0,1,2}""")
        self._cards[0].set_value("jntf", value)

    @property
    def orthmd(self) -> int:
        """Get or set the Othogonalize modes with respect to each other:
        EQ.0: true.
        EQ.1: total formulation (exact).
        """ # nopep8
        return self._cards[0].get_value("orthmd")

    @orthmd.setter
    def orthmd(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""orthmd must be one of {0,1}""")
        self._cards[0].set_value("orthmd", value)

    @property
    def partm(self) -> int:
        """Get or set the Use global mass matrix to determine part mass distribution. This mass matrix may contain mass from other parts that share nodes.
        EQ.0: true,
        EQ.1: false.
        """ # nopep8
        return self._cards[0].get_value("partm")

    @partm.setter
    def partm(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""partm must be one of {0,1}""")
        self._cards[0].set_value("partm", value)

    @property
    def sparse(self) -> int:
        """Get or set the Use sparse matrix multiply subroutines for the modal stuffness and damping matrices.
        EQ.0: false, do full matrix multiplies (frequently faster),
        EQ.1: true.
        """ # nopep8
        return self._cards[0].get_value("sparse")

    @sparse.setter
    def sparse(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sparse must be one of {0,1}""")
        self._cards[0].set_value("sparse", value)

    @property
    def metalf(self) -> int:
        """Get or set the Metalforming option, which should not be used for crash and other applications involving rigid bodies. Use fast update of rigid body nodes. If this option is active the rotational motion of all rigid bodies should be surpressed.
        EQ.0: full treatment is used,
        EQ.1: fast update for metalforming applications.
        """ # nopep8
        return self._cards[0].get_value("metalf")

    @metalf.setter
    def metalf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""metalf must be one of {0,1}""")
        self._cards[0].set_value("metalf", value)

    @property
    def plotel(self) -> int:
        """Get or set the Automatic generation of *ELEMENT_PLOTEL for *CONSTRAINED_NODAL_RIGID_BODY.
        EQ.0: no generation
        EQ.1: one part is generated for all nodal rigid bodies with the PID set to 1000000.
        EQ.2: one part is generated for each nodal rigid body in the problem with a part ID of 1000000+PID, where PID is the nodal rigid body ID.
        """ # nopep8
        return self._cards[0].get_value("plotel")

    @plotel.setter
    def plotel(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""plotel must be one of {0,1,2}""")
        self._cards[0].set_value("plotel", value)

    @property
    def rbsms(self) -> int:
        """Get or set the Flag to apply consistent treatment of rigid bodies in selective mass scaling.
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_value("rbsms")

    @rbsms.setter
    def rbsms(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""rbsms must be one of {0,1}""")
        self._cards[0].set_value("rbsms", value)

    @property
    def norbic(self) -> int:
        """Get or set the Circumvent rigid body inertia check, see Remark 5.
        EQ.0:	Off
        EQ.1:	On.
        """ # nopep8
        return self._cards[1].get_value("norbic")

    @norbic.setter
    def norbic(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""norbic must be one of {0,1}""")
        self._cards[1].set_value("norbic", value)

    @property
    def gjadstf(self) -> float:
        """Get or set the Rotational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in LCIDPH, LCIDT, LCIDPS that equal to GJADSTF.  For models with many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
        """ # nopep8
        return self._cards[1].get_value("gjadstf")

    @gjadstf.setter
    def gjadstf(self, value: float) -> None:
        self._cards[1].set_value("gjadstf", value)

    @property
    def gjadvsc(self) -> float:
        """Get or set the Rotational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in DLCIDPH, DLCIDT, DLCIDPS that equal to GJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint.
        """ # nopep8
        return self._cards[1].get_value("gjadvsc")

    @gjadvsc.setter
    def gjadvsc(self, value: float) -> None:
        self._cards[1].set_value("gjadvsc", value)

    @property
    def tjastf(self) -> float:
        """Get or set the Translational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in LCIDX, LCIDY, LCIDZ that equal to TJADSTF.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
        """ # nopep8
        return self._cards[1].get_value("tjastf")

    @tjastf.setter
    def tjastf(self, value: float) -> None:
        self._cards[1].set_value("tjastf", value)

    @property
    def tjadvsc(self) -> float:
        """Get or set the Translational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in DLCIDX, DLCIDY, DLCIDZ that equal to TJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
        """ # nopep8
        return self._cards[1].get_value("tjadvsc")

    @tjadvsc.setter
    def tjadvsc(self, value: float) -> None:
        self._cards[1].set_value("tjadvsc", value)

