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

class Mat4AMicromec(KeywordBase):
    """DYNA MAT_4A_MICROMEC keyword"""

    keyword = "MAT"
    subkeyword = "4A_MICROMEC"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "mmopt",
                        float,
                        10,
                        10,
                        kwargs.get("mmopt", 0.0)
                    ),
                    Field(
                        "bupd",
                        float,
                        20,
                        10,
                        kwargs.get("bupd", 0.01)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "failm",
                        float,
                        50,
                        10,
                        kwargs.get("failm")
                    ),
                    Field(
                        "failf",
                        int,
                        60,
                        10,
                        kwargs.get("failf", 0)
                    ),
                    Field(
                        "numint",
                        float,
                        70,
                        10,
                        kwargs.get("numint", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "macf",
                        int,
                        10,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fvf",
                        float,
                        0,
                        10,
                        kwargs.get("fvf")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "fl",
                        float,
                        20,
                        10,
                        kwargs.get("fl")
                    ),
                    Field(
                        "fd",
                        float,
                        30,
                        10,
                        kwargs.get("fd", 1.0)
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "a11",
                        float,
                        50,
                        10,
                        kwargs.get("a11", 1.0)
                    ),
                    Field(
                        "a22",
                        float,
                        60,
                        10,
                        kwargs.get("a22")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rof",
                        float,
                        0,
                        10,
                        kwargs.get("rof")
                    ),
                    Field(
                        "el",
                        float,
                        10,
                        10,
                        kwargs.get("el")
                    ),
                    Field(
                        "et",
                        float,
                        20,
                        10,
                        kwargs.get("et")
                    ),
                    Field(
                        "glt",
                        float,
                        30,
                        10,
                        kwargs.get("glt")
                    ),
                    Field(
                        "prtl",
                        float,
                        40,
                        10,
                        kwargs.get("prtl")
                    ),
                    Field(
                        "prtt",
                        float,
                        50,
                        10,
                        kwargs.get("prtt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xt",
                        float,
                        0,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "slimxt",
                        float,
                        60,
                        10,
                        kwargs.get("slimxt")
                    ),
                    Field(
                        "ncyred",
                        float,
                        70,
                        10,
                        kwargs.get("ncyred", 10.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rom",
                        float,
                        0,
                        10,
                        kwargs.get("rom")
                    ),
                    Field(
                        "e",
                        float,
                        10,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigyt",
                        float,
                        0,
                        10,
                        kwargs.get("sigyt")
                    ),
                    Field(
                        "etant",
                        float,
                        10,
                        10,
                        kwargs.get("etant")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "eps0",
                        float,
                        40,
                        10,
                        kwargs.get("eps0")
                    ),
                    Field(
                        "c",
                        float,
                        50,
                        10,
                        kwargs.get("c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidt",
                        int,
                        0,
                        10,
                        kwargs.get("lcidt")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcdi",
                        int,
                        40,
                        10,
                        kwargs.get("lcdi")
                    ),
                    Field(
                        "upf",
                        float,
                        50,
                        10,
                        kwargs.get("upf")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ncyred2",
                        float,
                        70,
                        10,
                        kwargs.get("ncyred2", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat4AMicromec.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mmopt(self) -> float:
        """Get or set the Option to define micromechanical material behavior
        EQ.0.0: elastic
        EQ.1.0: elastic-plastic
        """ # nopep8
        return self._cards[0].get_value("mmopt")

    @mmopt.setter
    def mmopt(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""mmopt must be one of {0.0,1.0}""")
        self._cards[0].set_value("mmopt", value)

    @property
    def bupd(self) -> float:
        """Get or set the Tolerance for update of Strain-Concentration Tensor.
        """ # nopep8
        return self._cards[0].get_value("bupd")

    @bupd.setter
    def bupd(self, value: float) -> None:
        self._cards[0].set_value("bupd", value)

    @property
    def failm(self) -> typing.Optional[float]:
        """Get or set the Option for matrix failure using a ductile DIEM model. See sections Damage Initiation and Damage Evolution in the manual page for *MAT_ADD_DAMAGE_DIEM for a description of ductile damage initialization (DITYP = 0) based on stress triaxiality and a linear damage evolution (DETYP = 0) type. Also see fields LCDI and UPF on Card 9.
        LT.0.0: | FAILM | is effective plastic matrix strain at failure.When the matrix plastic strain reaches this value, the element is deleted from the calculation.
        EQ.0.0 : Only visualization(triaxiality of matrix stresses)
        EQ.1.0 : Active DIEM(triaxiality of matrix stresses)
        EQ.10.0 : Only visualization(triaxiality of composite stresses)
        EQ.11.0 : Active DIEM(triaxiality of composite stresses)
        """ # nopep8
        return self._cards[0].get_value("failm")

    @failm.setter
    def failm(self, value: float) -> None:
        self._cards[0].set_value("failm", value)

    @property
    def failf(self) -> int:
        """Get or set the Option for fiber failure
        EQ.0: only visualization (equivalent fiber stresses)
        EQ.1: active (equivalent fiber stresses.
        """ # nopep8
        return self._cards[0].get_value("failf")

    @failf.setter
    def failf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""failf must be one of {0,1}""")
        self._cards[0].set_value("failf", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of failed integration points prior to element deletion.
        LT.0.0: Only for shells. |NUMINT| is the percentage of
        integration points which must exceed the failure criterion before element fails. For shell formulations with 4 integration
        points per layer, the layer is considered failed if any of the integration points in the layer fails.
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[0].set_value("numint", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center.This option is for solid elements only.
        EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector, v,and an originating point, P, defining the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE - _COORDINATE_VECTOR).
        The fiber orientation information may be overwritten using* INITIAL_STRESS_(T)SHELL / SOLID
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed
        """ # nopep8
        return self._cards[1].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, -2, 3, -3, 4, -4]:
            raise Exception("""macf must be one of {1,2,-2,3,-3,4,-4}""")
        self._cards[1].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4..
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4..
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4..
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overwritten on the element card, see
        *ELEMENT_(T)SHELL_BETA or *ELEMENT_SOLID_ORTHO..
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[2].set_value("beta", value)

    @property
    def fvf(self) -> typing.Optional[float]:
        """Get or set the Fiber-Volume-Fraction
        GT.0: Fiber-Volume-Fraction
        LT.0: |FVF| Fiber-Mass-Fraction.
        """ # nopep8
        return self._cards[3].get_value("fvf")

    @fvf.setter
    def fvf(self, value: float) -> None:
        self._cards[3].set_value("fvf", value)

    @property
    def fl(self) -> typing.Optional[float]:
        """Get or set the Fiber length - if FD = 1 then FL = aspect ratio (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
        """ # nopep8
        return self._cards[3].get_value("fl")

    @fl.setter
    def fl(self, value: float) -> None:
        self._cards[3].set_value("fl", value)

    @property
    def fd(self) -> float:
        """Get or set the Fiber diameter (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
        """ # nopep8
        return self._cards[3].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[3].set_value("fd", value)

    @property
    def a11(self) -> float:
        """Get or set the Value of first principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID)..
        """ # nopep8
        return self._cards[3].get_value("a11")

    @a11.setter
    def a11(self, value: float) -> None:
        self._cards[3].set_value("a11", value)

    @property
    def a22(self) -> typing.Optional[float]:
        """Get or set the Value of second principal fiber orientation (may be overwritten by *INITIAL_STRESS_(T)SHELL/SOLID).
        """ # nopep8
        return self._cards[3].get_value("a22")

    @a22.setter
    def a22(self, value: float) -> None:
        self._cards[3].set_value("a22", value)

    @property
    def rof(self) -> typing.Optional[float]:
        """Get or set the Mass density of fiber.
        """ # nopep8
        return self._cards[4].get_value("rof")

    @rof.setter
    def rof(self, value: float) -> None:
        self._cards[4].set_value("rof", value)

    @property
    def el(self) -> typing.Optional[float]:
        """Get or set the EL, Young's modulus of fiber – longitudinal direction.
        """ # nopep8
        return self._cards[4].get_value("el")

    @el.setter
    def el(self, value: float) -> None:
        self._cards[4].set_value("el", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the ET, Young's modulus of fiber – transverse direction..
        """ # nopep8
        return self._cards[4].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[4].set_value("et", value)

    @property
    def glt(self) -> typing.Optional[float]:
        """Get or set the GLT, Shear modulus LT.
        """ # nopep8
        return self._cards[4].get_value("glt")

    @glt.setter
    def glt(self, value: float) -> None:
        self._cards[4].set_value("glt", value)

    @property
    def prtl(self) -> typing.Optional[float]:
        """Get or set the TL, Poisson's ratio TL.
        """ # nopep8
        return self._cards[4].get_value("prtl")

    @prtl.setter
    def prtl(self, value: float) -> None:
        self._cards[4].set_value("prtl", value)

    @property
    def prtt(self) -> typing.Optional[float]:
        """Get or set the TT, Poisson's ratio TT
        """ # nopep8
        return self._cards[4].get_value("prtt")

    @prtt.setter
    def prtt(self, value: float) -> None:
        self._cards[4].set_value("prtt", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Fiber tensile strength – longitudinal direction.
        """ # nopep8
        return self._cards[5].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[5].set_value("xt", value)

    @property
    def slimxt(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit in the fiber after stress maximum (fiber tension).
        """ # nopep8
        return self._cards[5].get_value("slimxt")

    @slimxt.setter
    def slimxt(self, value: float) -> None:
        self._cards[5].set_value("slimxt", value)

    @property
    def ncyred(self) -> float:
        """Get or set the Number of cycles for stress reduction from maximum to minimum (fiber tension).
        """ # nopep8
        return self._cards[5].get_value("ncyred")

    @ncyred.setter
    def ncyred(self, value: float) -> None:
        self._cards[5].set_value("ncyred", value)

    @property
    def rom(self) -> typing.Optional[float]:
        """Get or set the Mass density of matrix..
        """ # nopep8
        return self._cards[6].get_value("rom")

    @rom.setter
    def rom(self, value: float) -> None:
        self._cards[6].set_value("rom", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of matrix.
        """ # nopep8
        return self._cards[6].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[6].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio of matrix.
        """ # nopep8
        return self._cards[6].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[6].set_value("pr", value)

    @property
    def sigyt(self) -> typing.Optional[float]:
        """Get or set the Yield stress of matrix in tension.
        """ # nopep8
        return self._cards[7].get_value("sigyt")

    @sigyt.setter
    def sigyt(self, value: float) -> None:
        self._cards[7].set_value("sigyt", value)

    @property
    def etant(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus of matrix in tension, ignore if (LCST.GT.0.) is defined.
        """ # nopep8
        return self._cards[7].get_value("etant")

    @etant.setter
    def etant(self, value: float) -> None:
        self._cards[7].set_value("etant", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Quasi-static threshold strain rate (Johnson-Cook model) for bi-linear hardening.
        """ # nopep8
        return self._cards[7].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[7].set_value("eps0", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Johnson-Cook constant for bi-linear hardening.
        """ # nopep8
        return self._cards[7].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[7].set_value("c", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID for defining effective stress versus
        effective plastic strain in tension of matrix material (Table to include strain-rate effects, viscoplastic formulation).
        """ # nopep8
        return self._cards[8].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[8].set_value("lcidt", value)

    @property
    def lcdi(self) -> typing.Optional[int]:
        """Get or set the Damage initiation parameter (ductile) shells:
        Load curve ID representing plastic strain at onset of damage as function of stress triaxiality.
        or Table ID representing plastic strain at onset of damage as function of stress triaxiality and plastic strain rate.
        solids: Load curve ID representing plastic strain at onset of damage as function of stress triaxiality.
        or Table ID representing plastic strain at onset of damage as function of stress triaxiality and lode angle.
        or Table3D ID representing plastic strain at onset of damage as
        function of stress triaxiality, lode angle and plastic strain rate..
        """ # nopep8
        return self._cards[8].get_value("lcdi")

    @lcdi.setter
    def lcdi(self, value: int) -> None:
        self._cards[8].set_value("lcdi", value)

    @property
    def upf(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter
        GT.0.0: plastic displacement at failure, 𝑢𝑓	𝑝
        LT.0.0: |UPF| is a table ID for 𝑢𝑓 𝑝 as a function of triaxiality and	damage.
        """ # nopep8
        return self._cards[8].get_value("upf")

    @upf.setter
    def upf(self, value: float) -> None:
        self._cards[8].set_value("upf", value)

    @property
    def ncyred2(self) -> float:
        """Get or set the In case of matrix failure (IFAILM.eq.1 or 11):
        Number of cycles for stress reduction of fiber stresses until the integration point will be marked as failed.
        """ # nopep8
        return self._cards[8].get_value("ncyred2")

    @ncyred2.setter
    def ncyred2(self, value: float) -> None:
        self._cards[8].set_value("ncyred2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

