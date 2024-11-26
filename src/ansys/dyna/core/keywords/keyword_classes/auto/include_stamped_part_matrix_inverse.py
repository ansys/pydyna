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

class IncludeStampedPartMatrixInverse(KeywordBase):
    """DYNA INCLUDE_STAMPED_PART_MATRIX_INVERSE keyword"""

    keyword = "INCLUDE"
    subkeyword = "STAMPED_PART_MATRIX_INVERSE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "thick",
                        int,
                        10,
                        10,
                        kwargs.get("thick", 0)
                    ),
                    Field(
                        "pstrn",
                        int,
                        20,
                        10,
                        kwargs.get("pstrn", 0)
                    ),
                    Field(
                        "strain",
                        int,
                        30,
                        10,
                        kwargs.get("strain", 0)
                    ),
                    Field(
                        "stress",
                        int,
                        40,
                        10,
                        kwargs.get("stress", 0)
                    ),
                    Field(
                        "incout",
                        int,
                        50,
                        10,
                        kwargs.get("incout", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "rmax",
                        float,
                        70,
                        10,
                        kwargs.get("rmax", 20.)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r11",
                        float,
                        0,
                        10,
                        kwargs.get("r11")
                    ),
                    Field(
                        "r12",
                        float,
                        10,
                        10,
                        kwargs.get("r12")
                    ),
                    Field(
                        "r13",
                        float,
                        20,
                        10,
                        kwargs.get("r13")
                    ),
                    Field(
                        "xp",
                        float,
                        30,
                        10,
                        kwargs.get("xp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r21",
                        float,
                        0,
                        10,
                        kwargs.get("r21")
                    ),
                    Field(
                        "r22",
                        float,
                        10,
                        10,
                        kwargs.get("r22")
                    ),
                    Field(
                        "r23",
                        float,
                        20,
                        10,
                        kwargs.get("r23")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r31",
                        float,
                        0,
                        10,
                        kwargs.get("r31")
                    ),
                    Field(
                        "r32",
                        float,
                        10,
                        10,
                        kwargs.get("r32")
                    ),
                    Field(
                        "r33",
                        float,
                        20,
                        10,
                        kwargs.get("r33")
                    ),
                    Field(
                        "zp",
                        float,
                        30,
                        10,
                        kwargs.get("zp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "isym",
                        int,
                        0,
                        10,
                        kwargs.get("isym", 0)
                    ),
                    Field(
                        "iafter",
                        int,
                        10,
                        10,
                        kwargs.get("iafter", 0)
                    ),
                    Field(
                        "percele",
                        float,
                        20,
                        10,
                        kwargs.get("percele")
                    ),
                    Field(
                        "iortho",
                        int,
                        30,
                        10,
                        kwargs.get("iortho")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "isrocut",
                        int,
                        50,
                        10,
                        kwargs.get("isrocut")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x01",
                        float,
                        0,
                        10,
                        kwargs.get("x01")
                    ),
                    Field(
                        "y01",
                        float,
                        10,
                        10,
                        kwargs.get("y01")
                    ),
                    Field(
                        "z01",
                        float,
                        20,
                        10,
                        kwargs.get("z01")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x02",
                        float,
                        0,
                        10,
                        kwargs.get("x02")
                    ),
                    Field(
                        "y02",
                        float,
                        10,
                        10,
                        kwargs.get("y02")
                    ),
                    Field(
                        "z02",
                        float,
                        20,
                        10,
                        kwargs.get("z02")
                    ),
                    Field(
                        "x03",
                        float,
                        30,
                        10,
                        kwargs.get("x03")
                    ),
                    Field(
                        "y03",
                        float,
                        40,
                        10,
                        kwargs.get("y03")
                    ),
                    Field(
                        "z03",
                        float,
                        50,
                        10,
                        kwargs.get("z03")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of crash part for remapping.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def thick(self) -> int:
        """Get or set the Thickness remap:
        EQ.0: map thickness
        EQ.1: do not map thickness
        EQ.2:	Average value inside a circle defined by RMAX
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""thick must be one of {0,1,2}""")
        self._cards[1].set_value("thick", value)

    @property
    def pstrn(self) -> int:
        """Get or set the Plastic strain remap:
        EQ.0: map plastic strain
        EQ.1: do not plastic strain
        EQ.2:	Average value inside a circle defined by RMAX
        """ # nopep8
        return self._cards[1].get_value("pstrn")

    @pstrn.setter
    def pstrn(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""pstrn must be one of {0,1,2}""")
        self._cards[1].set_value("pstrn", value)

    @property
    def strain(self) -> int:
        """Get or set the Strain remap:
        EQ.0: map strains
        EQ.1: do not map strains
        """ # nopep8
        return self._cards[1].get_value("strain")

    @strain.setter
    def strain(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""strain must be one of {0,1}""")
        self._cards[1].set_value("strain", value)

    @property
    def stress(self) -> int:
        """Get or set the Stress tensor remap:
        EQ.0: map stress tensorand history variables
        EQ.1:do not map stress tensor. only history varibales
        EQ.2:	Do not map stress tensor or history variables
        EQ. - 1:	Map stress tensor in an internal large format(binary files)
        EQ. - 3 : Do not map stress tensor in an internal large format, only history variables(binary files)
        """ # nopep8
        return self._cards[1].get_value("stress")

    @stress.setter
    def stress(self, value: int) -> None:
        if value not in [0, 1, 2, -1, -3]:
            raise Exception("""stress must be one of {0,1,2,-1,-3}""")
        self._cards[1].set_value("stress", value)

    @property
    def incout(self) -> int:
        """Get or set the Save mapped data:
        EQ.1:	Save the mapped data for the part / part set(PID) to a file called dyna.inc.This option is useful for when the mapped data may be required in a future simulation.
        EQ.2 : Save the mapped data for the specified part or part set(PID) to a file called dynain_‌xx(xx is the part or part set ID).
        EQ.3 : Save the mapped data for the specified part or part set(PID) to a file called nastran_‌xx(in nastran format).xx is the part or part set ID.
        """ # nopep8
        return self._cards[1].get_value("incout")

    @incout.setter
    def incout(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""incout must be one of {0,1,2,3}""")
        self._cards[1].set_value("incout", value)

    @property
    def rmax(self) -> float:
        """Get or set the Search radius.  LS-DYNA remaps history variables from the mesh of the stamped part to the mesh of the crash part with a spatial tolerance of RMAX.  If an element in the crash part lies within RMAX of the stamped part, data will be mapped to that element.  If set less than 0.001, RMAX automatically assumes the default value of 20.
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[1].set_value("rmax", value)

    @property
    def r11(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[2].get_value("r11")

    @r11.setter
    def r11(self, value: float) -> None:
        self._cards[2].set_value("r11", value)

    @property
    def r12(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[2].get_value("r12")

    @r12.setter
    def r12(self, value: float) -> None:
        self._cards[2].set_value("r12", value)

    @property
    def r13(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[2].get_value("r13")

    @r13.setter
    def r13(self, value: float) -> None:
        self._cards[2].set_value("r13", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Translational distance
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def r21(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[3].get_value("r21")

    @r21.setter
    def r21(self, value: float) -> None:
        self._cards[3].set_value("r21", value)

    @property
    def r22(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[3].get_value("r22")

    @r22.setter
    def r22(self, value: float) -> None:
        self._cards[3].set_value("r22", value)

    @property
    def r23(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[3].get_value("r23")

    @r23.setter
    def r23(self, value: float) -> None:
        self._cards[3].set_value("r23", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Translational distance
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[3].set_value("yp", value)

    @property
    def r31(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[4].get_value("r31")

    @r31.setter
    def r31(self, value: float) -> None:
        self._cards[4].set_value("r31", value)

    @property
    def r32(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[4].get_value("r32")

    @r32.setter
    def r32(self, value: float) -> None:
        self._cards[4].set_value("r32", value)

    @property
    def r33(self) -> typing.Optional[float]:
        """Get or set the Components of the transformation matrix
        """ # nopep8
        return self._cards[4].get_value("r33")

    @r33.setter
    def r33(self, value: float) -> None:
        self._cards[4].set_value("r33", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Translational distance
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def isym(self) -> int:
        """Get or set the Symmetric switch
        EQ.0:no symetric mapping
        EQ.1: yz plane symmetric mapping
        EQ.2: zx plane symmetric mapping
        EQ.3: zx and yz planes symmetric mapping
        EQ.4: user defined symmetric plane mapping
        """ # nopep8
        return self._cards[5].get_value("isym")

    @isym.setter
    def isym(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""isym must be one of {0,1,2,3,4}""")
        self._cards[5].set_value("isym", value)

    @property
    def iafter(self) -> int:
        """Get or set the Mirroring sequence switch
        EQ.0: generate a symmetric part before transformation
        EQ.1: generate a symmetric part after transformation
        """ # nopep8
        return self._cards[5].get_value("iafter")

    @iafter.setter
    def iafter(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iafter must be one of {0,1}""")
        self._cards[5].set_value("iafter", value)

    @property
    def percele(self) -> typing.Optional[float]:
        """Get or set the Percentage of elements that should be mapped for the simulation to proceed (default = 0); otherwise an error termination occurs. See Remark 6
        """ # nopep8
        return self._cards[5].get_value("percele")

    @percele.setter
    def percele(self, value: float) -> None:
        self._cards[5].set_value("percele", value)

    @property
    def iortho(self) -> typing.Optional[int]:
        """Get or set the Location of the material direction cosine in the array of history variables of an orthotropic material
        """ # nopep8
        return self._cards[5].get_value("iortho")

    @iortho.setter
    def iortho(self, value: int) -> None:
        self._cards[5].set_value("iortho", value)

    @property
    def isrocut(self) -> typing.Optional[int]:
        """Get or set the Optional output of stamped part after transformation(s)
        EQ.0:	No output is written.
        NE.0 : Keyword output file “srcmsh_‌<ISRCOUT>” is created
        """ # nopep8
        return self._cards[5].get_value("isrocut")

    @isrocut.setter
    def isrocut(self, value: int) -> None:
        self._cards[5].set_value("isrocut", value)

    @property
    def x01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[6].get_value("x01")

    @x01.setter
    def x01(self, value: float) -> None:
        self._cards[6].set_value("x01", value)

    @property
    def y01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[6].get_value("y01")

    @y01.setter
    def y01(self, value: float) -> None:
        self._cards[6].set_value("y01", value)

    @property
    def z01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[6].get_value("z01")

    @z01.setter
    def z01(self, value: float) -> None:
        self._cards[6].set_value("z01", value)

    @property
    def x02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("x02")

    @x02.setter
    def x02(self, value: float) -> None:
        self._cards[7].set_value("x02", value)

    @property
    def y02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("y02")

    @y02.setter
    def y02(self, value: float) -> None:
        self._cards[7].set_value("y02", value)

    @property
    def z02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("z02")

    @z02.setter
    def z02(self, value: float) -> None:
        self._cards[7].set_value("z02", value)

    @property
    def x03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("x03")

    @x03.setter
    def x03(self, value: float) -> None:
        self._cards[7].set_value("x03", value)

    @property
    def y03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("y03")

    @y03.setter
    def y03(self, value: float) -> None:
        self._cards[7].set_value("y03", value)

    @property
    def z03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[7].get_value("z03")

    @z03.setter
    def z03(self, value: float) -> None:
        self._cards[7].set_value("z03", value)

