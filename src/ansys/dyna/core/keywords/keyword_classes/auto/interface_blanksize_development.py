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

class InterfaceBlanksizeDevelopment(KeywordBase):
    """DYNA INTERFACE_BLANKSIZE_DEVELOPMENT keyword"""

    keyword = "INTERFACE"
    subkeyword = "BLANKSIZE_DEVELOPMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ioption",
                        int,
                        0,
                        10,
                        kwargs.get("ioption", 1)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "iadapt",
                        int,
                        20,
                        10,
                        kwargs.get("iadapt")
                    ),
                    Field(
                        "maxsize",
                        float,
                        30,
                        10,
                        kwargs.get("maxsize", 30.0)
                    ),
                    Field(
                        "referenc",
                        int,
                        40,
                        10,
                        kwargs.get("referenc", 0)
                    ),
                    Field(
                        "space",
                        float,
                        50,
                        10,
                        kwargs.get("space", 2.0)
                    ),
                    Field(
                        "maxgap",
                        float,
                        60,
                        10,
                        kwargs.get("maxgap", 30.0)
                    ),
                    Field(
                        "orient",
                        float,
                        70,
                        10,
                        kwargs.get("orient")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename1",
                        str,
                        0,
                        80,
                        kwargs.get("filename1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename2",
                        str,
                        0,
                        80,
                        kwargs.get("filename2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename3",
                        str,
                        0,
                        80,
                        kwargs.get("filename3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename4",
                        str,
                        0,
                        80,
                        kwargs.get("filename4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename13",
                        str,
                        0,
                        80,
                        kwargs.get("filename13")
                    ),
                ],
            ),
        ]

    @property
    def ioption(self) -> int:
        """Get or set the Target definition input type:
        EQ.1: (entire) blank mesh in keyword format.
        EQ.2: consecutive position coordinates of blank boundary loop
        curve in XYZ format. Blank geometry is located to the left side of the
        looped curve, as shown in Remarks below.
        EQ.2: consecutive position coordinates of blank boundary loop in
        XYZ format. Blank geometry is located to the right side of the looped	curve, as shown in Remarks below.
        """ # nopep8
        return self._cards[0].get_value("ioption")

    @ioption.setter
    def ioption(self, value: int) -> None:
        if value not in [1, 2, -2]:
            raise Exception("""ioption must be one of {1,2,-2}""")
        self._cards[0].set_value("ioption", value)

    @property
    def iadapt(self) -> typing.Optional[int]:
        """Get or set the Adaptive mesh control flag. If IADAPT=1, number of elements between
        initial (FILENAME2) and simulated blank (FILENAME3) meshes can be
        different, avoiding using sheet blank from the file �adapt.msh (set IOFLAG=1 in *CONTROL_ADAPTIVE) for the initial blank mesh.
        """ # nopep8
        return self._cards[0].get_value("iadapt")

    @iadapt.setter
    def iadapt(self, value: int) -> None:
        self._cards[0].set_value("iadapt", value)

    @property
    def maxsize(self) -> float:
        """Get or set the The expected maximum change in initial blank size. It is used where the initial blank is not flat, and the curvature is large in the boundary region.
        """ # nopep8
        return self._cards[0].get_value("maxsize")

    @maxsize.setter
    def maxsize(self, value: float) -> None:
        self._cards[0].set_value("maxsize", value)

    @property
    def referenc(self) -> int:
        """Get or set the Flag to indicate trim curve projection to a reference surface (mesh):
        EQ.0: no projection.
        EQ.1: the trim curves will be projected to the reference surface.In addition, the mesh file for the reference surface is given in FILENAME4.
        """ # nopep8
        return self._cards[0].get_value("referenc")

    @referenc.setter
    def referenc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""referenc must be one of {0,1}""")
        self._cards[0].set_value("referenc", value)

    @property
    def space(self) -> float:
        """Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
        """ # nopep8
        return self._cards[0].get_value("space")

    @space.setter
    def space(self, value: float) -> None:
        self._cards[0].set_value("space", value)

    @property
    def maxgap(self) -> float:
        """Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
        """ # nopep8
        return self._cards[0].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        self._cards[0].set_value("maxgap", value)

    @property
    def orient(self) -> typing.Optional[float]:
        """Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
        """ # nopep8
        return self._cards[0].get_value("orient")

    @orient.setter
    def orient(self, value: float) -> None:
        self._cards[0].set_value("orient", value)

    @property
    def filename1(self) -> typing.Optional[str]:
        """Get or set the The following file names, FILENAME1~3 are for the option DEVELOPMENT:
        Target input file name. If a blank mesh is used, the keyword file must contain
        *NODE and *ELEMENT_SHELL; if blank boundary is used, the file
        must consist of *DEFINE_TARGET_BOUNDARY. Once defined, the target
        never needs to be changed in an iterative optimization loop.
        """ # nopep8
        return self._cards[1].get_value("filename1")

    @filename1.setter
    def filename1(self, value: str) -> None:
        self._cards[1].set_value("filename1", value)

    @property
    def filename2(self) -> typing.Optional[str]:
        """Get or set the Simulated (formed or flanged) sheet blank mesh in keyword format. This
        can be the final state mesh from the current simulation.
        """ # nopep8
        return self._cards[2].get_value("filename2")

    @filename2.setter
    def filename2(self, value: str) -> None:
        self._cards[2].set_value("filename2", value)

    @property
    def filename3(self) -> typing.Optional[str]:
        """Get or set the Initial sheet blank mesh in keyword format. This can be the first state mesh
        from the current simulation. If IADAPT=1, then this mesh can just be a
        regular blank mesh (without adaptivity).
        """ # nopep8
        return self._cards[3].get_value("filename3")

    @filename3.setter
    def filename3(self, value: str) -> None:
        self._cards[3].set_value("filename3", value)

    @property
    def filename4(self) -> typing.Optional[str]:
        """Get or set the Reference surface (mesh) to extend the initial blank shape for trim curve projection (Figure 25-5) in keyword format. This file name needs to be defined when REFERENC is set to '1'.
        """ # nopep8
        return self._cards[4].get_value("filename4")

    @filename4.setter
    def filename4(self, value: str) -> None:
        self._cards[4].set_value("filename4", value)

    @property
    def filename13(self) -> typing.Optional[str]:
        """Get or set the Reference surface onto which adjustments to the blanks trim curves in its final state are projected (ref4.k in Figure 0-7).  This surface is typically a curved extension of the formed blank and must be defined as mesh in keyword format.  This file name must be defined when ORIENT is set to 2.
        """ # nopep8
        return self._cards[5].get_value("filename13")

    @filename13.setter
    def filename13(self, value: str) -> None:
        self._cards[5].set_value("filename13", value)

