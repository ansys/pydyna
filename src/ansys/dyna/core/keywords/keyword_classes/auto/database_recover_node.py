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

"""Module providing the DatabaseRecoverNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseRecoverNode(KeywordBase):
    """DYNA DATABASE_RECOVER_NODE keyword"""

    keyword = "DATABASE"
    subkeyword = "RECOVER_NODE"

    def __init__(self, **kwargs):
        """Initialize the DatabaseRecoverNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iax",
                        str,
                        10,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                    Field(
                        "iay",
                        str,
                        20,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                    Field(
                        "iaz",
                        str,
                        30,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                    Field(
                        "method",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ivx",
                        str,
                        50,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                    Field(
                        "ivy",
                        str,
                        60,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                    Field(
                        "ivz",
                        str,
                        70,
                        10,
                        "SMNPD",
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of solid elements whose nodal stress will be recovered.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def iax(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("iax")

    @iax.setter
    def iax(self, value: str) -> None:
        """Set the iax property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""iax must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("iax", value)

    @property
    def iay(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("iay")

    @iay.setter
    def iay(self, value: str) -> None:
        """Set the iay property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""iay must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("iay", value)

    @property
    def iaz(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("iaz")

    @iaz.setter
    def iaz(self, value: str) -> None:
        """Set the iaz property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""iaz must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("iaz", value)

    @property
    def method(self) -> int:
        """Get or set the Method used to recover the nodal stress
        EQ.0:	Zienkiewicz-Zhu's Superconvergent Patch Recovery method
        EQ.1:	Elemental extrapolation method.
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        """Set the method property."""
        if value not in [0, 1, None]:
            raise Exception("""method must be `None` or one of {0,1}.""")
        self._cards[0].set_value("method", value)

    @property
    def ivx(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("ivx")

    @ivx.setter
    def ivx(self, value: str) -> None:
        """Set the ivx property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""ivx must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("ivx", value)

    @property
    def ivy(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("ivy")

    @ivy.setter
    def ivy(self, value: str) -> None:
        """Set the ivy property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""ivy must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("ivy", value)

    @property
    def ivz(self) -> str:
        """Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
        EQ.SMNPD: the minimum principal deviator stress
        EQ.SMNPR : the minimum principal stress
        EQ.SMXPD : the maximum principal deviator stress
        EQ.SMXPR : the maximum principal stress
        EQ.SMXSH : the maximum shear stress
        EQ.SPR : nodal pressure
        EQ.SVM : nodal von Mises stress
        EQ.SXX : nodal normal stress along x direction
        EQ.SYY : nodal normal stress along y direction
        EQ.SZZ : nodal normal stress along z direction
        EQ.SXY : nodal shear stress along x - y direction
        EQ.SYZ : nodal shear stress along y - z direction
        EQ.SZX : nodal shear stress along z - x direction
        For shell elements append either "B" or "T" to the input string to
        recover nodal stresses at the bottom or top layer of shell elements.
        For example, SPRT recovers the nodal pressure at the top layer.
        """ # nopep8
        return self._cards[0].get_value("ivz")

    @ivz.setter
    def ivz(self, value: str) -> None:
        """Set the ivz property."""
        if value not in ["SMNPD", "SMNPR", "SMXPD", "SMXPR", "SMXSH", "SPR", "SXX", "SYY", "SZZ", "SXY", "SYZ", "SZX", "SVM", None]:
            raise Exception("""ivz must be `None` or one of {"SMNPD","SMNPR","SMXPD","SMXPR","SMXSH","SPR","SXX","SYY","SZZ","SXY","SYZ","SZX","SVM"}.""")
        self._cards[0].set_value("ivz", value)

