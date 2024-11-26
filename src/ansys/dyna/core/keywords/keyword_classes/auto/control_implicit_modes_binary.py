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

class ControlImplicitModesBinary(KeywordBase):
    """DYNA CONTROL_IMPLICIT_MODES_BINARY keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_MODES_BINARY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsidc",
                        int,
                        0,
                        10,
                        kwargs.get("nsidc", 0)
                    ),
                    Field(
                        "nsida",
                        int,
                        10,
                        10,
                        kwargs.get("nsida", 0)
                    ),
                    Field(
                        "neig",
                        int,
                        20,
                        10,
                        kwargs.get("neig")
                    ),
                    Field(
                        "ibase",
                        int,
                        30,
                        10,
                        kwargs.get("ibase")
                    ),
                    Field(
                        "se_mass",
                        str,
                        40,
                        10,
                        kwargs.get("se_mass")
                    ),
                    Field(
                        "se_damp",
                        str,
                        50,
                        10,
                        kwargs.get("se_damp")
                    ),
                    Field(
                        "se_stiff",
                        str,
                        60,
                        10,
                        kwargs.get("se_stiff")
                    ),
                    Field(
                        "se_inert",
                        str,
                        70,
                        10,
                        kwargs.get("se_inert")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        256,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

    @property
    def nsidc(self) -> int:
        """Get or set the Node set ID for constraint modes:
        EQ.0: no constraint modes will be generated
        """ # nopep8
        return self._cards[0].get_value("nsidc")

    @nsidc.setter
    def nsidc(self, value: int) -> None:
        self._cards[0].set_value("nsidc", value)

    @property
    def nsida(self) -> int:
        """Get or set the Node set ID for attachment modes
        EQ.0: no attachment modes will be generated
        """ # nopep8
        return self._cards[0].get_value("nsida")

    @nsida.setter
    def nsida(self, value: int) -> None:
        self._cards[0].set_value("nsida", value)

    @property
    def neig(self) -> typing.Optional[int]:
        """Get or set the Number of eigenmodes. EQ.0:  no attachment modes will be generated
        """ # nopep8
        return self._cards[0].get_value("neig")

    @neig.setter
    def neig(self, value: int) -> None:
        self._cards[0].set_value("neig", value)

    @property
    def ibase(self) -> typing.Optional[int]:
        """Get or set the Offset for numbering of the generalized internal degrees of freedom for the superelement.
        """ # nopep8
        return self._cards[0].get_value("ibase")

    @ibase.setter
    def ibase(self, value: int) -> None:
        self._cards[0].set_value("ibase", value)

    @property
    def se_mass(self) -> typing.Optional[str]:
        """Get or set the Name of the superelement mass matrix.  If left blank it is not generated.
        """ # nopep8
        return self._cards[0].get_value("se_mass")

    @se_mass.setter
    def se_mass(self, value: str) -> None:
        self._cards[0].set_value("se_mass", value)

    @property
    def se_damp(self) -> typing.Optional[str]:
        """Get or set the Name of the superelement damping matrix.  If left blank it is not generated.
        """ # nopep8
        return self._cards[0].get_value("se_damp")

    @se_damp.setter
    def se_damp(self, value: str) -> None:
        self._cards[0].set_value("se_damp", value)

    @property
    def se_stiff(self) -> typing.Optional[str]:
        """Get or set the Name of the superelement stiffness matrix.  If left blank it is not generated.
        """ # nopep8
        return self._cards[0].get_value("se_stiff")

    @se_stiff.setter
    def se_stiff(self, value: str) -> None:
        self._cards[0].set_value("se_stiff", value)

    @property
    def se_inert(self) -> typing.Optional[str]:
        """Get or set the Name of the superelement inertia matrix, required for gravity loading applications of the superelement.  If left blank it is not generated.
        """ # nopep8
        return self._cards[0].get_value("se_inert")

    @se_inert.setter
    def se_inert(self, value: str) -> None:
        self._cards[0].set_value("se_inert", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the If any of SE_MASS, SE_DAMP, SE_STIFF, or SE_INERT is blank then the second line is required and contains the file name for the superelement.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

