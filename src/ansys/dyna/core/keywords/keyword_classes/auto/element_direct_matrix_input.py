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

class ElementDirectMatrixInput(KeywordBase):
    """DYNA ELEMENT_DIRECT_MATRIX_INPUT keyword"""

    keyword = "ELEMENT"
    subkeyword = "DIRECT_MATRIX_INPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "ifrmt",
                        int,
                        10,
                        10,
                        kwargs.get("ifrmt")
                    ),
                ],
            ),
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
                        "mass",
                        str,
                        0,
                        10,
                        kwargs.get("mass")
                    ),
                    Field(
                        "damp",
                        str,
                        10,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "stif",
                        str,
                        20,
                        10,
                        kwargs.get("stif")
                    ),
                    Field(
                        "inert",
                        str,
                        30,
                        10,
                        kwargs.get("inert")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Super element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def ifrmt(self) -> typing.Optional[int]:
        """Get or set the Format:
        EQ.0: standard format
        NE.0:  extended precision format
        """ # nopep8
        return self._cards[0].get_value("ifrmt")

    @ifrmt.setter
    def ifrmt(self, value: int) -> None:
        self._cards[0].set_value("ifrmt", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of file that has the element matrices
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[1].set_value("filename", value)

    @property
    def mass(self) -> typing.Optional[str]:
        """Get or set the Name of mass matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
        """ # nopep8
        return self._cards[2].get_value("mass")

    @mass.setter
    def mass(self, value: str) -> None:
        self._cards[2].set_value("mass", value)

    @property
    def damp(self) -> typing.Optional[str]:
        """Get or set the Name of damping matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
        """ # nopep8
        return self._cards[2].get_value("damp")

    @damp.setter
    def damp(self, value: str) -> None:
        self._cards[2].set_value("damp", value)

    @property
    def stif(self) -> typing.Optional[str]:
        """Get or set the Name of stiffness matrix in the file defined by FILENAME. This filename should be no more than eight characters to be compatible with NASTRAN.
        """ # nopep8
        return self._cards[2].get_value("stif")

    @stif.setter
    def stif(self, value: str) -> None:
        self._cards[2].set_value("stif", value)

    @property
    def inert(self) -> typing.Optional[str]:
        """Get or set the Name of inertia matrix in the file defined by FILENAME.  This filename should be no more than eight characters to be compatible with NASTRAN.  This file must be present when *LOAD_BODY is used to put gravitational forces on the model..
        """ # nopep8
        return self._cards[2].get_value("inert")

    @inert.setter
    def inert(self, value: str) -> None:
        self._cards[2].set_value("inert", value)

