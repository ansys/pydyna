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

class InterfaceCompensation3DRefineRigid(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_3D_REFINE_RIGID keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_3D_REFINE_RIGID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
        ]

    @property
    def filename1(self) -> typing.Optional[str]:
        """Get or set the A keyword file name of rigid tool mesh to be refined.  This should be the tooling mesh used in the forming or flanging simulation, before any compensation is done.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.
        """ # nopep8
        return self._cards[0].get_value("filename1")

    @filename1.setter
    def filename1(self, value: str) -> None:
        self._cards[0].set_value("filename1", value)

    @property
    def filename2(self) -> typing.Optional[str]:
        """Get or set the A keyword file name with trim curves defined using *DEFINE_CURVE_TRIM_3D.  The curves will be used to refine and realign the FILENAME1 to improve the convergence in the iterative compensation process.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.
        """ # nopep8
        return self._cards[1].get_value("filename2")

    @filename2.setter
    def filename2(self, value: str) -> None:
        self._cards[1].set_value("filename2", value)

