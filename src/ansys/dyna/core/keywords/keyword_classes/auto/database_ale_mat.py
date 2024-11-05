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

class DatabaseAleMat(KeywordBase):
    """DYNA DATABASE_ALE_MAT keyword"""

    keyword = "DATABASE"
    subkeyword = "ALE_MAT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dtout",
                        float,
                        0,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "boxlow",
                        int,
                        10,
                        10,
                        kwargs.get("boxlow")
                    ),
                    Field(
                        "boxup",
                        int,
                        20,
                        10,
                        kwargs.get("boxup")
                    ),
                    Field(
                        "dtxy",
                        float,
                        30,
                        10,
                        kwargs.get("dtxy")
                    ),
                ],
            ),
        ]

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval between the outputs.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def boxlow(self) -> typing.Optional[int]:
        """Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
        the range while BOXUP is the upper bound. The series of
        volumes covered by the specified range of *DEFINE_BOX
        determines the mesh regions for which ALE material data are to be output.
        """ # nopep8
        return self._cards[0].get_value("boxlow")

    @boxlow.setter
    def boxlow(self, value: int) -> None:
        self._cards[0].set_value("boxlow", value)

    @property
    def boxup(self) -> typing.Optional[int]:
        """Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
        the range while BOXUP is the upper bound. The series of
        volumes covered by the specified range of *DEFINE_BOX
        determines the mesh regions for which ALE material data are to be output.
        """ # nopep8
        return self._cards[0].get_value("boxup")

    @boxup.setter
    def boxup(self, value: int) -> None:
        self._cards[0].set_value("boxup", value)

    @property
    def dtxy(self) -> typing.Optional[float]:
        """Get or set the Time interval between the extraction of *.xy files from datalemat.tmp
        """ # nopep8
        return self._cards[0].get_value("dtxy")

    @dtxy.setter
    def dtxy(self, value: float) -> None:
        self._cards[0].set_value("dtxy", value)

