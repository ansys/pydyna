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

class ControlReferenceControlVolumeCoordinates(KeywordBase):
    """DYNA CONTROL_REFERENCE_CONTROL_VOLUME_COORDINATES keyword"""

    keyword = "CONTROL"
    subkeyword = "REFERENCE_CONTROL_VOLUME_COORDINATES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
            Card(
                [
                    Field(
                        "opt",
                        int,
                        0,
                        10,
                        kwargs.get("opt", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        kwargs.get("psid")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the The file name contains the *INITIAL_FOAM_REFERENCE_GEOMETRY, *INITIAL_STRESS_SOLID, and *INITIAL_STRESS_SHELL to be read or written.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def opt(self) -> int:
        """Get or set the OPT = 0 writes the reference geometry to the specified file.
        OPT = 1 reads the reference geometry from the specified file.
        OPT = 2 skips the pressure initialization.
        """ # nopep8
        return self._cards[1].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""opt must be one of {0,1,2}""")
        self._cards[1].set_value("opt", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the the part set defining the elements to be read or written.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[1].set_value("psid", value)

