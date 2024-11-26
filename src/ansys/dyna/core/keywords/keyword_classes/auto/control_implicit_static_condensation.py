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

class ControlImplicitStaticCondensation(KeywordBase):
    """DYNA CONTROL_IMPLICIT_STATIC_CONDENSATION keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_STATIC_CONDENSATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sc_flag",
                        int,
                        0,
                        10,
                        kwargs.get("sc_flag", 0)
                    ),
                    Field(
                        "sc_nsid",
                        int,
                        10,
                        10,
                        kwargs.get("sc_nsid")
                    ),
                    Field(
                        "sc_psid",
                        int,
                        20,
                        10,
                        kwargs.get("sc_psid")
                    ),
                    Field(
                        "se_mass",
                        str,
                        30,
                        10,
                        kwargs.get("se_mass")
                    ),
                    Field(
                        "se_stiff",
                        str,
                        40,
                        10,
                        kwargs.get("se_stiff")
                    ),
                    Field(
                        "se_inert",
                        str,
                        50,
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
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
        ]

    @property
    def sc_flag(self) -> int:
        """Get or set the Static Condensation Control Flag
        EQ.0:  no static condensation will be performed
        EQ.1:  create superelement representation based on static condensation.
        EQ.2:  use static condensation to build a linearized representation for a part and use that linearized representation in the following analysis.
        """ # nopep8
        return self._cards[0].get_value("sc_flag")

    @sc_flag.setter
    def sc_flag(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sc_flag must be one of {0,1,2}""")
        self._cards[0].set_value("sc_flag", value)

    @property
    def sc_nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes to be preserved in the static condensation procedure.  Required when SC_FLAG = 1
        """ # nopep8
        return self._cards[0].get_value("sc_nsid")

    @sc_nsid.setter
    def sc_nsid(self, value: int) -> None:
        self._cards[0].set_value("sc_nsid", value)

    @property
    def sc_psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for parts to be included in the static condensation procedure.  When SC_FLAG = 1 SC_PSID can be used to specify a subset of the model with the default being the entire model.  When SC_FLAG = 2 SC_PSID is required.  SC_PSID = 0 implies that the entire model is condensed.
        """ # nopep8
        return self._cards[0].get_value("sc_psid")

    @sc_psid.setter
    def sc_psid(self, value: int) -> None:
        self._cards[0].set_value("sc_psid", value)

    @property
    def se_mass(self) -> typing.Optional[str]:
        """Get or set the Name of the superelement mass matrix.  If left blank it is not generated
        """ # nopep8
        return self._cards[0].get_value("se_mass")

    @se_mass.setter
    def se_mass(self, value: str) -> None:
        self._cards[0].set_value("se_mass", value)

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

