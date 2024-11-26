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

class IncludeStampedPartSolidToSolid(KeywordBase):
    """DYNA INCLUDE_STAMPED_PART_SOLID_TO_SOLID keyword"""

    keyword = "INCLUDE"
    subkeyword = "STAMPED_PART_SOLID_TO_SOLID"

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
                ],
            ),
            Card(
                [
                    Field(
                        "n1sorc",
                        int,
                        0,
                        10,
                        kwargs.get("n1sorc", 0)
                    ),
                    Field(
                        "n2sorc",
                        int,
                        10,
                        10,
                        kwargs.get("n2sorc", 0)
                    ),
                    Field(
                        "n3sorc",
                        int,
                        20,
                        10,
                        kwargs.get("n3sorc", 0)
                    ),
                    Field(
                        "n1trgt",
                        int,
                        30,
                        10,
                        kwargs.get("n1trgt", 0)
                    ),
                    Field(
                        "n2trgt",
                        int,
                        40,
                        10,
                        kwargs.get("n2trgt", 0)
                    ),
                    Field(
                        "n3trgt",
                        int,
                        50,
                        10,
                        kwargs.get("n3trgt", 0)
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of the dynain file to be included to map the results from, with maximum of 80 characters.
        This should be the native dynain file containing the results from metal stamping,
        directly from a LS-DYNA simulation.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the target part onto which the source model’s results in FILENAME will be mapped.
        LT.0:	part ID of the target part is | PID | and the normals of the target part are flipped before mapping.A negative PID would be used if the target part’s normals were oriented exactly opposite those of the source part.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def thick(self) -> int:
        """Get or set the Flag to map part thickness.  The thickness direction is determined from the element normals, hence the need for consistency in the element normals.  The thicknesses of the target part elements are adjusted so the target part thickness matches the source part thickness at any given location.  Currently, this variable is hardwired so that thickness mapping is always on.
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: int) -> None:
        self._cards[1].set_value("thick", value)

    @property
    def pstrn(self) -> int:
        """Get or set the Flag to map effective plastic strain.  Currently setting this flag with any integer will map the effective plastic strain, and there is no other option.
        EQ.0:	map effective plastic strain.
        NE.0 : do not map effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("pstrn")

    @pstrn.setter
    def pstrn(self, value: int) -> None:
        self._cards[1].set_value("pstrn", value)

    @property
    def strain(self) -> int:
        """Get or set the Flag to map the strain tensor.  Currently setting this flag with any integer will map the tensorial strains, and there is no other option available.  Note “STRFLG” in *DATABASE_‌EXTENT_‌BINARY must be set to “1” for output to d3plot as well as dynain files.
        EQ.0:	map strain tensor.
        NE.0 : do not map strain tensor
        """ # nopep8
        return self._cards[1].get_value("strain")

    @strain.setter
    def strain(self, value: int) -> None:
        self._cards[1].set_value("strain", value)

    @property
    def stress(self) -> int:
        """Get or set the Flag to map stress tensor.  Currently setting this flag with any integer will map the stresses and history variables, and there is no other option available.  Only the history variables included in the dynain file specified by FILENAME are mapped; see “NSHV” in *INTERFACE_‌SPRINGBACK_‌LSDYNA for control of history variable output to dynain.
        EQ.0:	map stress tensor and history variables.
        NE.0 : do not map stress tensorand history variables
        """ # nopep8
        return self._cards[1].get_value("stress")

    @stress.setter
    def stress(self, value: int) -> None:
        self._cards[1].set_value("stress", value)

    @property
    def n1sorc(self) -> int:
        """Get or set the First of 3 nodes needed to reorient the source part.  No transformation if undefined.
        """ # nopep8
        return self._cards[2].get_value("n1sorc")

    @n1sorc.setter
    def n1sorc(self, value: int) -> None:
        self._cards[2].set_value("n1sorc", value)

    @property
    def n2sorc(self) -> int:
        """Get or set the Second of 3 nodes needed to reorient the source part.  No transformation if undefined
        """ # nopep8
        return self._cards[2].get_value("n2sorc")

    @n2sorc.setter
    def n2sorc(self, value: int) -> None:
        self._cards[2].set_value("n2sorc", value)

    @property
    def n3sorc(self) -> int:
        """Get or set the Third of 3 nodes needed to reorient the source part.  No transformation if undefined.
        """ # nopep8
        return self._cards[2].get_value("n3sorc")

    @n3sorc.setter
    def n3sorc(self, value: int) -> None:
        self._cards[2].set_value("n3sorc", value)

    @property
    def n1trgt(self) -> int:
        """Get or set the First of 3 nodes needed to reorient the target part.  No transformation if undefined.
        """ # nopep8
        return self._cards[2].get_value("n1trgt")

    @n1trgt.setter
    def n1trgt(self, value: int) -> None:
        self._cards[2].set_value("n1trgt", value)

    @property
    def n2trgt(self) -> int:
        """Get or set the Second of 3 nodes needed to reorient the target part.  No transformation if undefined
        """ # nopep8
        return self._cards[2].get_value("n2trgt")

    @n2trgt.setter
    def n2trgt(self, value: int) -> None:
        self._cards[2].set_value("n2trgt", value)

    @property
    def n3trgt(self) -> int:
        """Get or set the Third of 3 nodes needed to reorient the target part.  No transformation if undefined
        """ # nopep8
        return self._cards[2].get_value("n3trgt")

    @n3trgt.setter
    def n3trgt(self, value: int) -> None:
        self._cards[2].set_value("n3trgt", value)

