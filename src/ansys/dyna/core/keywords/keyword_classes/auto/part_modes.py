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

"""Module providing the PartModes class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class PartModes(KeywordBase):
    """DYNA PART_MODES keyword"""

    keyword = "PART"
    subkeyword = "MODES"

    def __init__(self, **kwargs):
        """Initialize the PartModes class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nmfb",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "form",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ansid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "format",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "kmflag",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nupdf",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sigrec",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        70,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mode1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mode8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mstart",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mstop",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dampf",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part identification. This part must be a rigid body.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nmfb(self) -> typing.Optional[int]:
        """Get or set the Number of kept modes in flexible body.
        The number of modes in the file, FILENAME, must equal or exceed NMFB. If KMFLAG=0 the first NMFB modes in the file are used.
        """ # nopep8
        return self._cards[0].get_value("nmfb")

    @nmfb.setter
    def nmfb(self, value: int) -> None:
        """Set the nmfb property."""
        self._cards[0].set_value("nmfb", value)

    @property
    def form(self) -> int:
        """Get or set the Flexible body formulation:
        EQ.0: exact (default),
        EQ.1: fast.
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        if value not in [0, 1, None]:
            raise Exception("""form must be `None` or one of {0,1}.""")
        self._cards[0].set_value("form", value)

    @property
    def ansid(self) -> typing.Optional[int]:
        """Get or set the Attachment node set ID (optional).
        """ # nopep8
        return self._cards[0].get_value("ansid")

    @ansid.setter
    def ansid(self, value: int) -> None:
        """Set the ansid property."""
        self._cards[0].set_value("ansid", value)

    @property
    def format(self) -> int:
        """Get or set the Input format of modal information:
        EQ.0:  NASTRAN.pch file.
        EQ.1:  (not supported)
        EQ.2:  NASTRAN.pch file (LS-DYNA binary version).  The binary version of this file is automatically created if a NASTRAN.pch file is read.  The name of the binary file is the name of the NASTRAN.pch file but with ".bin" appended.  The binary file is smaller and can be read much faster.
        EQ.3:  LS-DYNA d3eigv binary eigenvalue database (see *CONTROL_IMPLICIT_EIGENVALUE).
        EQ.4:  LS-DYNA d3mode binary constraint/attachment mode database (see *CONTROL_IMPLICIT_MODE).
        EQ.5:  Both d3eigv and d3mode databases are input.  Database names must be "d3eigv" and "d3mode", and FILENAME below is ignored.  NMFB above gives the total number of modes in both databases.
        """ # nopep8
        return self._cards[0].get_value("format")

    @format.setter
    def format(self, value: int) -> None:
        """Set the format property."""
        if value not in [0, 2, 3, 4, 5, None]:
            raise Exception("""format must be `None` or one of {0,2,3,4,5}.""")
        self._cards[0].set_value("format", value)

    @property
    def kmflag(self) -> int:
        """Get or set the Kept mode flag. Selects method for identifying modes to keep:
        EQ.0: the first NMFB modes in the file, FILENAME, are used (default),
        EQ.1: define NMFB kept modes with additional input.
        """ # nopep8
        return self._cards[0].get_value("kmflag")

    @kmflag.setter
    def kmflag(self, value: int) -> None:
        """Set the kmflag property."""
        if value not in [0, 1, None]:
            raise Exception("""kmflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("kmflag", value)

    @property
    def nupdf(self) -> int:
        """Get or set the Nodal update flag.
        If active, an attachment node set, ANSID, must be defined.
        EQ.0: all nodes of the rigid part are updated each cycle (default),
        EQ.1: only attachment nodes are fully updated. All nodes in the body are output based on the rigid body motion without the addition of the modal displacements. For maximum benefit an attachment node set can also be defined with the *PART_ATTACHMENT_NODES option. The same attachment node set ID should be used here.
        """ # nopep8
        return self._cards[0].get_value("nupdf")

    @nupdf.setter
    def nupdf(self, value: int) -> None:
        """Set the nupdf property."""
        if value not in [0, 1, None]:
            raise Exception("""nupdf must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nupdf", value)

    @property
    def sigrec(self) -> int:
        """Get or set the Stree recovery flag. If active, attachment nodes should not be used.
        EQ.0: no stress recovery
        EQ.1: recover stresses.
        """ # nopep8
        return self._cards[0].get_value("sigrec")

    @sigrec.setter
    def sigrec(self, value: int) -> None:
        """Set the sigrec property."""
        if value not in [0, 1, None]:
            raise Exception("""sigrec must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sigrec", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the The path and name of a file which contains the modes for this rigid body.
        Maximum 80 characters.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

    @property
    def mode1(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode1")

    @mode1.setter
    def mode1(self, value: int) -> None:
        """Set the mode1 property."""
        self._cards[2].set_value("mode1", value)

    @property
    def mode2(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode2")

    @mode2.setter
    def mode2(self, value: int) -> None:
        """Set the mode2 property."""
        self._cards[2].set_value("mode2", value)

    @property
    def mode3(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode3")

    @mode3.setter
    def mode3(self, value: int) -> None:
        """Set the mode3 property."""
        self._cards[2].set_value("mode3", value)

    @property
    def mode4(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode4")

    @mode4.setter
    def mode4(self, value: int) -> None:
        """Set the mode4 property."""
        self._cards[2].set_value("mode4", value)

    @property
    def mode5(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode5")

    @mode5.setter
    def mode5(self, value: int) -> None:
        """Set the mode5 property."""
        self._cards[2].set_value("mode5", value)

    @property
    def mode6(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode6")

    @mode6.setter
    def mode6(self, value: int) -> None:
        """Set the mode6 property."""
        self._cards[2].set_value("mode6", value)

    @property
    def mode7(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode7")

    @mode7.setter
    def mode7(self, value: int) -> None:
        """Set the mode7 property."""
        self._cards[2].set_value("mode7", value)

    @property
    def mode8(self) -> typing.Optional[int]:
        """Get or set the Keep normal mode, MODEi.
        """ # nopep8
        return self._cards[2].get_value("mode8")

    @mode8.setter
    def mode8(self, value: int) -> None:
        """Set the mode8 property."""
        self._cards[2].set_value("mode8", value)

    @property
    def mstart(self) -> typing.Optional[int]:
        """Get or set the First mode for damping, (1 <= MSTART <= NMFB) .
        """ # nopep8
        return self._cards[3].get_value("mstart")

    @mstart.setter
    def mstart(self, value: int) -> None:
        """Set the mstart property."""
        self._cards[3].set_value("mstart", value)

    @property
    def mstop(self) -> typing.Optional[int]:
        """Get or set the Last mode for damping, MSTOP, (1 <= MSTOP <= NMFB) . All modes between MSTART and MSTOP inclusive are subject to the same modal damping coefficient, DAMPF.
        """ # nopep8
        return self._cards[3].get_value("mstop")

    @mstop.setter
    def mstop(self, value: int) -> None:
        """Set the mstop property."""
        self._cards[3].set_value("mstop", value)

    @property
    def dampf(self) -> typing.Optional[float]:
        """Get or set the Modal damping coefficient.
        """ # nopep8
        return self._cards[3].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        """Set the dampf property."""
        self._cards[3].set_value("dampf", value)

