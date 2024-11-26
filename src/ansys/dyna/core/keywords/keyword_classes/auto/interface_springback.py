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

class InterfaceSpringback(KeywordBase):
    """DYNA INTERFACE_SPRINGBACK keyword"""

    keyword = "INTERFACE"
    subkeyword = "SPRINGBACK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "nshv",
                        int,
                        10,
                        10,
                        kwargs.get("nshv")
                    ),
                    Field(
                        "ftype",
                        int,
                        20,
                        10,
                        kwargs.get("ftype", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ftensr",
                        int,
                        40,
                        10,
                        kwargs.get("ftensr", 0)
                    ),
                    Field(
                        "nthhsv",
                        int,
                        50,
                        10,
                        kwargs.get("nthhsv")
                    ),
                    Field(
                        "rflag",
                        int,
                        60,
                        10,
                        kwargs.get("rflag")
                    ),
                    Field(
                        "intstrn",
                        int,
                        70,
                        10,
                        kwargs.get("intstrn")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "optc",
                        str,
                        0,
                        10,
                        kwargs.get("optc", "OPTCARD")
                    ),
                    Field(
                        "sldo",
                        int,
                        10,
                        10,
                        kwargs.get("sldo", 0)
                    ),
                    Field(
                        "ncyc",
                        int,
                        20,
                        10,
                        kwargs.get("ncyc")
                    ),
                    Field(
                        "fsplit",
                        int,
                        30,
                        10,
                        kwargs.get("fsplit", 0)
                    ),
                    Field(
                        "ndflag",
                        int,
                        40,
                        10,
                        kwargs.get("ndflag", 0)
                    ),
                    Field(
                        "cflag",
                        int,
                        50,
                        10,
                        kwargs.get("cflag", 0)
                    ),
                    Field(
                        "hflag",
                        int,
                        60,
                        10,
                        kwargs.get("hflag")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for springback, see * SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def nshv(self) -> typing.Optional[int]:
        """Get or set the Number of additional shell history variables to be initialized. The shell stresses and plastic strains are written to the interface file. If NSHV is nonzero, the shell formulations and constitutive models should not change between runs.
        """ # nopep8
        return self._cards[0].get_value("nshv")

    @nshv.setter
    def nshv(self, value: int) -> None:
        self._cards[0].set_value("nshv", value)

    @property
    def ftype(self) -> int:
        """Get or set the EQ:0 ascii output.
        EQ:1 binary output.
        EQ:2 ascii and binary output.
        EQ.3: LSDA format
        EQ.10: ASCII large format (see *INITIAL_STRESS_SHELL)
        EQ.11: binary large format
        EQ.12: both ASCII and binary large format
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 10, 11, 12]:
            raise Exception("""ftype must be one of {0,1,2,3,10,11,12}""")
        self._cards[0].set_value("ftype", value)

    @property
    def ftensr(self) -> int:
        """Get or set the Flag for dumping tensor data from the element history variables into the dynain file.
        EQ.0: Dont dump tensor data from element history variables
        EQ.1: Dump any tensor data from element history variables into
        the dynain file in GLOBAL coordinate system. Currently, only Material 190 supports this option
        """ # nopep8
        return self._cards[0].get_value("ftensr")

    @ftensr.setter
    def ftensr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ftensr must be one of {0,1}""")
        self._cards[0].set_value("ftensr", value)

    @property
    def nthhsv(self) -> typing.Optional[int]:
        """Get or set the Number of thermal history variables
        """ # nopep8
        return self._cards[0].get_value("nthhsv")

    @nthhsv.setter
    def nthhsv(self, value: int) -> None:
        self._cards[0].set_value("nthhsv", value)

    @property
    def rflag(self) -> typing.Optional[int]:
        """Get or set the Flag to carry over reference quantities, for hyperelastic materials and such.
        EQ.0:	default, do not output.
        EQ.1:	output reference coordinates and nodal masses.
        """ # nopep8
        return self._cards[0].get_value("rflag")

    @rflag.setter
    def rflag(self, value: int) -> None:
        self._cards[0].set_value("rflag", value)

    @property
    def intstrn(self) -> typing.Optional[int]:
        """Get or set the Output of strains at all integration points of shell element is requested, see also *INITIAL_STRAIN_SHELL
        """ # nopep8
        return self._cards[0].get_value("intstrn")

    @intstrn.setter
    def intstrn(self, value: int) -> None:
        self._cards[0].set_value("intstrn", value)

    @property
    def optc(self) -> str:
        """Get or set the &
        """ # nopep8
        return self._cards[1].get_value("optc")

    @optc.setter
    def optc(self, value: str) -> None:
        if value not in ["OPTCARD"]:
            raise Exception("""optc must be one of {"OPTCARD"}""")
        self._cards[1].set_value("optc", value)

    @property
    def sldo(self) -> int:
        """Get or set the Output of solid element data as
        EQ.0:	*ELEMENT_SOLID, or
        EQ.1:	*ELEMENT_SOLID_ORTHO(only for anisotropic material).
        """ # nopep8
        return self._cards[1].get_value("sldo")

    @sldo.setter
    def sldo(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sldo must be one of {0,1}""")
        self._cards[1].set_value("sldo", value)

    @property
    def ncyc(self) -> typing.Optional[int]:
        """Get or set the Number of process cycles this simulation corresponds to in the simulation of wear processes
        """ # nopep8
        return self._cards[1].get_value("ncyc")

    @ncyc.setter
    def ncyc(self, value: int) -> None:
        self._cards[1].set_value("ncyc", value)

    @property
    def fsplit(self) -> int:
        """Get or set the Flag for splitting of the dynain file (only for ASCII format).
        EQ.0:	dynain file written in one piece.
        EQ.1:	Output is divided into two files, dynain_geo including the geometry data and dynain_ini including initial stresses and strains.
        """ # nopep8
        return self._cards[1].get_value("fsplit")

    @fsplit.setter
    def fsplit(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""fsplit must be one of {0,1}""")
        self._cards[1].set_value("fsplit", value)

    @property
    def ndflag(self) -> int:
        """Get or set the Flag to dump nodes into dynain file.
        EQ.0: default, dump only sph and element nodes
        EQ.1: dump all nodes.
        """ # nopep8
        return self._cards[1].get_value("ndflag")

    @ndflag.setter
    def ndflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ndflag must be one of {0,1}""")
        self._cards[1].set_value("ndflag", value)

    @property
    def cflag(self) -> int:
        """Get or set the Output contact state.
        EQ.0: default, do not output
        EQ.1: output contact state, currently only Mortar segment pair information and selected tied contacts with restrictions.
        """ # nopep8
        return self._cards[1].get_value("cflag")

    @cflag.setter
    def cflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cflag must be one of {0,1}""")
        self._cards[1].set_value("cflag", value)

    @property
    def hflag(self) -> typing.Optional[int]:
        """Get or set the Output hourglass state, only valid for FTYPE=3:
        EQ.0:	default, do not output.
        EQ.1:	output hourglass stresses for carrying over to next simulation.
        """ # nopep8
        return self._cards[1].get_value("hflag")

    @hflag.setter
    def hflag(self, value: int) -> None:
        self._cards[1].set_value("hflag", value)

