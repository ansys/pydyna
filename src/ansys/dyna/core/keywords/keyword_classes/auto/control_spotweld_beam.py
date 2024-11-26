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

class ControlSpotweldBeam(KeywordBase):
    """DYNA CONTROL_SPOTWELD_BEAM keyword"""

    keyword = "CONTROL"
    subkeyword = "SPOTWELD_BEAM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lct",
                        int,
                        0,
                        10,
                        kwargs.get("lct", 0)
                    ),
                    Field(
                        "lcs",
                        int,
                        10,
                        10,
                        kwargs.get("lcs")
                    ),
                    Field(
                        "t_ort",
                        int,
                        20,
                        10,
                        kwargs.get("t_ort", 0)
                    ),
                    Field(
                        "prtflg",
                        int,
                        30,
                        10,
                        kwargs.get("prtflg", 0)
                    ),
                    Field(
                        "t_ors",
                        int,
                        40,
                        10,
                        kwargs.get("t_ors", 0)
                    ),
                    Field(
                        "rpbhx",
                        int,
                        50,
                        10,
                        kwargs.get("rpbhx", 0)
                    ),
                    Field(
                        "bmsid",
                        int,
                        60,
                        10,
                        kwargs.get("bmsid", 0)
                    ),
                    Field(
                        "id_off",
                        int,
                        70,
                        10,
                        kwargs.get("id_off", 0)
                    ),
                ],
            ),
        ]

    @property
    def lct(self) -> int:
        """Get or set the Load curve ID for scaling the response in tension based on the shell element size
        """ # nopep8
        return self._cards[0].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        self._cards[0].set_value("lct", value)

    @property
    def lcs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for scaling the response in shear based on the shell element size.
        """ # nopep8
        return self._cards[0].get_value("lcs")

    @lcs.setter
    def lcs(self, value: int) -> None:
        self._cards[0].set_value("lcs", value)

    @property
    def t_ort(self) -> int:
        """Get or set the Table ID for scaling the tension response (and shear response if T_ORS=0) based on the location of the beam node relative to the centroid of the shell.
        """ # nopep8
        return self._cards[0].get_value("t_ort")

    @t_ort.setter
    def t_ort(self, value: int) -> None:
        self._cards[0].set_value("t_ort", value)

    @property
    def prtflg(self) -> int:
        """Get or set the Set this flag to 1 to print for each spotweld attachment: the beam, node, and shell ID's, the parametric coordinates that define the constraint location, the angle used in the table lookup, and the three scale factors obtained from the load curves and table lookup.
        """ # nopep8
        return self._cards[0].get_value("prtflg")

    @prtflg.setter
    def prtflg(self, value: int) -> None:
        self._cards[0].set_value("prtflg", value)

    @property
    def t_ors(self) -> int:
        """Get or set the Optional table ID for scaling the shear response based on the location of the beam node relative to the centroid of the shell.
        """ # nopep8
        return self._cards[0].get_value("t_ors")

    @t_ors.setter
    def t_ors(self, value: int) -> None:
        self._cards[0].set_value("t_ors", value)

    @property
    def rpbhx(self) -> int:
        """Get or set the Replace each spot weld beam element with a cluster of RPBHX solid elements.  RPBHX may be set to 1, 4, or 8.  When RPBHX is set to 4 or 8, a table is generated to output the force and moment resultants into the SWFORC file, if this file is active.  This table is described by the keyword: *DEFINE_HEX_SPOTWELD_ASSEMBLY.   The ID's of the beam elements are used as the cluster spot weld ID's so the ID's in the SWFORC file are unchanged.  The beam elements are automatically deleted from the calculation, and the section and material data is automatically changed to be used with solid elements.
        """ # nopep8
        return self._cards[0].get_value("rpbhx")

    @rpbhx.setter
    def rpbhx(self, value: int) -> None:
        self._cards[0].set_value("rpbhx", value)

    @property
    def bmsid(self) -> int:
        """Get or set the Optional beam setID defining the beam element ID's that are to be converted to hex assemblies. If zero, all spotweld beam elements are converted to hex assemblies. See the keyword, *SET_BEAM_GENERAL for an efficient way of defining beam sets
        """ # nopep8
        return self._cards[0].get_value("bmsid")

    @bmsid.setter
    def bmsid(self, value: int) -> None:
        self._cards[0].set_value("bmsid", value)

    @property
    def id_off(self) -> int:
        """Get or set the This optional ID offset applies if and only if BMSID is nonzero. Beams, which share part ID's with beams that are converted to hex assemblies, will be assigned new part ID's by adding to the original part ID the value of ID_OFF. If ID_OFF is zero the new part ID for such beams will be assigned to be larger than the largest part ID in the model
        """ # nopep8
        return self._cards[0].get_value("id_off")

    @id_off.setter
    def id_off(self, value: int) -> None:
        self._cards[0].set_value("id_off", value)

