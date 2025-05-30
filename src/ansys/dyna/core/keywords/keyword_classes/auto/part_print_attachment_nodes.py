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

"""Module providing the PartPrintAttachmentNodes class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class PartPrintAttachmentNodes(KeywordBase):
    """DYNA PART_PRINT_ATTACHMENT_NODES keyword"""

    keyword = "PART"
    subkeyword = "PRINT_ATTACHMENT_NODES"

    def __init__(self, **kwargs):
        """Initialize the PartPrintAttachmentNodes class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "title",
                        str,
                        0,
                        80,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "secid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eosid",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "hgid",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "grav",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "adpopt",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tmid",
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
                        "prbf",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ansid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID defined in *SECTION section.
        """ # nopep8
        return self._cards[1].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[1].set_value("secid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT section.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[1].set_value("mid", value)

    @property
    def eosid(self) -> int:
        """Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
        """ # nopep8
        return self._cards[1].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[1].set_value("eosid", value)

    @property
    def hgid(self) -> int:
        """Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
        EQ.0: default values are used.
        """ # nopep8
        return self._cards[1].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        """Set the hgid property."""
        self._cards[1].set_value("hgid", value)

    @property
    def grav(self) -> int:
        """Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
        EQ.0: all parts initialized,
        EQ.1: only current material initialized.
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: int) -> None:
        """Set the grav property."""
        if value not in [0, 1, None]:
            raise Exception("""grav must be `None` or one of {0,1}.""")
        self._cards[1].set_value("grav", value)

    @property
    def adpopt(self) -> typing.Optional[int]:
        """Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
        LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
        EQ.0:Adaptive remeshing is inactive for this part ID.
        EQ.1:	h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
        EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
        EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
        EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        """Set the adpopt property."""
        self._cards[1].set_value("adpopt", value)

    @property
    def tmid(self) -> int:
        """Get or set the Thermal material property identification defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
        EQ.0: defaults to MID.
        """ # nopep8
        return self._cards[1].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        """Set the tmid property."""
        self._cards[1].set_value("tmid", value)

    @property
    def prbf(self) -> int:
        """Get or set the Print flag for RBDOUT and MATSUM files
        EQ.0: default is taken from the keyword *CONTROL_OUTPUT
        EQ.1: write data into RDBOUT file only
        EQ.2: write data into MATSUM file only
        EQ.3: do not write data into RBDOUT AND MATSUM files
        """ # nopep8
        return self._cards[2].get_value("prbf")

    @prbf.setter
    def prbf(self, value: int) -> None:
        """Set the prbf property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""prbf must be `None` or one of {0,1,2,3}.""")
        self._cards[2].set_value("prbf", value)

    @property
    def ansid(self) -> int:
        """Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.
        EQ.0: All nodal updates are skipped for this rigid body. The null option can be used if the rigid body is fixed in space or if the rigid body does not interact with other parts, e.g., the rigid body is only used for some visual purpose (default).
        """ # nopep8
        return self._cards[3].get_value("ansid")

    @ansid.setter
    def ansid(self, value: int) -> None:
        """Set the ansid property."""
        self._cards[3].set_value("ansid", value)

