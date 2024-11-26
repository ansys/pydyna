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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineFrictionAutomaticSingleSurfaceMortar(KeywordBase):
    """DYNA DEFINE_FRICTION_AUTOMATIC_SINGLE_SURFACE_MORTAR keyword"""

    keyword = "DEFINE"
    subkeyword = "FRICTION_AUTOMATIC_SINGLE_SURFACE_MORTAR"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "fs_d",
                        float,
                        10,
                        10,
                        kwargs.get("fs_d", 0.0)
                    ),
                    Field(
                        "fd_d",
                        float,
                        20,
                        10,
                        kwargs.get("fd_d", 0.0)
                    ),
                    Field(
                        "dc_d",
                        float,
                        30,
                        10,
                        kwargs.get("dc_d", 0.0)
                    ),
                    Field(
                        "vc_d",
                        float,
                        40,
                        10,
                        kwargs.get("vc_d", 0.0)
                    ),
                    Field(
                        "icnep",
                        int,
                        50,
                        10,
                        kwargs.get("icnep", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid_i",
                        int,
                        0,
                        10,
                        kwargs.get("pid_i")
                    ),
                    Field(
                        "pid_j",
                        int,
                        10,
                        10,
                        kwargs.get("pid_j")
                    ),
                    Field(
                        "fs_ij",
                        float,
                        20,
                        10,
                        kwargs.get("fs_ij", 0.0)
                    ),
                    Field(
                        "fd_ij",
                        float,
                        30,
                        10,
                        kwargs.get("fd_ij", 0.0)
                    ),
                    Field(
                        "dc_ij",
                        float,
                        40,
                        10,
                        kwargs.get("dc_ij", 0.0)
                    ),
                    Field(
                        "vc_ij",
                        float,
                        50,
                        10,
                        kwargs.get("vc_ij", 0.0)
                    ),
                    Field(
                        "ptypei",
                        str,
                        60,
                        10,
                        kwargs.get("ptypei")
                    ),
                    Field(
                        "ptypej",
                        str,
                        70,
                        10,
                        kwargs.get("ptypej")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineFrictionAutomaticSingleSurfaceMortar.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def id(self) -> int:
        """Get or set the Identification number. Only one table is allowed
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def fs_d(self) -> float:
        """Get or set the Default value of the static coefficient of friction. The frictional coefficient is assumed to be dependent on the relative V of the surface in the contact. Default values are used when part pair are undefined
        """ # nopep8
        return self._cards[0].get_value("fs_d")

    @fs_d.setter
    def fs_d(self, value: float) -> None:
        self._cards[0].set_value("fs_d", value)

    @property
    def fd_d(self) -> float:
        """Get or set the Default value of the dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
        """ # nopep8
        return self._cards[0].get_value("fd_d")

    @fd_d.setter
    def fd_d(self, value: float) -> None:
        self._cards[0].set_value("fd_d", value)

    @property
    def dc_d(self) -> float:
        """Get or set the Default value of the exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity V of the surfaces in contact. Default values are used when part pair are undefined
        """ # nopep8
        return self._cards[0].get_value("dc_d")

    @dc_d.setter
    def dc_d(self, value: float) -> None:
        self._cards[0].set_value("dc_d", value)

    @property
    def vc_d(self) -> float:
        """Get or set the Default value of the coefficient for viscous friction. This is necessary to limit the friction force to a maximum. A limiting force is computed F=VC*Acont. Acont being the area of the segment contacted by the node in contact. The suggested value for VC is to use the yield stress in shear VC=sigma/SQRT(3.0). Where sigma is the yield stress of the contacted material.Default values are used when part pair are undefined
        """ # nopep8
        return self._cards[0].get_value("vc_d")

    @vc_d.setter
    def vc_d(self, value: float) -> None:
        self._cards[0].set_value("vc_d", value)

    @property
    def icnep(self) -> int:
        """Get or set the Flag to check for non-existing parts, or part sets (PIDi, PIDj) on Card 2.
        EQ.0:	Existence of parts or part sets is checked,and an error occurs when any is missing(default).
        EQ.1 : Existence of parts or part sets is checked and lines with non - existent parts will be ignored..
        """ # nopep8
        return self._cards[0].get_value("icnep")

    @icnep.setter
    def icnep(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icnep must be one of {0,1}""")
        self._cards[0].set_value("icnep", value)

    @property
    def pid_i(self) -> typing.Optional[int]:
        """Get or set the Part ID I.
        """ # nopep8
        return self._cards[1].get_value("pid_i")

    @pid_i.setter
    def pid_i(self, value: int) -> None:
        self._cards[1].set_value("pid_i", value)

    @property
    def pid_j(self) -> typing.Optional[int]:
        """Get or set the Part ID J.
        """ # nopep8
        return self._cards[1].get_value("pid_j")

    @pid_j.setter
    def pid_j(self, value: int) -> None:
        self._cards[1].set_value("pid_j", value)

    @property
    def fs_ij(self) -> float:
        """Get or set the Static coefficient of friction between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("fs_ij")

    @fs_ij.setter
    def fs_ij(self, value: float) -> None:
        self._cards[1].set_value("fs_ij", value)

    @property
    def fd_ij(self) -> float:
        """Get or set the Dynamic coefficient of friction between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("fd_ij")

    @fd_ij.setter
    def fd_ij(self, value: float) -> None:
        self._cards[1].set_value("fd_ij", value)

    @property
    def dc_ij(self) -> float:
        """Get or set the Exponential decay coefficient between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("dc_ij")

    @dc_ij.setter
    def dc_ij(self, value: float) -> None:
        self._cards[1].set_value("dc_ij", value)

    @property
    def vc_ij(self) -> float:
        """Get or set the Viscous friction between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("vc_ij")

    @vc_ij.setter
    def vc_ij(self, value: float) -> None:
        self._cards[1].set_value("vc_ij", value)

    @property
    def ptypei(self) -> typing.Optional[str]:
        """Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
        """ # nopep8
        return self._cards[1].get_value("ptypei")

    @ptypei.setter
    def ptypei(self, value: str) -> None:
        self._cards[1].set_value("ptypei", value)

    @property
    def ptypej(self) -> typing.Optional[str]:
        """Get or set the EQ:"PSET" when PTYPEI or PTYPEJ refers to a set_part.
        """ # nopep8
        return self._cards[1].get_value("ptypej")

    @ptypej.setter
    def ptypej(self, value: str) -> None:
        self._cards[1].set_value("ptypej", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

