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

class DefineHazProperties(KeywordBase):
    """DYNA DEFINE_HAZ_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "HAZ_PROPERTIES"
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
                        "id_haz",
                        int,
                        0,
                        10,
                        kwargs.get("id_haz", 0)
                    ),
                    Field(
                        "iop",
                        int,
                        10,
                        10,
                        kwargs.get("iop", 0)
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "pid_typ",
                        int,
                        30,
                        10,
                        kwargs.get("pid_typ", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iss",
                        int,
                        0,
                        10,
                        kwargs.get("iss", 0)
                    ),
                    Field(
                        "ifs",
                        int,
                        10,
                        10,
                        kwargs.get("ifs", 0)
                    ),
                    Field(
                        "isb",
                        int,
                        20,
                        10,
                        kwargs.get("isb", 0)
                    ),
                    Field(
                        "ifb",
                        int,
                        30,
                        10,
                        kwargs.get("ifb", 0)
                    ),
                    Field(
                        "isc",
                        int,
                        40,
                        10,
                        kwargs.get("isc", 0)
                    ),
                    Field(
                        "ifc",
                        int,
                        50,
                        10,
                        kwargs.get("ifc", 0)
                    ),
                    Field(
                        "isw",
                        int,
                        60,
                        10,
                        kwargs.get("isw", 0)
                    ),
                    Field(
                        "ifw",
                        int,
                        70,
                        10,
                        kwargs.get("ifw", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineHazProperties.option_specs[0],
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
    def id_haz(self) -> int:
        """Get or set the Property set ID. A unique ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("id_haz")

    @id_haz.setter
    def id_haz(self, value: int) -> None:
        self._cards[0].set_value("id_haz", value)

    @property
    def iop(self) -> int:
        """Get or set the Activity flag. If IOP = 0, then the scaling is not applied, and if IOP = 1, the scaling is active.
        """ # nopep8
        return self._cards[0].get_value("iop")

    @iop.setter
    def iop(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iop must be one of {0,1}""")
        self._cards[0].set_value("iop", value)

    @property
    def pid(self) -> int:
        """Get or set the Part or part set ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def pid_typ(self) -> int:
        """Get or set the PID type. PID_TYP = 0 indicates that PID is a *PART ID, and PID_TYP = 1, a part set..
        """ # nopep8
        return self._cards[0].get_value("pid_typ")

    @pid_typ.setter
    def pid_typ(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pid_typ must be one of {0,1}""")
        self._cards[0].set_value("pid_typ", value)

    @property
    def iss(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("iss")

    @iss.setter
    def iss(self, value: int) -> None:
        self._cards[1].set_value("iss", value)

    @property
    def ifs(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the	closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifs")

    @ifs.setter
    def ifs(self, value: int) -> None:
        self._cards[1].set_value("ifs", value)

    @property
    def isb(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isb")

    @isb.setter
    def isb(self, value: int) -> None:
        self._cards[1].set_value("isb", value)

    @property
    def ifb(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifb")

    @ifb.setter
    def ifb(self, value: int) -> None:
        self._cards[1].set_value("ifb", value)

    @property
    def isc(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isc")

    @isc.setter
    def isc(self, value: int) -> None:
        self._cards[1].set_value("isc", value)

    @property
    def ifc(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifc")

    @ifc.setter
    def ifc(self, value: int) -> None:
        self._cards[1].set_value("ifc", value)

    @property
    def isw(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isw")

    @isw.setter
    def isw(self, value: int) -> None:
        self._cards[1].set_value("isw", value)

    @property
    def ifw(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifw")

    @ifw.setter
    def ifw(self, value: int) -> None:
        self._cards[1].set_value("ifw", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

