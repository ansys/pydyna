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

class DefineAdaptiveSolidToDesId(KeywordBase):
    """DYNA DEFINE_ADAPTIVE_SOLID_TO_DES_ID keyword"""

    keyword = "DEFINE"
    subkeyword = "ADAPTIVE_SOLID_TO_DES_ID"
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
                        "did",
                        int,
                        0,
                        10,
                        kwargs.get("did")
                    ),
                    Field(
                        "heading",
                        str,
                        10,
                        70,
                        kwargs.get("heading")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ipid",
                        int,
                        0,
                        10,
                        kwargs.get("ipid")
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        kwargs.get("itype", 0)
                    ),
                    Field(
                        "nq",
                        int,
                        20,
                        10,
                        kwargs.get("nq", 1)
                    ),
                    Field(
                        "ipdes",
                        int,
                        30,
                        10,
                        kwargs.get("ipdes")
                    ),
                    Field(
                        "isdes",
                        int,
                        40,
                        10,
                        kwargs.get("isdes")
                    ),
                    Field(
                        "rsf",
                        float,
                        50,
                        10,
                        kwargs.get("rsf", 1.0)
                    ),
                    Field(
                        "outdes",
                        int,
                        60,
                        10,
                        kwargs.get("outdes", 0)
                    ),
                    Field(
                        "ibond",
                        int,
                        70,
                        10,
                        kwargs.get("ibond", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pbn",
                        float,
                        0,
                        10,
                        kwargs.get("pbn")
                    ),
                    Field(
                        "pbs",
                        float,
                        10,
                        10,
                        kwargs.get("pbs")
                    ),
                    Field(
                        "pbn_s",
                        float,
                        20,
                        10,
                        kwargs.get("pbn_s")
                    ),
                    Field(
                        "pbs_s",
                        float,
                        30,
                        10,
                        kwargs.get("pbs_s")
                    ),
                    Field(
                        "sfa",
                        float,
                        40,
                        10,
                        kwargs.get("sfa", 1.0)
                    ),
                    Field(
                        "alpha",
                        float,
                        50,
                        10,
                        kwargs.get("alpha", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineAdaptiveSolidToDesId.option_specs[0],
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
    def did(self) -> typing.Optional[int]:
        """Get or set the Definition ID. This must be a unique number..
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        self._cards[0].set_value("did", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Definition descriptor. It is suggested that unique descriptions be	used.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[0].set_value("heading", value)

    @property
    def ipid(self) -> typing.Optional[int]:
        """Get or set the ID of the solid part or part set to transform.
        """ # nopep8
        return self._cards[1].get_value("ipid")

    @ipid.setter
    def ipid(self, value: int) -> None:
        self._cards[1].set_value("ipid", value)

    @property
    def itype(self) -> int:
        """Get or set the Set type of the IPID
        EQ. 0: Part ID
        NE. 0: Part set ID
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itype must be one of {0,1}""")
        self._cards[1].set_value("itype", value)

    @property
    def nq(self) -> int:
        """Get or set the Adaptive option for hexahedral elements. For tetrahedral and
        pentahedral elements, see Remark 1:
        EQ.1: Adapt one solid element to one discrete element,
        EQ.2: Adapt one solid element to 8 discrete elements,
        EQ.3: Adapt one solid element to 27 discrete elements.
        """ # nopep8
        return self._cards[1].get_value("nq")

    @nq.setter
    def nq(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""nq must be one of {1,2,3}""")
        self._cards[1].set_value("nq", value)

    @property
    def ipdes(self) -> typing.Optional[int]:
        """Get or set the Part ID for newly generated discrete elements, See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("ipdes")

    @ipdes.setter
    def ipdes(self, value: int) -> None:
        self._cards[1].set_value("ipdes", value)

    @property
    def isdes(self) -> typing.Optional[int]:
        """Get or set the Section ID for discrete elements, See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("isdes")

    @isdes.setter
    def isdes(self, value: int) -> None:
        self._cards[1].set_value("isdes", value)

    @property
    def rsf(self) -> float:
        """Get or set the DES radius scale down factor, which is the ratio of the radius of the generated DES to the calculated radius based on volume consistency.
        """ # nopep8
        return self._cards[1].get_value("rsf")

    @rsf.setter
    def rsf(self, value: float) -> None:
        self._cards[1].set_value("rsf", value)

    @property
    def outdes(self) -> int:
        """Get or set the Allow user output generated discrete element nodes and DES properties toa keyword file.
        EQ.0:	No output. (Default)
        EQ.1:	Write data under filename, desvfill.inc.
        """ # nopep8
        return self._cards[1].get_value("outdes")

    @outdes.setter
    def outdes(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""outdes must be one of {0,1}""")
        self._cards[1].set_value("outdes", value)

    @property
    def ibond(self) -> int:
        """Get or set the Allow user define bonds between DES generated from the same solid element.
        EQ.0:	No bonds. (Default)
        EQ.1:	Bonds generated, need to define Card 2.
        """ # nopep8
        return self._cards[1].get_value("ibond")

    @ibond.setter
    def ibond(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibond must be one of {0,1}""")
        self._cards[1].set_value("ibond", value)

    @property
    def pbn(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond modulus [Pa]. See Remark 1 in *DEFINE_DE_BOND.
        """ # nopep8
        return self._cards[2].get_value("pbn")

    @pbn.setter
    def pbn(self, value: float) -> None:
        self._cards[2].set_value("pbn", value)

    @property
    def pbs(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond stiffness ratio. Shear stiffness/normal stiffness. See Remark 2 in *DEFINE_DE_BOND
        """ # nopep8
        return self._cards[2].get_value("pbs")

    @pbs.setter
    def pbs(self, value: float) -> None:
        self._cards[2].set_value("pbs", value)

    @property
    def pbn_s(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress
        """ # nopep8
        return self._cards[2].get_value("pbn_s")

    @pbn_s.setter
    def pbn_s(self, value: float) -> None:
        self._cards[2].set_value("pbn_s", value)

    @property
    def pbs_s(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress.
        """ # nopep8
        return self._cards[2].get_value("pbs_s")

    @pbs_s.setter
    def pbs_s(self, value: float) -> None:
        self._cards[2].set_value("pbs_s", value)

    @property
    def sfa(self) -> float:
        """Get or set the Bond radius multiplier. Default is 1.
        """ # nopep8
        return self._cards[2].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        self._cards[2].set_value("sfa", value)

    @property
    def alpha(self) -> float:
        """Get or set the Numerical damping.
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[2].set_value("alpha", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

