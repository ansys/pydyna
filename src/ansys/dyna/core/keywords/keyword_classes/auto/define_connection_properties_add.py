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

class DefineConnectionPropertiesAdd(KeywordBase):
    """DYNA DEFINE_CONNECTION_PROPERTIES_ADD keyword"""

    keyword = "DEFINE"
    subkeyword = "CONNECTION_PROPERTIES_ADD"
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
                        "con_id",
                        int,
                        0,
                        10,
                        kwargs.get("con_id")
                    ),
                    Field(
                        "proprul",
                        int,
                        10,
                        10,
                        kwargs.get("proprul", 0)
                    ),
                    Field(
                        "areaeq",
                        int,
                        20,
                        10,
                        kwargs.get("areaeq", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "dg_typ",
                        int,
                        40,
                        10,
                        kwargs.get("dg_typ", 0)
                    ),
                    Field(
                        "moarfl",
                        int,
                        50,
                        10,
                        kwargs.get("moarfl", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "sgiy",
                        float,
                        10,
                        10,
                        kwargs.get("sgiy")
                    ),
                    Field(
                        "etan",
                        float,
                        20,
                        10,
                        kwargs.get("etan")
                    ),
                    Field(
                        "dgpr",
                        float,
                        30,
                        10,
                        kwargs.get("dgpr", 1.0E+10)
                    ),
                    Field(
                        "rank",
                        float,
                        40,
                        10,
                        kwargs.get("rank")
                    ),
                    Field(
                        "sn",
                        float,
                        50,
                        10,
                        kwargs.get("sn")
                    ),
                    Field(
                        "sb",
                        float,
                        60,
                        10,
                        kwargs.get("sb")
                    ),
                    Field(
                        "ss",
                        float,
                        70,
                        10,
                        kwargs.get("ss")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "exsn",
                        float,
                        0,
                        10,
                        kwargs.get("exsn")
                    ),
                    Field(
                        "exsb",
                        float,
                        10,
                        10,
                        kwargs.get("exsb")
                    ),
                    Field(
                        "exss",
                        float,
                        20,
                        10,
                        kwargs.get("exss")
                    ),
                    Field(
                        "lcsn",
                        int,
                        30,
                        10,
                        kwargs.get("lcsn")
                    ),
                    Field(
                        "lcsb",
                        int,
                        40,
                        10,
                        kwargs.get("lcsb")
                    ),
                    Field(
                        "lcss",
                        int,
                        50,
                        10,
                        kwargs.get("lcss")
                    ),
                    Field(
                        "gfad",
                        int,
                        60,
                        10,
                        kwargs.get("gfad")
                    ),
                    Field(
                        "sclmrr",
                        float,
                        70,
                        10,
                        kwargs.get("sclmrr", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineConnectionPropertiesAdd.option_specs[0],
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
    def con_id(self) -> typing.Optional[int]:
        """Get or set the Connection ID of an existing *DEFINE_CONNECTION_PROPERTIES table
        """ # nopep8
        return self._cards[0].get_value("con_id")

    @con_id.setter
    def con_id(self, value: int) -> None:
        self._cards[0].set_value("con_id", value)

    @property
    def proprul(self) -> int:
        """Get or set the The failure rule number for this connection
        """ # nopep8
        return self._cards[0].get_value("proprul")

    @proprul.setter
    def proprul(self, value: int) -> None:
        self._cards[0].set_value("proprul", value)

    @property
    def areaeq(self) -> int:
        """Get or set the Area equation number for the connection area calculation.
        EQ.0:	(default) area_true=area_modeled
        EQ.1: 	millimeter form;
        EQ.-1:	meter form;
        """ # nopep8
        return self._cards[0].get_value("areaeq")

    @areaeq.setter
    def areaeq(self, value: int) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""areaeq must be one of {0,1,-1}""")
        self._cards[0].set_value("areaeq", value)

    @property
    def dg_typ(self) -> int:
        """Get or set the Damage type
        EQ.0:  no damage function is used
        EQ.1:  strain based damage
        EQ.2:  failure function based damage
        EQ.3 or 4:  fading energy based damage
        """ # nopep8
        return self._cards[0].get_value("dg_typ")

    @dg_typ.setter
    def dg_typ(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""dg_typ must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("dg_typ", value)

    @property
    def moarfl(self) -> int:
        """Get or set the Modeled area flag
        EQ.0: Areamodelled goes down with shear (default)
        EQ.1: Areamodelled stays constant
        """ # nopep8
        return self._cards[0].get_value("moarfl")

    @moarfl.setter
    def moarfl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""moarfl must be one of {0,1}""")
        self._cards[0].set_value("moarfl", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID of the shell material for which properties are defined.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[1].set_value("mid", value)

    @property
    def sgiy(self) -> typing.Optional[float]:
        """Get or set the Yield stress to be used in the spot weld element calculation
        """ # nopep8
        return self._cards[1].get_value("sgiy")

    @sgiy.setter
    def sgiy(self, value: float) -> None:
        self._cards[1].set_value("sgiy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus to be used in the spot weld element calculation.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        self._cards[1].set_value("etan", value)

    @property
    def dgpr(self) -> float:
        """Get or set the Damage parameter for hyperbolic based damage function.
        """ # nopep8
        return self._cards[1].get_value("dgpr")

    @dgpr.setter
    def dgpr(self, value: float) -> None:
        self._cards[1].set_value("dgpr", value)

    @property
    def rank(self) -> typing.Optional[float]:
        """Get or set the Rank value
        """ # nopep8
        return self._cards[1].get_value("rank")

    @rank.setter
    def rank(self, value: float) -> None:
        self._cards[1].set_value("rank", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Normal strength.
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        self._cards[1].set_value("sn", value)

    @property
    def sb(self) -> typing.Optional[float]:
        """Get or set the Bending strength.
        """ # nopep8
        return self._cards[1].get_value("sb")

    @sb.setter
    def sb(self, value: float) -> None:
        self._cards[1].set_value("sb", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Shear strength
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        self._cards[1].set_value("ss", value)

    @property
    def exsn(self) -> typing.Optional[float]:
        """Get or set the Exponent on normal stress term.
        """ # nopep8
        return self._cards[2].get_value("exsn")

    @exsn.setter
    def exsn(self, value: float) -> None:
        self._cards[2].set_value("exsn", value)

    @property
    def exsb(self) -> typing.Optional[float]:
        """Get or set the Exponent on bending stress term.
        """ # nopep8
        return self._cards[2].get_value("exsb")

    @exsb.setter
    def exsb(self, value: float) -> None:
        self._cards[2].set_value("exsb", value)

    @property
    def exss(self) -> typing.Optional[float]:
        """Get or set the Exponent on shear stress term.
        """ # nopep8
        return self._cards[2].get_value("exss")

    @exss.setter
    def exss(self, value: float) -> None:
        self._cards[2].set_value("exss", value)

    @property
    def lcsn(self) -> typing.Optional[int]:
        """Get or set the Curve ID for normal strength scale factor as a function of strain rate
        """ # nopep8
        return self._cards[2].get_value("lcsn")

    @lcsn.setter
    def lcsn(self, value: int) -> None:
        self._cards[2].set_value("lcsn", value)

    @property
    def lcsb(self) -> typing.Optional[int]:
        """Get or set the Curve ID for bending strength scale factor as a function of strain rate.
        """ # nopep8
        return self._cards[2].get_value("lcsb")

    @lcsb.setter
    def lcsb(self, value: int) -> None:
        self._cards[2].set_value("lcsb", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Curve ID for shear strength scale factor as a function of strain rate
        """ # nopep8
        return self._cards[2].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[2].set_value("lcss", value)

    @property
    def gfad(self) -> typing.Optional[int]:
        """Get or set the Fading energy for damage type 3.
        """ # nopep8
        return self._cards[2].get_value("gfad")

    @gfad.setter
    def gfad(self, value: int) -> None:
        self._cards[2].set_value("gfad", value)

    @property
    def sclmrr(self) -> float:
        """Get or set the Scaling factor for torsional moment in failure function.
        """ # nopep8
        return self._cards[2].get_value("sclmrr")

    @sclmrr.setter
    def sclmrr(self, value: float) -> None:
        self._cards[2].set_value("sclmrr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

