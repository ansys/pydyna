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

"""Module providing the DefineConnectionProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineConnectionProperties(KeywordBase):
    """DYNA DEFINE_CONNECTION_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "CONNECTION_PROPERTIES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineConnectionProperties class."""
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "proprul",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "areaeq",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dgtyp",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "moarfl",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dsigy",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "detan",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ddg_pr",
                        float,
                        30,
                        10,
                        1.0E+10,
                        **kwargs,
                    ),
                    Field(
                        "drank",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dsn",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dsb",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dss",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dexsn",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "dexsb",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "dexss",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "dcsn",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dlcsb",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dlcss",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dgfad",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dsclmrr",
                        float,
                        70,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineConnectionProperties.option_specs[0],
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
    def con_id(self) -> int:
        """Get or set the Connection ID, referenced on *MAT_SPOTWELD_DAIMLERCHRYSLER.  Multiple sets of connection data may be used by assigning different connection IDs
        """ # nopep8
        return self._cards[0].get_value("con_id")

    @con_id.setter
    def con_id(self, value: int) -> None:
        """Set the con_id property."""
        self._cards[0].set_value("con_id", value)

    @property
    def proprul(self) -> int:
        """Get or set the The failure rule number for this connection
        """ # nopep8
        return self._cards[0].get_value("proprul")

    @proprul.setter
    def proprul(self, value: int) -> None:
        """Set the proprul property."""
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
        """Set the areaeq property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""areaeq must be `None` or one of {0,1,-1}.""")
        self._cards[0].set_value("areaeq", value)

    @property
    def dgtyp(self) -> int:
        """Get or set the Damage type
        EQ.0:  no damage function is used
        EQ.1:  strain based damage
        EQ.2:  failure function based damage
        EQ.3 or 4:  fading energy based damage
        EQ.5:	Improved version of DGTYP=4; see Remark 4
        """ # nopep8
        return self._cards[0].get_value("dgtyp")

    @dgtyp.setter
    def dgtyp(self, value: int) -> None:
        """Set the dgtyp property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""dgtyp must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("dgtyp", value)

    @property
    def moarfl(self) -> int:
        """Get or set the Modeled area flag
        EQ.0: Areamodelled goes down with shear (default)
        EQ.1: Areamodelled stays constant
        """ # nopep8
        return self._cards[0].get_value("moarfl")

    @moarfl.setter
    def moarfl(self, value: int) -> None:
        """Set the moarfl property."""
        if value not in [0, 1, None]:
            raise Exception("""moarfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("moarfl", value)

    @property
    def dsigy(self) -> typing.Optional[float]:
        """Get or set the Default yield stress for the spot weld element
        """ # nopep8
        return self._cards[1].get_value("dsigy")

    @dsigy.setter
    def dsigy(self, value: float) -> None:
        """Set the dsigy property."""
        self._cards[1].set_value("dsigy", value)

    @property
    def detan(self) -> typing.Optional[float]:
        """Get or set the Default tangent modulus for the spot weld element.
        """ # nopep8
        return self._cards[1].get_value("detan")

    @detan.setter
    def detan(self, value: float) -> None:
        """Set the detan property."""
        self._cards[1].set_value("detan", value)

    @property
    def ddg_pr(self) -> float:
        """Get or set the Default damage parameter for hyperbolic based damage function
        """ # nopep8
        return self._cards[1].get_value("ddg_pr")

    @ddg_pr.setter
    def ddg_pr(self, value: float) -> None:
        """Set the ddg_pr property."""
        self._cards[1].set_value("ddg_pr", value)

    @property
    def drank(self) -> typing.Optional[float]:
        """Get or set the Default rank value
        """ # nopep8
        return self._cards[1].get_value("drank")

    @drank.setter
    def drank(self, value: float) -> None:
        """Set the drank property."""
        self._cards[1].set_value("drank", value)

    @property
    def dsn(self) -> typing.Optional[float]:
        """Get or set the Default normal strength
        """ # nopep8
        return self._cards[1].get_value("dsn")

    @dsn.setter
    def dsn(self, value: float) -> None:
        """Set the dsn property."""
        self._cards[1].set_value("dsn", value)

    @property
    def dsb(self) -> typing.Optional[float]:
        """Get or set the Default bending strength.
        """ # nopep8
        return self._cards[1].get_value("dsb")

    @dsb.setter
    def dsb(self, value: float) -> None:
        """Set the dsb property."""
        self._cards[1].set_value("dsb", value)

    @property
    def dss(self) -> typing.Optional[float]:
        """Get or set the Default shear strength.
        """ # nopep8
        return self._cards[1].get_value("dss")

    @dss.setter
    def dss(self, value: float) -> None:
        """Set the dss property."""
        self._cards[1].set_value("dss", value)

    @property
    def dexsn(self) -> float:
        """Get or set the Default exponent on normal stress term.
        """ # nopep8
        return self._cards[2].get_value("dexsn")

    @dexsn.setter
    def dexsn(self, value: float) -> None:
        """Set the dexsn property."""
        self._cards[2].set_value("dexsn", value)

    @property
    def dexsb(self) -> float:
        """Get or set the Default exponent on bending stress term
        """ # nopep8
        return self._cards[2].get_value("dexsb")

    @dexsb.setter
    def dexsb(self, value: float) -> None:
        """Set the dexsb property."""
        self._cards[2].set_value("dexsb", value)

    @property
    def dexss(self) -> float:
        """Get or set the Default exponent on shear stress term.
        """ # nopep8
        return self._cards[2].get_value("dexss")

    @dexss.setter
    def dexss(self, value: float) -> None:
        """Set the dexss property."""
        self._cards[2].set_value("dexss", value)

    @property
    def dcsn(self) -> typing.Optional[int]:
        """Get or set the Default curve ID for normal strength scale factor as a function of strain rate.
        """ # nopep8
        return self._cards[2].get_value("dcsn")

    @dcsn.setter
    def dcsn(self, value: int) -> None:
        """Set the dcsn property."""
        self._cards[2].set_value("dcsn", value)

    @property
    def dlcsb(self) -> typing.Optional[int]:
        """Get or set the Default curve ID for bending strength scale factor as a function of strain rate.
        """ # nopep8
        return self._cards[2].get_value("dlcsb")

    @dlcsb.setter
    def dlcsb(self, value: int) -> None:
        """Set the dlcsb property."""
        self._cards[2].set_value("dlcsb", value)

    @property
    def dlcss(self) -> typing.Optional[int]:
        """Get or set the Default curve ID for shear strength scale factor as a function of strain rate.
        """ # nopep8
        return self._cards[2].get_value("dlcss")

    @dlcss.setter
    def dlcss(self, value: int) -> None:
        """Set the dlcss property."""
        self._cards[2].set_value("dlcss", value)

    @property
    def dgfad(self) -> typing.Optional[int]:
        """Get or set the Default fading energy for damage type 3.
        """ # nopep8
        return self._cards[2].get_value("dgfad")

    @dgfad.setter
    def dgfad(self, value: int) -> None:
        """Set the dgfad property."""
        self._cards[2].set_value("dgfad", value)

    @property
    def dsclmrr(self) -> float:
        """Get or set the Default scaling factor for torsional moment in failure function.
        """ # nopep8
        return self._cards[2].get_value("dsclmrr")

    @dsclmrr.setter
    def dsclmrr(self, value: float) -> None:
        """Set the dsclmrr property."""
        self._cards[2].set_value("dsclmrr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

