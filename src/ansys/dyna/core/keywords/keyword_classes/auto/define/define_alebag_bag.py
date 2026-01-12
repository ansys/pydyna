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

"""Module providing the DefineAlebagBag class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEALEBAGBAG_CARD0 = (
    FieldSchema("bagid", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("sidtype", int, 20, 10, 0),
    FieldSchema("cvbag", int, 30, 10, 1),
    FieldSchema("iblock", int, 40, 10, 0),
    FieldSchema("vcof", float, 50, 10, None),
    FieldSchema("vset", int, 60, 10, None),
    FieldSchema("vtype", int, 70, 10, 0),
)

_DEFINEALEBAGBAG_CARD1 = (
    FieldSchema("nquad", int, 0, 10, 0),
    FieldSchema("ctype", int, 10, 10, 2),
    FieldSchema("pfac", float, 20, 10, 0.1),
    FieldSchema("fric", float, 30, 10, 0.0),
    FieldSchema("frcmin", float, 40, 10, 0.5),
    FieldSchema("normtyp", int, 50, 10, 0),
    FieldSchema("ileak", int, 60, 10, 0),
    FieldSchema("pleak", float, 70, 10, 0.01),
)

_DEFINEALEBAGBAG_CARD2 = (
    FieldSchema("norm", int, 0, 10, 0),
    FieldSchema("start", float, 10, 10, 0.0),
    FieldSchema("end", float, 20, 10, 10000000000.0),
)

_DEFINEALEBAGBAG_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineAlebagBag(KeywordBase):
    """DYNA DEFINE_ALEBAG_BAG keyword"""

    keyword = "DEFINE"
    subkeyword = "ALEBAG_BAG"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineAlebagBag class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEALEBAGBAG_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEALEBAGBAG_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEALEBAGBAG_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineAlebagBag.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEALEBAGBAG_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def bagid(self) -> typing.Optional[int]:
        """Get or set the Bag mesh definition ID, referred in *AIRBAG_ADVANCED_ALE.
        """ # nopep8
        return self._cards[0].get_value("bagid")

    @bagid.setter
    def bagid(self, value: int) -> None:
        """Set the bagid property."""
        self._cards[0].set_value("bagid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set of Lagrange shell elements to interact with inflator gas
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def sidtype(self) -> int:
        """Get or set the Type of SID
        EQ:'PSET' or '0' for set of parts
        EQ:'PART' or '1' for part
        """ # nopep8
        return self._cards[0].get_value("sidtype")

    @sidtype.setter
    def sidtype(self, value: int) -> None:
        """Set the sidtype property."""
        if value not in [0, 1, None]:
            raise Exception("""sidtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sidtype", value)

    @property
    def cvbag(self) -> int:
        """Get or set the EQ:'NO' or '0' when SID is used only for coupling, and not considered part of a bag.  Therefore it will not be considered part of control volume bag.  The inner bag of a bag-in-bag is an example of application.
        EQ:'YES' or '1' when SID is part of a control volume bag and will couple with gas
        """ # nopep8
        return self._cards[0].get_value("cvbag")

    @cvbag.setter
    def cvbag(self, value: int) -> None:
        """Set the cvbag property."""
        if value not in [1, 0, None]:
            raise Exception("""cvbag must be `None` or one of {1,0}.""")
        self._cards[0].set_value("cvbag", value)

    @property
    def iblock(self) -> int:
        """Get or set the flag for hole venting blockage effect: 0, no; 1, yes
        """ # nopep8
        return self._cards[0].get_value("iblock")

    @iblock.setter
    def iblock(self, value: int) -> None:
        """Set the iblock property."""
        if value not in [0, 1, None]:
            raise Exception("""iblock must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iblock", value)

    @property
    def vcof(self) -> typing.Optional[float]:
        """Get or set the Venting coefficient
        """ # nopep8
        return self._cards[0].get_value("vcof")

    @vcof.setter
    def vcof(self, value: float) -> None:
        """Set the vcof property."""
        self._cards[0].set_value("vcof", value)

    @property
    def vset(self) -> typing.Optional[int]:
        """Get or set the Set representing venting holes
        """ # nopep8
        return self._cards[0].get_value("vset")

    @vset.setter
    def vset(self, value: int) -> None:
        """Set the vset property."""
        self._cards[0].set_value("vset", value)

    @property
    def vtype(self) -> int:
        """Get or set the Type of VSET :
        EQ:'PSET' or '0' for set of parts
        EQ:'PART' or '1' for part
        EQ:'SEGSET' or '2' for segment set
        """ # nopep8
        return self._cards[0].get_value("vtype")

    @vtype.setter
    def vtype(self, value: int) -> None:
        """Set the vtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""vtype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("vtype", value)

    @property
    def nquad(self) -> int:
        """Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
        EQ.0: at nodes only,
        EQ.n: use a rectangular grid of n*n points,
        EQ.-n: at nodes and a rectangular grid of n*n points.
        """ # nopep8
        return self._cards[1].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        """Set the nquad property."""
        self._cards[1].set_value("nquad", value)

    @property
    def ctype(self) -> int:
        """Get or set the Coupling type
        EQ.1: constrained acceleration,
        EQ.2: constrained acceleration and velocity (default),
        EQ.3: constrained acceleration and velocity, normal direction only,
        EQ.4: penalty coupling (Shell and solid Elements),
        EQ.5: penalty coupling allowing erosion in the lagrangian entities (Solid Elements).
        EQ.6: Penalty coupling designed for airbag modeling(testing).DIREC is automatically reset to DIREC=1.
        """ # nopep8
        return self._cards[1].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [2, 1, 3, 4, 5, 6, None]:
            raise Exception("""ctype must be `None` or one of {2,1,3,4,5,6}.""")
        self._cards[1].set_value("ctype", value)

    @property
    def pfac(self) -> float:
        """Get or set the Penalty factor (CTYPE 4 and 5 only).
        """ # nopep8
        return self._cards[1].get_value("pfac")

    @pfac.setter
    def pfac(self, value: float) -> None:
        """Set the pfac property."""
        self._cards[1].set_value("pfac", value)

    @property
    def fric(self) -> float:
        """Get or set the Coefficient of friction (DIREC 2 only).
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[1].set_value("fric", value)

    @property
    def frcmin(self) -> float:
        """Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
        """ # nopep8
        return self._cards[1].get_value("frcmin")

    @frcmin.setter
    def frcmin(self, value: float) -> None:
        """Set the frcmin property."""
        self._cards[1].set_value("frcmin", value)

    @property
    def normtyp(self) -> int:
        """Get or set the Penality spring direction(DIREC 1 and 2 ):
        EQ.0: interpolated from node normals(default),
        EQ.1: segment normal.
        """ # nopep8
        return self._cards[1].get_value("normtyp")

    @normtyp.setter
    def normtyp(self, value: int) -> None:
        """Set the normtyp property."""
        self._cards[1].set_value("normtyp", value)

    @property
    def ileak(self) -> int:
        """Get or set the Leakage control:
        EQ.0: none(default),
        EQ.1: weak,
        EQ.2: strong.
        """ # nopep8
        return self._cards[1].get_value("ileak")

    @ileak.setter
    def ileak(self, value: int) -> None:
        """Set the ileak property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ileak must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("ileak", value)

    @property
    def pleak(self) -> float:
        """Get or set the Leakage control penalty factor
        """ # nopep8
        return self._cards[1].get_value("pleak")

    @pleak.setter
    def pleak(self, value: float) -> None:
        """Set the pleak property."""
        self._cards[1].set_value("pleak", value)

    @property
    def norm(self) -> int:
        """Get or set the Shell and segment normal orientation:
        EQ.0: right hand rule (default)
        EQ.1: left hand rule.
        """ # nopep8
        return self._cards[2].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        """Set the norm property."""
        self._cards[2].set_value("norm", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for coupling (default=0.0).
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time for coupling (default=1.0E+10).
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        """Set the end property."""
        self._cards[2].set_value("end", value)

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

