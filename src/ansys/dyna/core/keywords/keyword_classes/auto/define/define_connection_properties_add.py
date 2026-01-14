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

"""Module providing the DefineConnectionPropertiesAdd class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINECONNECTIONPROPERTIESADD_CARD0 = (
    FieldSchema("con_id", int, 0, 10, None),
    FieldSchema("proprul", int, 10, 10, 0),
    FieldSchema("areaeq", int, 20, 10, 0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("dg_typ", int, 40, 10, 0),
    FieldSchema("moarfl", int, 50, 10, 0),
)

_DEFINECONNECTIONPROPERTIESADD_CARD1 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("sgiy", float, 10, 10, None),
    FieldSchema("etan", float, 20, 10, None),
    FieldSchema("dgpr", float, 30, 10, 10000000000.0),
    FieldSchema("rank", float, 40, 10, None),
    FieldSchema("sn", float, 50, 10, None),
    FieldSchema("sb", float, 60, 10, None),
    FieldSchema("ss", float, 70, 10, None),
)

_DEFINECONNECTIONPROPERTIESADD_CARD2 = (
    FieldSchema("exsn", float, 0, 10, None),
    FieldSchema("exsb", float, 10, 10, None),
    FieldSchema("exss", float, 20, 10, None),
    FieldSchema("lcsn", int, 30, 10, None),
    FieldSchema("lcsb", int, 40, 10, None),
    FieldSchema("lcss", int, 50, 10, None),
    FieldSchema("gfad", int, 60, 10, None),
    FieldSchema("sclmrr", float, 70, 10, 1.0),
)

_DEFINECONNECTIONPROPERTIESADD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineConnectionPropertiesAdd(KeywordBase):
    """DYNA DEFINE_CONNECTION_PROPERTIES_ADD keyword"""

    keyword = "DEFINE"
    subkeyword = "CONNECTION_PROPERTIES_ADD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineConnectionPropertiesAdd class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONNECTIONPROPERTIESADD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECONNECTIONPROPERTIESADD_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINECONNECTIONPROPERTIESADD_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineConnectionPropertiesAdd.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONNECTIONPROPERTIESADD_OPTION0_CARD0,
                        **kwargs,
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
        """Set the dg_typ property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""dg_typ must be `None` or one of {0,1,2,3,4}.""")
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
        """Set the moarfl property."""
        if value not in [0, 1, None]:
            raise Exception("""moarfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("moarfl", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID of the shell material for which properties are defined.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[1].set_value("mid", value)

    @property
    def sgiy(self) -> typing.Optional[float]:
        """Get or set the Yield stress to be used in the spot weld element calculation
        """ # nopep8
        return self._cards[1].get_value("sgiy")

    @sgiy.setter
    def sgiy(self, value: float) -> None:
        """Set the sgiy property."""
        self._cards[1].set_value("sgiy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus to be used in the spot weld element calculation.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[1].set_value("etan", value)

    @property
    def dgpr(self) -> float:
        """Get or set the Damage parameter for hyperbolic based damage function.
        """ # nopep8
        return self._cards[1].get_value("dgpr")

    @dgpr.setter
    def dgpr(self, value: float) -> None:
        """Set the dgpr property."""
        self._cards[1].set_value("dgpr", value)

    @property
    def rank(self) -> typing.Optional[float]:
        """Get or set the Rank value
        """ # nopep8
        return self._cards[1].get_value("rank")

    @rank.setter
    def rank(self, value: float) -> None:
        """Set the rank property."""
        self._cards[1].set_value("rank", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Normal strength.
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[1].set_value("sn", value)

    @property
    def sb(self) -> typing.Optional[float]:
        """Get or set the Bending strength.
        """ # nopep8
        return self._cards[1].get_value("sb")

    @sb.setter
    def sb(self, value: float) -> None:
        """Set the sb property."""
        self._cards[1].set_value("sb", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Shear strength
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[1].set_value("ss", value)

    @property
    def exsn(self) -> typing.Optional[float]:
        """Get or set the Exponent on normal stress term.
        """ # nopep8
        return self._cards[2].get_value("exsn")

    @exsn.setter
    def exsn(self, value: float) -> None:
        """Set the exsn property."""
        self._cards[2].set_value("exsn", value)

    @property
    def exsb(self) -> typing.Optional[float]:
        """Get or set the Exponent on bending stress term.
        """ # nopep8
        return self._cards[2].get_value("exsb")

    @exsb.setter
    def exsb(self, value: float) -> None:
        """Set the exsb property."""
        self._cards[2].set_value("exsb", value)

    @property
    def exss(self) -> typing.Optional[float]:
        """Get or set the Exponent on shear stress term.
        """ # nopep8
        return self._cards[2].get_value("exss")

    @exss.setter
    def exss(self, value: float) -> None:
        """Set the exss property."""
        self._cards[2].set_value("exss", value)

    @property
    def lcsn(self) -> typing.Optional[int]:
        """Get or set the Curve ID for normal strength scale factor as a function of strain rate
        """ # nopep8
        return self._cards[2].get_value("lcsn")

    @lcsn.setter
    def lcsn(self, value: int) -> None:
        """Set the lcsn property."""
        self._cards[2].set_value("lcsn", value)

    @property
    def lcsb(self) -> typing.Optional[int]:
        """Get or set the Curve ID for bending strength scale factor as a function of strain rate.
        """ # nopep8
        return self._cards[2].get_value("lcsb")

    @lcsb.setter
    def lcsb(self, value: int) -> None:
        """Set the lcsb property."""
        self._cards[2].set_value("lcsb", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Curve ID for shear strength scale factor as a function of strain rate
        """ # nopep8
        return self._cards[2].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[2].set_value("lcss", value)

    @property
    def gfad(self) -> typing.Optional[int]:
        """Get or set the Fading energy for damage type 3.
        """ # nopep8
        return self._cards[2].get_value("gfad")

    @gfad.setter
    def gfad(self, value: int) -> None:
        """Set the gfad property."""
        self._cards[2].set_value("gfad", value)

    @property
    def sclmrr(self) -> float:
        """Get or set the Scaling factor for torsional moment in failure function.
        """ # nopep8
        return self._cards[2].get_value("sclmrr")

    @sclmrr.setter
    def sclmrr(self, value: float) -> None:
        """Set the sclmrr property."""
        self._cards[2].set_value("sclmrr", value)

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

    @property
    def mid_link(self) -> KeywordBase:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

