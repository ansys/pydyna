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

"""Module providing the EmEosPermeability class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMEOSPERMEABILITY_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eostype", int, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("poly", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
)

class EmEosPermeability(KeywordBase):
    """DYNA EM_EOS_PERMEABILITY keyword"""

    keyword = "EM"
    subkeyword = "EOS_PERMEABILITY"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEosPermeability class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEOSPERMEABILITY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Id of the *EM_EOS.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def eostype(self) -> typing.Optional[int]:
        """Get or set the Define the type of EOS:
        EQ.1: Permeability defined by B as a function of H curve (B=?H). The abscissa of the curve is H, and the ordinate is B.
        EQ.2: Permeability defined by a H function of B curve(H = B / ?).The abscissa of the curve is B, and the ordinate is H.
        EQ.3 : Permeability defined by a load curve where B - H curves(B = ?H) may be defined as a function of von Mises stress.The abscissa of the curve is von Mises stress, and the ordinate is a load curve ID for the B - H load curves.The abscissa for each B - H load curve is H, and the ordinate is B.
        EQ.4 : Permeability defined by a load curve where B - H load curves(B = ?H) may be defined as a function of temperature.The abscissa of the curve is temperature, and the ordinate is load curve ID for the B - H load curves.The abscissa for each B - H load curve is H, and the ordinate is B.
        EQ.5 : Permeability defined by a load curve where B - H curves(B = �H) may be defined as a function of an equivalent stress model calculated from uniaxial stress.The abscissa of the curve is uniaxial stress; the ordinate is a load curve ID for the B - H load curves.The abscissa for each B - H load curve is H, and the ordinate is B.See Remark 4.
        EQ.7: Permeability given by an analytical arctan law defining B as a function of H and optionally temperature.See Remark 1.
        EQ.8 : Permeability given by the Froelich law defining B as a function of H and optionally temperature.See Remark 2.
        EQ.31 : Same as 3, except the signed von Mises stress is used.
        """ # nopep8
        return self._cards[0].get_value("eostype")

    @eostype.setter
    def eostype(self, value: int) -> None:
        """Set the eostype property."""
        self._cards[0].set_value("eostype", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for EOSTYPE = 1, 2, 3, and 4. Ignored if EOSTYPE = 7 or 8.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def poly(self) -> int:
        """Get or set the Applies a polynomial smoothing on the input load curve for EOSTYPE = 1 and 2. See Remark 3.
        EQ.0: Off.
        EQ.1: On.
        """ # nopep8
        return self._cards[0].get_value("poly")

    @poly.setter
    def poly(self, value: int) -> None:
        """Set the poly property."""
        if value not in [0, 1, None]:
            raise Exception("""poly must be `None` or one of {0,1}.""")
        self._cards[0].set_value("poly", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

