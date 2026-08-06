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

"""Module providing the InitialStressSpg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALSTRESSSPG_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("nhisv", int, 10, 10, 0),
    FieldSchema("sigxx", float, 20, 10, 0.0),
    FieldSchema("sigyy", float, 30, 10, 0.0),
    FieldSchema("sigzz", float, 40, 10, 0.0),
    FieldSchema("sigxy", float, 50, 10, 0.0),
    FieldSchema("sigyz", float, 60, 10, 0.0),
    FieldSchema("sigzx", float, 70, 10, 0.0),
)

_INITIALSTRESSSPG_CARD1 = (
    FieldSchema("eps", float, 0, 10, 0.0),
    FieldSchema("hisv1_axx", float, 10, 10, 0.0),
    FieldSchema("hisv2_ayy", float, 20, 10, 0.0),
    FieldSchema("hisv3_axy", float, 30, 10, 0.0),
    FieldSchema("hisv4_ayz", float, 40, 10, 0.0),
    FieldSchema("hisv5_azx", float, 50, 10, 0.0),
    FieldSchema("hisv6_fvf", float, 60, 10, 0.0),
    FieldSchema("hisv7", float, 70, 10, 0.0),
)

class InitialStressSpg(KeywordBase):
    """DYNA INITIAL_STRESS_SPG keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_SPG"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialStressSpg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRESSSPG_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INITIALSTRESSSPG_CARD1,
                **kwargs,
            ),
        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Nid is particle ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def nhisv(self) -> int:
        """Get or set the nhisv is the number of history variables per particle.
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        """Set the nhisv property."""
        self._cards[0].set_value("nhisv", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Define the xx stress
        """ # nopep8
        return self._cards[0].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        """Set the sigxx property."""
        self._cards[0].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Define the yy stress.
        """ # nopep8
        return self._cards[0].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        """Set the sigyy property."""
        self._cards[0].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Define the zz stress.
        """ # nopep8
        return self._cards[0].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        """Set the sigzz property."""
        self._cards[0].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Define the xy stress.
        """ # nopep8
        return self._cards[0].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        """Set the sigxy property."""
        self._cards[0].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Define the yz stress.
        """ # nopep8
        return self._cards[0].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        """Set the sigyz property."""
        self._cards[0].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Define the zx stress.
        """ # nopep8
        return self._cards[0].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        """Set the sigzx property."""
        self._cards[0].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        """Set the eps property."""
        self._cards[1].set_value("eps", value)

    @property
    def hisv1_axx(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv1_axx")

    @hisv1_axx.setter
    def hisv1_axx(self, value: float) -> None:
        """Set the hisv1_axx property."""
        self._cards[1].set_value("hisv1_axx", value)

    @property
    def hisv2_ayy(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv2_ayy")

    @hisv2_ayy.setter
    def hisv2_ayy(self, value: float) -> None:
        """Set the hisv2_ayy property."""
        self._cards[1].set_value("hisv2_ayy", value)

    @property
    def hisv3_axy(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv3_axy")

    @hisv3_axy.setter
    def hisv3_axy(self, value: float) -> None:
        """Set the hisv3_axy property."""
        self._cards[1].set_value("hisv3_axy", value)

    @property
    def hisv4_ayz(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv4_ayz")

    @hisv4_ayz.setter
    def hisv4_ayz(self, value: float) -> None:
        """Set the hisv4_ayz property."""
        self._cards[1].set_value("hisv4_ayz", value)

    @property
    def hisv5_azx(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv5_azx")

    @hisv5_azx.setter
    def hisv5_azx(self, value: float) -> None:
        """Set the hisv5_azx property."""
        self._cards[1].set_value("hisv5_azx", value)

    @property
    def hisv6_fvf(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv6_fvf")

    @hisv6_fvf.setter
    def hisv6_fvf(self, value: float) -> None:
        """Set the hisv6_fvf property."""
        self._cards[1].set_value("hisv6_fvf", value)

    @property
    def hisv7(self) -> float:
        """Get or set the the history variable
        """ # nopep8
        return self._cards[1].get_value("hisv7")

    @hisv7.setter
    def hisv7(self, value: float) -> None:
        """Set the hisv7 property."""
        self._cards[1].set_value("hisv7", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

