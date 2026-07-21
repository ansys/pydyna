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

"""Module providing the DefineSpgToSpgCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPGTOSPGCOUPLING_CARD0 = (
    FieldSchema("surfa", int, 0, 10, None),
    FieldSchema("surfb", int, 10, 10, None),
    FieldSchema("spgpa", int, 20, 10, None),
    FieldSchema("spgpb", int, 30, 10, None),
)

_DEFINESPGTOSPGCOUPLING_CARD1 = (
    FieldSchema("sbc", int, 0, 10, 0),
    FieldSchema("fs", float, 10, 10, None),
    FieldSchema("fd", float, 20, 10, None),
    FieldSchema("dc", float, 30, 10, None),
    FieldSchema("sfp", float, 40, 10, 0.1),
    FieldSchema("thk", float, 50, 10, 0.5),
    FieldSchema("frq", int, 60, 10, 50),
)

_DEFINESPGTOSPGCOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSpgToSpgCoupling(KeywordBase):
    """DYNA DEFINE_SPG_TO_SPG_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "SPG_TO_SPG_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "surfa": LinkType.SET_SEGMENT,
        "surfb": LinkType.SET_SEGMENT,
        "spgpa": LinkType.PART,
        "spgpb": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSpgToSpgCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPGTOSPGCOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPGTOSPGCOUPLING_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSpgToSpgCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPGTOSPGCOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Segment set id at surface of spga to define the interface between spga and spgb
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        """Set the surfa property."""
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Segment set id at surface of spgb to define the interface between spga and spgb
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        """Set the surfb property."""
        self._cards[0].set_value("surfb", value)

    @property
    def spgpa(self) -> typing.Optional[int]:
        """Get or set the SPG part id
        """ # nopep8
        return self._cards[0].get_value("spgpa")

    @spgpa.setter
    def spgpa(self, value: int) -> None:
        """Set the spgpa property."""
        self._cards[0].set_value("spgpa", value)

    @property
    def spgpb(self) -> typing.Optional[int]:
        """Get or set the SPG part id
        """ # nopep8
        return self._cards[0].get_value("spgpb")

    @spgpb.setter
    def spgpb(self, value: int) -> None:
        """Set the spgpb property."""
        self._cards[0].set_value("spgpb", value)

    @property
    def sbc(self) -> int:
        """Get or set the Type of boundary condition:
        EQ.0: Free - slip boundary
        EQ.1 : No - slip boundary(tied after contact)
        """ # nopep8
        return self._cards[1].get_value("sbc")

    @sbc.setter
    def sbc(self, value: int) -> None:
        """Set the sbc property."""
        if value not in [0, 1, None]:
            raise Exception("""sbc must be `None` or one of {0,1}.""")
        self._cards[1].set_value("sbc", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction. If SBC = 1, then FS is not used
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[1].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction. It is set to the value of FS by default.
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[1].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay of the friction coefficient
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[1].set_value("dc", value)

    @property
    def sfp(self) -> float:
        """Get or set the Scaling factor of the penetration stiffness coefficient along the normal direction of the contact interface. The default value is 0.1.
        """ # nopep8
        return self._cards[1].get_value("sfp")

    @sfp.setter
    def sfp(self, value: float) -> None:
        """Set the sfp property."""
        self._cards[1].set_value("sfp", value)

    @property
    def thk(self) -> float:
        """Get or set the Thickness scaling factor for contact search. During initialization, the code searches for the particle that is closest to the surface. The distance of this particle from the surface at the beginning of the simulation is scaled by THK. This scaled distance is used as the contact thickness for the surface�s segments. The default value of THK is 0.5.
        """ # nopep8
        return self._cards[1].get_value("thk")

    @thk.setter
    def thk(self, value: float) -> None:
        """Set the thk property."""
        self._cards[1].set_value("thk", value)

    @property
    def frq(self) -> int:
        """Get or set the Bucket sorting frequency. The default is 50.
        """ # nopep8
        return self._cards[1].get_value("frq")

    @frq.setter
    def frq(self, value: int) -> None:
        """Set the frq property."""
        self._cards[1].set_value("frq", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def surfa_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for surfa."""
        return self._get_set_link("SEGMENT", self.surfa)

    @surfa_link.setter
    def surfa_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for surfa."""
        self.surfa = value.sid

    @property
    def surfb_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for surfb."""
        return self._get_set_link("SEGMENT", self.surfb)

    @surfb_link.setter
    def surfb_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for surfb."""
        self.surfb = value.sid

    @property
    def spgpa_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spgpa."""
        return self._get_link_by_attr("PART", "pid", self.spgpa, "parts")

    @property
    def spgpb_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given spgpb."""
        return self._get_link_by_attr("PART", "pid", self.spgpb, "parts")

