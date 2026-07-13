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

"""Module providing the DefineSpgToSurfaceCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPGTOSURFACECOUPLING_CARD0 = (
    FieldSchema("spgp", int, 0, 10, None),
    FieldSchema("surf", int, 10, 10, None),
    FieldSchema("sptype", int, 20, 10, 0),
    FieldSchema("sftype", int, 30, 10, None),
)

_DEFINESPGTOSURFACECOUPLING_CARD1 = (
    FieldSchema("sbc", int, 0, 10, None),
    FieldSchema("fs", float, 10, 10, None),
    FieldSchema("fd", float, 20, 10, None),
    FieldSchema("dc", float, 30, 10, None),
    FieldSchema("sfp", float, 40, 10, 0.1),
    FieldSchema("thk", float, 50, 10, 0.5),
    FieldSchema("frq", int, 60, 10, 50),
)

_DEFINESPGTOSURFACECOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSpgToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_SPG_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "SPG_TO_SURFACE_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "surf": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSpgToSurfaceCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPGTOSURFACECOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPGTOSURFACECOUPLING_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSpgToSurfaceCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPGTOSURFACECOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def spgp(self) -> typing.Optional[int]:
        """Get or set the Part ID/Part set ID for the SPG particles.
        """ # nopep8
        return self._cards[0].get_value("spgp")

    @spgp.setter
    def spgp(self, value: int) -> None:
        """Set the spgp property."""
        self._cards[0].set_value("spgp", value)

    @property
    def surf(self) -> typing.Optional[int]:
        """Get or set the Segment set ID specifying the surface.
        """ # nopep8
        return self._cards[0].get_value("surf")

    @surf.setter
    def surf(self, value: int) -> None:
        """Set the surf property."""
        self._cards[0].set_value("surf", value)

    @property
    def sptype(self) -> int:
        """Get or set the Type for SPGP:
        EQ.0: Part set ID
        EQ.1: Part ID
        """ # nopep8
        return self._cards[0].get_value("sptype")

    @sptype.setter
    def sptype(self, value: int) -> None:
        """Set the sptype property."""
        if value not in [0, 1, None]:
            raise Exception("""sptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sptype", value)

    @property
    def sftype(self) -> typing.Optional[int]:
        """Get or set the Type for SURF:
        EQ.0: Segment set ID
        """ # nopep8
        return self._cards[0].get_value("sftype")

    @sftype.setter
    def sftype(self, value: int) -> None:
        """Set the sftype property."""
        self._cards[0].set_value("sftype", value)

    @property
    def sbc(self) -> typing.Optional[int]:
        """Get or set the Type of boundary condition:
        EQ.0: Free - slip boundary
        EQ.1: Non - slip boundary(tied after contact)
        """ # nopep8
        return self._cards[1].get_value("sbc")

    @sbc.setter
    def sbc(self, value: int) -> None:
        """Set the sbc property."""
        self._cards[1].set_value("sbc", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction. If SBC=1, then FS is not used.
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[1].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction. It is set the value of FS by default.
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[1].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay of the friction coefficient.
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[1].set_value("dc", value)

    @property
    def sfp(self) -> float:
        """Get or set the Scaling factor of the penetration stiffness coefficient along the normal direction of the contact interface. Default value is 0.1.
        """ # nopep8
        return self._cards[1].get_value("sfp")

    @sfp.setter
    def sfp(self, value: float) -> None:
        """Set the sfp property."""
        self._cards[1].set_value("sfp", value)

    @property
    def thk(self) -> float:
        """Get or set the Thickness scaling factor for contact search. During initialization, LS-DYNA searches for the particle that is closest to the surface. The distance of this particle from the surface at the beginning of the simulation is scaled by THK. This scaled distance is used as the contact thickness for the surfaces segments. The default value of THK is 0.5.
        """ # nopep8
        return self._cards[1].get_value("thk")

    @thk.setter
    def thk(self, value: float) -> None:
        """Set the thk property."""
        self._cards[1].set_value("thk", value)

    @property
    def frq(self) -> int:
        """Get or set the Bucket sorting frequency. Default is 50.
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
    def surf_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for surf."""
        return self._get_set_link("SEGMENT", self.surf)

    @surf_link.setter
    def surf_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for surf."""
        self.surf = value.sid

