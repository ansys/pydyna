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

"""Module providing the DefineDeToSurfaceTied class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINEDETOSURFACETIED_CARD0 = (
    FieldSchema("desid", int, 0, 10, 0),
    FieldSchema("surfid", int, 10, 10, 0),
    FieldSchema("destyp", int, 20, 10, 0),
    FieldSchema("surftyp", int, 30, 10, 0),
)

_DEFINEDETOSURFACETIED_CARD1 = (
    FieldSchema("nflf", float, 0, 10, None),
    FieldSchema("sflf", float, 10, 10, None),
    FieldSchema("nen", float, 20, 10, 2.0),
    FieldSchema("mes", float, 30, 10, 2.0),
    FieldSchema("lcid", int, 40, 10, None),
    FieldSchema("nsort", int, 50, 10, 100),
    FieldSchema("maxgap", float, 60, 10, None),
)

_DEFINEDETOSURFACETIED_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeToSurfaceTied(KeywordBase):
    """DYNA DEFINE_DE_TO_SURFACE_TIED keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_SURFACE_TIED"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineDeToSurfaceTied class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACETIED_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACETIED_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineDeToSurfaceTied._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDETOSURFACETIED_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def desid(self) -> int:
        """Get or set the Node set ID, node ID, part set ID or part ID specifying the DES in the tired interface. DESTYP below indicates the ID type
        """ # nopep8
        return self._cards[0].get_value("desid")

    @desid.setter
    def desid(self, value: int) -> None:
        """Set the desid property."""
        self._cards[0].set_value("desid", value)

    @property
    def surfid(self) -> int:
        """Get or set the Part set ID or part ID specifying the surface. SURFID below indicates the ID type.
        """ # nopep8
        return self._cards[0].get_value("surfid")

    @surfid.setter
    def surfid(self, value: int) -> None:
        """Set the surfid property."""
        self._cards[0].set_value("surfid", value)

    @property
    def destyp(self) -> int:
        """Get or set the Type for DESID:
        EQ.0: Node set
        EQ.1: Node
        EQ.2: Part setn
        EQ.3: Part
        """ # nopep8
        return self._cards[0].get_value("destyp")

    @destyp.setter
    def destyp(self, value: int) -> None:
        """Set the destyp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""destyp must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("destyp", value)

    @property
    def surftyp(self) -> int:
        """Get or set the SURFID type:
        EQ.0: Part set
        EQ.1: Part
        """ # nopep8
        return self._cards[0].get_value("surftyp")

    @surftyp.setter
    def surftyp(self, value: int) -> None:
        """Set the surftyp property."""
        if value not in [0, 1, None]:
            raise Exception("""surftyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("surftyp", value)

    @property
    def nflf(self) -> typing.Optional[float]:
        """Get or set the Normal failure force. Only tensile failure, i.e., tensile normal forces, will be considered in the failure criterion.
        """ # nopep8
        return self._cards[1].get_value("nflf")

    @nflf.setter
    def nflf(self, value: float) -> None:
        """Set the nflf property."""
        self._cards[1].set_value("nflf", value)

    @property
    def sflf(self) -> typing.Optional[float]:
        """Get or set the Shear failure force.
        """ # nopep8
        return self._cards[1].get_value("sflf")

    @sflf.setter
    def sflf(self, value: float) -> None:
        """Set the sflf property."""
        self._cards[1].set_value("sflf", value)

    @property
    def nen(self) -> float:
        """Get or set the Exponent for normal force.
        """ # nopep8
        return self._cards[1].get_value("nen")

    @nen.setter
    def nen(self, value: float) -> None:
        """Set the nen property."""
        self._cards[1].set_value("nen", value)

    @property
    def mes(self) -> float:
        """Get or set the Exponent for shear force. Failure criterion.
        """ # nopep8
        return self._cards[1].get_value("mes")

    @mes.setter
    def mes(self, value: float) -> None:
        """Set the mes property."""
        self._cards[1].set_value("mes", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Optional curve ID defining a scale factor vs. time.  The scale factor is applied to NFLF and SFLF, making the failure forces time-dependent.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def nsort(self) -> int:
        """Get or set the Number of cycle between bucket sort.
        """ # nopep8
        return self._cards[1].get_value("nsort")

    @nsort.setter
    def nsort(self, value: int) -> None:
        """Set the nsort property."""
        self._cards[1].set_value("nsort", value)

    @property
    def maxgap(self) -> typing.Optional[float]:
        """Get or set the Maximum gap between DES and master surface:
        GT.0.0: defines the ratio of the DES radius as the maximum gap, that is, MAXGAP  r_DES
        LT.0.0: absolute value is used as the maximum gap
        """ # nopep8
        return self._cards[1].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        """Set the maxgap property."""
        self._cards[1].set_value("maxgap", value)

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

