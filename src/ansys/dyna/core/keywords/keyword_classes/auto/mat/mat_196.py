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

"""Module providing the Mat196 class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT196_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("dospot", int, 70, 10, 0),
)

_MAT196_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat196(KeywordBase):
    """DYNA MAT_196 keyword"""

    keyword = "MAT"
    subkeyword = "196"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "flcid": LinkType.DEFINE_CURVE,
        "hlcid": LinkType.DEFINE_CURVE,
        "glcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat196 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT196_CARD0,
                **kwargs,
            ),            TableCardGroup(
                [
                    (
                        FieldSchema("dof", int, 0, 10, None),
                        FieldSchema("type", int, 10, 10, None),
                        FieldSchema("k", float, 20, 10, None),
                        FieldSchema("d", float, 30, 10, None),
                        FieldSchema("cdf", float, 40, 10, None),
                        FieldSchema("tdf", float, 50, 10, None),
                    ),
                    (
                        FieldSchema("flcid", int, 0, 10, None),
                        FieldSchema("hlcid", int, 10, 10, None),
                        FieldSchema("c1", float, 20, 10, None),
                        FieldSchema("c2", float, 30, 10, None),
                        FieldSchema("dle", float, 40, 10, None),
                        FieldSchema("glcid", int, 50, 10, None),
                    ),
                ],
                None,
                None,
                "springs",
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat196.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT196_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def dospot(self) -> int:
        """Get or set the Activate thinning of tied shell elements when SPOTHIN>0 on *CONTROL_‌CONTACT.
        EQ.0:	Spot weld thinning is inactive for shells tied to discrete beams that use this material(default)
        EQ.1 : Spot weld thinning is active for shells tied to discrete beams that use this material
        """ # nopep8
        return self._cards[0].get_value("dospot")

    @dospot.setter
    def dospot(self, value: int) -> None:
        """Set the dospot property."""
        if value not in [0, 1, None]:
            raise Exception("""dospot must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dospot", value)

    @property
    def springs(self) -> pd.DataFrame:
        """Gets the full table of springs."""
        return self._cards[1].table

    @springs.setter
    def springs(self, df: pd.DataFrame):
        """sets springs from the dataframe df."""
        self._cards[1].table = df

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
    def flcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for flcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.flcid:
                return kwd
        return None

    @flcid_link.setter
    def flcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for flcid."""
        self.flcid = value.lcid

    @property
    def hlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for hlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcid:
                return kwd
        return None

    @hlcid_link.setter
    def hlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcid."""
        self.hlcid = value.lcid

    @property
    def glcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for glcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.glcid:
                return kwd
        return None

    @glcid_link.setter
    def glcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for glcid."""
        self.glcid = value.lcid


class MatGeneralSpringDiscreteBeam(Mat196):
    """Alias for MAT keyword."""
    subkeyword = "GENERAL_SPRING_DISCRETE_BEAM"
