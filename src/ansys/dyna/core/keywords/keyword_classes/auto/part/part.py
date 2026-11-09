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

"""Module providing the Part class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.hourglass.hourglass import Hourglass

class Part(KeywordBase):
    """DYNA PART keyword"""

    keyword = "PART"
    subkeyword = "PART"
    _link_fields = {
        "mid": LinkType.MAT,
        "secid": LinkType.SECTION,
        "hgid": LinkType.HOURGLASS,
    }

    def __init__(self, **kwargs):
        """Initialize the Part class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    (
                        FieldSchema("heading", str, 0, 70, None),
                    ),
                    (
                        FieldSchema("pid", int, 0, 10, None),
                        FieldSchema("secid", int, 10, 10, None),
                        FieldSchema("mid", int, 20, 10, None),
                        FieldSchema("eosid", int, 30, 10, 0),
                        FieldSchema("hgid", int, 40, 10, 0),
                        FieldSchema("grav", int, 50, 10, 0),
                        FieldSchema("adpopt", int, 60, 10, None),
                        FieldSchema("tmid", int, 70, 10, 0),
                    ),
                ],
                None,
                None,
                "parts",
                **kwargs,
            ),
        ]
    @property
    def parts(self) -> pd.DataFrame:
        """Gets the full table of parts."""
        return self._cards[0].table

    @parts.setter
    def parts(self, df: pd.DataFrame):
        """sets parts from the dataframe df."""
        self._cards[0].table = df

    @property
    def heading(self) -> pd.Series:
        """Get or set the Get or set the Heading for the part
        """ # nopep8
        return self._cards[0].get_column("heading")

    @heading.setter
    def heading(self, value: typing.Union[str, typing.Sequence[str], pd.Series]) -> None:
        """Set the heading column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("heading", value)

    @property
    def pid(self) -> pd.Series:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def secid(self) -> pd.Series:
        """Get or set the Section ID defined in *SECTION section.
        """ # nopep8
        return self._cards[0].get_column("secid")

    @secid.setter
    def secid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the secid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("secid", value)

    @property
    def mid(self) -> pd.Series:
        """Get or set the Material ID defined in *MAT section.
        """ # nopep8
        return self._cards[0].get_column("mid")

    @mid.setter
    def mid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the mid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("mid", value)

    @property
    def eosid(self) -> pd.Series:
        """Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
        """ # nopep8
        return self._cards[0].get_column("eosid")

    @eosid.setter
    def eosid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the eosid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("eosid", value)

    @property
    def hgid(self) -> pd.Series:
        """Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
        EQ.0: default values are used.
        """ # nopep8
        return self._cards[0].get_column("hgid")

    @hgid.setter
    def hgid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the hgid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("hgid", value)

    @property
    def grav(self) -> pd.Series:
        """Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
        EQ.0: all parts initialized,
        EQ.1: only current material initialized.
        """ # nopep8
        return self._cards[0].get_column("grav")

    @grav.setter
    def grav(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the grav column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("grav", value)

    @property
    def adpopt(self) -> pd.Series:
        """Get or set the Indicate if this part is adapted. See also *CONTROL_ADAPTIVITY.
        LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
        EQ.0:Adaptive remeshing is inactive for this part ID.
        EQ.1: h - adaptivity for 3D shells and shell/solid/shell sandwich composites.
        EQ.2: r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword Users Manual(Multiscale Solvers).
        EQ.3: Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword Users Manual(Multiscale Solvers).
        EQ.9: Passive h - adaptivity for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
        """ # nopep8
        return self._cards[0].get_column("adpopt")

    @adpopt.setter
    def adpopt(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the adpopt column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("adpopt", value)

    @property
    def tmid(self) -> pd.Series:
        """Get or set the Thermal material property identification defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell,
        and thick shell parts for thermal or coupled thermal-structual analyses. Beams and discrete elements are not considered in thermal analyses. See Remark 7.
        """ # nopep8
        return self._cards[0].get_column("tmid")

    @tmid.setter
    def tmid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the tmid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("tmid", value)

    @property
    def mid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all MAT_* keywords for mid, keyed by pid."""
        return self._get_table_group_links("MAT", "mid", "parts", "mid", "pid")

    def get_mid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for the given pid."""
        return self._get_table_group_link("MAT", "mid", "parts", "mid", "pid", pid)

    @property
    def secid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all SECTION_* keywords for secid, keyed by pid."""
        return self._get_table_group_links("SECTION", "secid", "parts", "secid", "pid")

    def get_secid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the SECTION_* keyword for the given pid."""
        return self._get_table_group_link("SECTION", "secid", "parts", "secid", "pid", pid)

    @property
    def hgid_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all HOURGLASS_* keywords for hgid, keyed by pid."""
        return self._get_table_group_links("HOURGLASS", "hgid", "parts", "hgid", "pid")

    def get_hgid_link(self, pid: int) -> typing.Optional[KeywordBase]:
        """Get the HOURGLASS_* keyword for the given pid."""
        return self._get_table_group_link("HOURGLASS", "hgid", "parts", "hgid", "pid", pid)

