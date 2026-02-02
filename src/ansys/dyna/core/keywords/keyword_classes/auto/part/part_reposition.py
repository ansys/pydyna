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

"""Module providing the PartReposition class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.hourglass.hourglass import Hourglass

_PARTREPOSITION_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

_PARTREPOSITION_CARD1 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("secid", int, 10, 10, None),
    FieldSchema("mid", int, 20, 10, None),
    FieldSchema("eosid", int, 30, 10, 0),
    FieldSchema("hgid", int, 40, 10, 0),
    FieldSchema("grav", int, 50, 10, 0),
    FieldSchema("adpopt", int, 60, 10, None),
    FieldSchema("tmid", int, 70, 10, 0),
)

_PARTREPOSITION_CARD2 = (
    FieldSchema("cmsn", int, 0, 10, None),
    FieldSchema("mdep", int, 10, 10, 0),
    FieldSchema("movopt", int, 20, 10, 0),
)

class PartReposition(KeywordBase):
    """DYNA PART_REPOSITION keyword"""

    keyword = "PART"
    subkeyword = "REPOSITION"
    _link_fields = {
        "mid": LinkType.MAT,
        "secid": LinkType.SECTION,
        "hgid": LinkType.HOURGLASS,
    }

    def __init__(self, **kwargs):
        """Initialize the PartReposition class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARTREPOSITION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PARTREPOSITION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PARTREPOSITION_CARD2,
                **kwargs,
            ),        ]
    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID defined in *SECTION section.
        """ # nopep8
        return self._cards[1].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[1].set_value("secid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT section.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[1].set_value("mid", value)

    @property
    def eosid(self) -> int:
        """Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
        """ # nopep8
        return self._cards[1].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[1].set_value("eosid", value)

    @property
    def hgid(self) -> int:
        """Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
        EQ.0: default values are used.
        """ # nopep8
        return self._cards[1].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        """Set the hgid property."""
        self._cards[1].set_value("hgid", value)

    @property
    def grav(self) -> int:
        """Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
        EQ.0: all parts initialized,
        EQ.1: only current material initialized.
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: int) -> None:
        """Set the grav property."""
        if value not in [0, 1, None]:
            raise Exception("""grav must be `None` or one of {0,1}.""")
        self._cards[1].set_value("grav", value)

    @property
    def adpopt(self) -> typing.Optional[int]:
        """Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
        LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
        EQ.0:Adaptive remeshing is inactive for this part ID.
        EQ.1:	h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
        EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.
        EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).
        EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        """Set the adpopt property."""
        self._cards[1].set_value("adpopt", value)

    @property
    def tmid(self) -> int:
        """Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
        EQ.0: defaults to MID.
        """ # nopep8
        return self._cards[1].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        """Set the tmid property."""
        self._cards[1].set_value("tmid", value)

    @property
    def cmsn(self) -> typing.Optional[int]:
        """Get or set the CAL3D segment number/MADYMO system number. See the numbering in the corresponding program.
        """ # nopep8
        return self._cards[2].get_value("cmsn")

    @cmsn.setter
    def cmsn(self, value: int) -> None:
        """Set the cmsn property."""
        self._cards[2].set_value("cmsn", value)

    @property
    def mdep(self) -> int:
        """Get or set the MADYMO ellipse/plane number:
        GT.0: ellipse number,
        EQ.0: default,
        LT.0: absolute value is plane number.
        """ # nopep8
        return self._cards[2].get_value("mdep")

    @mdep.setter
    def mdep(self, value: int) -> None:
        """Set the mdep property."""
        self._cards[2].set_value("mdep", value)

    @property
    def movopt(self) -> int:
        """Get or set the Flag to deactivate moving for merged rigid bodies, see *CONSTRAINED_RIGID_BODIES. This option allows a merged rigid body to be fixed in space while the nodes and elements of the generated CAL3D/MADYMO parts are repositioned:
        EQ.0: merged rigid body is repositioned (default),
        EQ.1: merged rigid body is not repositioned.
        """ # nopep8
        return self._cards[2].get_value("movopt")

    @movopt.setter
    def movopt(self, value: int) -> None:
        """Set the movopt property."""
        if value not in [0, 1, None]:
            raise Exception("""movopt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("movopt", value)

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

    @property
    def secid_link(self) -> KeywordBase:
        """Get the SECTION_* keyword for secid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("SECTION"):
            if kwd.secid == self.secid:
                return kwd
        return None

    @secid_link.setter
    def secid_link(self, value: KeywordBase) -> None:
        """Set the SECTION_* keyword for secid."""
        self.secid = value.secid

    @property
    def hgid_link(self) -> Hourglass:
        """Get the Hourglass object for hgid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("HOURGLASS", "HOURGLASS"):
            if kwd.hgid == self.hgid:
                return kwd
        return None

    @hgid_link.setter
    def hgid_link(self, value: Hourglass) -> None:
        """Set the Hourglass object for hgid."""
        self.hgid = value.hgid

