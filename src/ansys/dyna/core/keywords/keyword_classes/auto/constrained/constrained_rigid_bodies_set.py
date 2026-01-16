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

"""Module providing the ConstrainedRigidBodiesSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONSTRAINEDRIGIDBODIESSET_CARD0 = (
    FieldSchema("pidl", int, 0, 10, None),
    FieldSchema("pidc", int, 10, 10, None),
    FieldSchema("iflag", int, 20, 10, None),
)

class ConstrainedRigidBodiesSet(KeywordBase):
    """DYNA CONSTRAINED_RIGID_BODIES_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "RIGID_BODIES_SET"
    _link_fields = {
        "pidc": LinkType.SET_PART,
        "pidl": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedRigidBodiesSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDRIGIDBODIESSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def pidl(self) -> typing.Optional[int]:
        """Get or set the Lead rigid body part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidl")

    @pidl.setter
    def pidl(self, value: int) -> None:
        """Set the pidl property."""
        self._cards[0].set_value("pidl", value)

    @property
    def pidc(self) -> typing.Optional[int]:
        """Get or set the Constrained rigid body part set ID, see *SET_PART (If _SET option is used, this input references to a part set ID, see *SET_PART.).
        """ # nopep8
        return self._cards[0].get_value("pidc")

    @pidc.setter
    def pidc(self, value: int) -> None:
        """Set the pidc property."""
        self._cards[0].set_value("pidc", value)

    @property
    def iflag(self) -> typing.Optional[int]:
        """Get or set the This flag is meaningful if and only if the inertia properties of the lead part, PIDL, are defined in *PART_‌INERTIA.  See Remark 1.
        EQ.1:	Update the center - of - gravity, the translational mass,and the inertia matrix of PIDL to reflect its merging with the constrained rigid body(PIDC).
        EQ.0 : The merged PIDC will not affect the properties defined in * PART_‌INERTIA for PIDL since the properties are assumed to already account for merged parts.If the properties are not defined in a* PART_‌INERTIA definition, the inertia properties of PIDC will be computed from its nodal masses.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        self._cards[0].set_value("iflag", value)

    @property
    def pidc_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for pidc."""
        return self._get_set_link("PART", self.pidc)

    @pidc_link.setter
    def pidc_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pidc."""
        self.pidc = value.sid

    @property
    def pidl_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidl."""
        return self._get_link_by_attr("PART", "pid", self.pidl, "parts")

