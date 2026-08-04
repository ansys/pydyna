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

"""Module providing the EfvStructuredMeshRefine class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_EFVSTRUCTUREDMESHREFINE_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("ifx", int, 10, 10, 1),
    FieldSchema("ify", int, 20, 10, 1),
    FieldSchema("ifz", int, 30, 10, 1),
    FieldSchema("ibox", int, 40, 10, None),
)

class EfvStructuredMeshRefine(KeywordBase):
    """DYNA EFV_STRUCTURED_MESH_REFINE keyword"""

    keyword = "EFV"
    subkeyword = "STRUCTURED_MESH_REFINE"
    _link_fields = {
        "ibox": LinkType.DEFINE_BOX,
    }

    def __init__(self, **kwargs):
        """Initialize the EfvStructuredMeshRefine class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRUCTUREDMESHREFINE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the ID of the Finite Volume Euler mesh to refine.It�s the part set ID in *EFV_STRUCTURED_MESH.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def ifx(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers.
        IFX.LT.0:	|IFX| is the level of refinement in all directions.
        """ # nopep8
        return self._cards[0].get_value("ifx")

    @ifx.setter
    def ifx(self, value: int) -> None:
        """Set the ifx property."""
        self._cards[0].set_value("ifx", value)

    @property
    def ify(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers.
        IFX.LT.0:	|IFX| is the level of refinement in all directions.
        """ # nopep8
        return self._cards[0].get_value("ify")

    @ify.setter
    def ify(self, value: int) -> None:
        """Set the ify property."""
        self._cards[0].set_value("ify", value)

    @property
    def ifz(self) -> int:
        """Get or set the Refinement factor for each local direction.  The number of elements in each direction of the new mesh is the refinement factor for that direction multiplied by the current number of elements in that direction.  They must be integers.
        IFX.LT.0:	|IFX| is the level of refinement in all directions.
        """ # nopep8
        return self._cards[0].get_value("ifz")

    @ifz.setter
    def ifz(self, value: int) -> None:
        """Set the ifz property."""
        self._cards[0].set_value("ifz", value)

    @property
    def ibox(self) -> typing.Optional[int]:
        """Get or set the ID of a box controlled by *DEFINE_BOX that delimits a region of the Finite Volume Euler mesh to refine.
        """ # nopep8
        return self._cards[0].get_value("ibox")

    @ibox.setter
    def ibox(self, value: int) -> None:
        """Set the ibox property."""
        self._cards[0].set_value("ibox", value)

    @property
    def ibox_link(self) -> typing.Optional[DefineBox]:
        """Get the DefineBox object for ibox."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.ibox:
                return kwd
        return None

    @ibox_link.setter
    def ibox_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for ibox."""
        self.ibox = value.boxid

