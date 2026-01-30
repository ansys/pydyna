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

"""Module providing the NodeTransform class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_transformation import DefineTransformation

_NODETRANSFORM_CARD0 = (
    FieldSchema("trsid", int, 0, 10, None),
    FieldSchema("nsid", int, 10, 10, None),
    FieldSchema("immed", int, 20, 10, 0),
)

class NodeTransform(KeywordBase):
    """DYNA NODE_TRANSFORM keyword"""

    keyword = "NODE"
    subkeyword = "TRANSFORM"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
        "trsid": LinkType.DEFINE_TRANSFORMATION,
    }

    def __init__(self, **kwargs):
        """Initialize the NodeTransform class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _NODETRANSFORM_CARD0,
                **kwargs,
            ),        ]
    @property
    def trsid(self) -> typing.Optional[int]:
        """Get or set the The ID of the transformation defined under *DEFINE_TRANSFOR-MATION
        """ # nopep8
        return self._cards[0].get_value("trsid")

    @trsid.setter
    def trsid(self, value: int) -> None:
        """Set the trsid property."""
        self._cards[0].set_value("trsid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of the set of nodes to be subject to the transformation.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def immed(self) -> int:
        """Get or set the Optional flag for transformation processing :
        EQ.0:	Node transformation is performed after all input cards are read through.It is more efficient,and the definition sequence of  NODE_TRANSFORMand its NSID is irrelevant, i.e., the referred NSID doesn�t have to be defined prior to* NODE_TRANSFORM.However, for example, if nodes in NSID are used in POS6N of* DEFINE_TRANSFORMATION, its original coordinates, not the transformed coordinates, will be used to define the transformation matrix.
        EQ.1 : Node transformation is performed immediately after * NODE_TRANSFORM is read.The referred NSID and its nodes have to be defined prior to * NODE_TRANSFORM
        """ # nopep8
        return self._cards[0].get_value("immed")

    @immed.setter
    def immed(self, value: int) -> None:
        """Set the immed property."""
        if value not in [0, 1, None]:
            raise Exception("""immed must be `None` or one of {0,1}.""")
        self._cards[0].set_value("immed", value)

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def trsid_link(self) -> DefineTransformation:
        """Get the DefineTransformation object for trsid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TRANSFORMATION"):
            if kwd.tranid == self.trsid:
                return kwd
        return None

    @trsid_link.setter
    def trsid_link(self, value: DefineTransformation) -> None:
        """Set the DefineTransformation object for trsid."""
        self.trsid = value.tranid

