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

"""Module providing the DefineBoxSph class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_DEFINEBOXSPH_CARD0 = (
    FieldSchema("boxid", int, 0, 10, None),
    FieldSchema("xmn", float, 10, 10, 0.0),
    FieldSchema("xmx", float, 20, 10, 0.0),
    FieldSchema("ymn", float, 30, 10, 0.0),
    FieldSchema("ymx", float, 40, 10, 0.0),
    FieldSchema("zmn", float, 50, 10, 0.0),
    FieldSchema("zmx", float, 60, 10, 0.0),
    FieldSchema("vid", int, 70, 10, 0),
)

_DEFINEBOXSPH_CARD1 = (
    FieldSchema("lcid", int, 0, 10, 0),
    FieldSchema("vd", int, 10, 10, 0),
    FieldSchema("nid", int, 20, 10, 0),
    FieldSchema("ireact", int, 30, 10, 0),
    FieldSchema("ibuff", int, 40, 10, 0),
    FieldSchema("ishow", int, 50, 10, 0),
    FieldSchema("pid", int, 60, 10, 0),
)

_DEFINEBOXSPH_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineBoxSph(KeywordBase):
    """DYNA DEFINE_BOX_SPH keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_SPH"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "nid": LinkType.NODE,
        "vid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineBoxSph class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEBOXSPH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEBOXSPH_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineBoxSph.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEBOXSPH_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Box ID. A unique number must be defined.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        """Set the xmn property."""
        self._cards[0].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        """Set the xmx property."""
        self._cards[0].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        """Set the ymn property."""
        self._cards[0].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        """Set the ymx property."""
        self._cards[0].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        """Set the zmn property."""
        self._cards[0].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        """Set the zmx property."""
        self._cards[0].set_value("zmx", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector ID of DOF, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID to describe motion value versus time, see *DEFINE_CURVE
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def vd(self) -> int:
        """Get or set the Velocity/Displacement flag:
        EQ.0: velocity,
        EQ.1: displacement
        EQ.2:  referential node
        """ # nopep8
        return self._cards[1].get_value("vd")

    @vd.setter
    def vd(self, value: int) -> None:
        """Set the vd property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""vd must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("vd", value)

    @property
    def nid(self) -> int:
        """Get or set the Referential nodal ID for VD=2 (SPH box will move with this node)
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def ireact(self) -> int:
        """Get or set the Reactivation flag:
        EQ.0:	particles outside of the box are permanently deactivated,
        EQ.1 : deactivated particles get reactivated when they enter the box
        """ # nopep8
        return self._cards[1].get_value("ireact")

    @ireact.setter
    def ireact(self, value: int) -> None:
        """Set the ireact property."""
        if value not in [0, 1, None]:
            raise Exception("""ireact must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ireact", value)

    @property
    def ibuff(self) -> int:
        """Get or set the Buffer zone flag:
        EQ.0: particles on the edge of the box don’t get any special treatment.
        EQ.1 : particles on the edge of the box are frozen in space and act as neighbors for active particles inside the box.
        This option is mainly used for fluid simulations to prevent the fluid from spilling out of the activation box.
        """ # nopep8
        return self._cards[1].get_value("ibuff")

    @ibuff.setter
    def ibuff(self, value: int) -> None:
        """Set the ibuff property."""
        if value not in [0, 1, None]:
            raise Exception("""ibuff must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ibuff", value)

    @property
    def ishow(self) -> int:
        """Get or set the Create dummy part to visualize position of activation box in post-processing.
        EQ.0: no part is created.
        EQ.1 : a dummy part is added for visualization
        """ # nopep8
        return self._cards[1].get_value("ishow")

    @ishow.setter
    def ishow(self, value: int) -> None:
        """Set the ishow property."""
        if value not in [0, 1, None]:
            raise Exception("""ishow must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ishow", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID used for visualization if ISHOW=1.
        EQ.0:	a unique Part ID is automatically created.
        GT.0 : the part created by ISHOW = 1 is numbered PID.This should be a unique part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

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
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def vid_link(self) -> DefineVector:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

