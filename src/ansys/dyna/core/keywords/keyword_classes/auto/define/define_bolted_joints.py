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

"""Module providing the DefineBoltedJoints class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEBOLTEDJOINTS_CARD0 = (
    FieldSchema("bjid", int, 0, 10, None),
    FieldSchema("slsfac", float, 10, 10, None),
    FieldSchema("friction", float, 20, 10, None),
    FieldSchema("vdc", float, 30, 10, None),
)

_DEFINEBOLTEDJOINTS_CARD1 = (
    FieldSchema("bsid", int, 0, 10, None),
    FieldSchema("hdang", float, 10, 10, 0.0),
    FieldSchema("ntang", float, 20, 10, 0.0),
    FieldSchema("hssiam", float, 30, 10, None),
    FieldSchema("ntdiam", float, 40, 10, None),
)

_DEFINEBOLTEDJOINTS_CARD2 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("hddept", float, 10, 10, 0.0),
    FieldSchema("ntdept", float, 20, 10, 0.0),
)

_DEFINEBOLTEDJOINTS_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineBoltedJoints(KeywordBase):
    """DYNA DEFINE_BOLTED_JOINTS keyword"""

    keyword = "DEFINE"
    subkeyword = "BOLTED_JOINTS"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "bsid": LinkType.SET_BEAM,
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineBoltedJoints class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEBOLTEDJOINTS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEBOLTEDJOINTS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEBOLTEDJOINTS_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineBoltedJoints._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEBOLTEDJOINTS_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def bjid(self) -> typing.Optional[int]:
        """Get or set the Bolted joint ID. It should be unique from other bolted joints.
        """ # nopep8
        return self._cards[0].get_value("bjid")

    @bjid.setter
    def bjid(self, value: int) -> None:
        """Set the bjid property."""
        self._cards[0].set_value("bjid", value)

    @property
    def slsfac(self) -> typing.Optional[float]:
        """Get or set the Optional scale factor for the bolted joint sliding interface penalty stiffness.
        """ # nopep8
        return self._cards[0].get_value("slsfac")

    @slsfac.setter
    def slsfac(self, value: float) -> None:
        """Set the slsfac property."""
        self._cards[0].set_value("slsfac", value)

    @property
    def friction(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient. The default is no friction.
        """ # nopep8
        return self._cards[0].get_value("friction")

    @friction.setter
    def friction(self, value: float) -> None:
        """Set the friction property."""
        self._cards[0].set_value("friction", value)

    @property
    def vdc(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient as a fraction of the critical viscous damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        """Set the vdc property."""
        self._cards[0].set_value("vdc", value)

    @property
    def bsid(self) -> typing.Optional[int]:
        """Get or set the Beam set ID containing bolt beams.
        """ # nopep8
        return self._cards[1].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        """Set the bsid property."""
        self._cards[1].set_value("bsid", value)

    @property
    def hdang(self) -> float:
        """Get or set the Head countersunk angle in degrees.
        """ # nopep8
        return self._cards[1].get_value("hdang")

    @hdang.setter
    def hdang(self, value: float) -> None:
        """Set the hdang property."""
        self._cards[1].set_value("hdang", value)

    @property
    def ntang(self) -> float:
        """Get or set the Nut countersunk angle in degrees.
        """ # nopep8
        return self._cards[1].get_value("ntang")

    @ntang.setter
    def ntang(self, value: float) -> None:
        """Set the ntang property."""
        self._cards[1].set_value("ntang", value)

    @property
    def hssiam(self) -> typing.Optional[float]:
        """Get or set the Head outer diameter. The default is twice the diameter of the closest beam.
        """ # nopep8
        return self._cards[1].get_value("hssiam")

    @hssiam.setter
    def hssiam(self, value: float) -> None:
        """Set the hssiam property."""
        self._cards[1].set_value("hssiam", value)

    @property
    def ntdiam(self) -> typing.Optional[float]:
        """Get or set the Nut outer diameter. The default is twice the diameter of the closest beam.
        """ # nopep8
        return self._cards[1].get_value("ntdiam")

    @ntdiam.setter
    def ntdiam(self, value: float) -> None:
        """Set the ntdiam property."""
        self._cards[1].set_value("ntdiam", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing nodes on the hole edges for this bolted joint. Only those nodes in this set will be used to define the hole in this bolted joint. See Remark 1.
        """ # nopep8
        return self._cards[2].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[2].set_value("nsid", value)

    @property
    def hddept(self) -> float:
        """Get or set the Countersunk depth at head side of hole. Note that this will only affect the shell elements that are closest to the head. Solid elements closest to the head must include the countersunk geometry. See Remark 2
        """ # nopep8
        return self._cards[2].get_value("hddept")

    @hddept.setter
    def hddept(self, value: float) -> None:
        """Set the hddept property."""
        self._cards[2].set_value("hddept", value)

    @property
    def ntdept(self) -> float:
        """Get or set the Countersunk depth at nut side of hole. Note that this will only affect the shell elements that are closest to the nut. Solid elements closest to the nut must include the countersunk geometry. See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("ntdept")

    @ntdept.setter
    def ntdept(self, value: float) -> None:
        """Set the ntdept property."""
        self._cards[2].set_value("ntdept", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def bsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_BEAM_* keyword for bsid."""
        return self._get_set_link("BEAM", self.bsid)

    @bsid_link.setter
    def bsid_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsid."""
        self.bsid = value.sid

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

