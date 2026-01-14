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

"""Module providing the DefineQuasarCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINEQUASARCOUPLING_CARD0 = (
    FieldSchema("node", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 0),
    FieldSchema("romid", int, 20, 10, None),
    FieldSchema("pid", int, 30, 10, None),
    FieldSchema("ptype", int, 40, 10, 0),
    FieldSchema("iopt", int, 50, 10, 0),
    FieldSchema("cid", int, 60, 10, None),
    FieldSchema("ex_id", int, 70, 10, None),
)

_DEFINEQUASARCOUPLING_CARD1 = (
    FieldSchema("frcfrq", int, 0, 10, None),
)

_DEFINEQUASARCOUPLING_CARD2 = (
    FieldSchema("encname1", str, 0, 80, None),
)

_DEFINEQUASARCOUPLING_CARD3 = (
    FieldSchema("encname2", str, 0, 80, None),
)

_DEFINEQUASARCOUPLING_CARD4 = (
    FieldSchema("var1", float, 0, 10, None),
    FieldSchema("var2", float, 10, 10, None),
    FieldSchema("var3", float, 20, 10, None),
    FieldSchema("var4", float, 30, 10, None),
    FieldSchema("var5", float, 40, 10, None),
    FieldSchema("var6", float, 50, 10, None),
    FieldSchema("var7", float, 60, 10, None),
    FieldSchema("var8", float, 70, 10, None),
)

_DEFINEQUASARCOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineQuasarCoupling(KeywordBase):
    """DYNA DEFINE_QUASAR_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "QUASAR_COUPLING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineQuasarCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEQUASARCOUPLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEQUASARCOUPLING_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEQUASARCOUPLING_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEQUASARCOUPLING_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEQUASARCOUPLING_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineQuasarCoupling.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEQUASARCOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def node(self) -> typing.Optional[int]:
        """Get or set the Coupled node/node set
        """ # nopep8
        return self._cards[0].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        """Set the node property."""
        self._cards[0].set_value("node", value)

    @property
    def type(self) -> int:
        """Get or set the Region type:
        EQ.0:	node ID
        EQ.1 : node set ID
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, None]:
            raise Exception("""type must be `None` or one of {0,1}.""")
        self._cards[0].set_value("type", value)

    @property
    def romid(self) -> typing.Optional[int]:
        """Get or set the Cadlm’s ROM ID
        """ # nopep8
        return self._cards[0].get_value("romid")

    @romid.setter
    def romid(self, value: int) -> None:
        """Set the romid property."""
        self._cards[0].set_value("romid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the LS-DYNA Part/Part set ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ptype(self) -> int:
        """Get or set the Type for PID:
        EQ.0:	part ID(Default)
        EQ.1 : part set ID
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        if value not in [0, 1, None]:
            raise Exception("""ptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ptype", value)

    @property
    def iopt(self) -> int:
        """Get or set the Option for exchanging data between LS-DYNA/Cadlm Quasar
        EQ.0:	Default.LS - DYNA output nodal translational and rotational coordinates and input nodal translational and rotational forces
        EQ.1 : LS - DYNA output nodal translational and rotational displacements and input nodal translational and rotational forces
        EQ.2 : LS - DYNA output nodal translational coordinates and input nodal translational forces
        EQ.3 : LS - DYNA output nodal translational displacements and input nodal translational forces
        """ # nopep8
        return self._cards[0].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        """Set the iopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iopt must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("iopt", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Reference coordinate system for transform data from LS-DYNA global system to Quasar local system
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def ex_id(self) -> typing.Optional[int]:
        """Get or set the Node set to exclude from Quasar output.  LS-DYNA still expects the complete set of data. (Quasar can predict the forces from a reduced data set.)
        """ # nopep8
        return self._cards[0].get_value("ex_id")

    @ex_id.setter
    def ex_id(self, value: int) -> None:
        """Set the ex_id property."""
        self._cards[0].set_value("ex_id", value)

    @property
    def frcfrq(self) -> typing.Optional[int]:
        """Get or set the Number of cycles between QUASAR force updates for the coupling interface
        """ # nopep8
        return self._cards[1].get_value("frcfrq")

    @frcfrq.setter
    def frcfrq(self, value: int) -> None:
        """Set the frcfrq property."""
        self._cards[1].set_value("frcfrq", value)

    @property
    def encname1(self) -> typing.Optional[str]:
        """Get or set the LS-DYNA output file to QUASAR
        """ # nopep8
        return self._cards[2].get_value("encname1")

    @encname1.setter
    def encname1(self, value: str) -> None:
        """Set the encname1 property."""
        self._cards[2].set_value("encname1", value)

    @property
    def encname2(self) -> typing.Optional[str]:
        """Get or set the QUASAR output file to LS-DYNA
        """ # nopep8
        return self._cards[3].get_value("encname2")

    @encname2.setter
    def encname2(self, value: str) -> None:
        """Set the encname2 property."""
        self._cards[3].set_value("encname2", value)

    @property
    def var1(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var1")

    @var1.setter
    def var1(self, value: float) -> None:
        """Set the var1 property."""
        self._cards[4].set_value("var1", value)

    @property
    def var2(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var2")

    @var2.setter
    def var2(self, value: float) -> None:
        """Set the var2 property."""
        self._cards[4].set_value("var2", value)

    @property
    def var3(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var3")

    @var3.setter
    def var3(self, value: float) -> None:
        """Set the var3 property."""
        self._cards[4].set_value("var3", value)

    @property
    def var4(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var4")

    @var4.setter
    def var4(self, value: float) -> None:
        """Set the var4 property."""
        self._cards[4].set_value("var4", value)

    @property
    def var5(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var5")

    @var5.setter
    def var5(self, value: float) -> None:
        """Set the var5 property."""
        self._cards[4].set_value("var5", value)

    @property
    def var6(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var6")

    @var6.setter
    def var6(self, value: float) -> None:
        """Set the var6 property."""
        self._cards[4].set_value("var6", value)

    @property
    def var7(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var7")

    @var7.setter
    def var7(self, value: float) -> None:
        """Set the var7 property."""
        self._cards[4].set_value("var7", value)

    @property
    def var8(self) -> typing.Optional[float]:
        """Get or set the User defined constant
        """ # nopep8
        return self._cards[4].get_value("var8")

    @var8.setter
    def var8(self, value: float) -> None:
        """Set the var8 property."""
        self._cards[4].set_value("var8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def cid_link(self) -> DefineCurve:
        """Get the DefineCurve object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for cid."""
        self.cid = value.lcid

