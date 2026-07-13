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

"""Module providing the DefineTwinbuilderCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINETWINBUILDERCOUPLING_CARD0 = (
    FieldSchema("twinpath", str, 0, 80, None),
)

_DEFINETWINBUILDERCOUPLING_CARD1 = (
    FieldSchema("romname", str, 0, 80, None),
)

_DEFINETWINBUILDERCOUPLING_CARD2 = (
    FieldSchema("logname", str, 0, 80, None),
)

_DEFINETWINBUILDERCOUPLING_CARD3 = (
    FieldSchema("node", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, 0),
    FieldSchema("iopt", int, 20, 10, 0),
    FieldSchema("cid", int, 30, 10, None),
    FieldSchema("frcfrq", int, 40, 10, None),
    FieldSchema("fswitch", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("debug", int, 70, 10, 0),
)

_DEFINETWINBUILDERCOUPLING_CARD4 = (
    FieldSchema("fnopt", int, 0, 10, 0),
    FieldSchema("alpha", int, 10, 10, None),
)

_DEFINETWINBUILDERCOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTwinbuilderCoupling(KeywordBase):
    """DYNA DEFINE_TWINBUILDER_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "TWINBUILDER_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineTwinbuilderCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETWINBUILDERCOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETWINBUILDERCOUPLING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETWINBUILDERCOUPLING_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETWINBUILDERCOUPLING_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINETWINBUILDERCOUPLING_CARD4,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineTwinbuilderCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINETWINBUILDERCOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def twinpath(self) -> typing.Optional[str]:
        """Get or set the Full path, including the filename, to the Twin Builder .twin file.
        """ # nopep8
        return self._cards[0].get_value("twinpath")

    @twinpath.setter
    def twinpath(self, value: str) -> None:
        """Set the twinpath property."""
        self._cards[0].set_value("twinpath", value)

    @property
    def romname(self) -> typing.Optional[str]:
        """Get or set the Twin Builder ROM name within the .twin file.
        """ # nopep8
        return self._cards[1].get_value("romname")

    @romname.setter
    def romname(self, value: str) -> None:
        """Set the romname property."""
        self._cards[1].set_value("romname", value)

    @property
    def logname(self) -> typing.Optional[str]:
        """Get or set the Twin Builder log file name. This log file is generated in the current working directory.
        """ # nopep8
        return self._cards[2].get_value("logname")

    @logname.setter
    def logname(self, value: str) -> None:
        """Set the logname property."""
        self._cards[2].set_value("logname", value)

    @property
    def node(self) -> typing.Optional[int]:
        """Get or set the Coupled node/node set. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        """Set the node property."""
        self._cards[3].set_value("node", value)

    @property
    def type(self) -> int:
        """Get or set the Type for NODE:
        EQ.0:	Node ID
        EQ.1 : Node set ID
        """ # nopep8
        return self._cards[3].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, None]:
            raise Exception("""type must be `None` or one of {0,1}.""")
        self._cards[3].set_value("type", value)

    @property
    def iopt(self) -> int:
        """Get or set the Option for exchanging data:
        EQ.0:	Output nodal translational and rotational coordinates and receive nodal translational and rotational forces from Twin Builder(default).
        EQ.1 : Output nodal translational and rotational displacements and receive nodal translational and rotational forces from Twin Builder.
        EQ.2 : Output nodal translational coordinates and receive nodal translational forces from Twin Builder.
        EQ.3 : Output nodal translational displacements and receive nodal translational forces from Twin Builder.
        """ # nopep8
        return self._cards[3].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        """Set the iopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iopt must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("iopt", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Reference coordinate system needed to transform the data from the global system to the Twin Builder local system. If zero or blank, the global coordinate system is used.
        """ # nopep8
        return self._cards[3].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[3].set_value("cid", value)

    @property
    def frcfrq(self) -> typing.Optional[int]:
        """Get or set the Number of cycles between Twin Builder force updates for the coupling interface
        """ # nopep8
        return self._cards[3].get_value("frcfrq")

    @frcfrq.setter
    def frcfrq(self, value: int) -> None:
        """Set the frcfrq property."""
        self._cards[3].set_value("frcfrq", value)

    @property
    def fswitch(self) -> int:
        """Get or set the Flag to flip the sign of the Twin Builder predicted forces if needed for consistency with ROM training:
        EQ.0:	Add ROM - predicted force to the internal force of the node / nodes(default).
        EQ.1 : Add ROM - predicted force to the internal force in the negative direction for the node / nodes.
        """ # nopep8
        return self._cards[3].get_value("fswitch")

    @fswitch.setter
    def fswitch(self, value: int) -> None:
        """Set the fswitch property."""
        if value not in [0, 1, None]:
            raise Exception("""fswitch must be `None` or one of {0,1}.""")
        self._cards[3].set_value("fswitch", value)

    @property
    def debug(self) -> int:
        """Get or set the Debug level for identifying issues in the ROM or coupling:
        EQ.0:	No debug output(default).
        EQ.1 : Debug level I.This level outputs tbModalInputs.csv, tbModalOutputs.csv, tbdisp.bin, and tb.forc.bin.It also prints extra data to stdout.
        """ # nopep8
        return self._cards[3].get_value("debug")

    @debug.setter
    def debug(self, value: int) -> None:
        """Set the debug property."""
        if value not in [0, 1, None]:
            raise Exception("""debug must be `None` or one of {0,1}.""")
        self._cards[3].set_value("debug", value)

    @property
    def fnopt(self) -> int:
        """Get or set the Option for function smoothing. The predicted force is applied at the interface every cycle according to the following (see Remark 3):
        EQ.0:	No smoothing(default)
        EQ.1 : Power law
        EQ.2 : C1 transition
        """ # nopep8
        return self._cards[4].get_value("fnopt")

    @fnopt.setter
    def fnopt(self, value: int) -> None:
        """Set the fnopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""fnopt must be `None` or one of {0,1,2}.""")
        self._cards[4].set_value("fnopt", value)

    @property
    def alpha(self) -> typing.Optional[int]:
        """Get or set the Parameter for smoothing function. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("alpha")

    @alpha.setter
    def alpha(self, value: int) -> None:
        """Set the alpha property."""
        self._cards[4].set_value("alpha", value)

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
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

