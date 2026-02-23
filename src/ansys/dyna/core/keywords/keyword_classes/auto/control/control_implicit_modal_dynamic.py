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

"""Module providing the ControlImplicitModalDynamic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLIMPLICITMODALDYNAMIC_CARD0 = (
    FieldSchema("mdflag", int, 0, 10, 0),
    FieldSchema("zeta", float, 10, 10, None),
    FieldSchema("md_strs", int, 20, 10, None),
    FieldSchema("dtout", float, 30, 10, None),
    FieldSchema("integ", int, 40, 10, 0),
    FieldSchema("nsid", int, 50, 10, None),
)

_CONTROLIMPLICITMODALDYNAMIC_CARD1 = (
    FieldSchema("filename", str, 0, 80, None),
)

_CONTROLIMPLICITMODALDYNAMIC_CARD2 = (
    FieldSchema("filename2", str, 0, 80, None),
)

class ControlImplicitModalDynamic(KeywordBase):
    """DYNA CONTROL_IMPLICIT_MODAL_DYNAMIC keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_MODAL_DYNAMIC"
    _link_fields = {
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitModalDynamic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITMODALDYNAMIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITMODALDYNAMIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITMODALDYNAMIC_CARD2,
                **kwargs,
            ),        ]
    @property
    def mdflag(self) -> int:
        """Get or set the Modal Dynamic flag
        EQ.0: no modal dynamic analysis
        EQ.1: perform modal dynamic analysis.
        EQ.2:	perform modal dynamic analysis with prescribed motion constraints on the constraint modes input with Card 3.  See Remark 7
        """ # nopep8
        return self._cards[0].get_value("mdflag")

    @mdflag.setter
    def mdflag(self, value: int) -> None:
        """Set the mdflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""mdflag must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("mdflag", value)

    @property
    def zeta(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping constant.
        """ # nopep8
        return self._cards[0].get_value("zeta")

    @zeta.setter
    def zeta(self, value: float) -> None:
        """Set the zeta property."""
        self._cards[0].set_value("zeta", value)

    @property
    def md_strs(self) -> typing.Optional[int]:
        """Get or set the Calculate modal dynamic stress.
        """ # nopep8
        return self._cards[0].get_value("md_strs")

    @md_strs.setter
    def md_strs(self, value: int) -> None:
        """Set the md_strs property."""
        self._cards[0].set_value("md_strs", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Modal dynamics output interval.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        """Set the dtout property."""
        self._cards[0].set_value("dtout", value)

    @property
    def integ(self) -> int:
        """Get or set the Integration method
        EQ.0:	defaults to 1.
        EQ.1:	perform modal dynamic analysis with explicit time integration.
        EQ.2:	perform modal dynamic analysis with implicit time integration..
        """ # nopep8
        return self._cards[0].get_value("integ")

    @integ.setter
    def integ(self, value: int) -> None:
        """Set the integ property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""integ must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("integ", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the The node set ID of the nodes in the modal model that are subjected to loads. If the set is not specified, then the forces are summed over all the nodes, and that is usually much more expensive than summing over only those subjected to a load..
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the If specified the eigenmodes are read from the specified file. Otherwise
        the eigenmodes are computed as specified on *CONTROL_IMPLICIT_	EIGENVALUE.
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

    @property
    def filename2(self) -> typing.Optional[str]:
        """Get or set the If specified the eigenmodes are read from the specified file. Otherwise
        the eigenmodes are computed as specified on *CONTROL_IMPLICIT_	EIGENVALUE.
        """ # nopep8
        return self._cards[2].get_value("filename2")

    @filename2.setter
    def filename2(self, value: str) -> None:
        """Set the filename2 property."""
        self._cards[2].set_value("filename2", value)

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

