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

"""Module providing the EmEpCreatefiberorientation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMEPCREATEFIBERORIENTATION_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("ldid1", int, 10, 10, None),
    FieldSchema("ldid2", int, 20, 10, None),
    FieldSchema("alpha", float, 30, 10, None),
    FieldSchema("beta", float, 40, 10, None),
    FieldSchema("iexpor", int, 50, 10, None),
    FieldSchema("ipreru", int, 60, 10, None),
)

class EmEpCreatefiberorientation(KeywordBase):
    """DYNA EM_EP_CREATEFIBERORIENTATION keyword"""

    keyword = "EM"
    subkeyword = "EP_CREATEFIBERORIENTATION"
    _link_fields = {
        "psid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpCreatefiberorientation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCREATEFIBERORIENTATION_CARD0,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the part set on which the system is solved
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def ldid1(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system that is solved in the transmural direction
        """ # nopep8
        return self._cards[0].get_value("ldid1")

    @ldid1.setter
    def ldid1(self, value: int) -> None:
        """Set the ldid1 property."""
        self._cards[0].set_value("ldid1", value)

    @property
    def ldid2(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system that is solved in the apicobasal direction
        """ # nopep8
        return self._cards[0].get_value("ldid2")

    @ldid2.setter
    def ldid2(self, value: int) -> None:
        """Set the ldid2 property."""
        self._cards[0].set_value("ldid2", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Helical angle with respect to the counterclockwise circumferential direction in the heart when looking from the base towards the apex.
        LT.0: | ALPHA | is the ID for the *DEFINE_FUNCTION giving the helical angle.See Remark 1 for available arguments.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Angle with respect to the outward transmural axis of the heart.
        LT.0: | BETA | is the ID for the *DEFINE_FUNCTION giving the angle.See Remark 1 for available arguments.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def iexpor(self) -> typing.Optional[int]:
        """Get or set the Selects whether result files (ELEMENT_SOLID_ORTHO.k and vtk files) are exported:
        EQ.0: Not exported
        EQ.1: Exported
        """ # nopep8
        return self._cards[0].get_value("iexpor")

    @iexpor.setter
    def iexpor(self, value: int) -> None:
        """Set the iexpor property."""
        self._cards[0].set_value("iexpor", value)

    @property
    def ipreru(self) -> typing.Optional[int]:
        """Get or set the Select whether the run is stopped after creating fibers:
        EQ.0: Do not stop after fiber creation
        EQ.1: Stop after fiber creation
        """ # nopep8
        return self._cards[0].get_value("ipreru")

    @ipreru.setter
    def ipreru(self, value: int) -> None:
        """Set the ipreru property."""
        self._cards[0].set_value("ipreru", value)

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given psid."""
        return self._get_link_by_attr("PART", "pid", self.psid, "parts")

