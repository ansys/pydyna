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
    FieldSchema("parstld", int, 0, 10, None),
    FieldSchema("solvelde", int, 10, 10, None),
    FieldSchema("solvelde", int, 20, 10, None),
    FieldSchema("alpha", int, 30, 10, None),
    FieldSchema("beta", int, 40, 10, None),
    FieldSchema("w_file", int, 50, 10, None),
    FieldSchema("prerun", int, 60, 10, None),
)

class EmEpCreatefiberorientation(KeywordBase):
    """DYNA EM_EP_CREATEFIBERORIENTATION keyword"""

    keyword = "EM"
    subkeyword = "EP_CREATEFIBERORIENTATION"
    _link_fields = {
        "parstld": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpCreatefiberorientation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCREATEFIBERORIENTATION_CARD0,
                **kwargs,
            ),        ]
    @property
    def parstld(self) -> typing.Optional[int]:
        """Get or set the Part set on which the system is solved
        """ # nopep8
        return self._cards[0].get_value("parstld")

    @parstld.setter
    def parstld(self, value: int) -> None:
        """Set the parstld property."""
        self._cards[0].set_value("parstld", value)

    @property
    def solvelde(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system that is solved in the transmural direction
        """ # nopep8
        return self._cards[0].get_value("solvelde")

    @solvelde.setter
    def solvelde(self, value: int) -> None:
        """Set the solvelde property."""
        self._cards[0].set_value("solvelde", value)

    @property
    def solvelde(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system that is solved in the apicobasal direction
        """ # nopep8
        return self._cards[0].get_value("solvelde")

    @solvelde.setter
    def solvelde(self, value: int) -> None:
        """Set the solvelde property."""
        self._cards[0].set_value("solvelde", value)

    @property
    def alpha(self) -> typing.Optional[int]:
        """Get or set the helical angle with respect to the counterclockwise circumferential direction in the heart when looking from the base towards the apex. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: int) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[int]:
        """Get or set the angle with respect to the outward transmural axis of the heart. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: int) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def w_file(self) -> typing.Optional[int]:
        """Get or set the Selects whether result files (ELEMENT_‌SOLID_‌ORTHO.k and vtk files) are exported:
        EQ.0:	not exported
        EQ.1 : exported
        """ # nopep8
        return self._cards[0].get_value("w_file")

    @w_file.setter
    def w_file(self, value: int) -> None:
        """Set the w_file property."""
        self._cards[0].set_value("w_file", value)

    @property
    def prerun(self) -> typing.Optional[int]:
        """Get or set the Select whether the run is stopped after creating fibers:
        EQ.0:	do not stop after fiber creation
        EQ.1 : stop after fiber creation
        """ # nopep8
        return self._cards[0].get_value("prerun")

    @prerun.setter
    def prerun(self, value: int) -> None:
        """Set the prerun property."""
        self._cards[0].set_value("prerun", value)

    @property
    def parstld_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given parstld."""
        return self._get_link_by_attr("PART", "pid", self.parstld, "parts")

