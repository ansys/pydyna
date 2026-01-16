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

"""Module providing the ControlReferenceControlVolumeCoordinates class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLREFERENCECONTROLVOLUMECOORDINATES_CARD0 = (
    FieldSchema("filename", str, 0, 256, None),
)

_CONTROLREFERENCECONTROLVOLUMECOORDINATES_CARD1 = (
    FieldSchema("opt", int, 0, 10, 0),
    FieldSchema("psid", int, 10, 10, None),
)

class ControlReferenceControlVolumeCoordinates(KeywordBase):
    """DYNA CONTROL_REFERENCE_CONTROL_VOLUME_COORDINATES keyword"""

    keyword = "CONTROL"
    subkeyword = "REFERENCE_CONTROL_VOLUME_COORDINATES"
    _link_fields = {
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlReferenceControlVolumeCoordinates class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLREFERENCECONTROLVOLUMECOORDINATES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLREFERENCECONTROLVOLUMECOORDINATES_CARD1,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the The file name contains the *INITIAL_FOAM_REFERENCE_GEOMETRY, *INITIAL_STRESS_SOLID, and *INITIAL_STRESS_SHELL to be read or written.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def opt(self) -> int:
        """Get or set the OPT = 0 writes the reference geometry to the specified file.
        OPT = 1 reads the reference geometry from the specified file.
        OPT = 2 skips the pressure initialization.
        """ # nopep8
        return self._cards[1].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        """Set the opt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""opt must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("opt", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the the part set defining the elements to be read or written.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

