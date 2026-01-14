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

"""Module providing the AleAmbientHydrostatic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALEAMBIENTHYDROSTATIC_CARD0 = (
    FieldSchema("alesid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("vecid", int, 20, 10, None),
    FieldSchema("grav", float, 30, 10, None),
    FieldSchema("pbase", float, 40, 10, 0.0),
    FieldSchema("ramptlc", int, 50, 10, 0),
)

_ALEAMBIENTHYDROSTATIC_CARD1 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("mmgbl", int, 10, 10, None),
)

class AleAmbientHydrostatic(KeywordBase):
    """DYNA ALE_AMBIENT_HYDROSTATIC keyword"""

    keyword = "ALE"
    subkeyword = "AMBIENT_HYDROSTATIC"

    def __init__(self, **kwargs):
        """Initialize the AleAmbientHydrostatic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEAMBIENTHYDROSTATIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEAMBIENTHYDROSTATIC_CARD1,
                **kwargs,
            ),        ]
    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the ALESID defines the reservoir-type. ALE domain/mesh whose hydrostatic pressure field due to gravity is being initialized by this keyword. See Remark 4.
        """ # nopep8
        return self._cards[0].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        """Set the alesid property."""
        self._cards[0].set_value("alesid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
        EQ.2:Solid set ID (SSID).
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("stype", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the A vector ID defining the direction of gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[0].set_value("vecid", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Magnitude of the gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[0].set_value("grav", value)

    @property
    def pbase(self) -> float:
        """Get or set the The “base” pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
        """ # nopep8
        return self._cards[0].get_value("pbase")

    @pbase.setter
    def pbase(self, value: float) -> None:
        """Set the pbase property."""
        self._cards[0].set_value("pbase", value)

    @property
    def ramptlc(self) -> int:
        """Get or set the This ID refers to a load curve (*DEFINE_CURVE) which defines how gravity is ramped up as a function of time.  Given the value of the gravitational acceleration, this curve, a time function, should typically vary from 0.0 to 1.0.
        """ # nopep8
        return self._cards[0].get_value("ramptlc")

    @ramptlc.setter
    def ramptlc(self, value: int) -> None:
        """Set the ramptlc property."""
        self._cards[0].set_value("ramptlc", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID defining the top location of a material/fluid layer.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def mmgbl(self) -> typing.Optional[int]:
        """Get or set the The ALE multi-material group ID (AMMG) of the fluid occupying the space below this corresponding node (NID).
        """ # nopep8
        return self._cards[1].get_value("mmgbl")

    @mmgbl.setter
    def mmgbl(self, value: int) -> None:
        """Set the mmgbl property."""
        self._cards[1].set_value("mmgbl", value)

