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

"""Module providing the IcfdControlAdaptSize class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLADAPTSIZE_CARD0 = (
    FieldSchema("asize", int, 0, 10, 0),
    FieldSchema("nit", int, 10, 10, None),
    FieldSchema("kis", int, 20, 10, 0),
)

_ICFDCONTROLADAPTSIZE_CARD1 = (
    FieldSchema("drasize", int, 0, 10, 0),
    FieldSchema("drnit", int, 10, 10, None),
    FieldSchema("drkis", int, 20, 10, 0),
)

class IcfdControlAdaptSize(KeywordBase):
    """DYNA ICFD_CONTROL_ADAPT_SIZE keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_ADAPT_SIZE"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlAdaptSize class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLADAPTSIZE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLADAPTSIZE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def asize(self) -> int:
        """Get or set the EQ. 0:only re-mesh in cases where elements invert..
        EQ. 1:re-mesh if elements invert or if element quality deteriorates.
        .
        """ # nopep8
        return self._cards[0].get_value("asize")

    @asize.setter
    def asize(self, value: int) -> None:
        """Set the asize property."""
        if value not in [0, 1, None]:
            raise Exception("""asize must be `None` or one of {0,1}.""")
        self._cards[0].set_value("asize", value)

    @property
    def nit(self) -> typing.Optional[int]:
        """Get or set the Number of iterations before a re-meshing is forced.
        """ # nopep8
        return self._cards[0].get_value("nit")

    @nit.setter
    def nit(self, value: int) -> None:
        """Set the nit property."""
        self._cards[0].set_value("nit", value)

    @property
    def kis(self) -> int:
        """Get or set the Keep initial mesh size:
        EQ.0: Turned off.The remeshing process will ignore the initial mesh size in the volume.
        EQ.1: Turned on.Whenever a remeshing occurs, the new local mesh size will not be allowed to be substantially coarser than the one from the previous mesh.The object is to diminish the excessive coarsening that can occur between two remeshes.
        """ # nopep8
        return self._cards[0].get_value("kis")

    @kis.setter
    def kis(self, value: int) -> None:
        """Set the kis property."""
        if value not in [0, 1, None]:
            raise Exception("""kis must be `None` or one of {0,1}.""")
        self._cards[0].set_value("kis", value)

    @property
    def drasize(self) -> int:
        """Get or set the Same as ASIZE but for dynamic relaxation
        """ # nopep8
        return self._cards[1].get_value("drasize")

    @drasize.setter
    def drasize(self, value: int) -> None:
        """Set the drasize property."""
        if value not in [0, 1, None]:
            raise Exception("""drasize must be `None` or one of {0,1}.""")
        self._cards[1].set_value("drasize", value)

    @property
    def drnit(self) -> typing.Optional[int]:
        """Get or set the Same as NIT but for dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("drnit")

    @drnit.setter
    def drnit(self, value: int) -> None:
        """Set the drnit property."""
        self._cards[1].set_value("drnit", value)

    @property
    def drkis(self) -> int:
        """Get or set the Same as KIS but for dynamic relaxation
        """ # nopep8
        return self._cards[1].get_value("drkis")

    @drkis.setter
    def drkis(self, value: int) -> None:
        """Set the drkis property."""
        if value not in [0, 1, None]:
            raise Exception("""drkis must be `None` or one of {0,1}.""")
        self._cards[1].set_value("drkis", value)

