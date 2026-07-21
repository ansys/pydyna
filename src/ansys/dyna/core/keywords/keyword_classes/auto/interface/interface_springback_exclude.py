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

"""Module providing the InterfaceSpringbackExclude class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INTERFACESPRINGBACKEXCLUDE_CARD0 = (
    FieldSchema("kwdname", str, 0, 80, None),
)

class InterfaceSpringbackExclude(KeywordBase):
    """DYNA INTERFACE_SPRINGBACK_EXCLUDE keyword"""

    keyword = "INTERFACE"
    subkeyword = "SPRINGBACK_EXCLUDE"

    def __init__(self, **kwargs):
        """Initialize the InterfaceSpringbackExclude class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACESPRINGBACKEXCLUDE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def kwdname(self) -> typing.Optional[str]:
        """Get or set the used to limit what data will be output to the LSDYNA dynain file, and consists of any number
        of keyword cards WITHOUT the leading *.  These cards and their associated data will not be output.
        The currently recognized keywords that can be excluded are:
        BOUNDARY_SLIDING_PLANE
        BOUNDARY_SPC_NODE
        CONSTRAINED_ADAPTIVITY
        DEFINE_COORDINATE_NODES
        DEFINE_COORDINATE_VECTOR
        ELEMENT_BEAM
        ELEMENT_SHELL
        ELEMENT_SOLID
        INITIAL_STRAIN_SHELL
        INITIAL_STRAIN_SOLID
        INITIAL_STRESS_BEAM
        INITIAL_STRESS_SHELL
        INITIAL_STRESS_SOLID
        INITIAL_TEMPERATURE_NODE
        INITIAL_VELOCITY_NODE
        NODE
        REFERENCE_GEOMETRY

        """ # nopep8
        return self._cards[0].get_value("kwdname")

    @kwdname.setter
    def kwdname(self, value: str) -> None:
        """Set the kwdname property."""
        self._cards[0].set_value("kwdname", value)

