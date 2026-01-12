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

"""Module providing the DefineTracerParticles2D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINETRACERPARTICLES2D_CARD0 = (
    FieldSchema("nset", int, 0, 10, None),
    FieldSchema("pset", int, 10, 10, None),
)

_DEFINETRACERPARTICLES2D_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTracerParticles2D(KeywordBase):
    """DYNA DEFINE_TRACER_PARTICLES_2D keyword"""

    keyword = "DEFINE"
    subkeyword = "TRACER_PARTICLES_2D"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTracerParticles2D class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETRACERPARTICLES2D_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineTracerParticles2D.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINETRACERPARTICLES2D_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def nset(self) -> typing.Optional[int]:
        """Get or set the The node set ID for the nodes used as tracer particles.
        """ # nopep8
        return self._cards[0].get_value("nset")

    @nset.setter
    def nset(self, value: int) -> None:
        """Set the nset property."""
        self._cards[0].set_value("nset", value)

    @property
    def pset(self) -> typing.Optional[int]:
        """Get or set the Optional part set ID. If this part set is specified, only tracer
        particles in these parts are updated and the others are stationary.
        If this part set is not specified, all tracer particles are updated.
        """ # nopep8
        return self._cards[0].get_value("pset")

    @pset.setter
    def pset(self, value: int) -> None:
        """Set the pset property."""
        self._cards[0].set_value("pset", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

