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

"""Module providing the DefineAdaptiveSolidToSphId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEADAPTIVESOLIDTOSPHID_CARD0 = (
    FieldSchema("did", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_DEFINEADAPTIVESOLIDTOSPHID_CARD1 = (
    FieldSchema("ipid", int, 0, 10, None),
    FieldSchema("itype", int, 10, 10, 0),
    FieldSchema("nq", int, 20, 10, 1),
    FieldSchema("ipsph", int, 30, 10, None),
    FieldSchema("issph", int, 40, 10, None),
    FieldSchema("icpl", int, 50, 10, 0),
    FieldSchema("iopt", int, 60, 10, 0),
    FieldSchema("cpcd", float, 70, 10, None),
)

_DEFINEADAPTIVESOLIDTOSPHID_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineAdaptiveSolidToSphId(KeywordBase):
    """DYNA DEFINE_ADAPTIVE_SOLID_TO_SPH_ID keyword"""

    keyword = "DEFINE"
    subkeyword = "ADAPTIVE_SOLID_TO_SPH_ID"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineAdaptiveSolidToSphId class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEADAPTIVESOLIDTOSPHID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEADAPTIVESOLIDTOSPHID_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineAdaptiveSolidToSphId.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEADAPTIVESOLIDTOSPHID_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Definition ID. This must be a unique number..
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        """Set the did property."""
        self._cards[0].set_value("did", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Definition descriptor. It is suggested that unique descriptions be	used.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def ipid(self) -> typing.Optional[int]:
        """Get or set the Solid Element part or part Set ID.
        """ # nopep8
        return self._cards[1].get_value("ipid")

    @ipid.setter
    def ipid(self, value: int) -> None:
        """Set the ipid property."""
        self._cards[1].set_value("ipid", value)

    @property
    def itype(self) -> int:
        """Get or set the Set type of the IPID
        EQ. 0: Part ID
        NE. 0: Part set ID
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [0, 1, None]:
            raise Exception("""itype must be `None` or one of {0,1}.""")
        self._cards[1].set_value("itype", value)

    @property
    def nq(self) -> int:
        """Get or set the Adaptive option:
        EQ. 1: Adapt one solid element to one SPH element
        EQ. 2: Adapt one solid element to 8 SPH elements (Hexahedra only)
        EQ. 3: Adapt one solid element to 27 SPH elements (Hexahedra only)
        For the adaptive scheme for Tetrahedron and Prism elements,
        """ # nopep8
        return self._cards[1].get_value("nq")

    @nq.setter
    def nq(self, value: int) -> None:
        """Set the nq property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""nq must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("nq", value)

    @property
    def ipsph(self) -> typing.Optional[int]:
        """Get or set the Part ID for newly generated SPH elements
        """ # nopep8
        return self._cards[1].get_value("ipsph")

    @ipsph.setter
    def ipsph(self, value: int) -> None:
        """Set the ipsph property."""
        self._cards[1].set_value("ipsph", value)

    @property
    def issph(self) -> typing.Optional[int]:
        """Get or set the Section ID for SPH elements
        """ # nopep8
        return self._cards[1].get_value("issph")

    @issph.setter
    def issph(self, value: int) -> None:
        """Set the issph property."""
        self._cards[1].set_value("issph", value)

    @property
    def icpl(self) -> int:
        """Get or set the Coupling of newly generated SPH elements to the adjacent solid elements:
        EQ.0:	Failure without coupling (debris simulation),
        EQ.1:	Coupled to solid element,
        EQ.3:	Provide only thermal coupling between SPH part and solid part (must be combined with IOPT = 0 option; see Remark 4).
        """ # nopep8
        return self._cards[1].get_value("icpl")

    @icpl.setter
    def icpl(self, value: int) -> None:
        """Set the icpl property."""
        if value not in [0, 1, 3, None]:
            raise Exception("""icpl must be `None` or one of {0,1,3}.""")
        self._cards[1].set_value("icpl", value)

    @property
    def iopt(self) -> int:
        """Get or set the Coupling method.
        EQ. 0: Coupling from beginning (used as constraint between SPH elements and Solid elements)
        EQ. 1: Coupling begins when Lagrange element fails
        """ # nopep8
        return self._cards[1].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        """Set the iopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iopt must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("iopt", value)

    @property
    def cpcd(self) -> typing.Optional[float]:
        """Get or set the Thermal coupling conductivity between SPH part and solid part for ICPL = 3 option.
        The default value is set as the average value of the conductivity from SPH part and the conductivity from solid part
        """ # nopep8
        return self._cards[1].get_value("cpcd")

    @cpcd.setter
    def cpcd(self, value: float) -> None:
        """Set the cpcd property."""
        self._cards[1].set_value("cpcd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

