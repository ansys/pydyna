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

"""Module providing the DefineSphToSphCouplingId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINESPHTOSPHCOUPLINGID_CARD0 = (
    FieldSchema("sida", int, 0, 10, None),
    FieldSchema("sidb", int, 10, 10, None),
    FieldSchema("satyp", int, 20, 10, 0),
    FieldSchema("sbtyp", int, 30, 10, 0),
    FieldSchema("iboxa", int, 40, 10, None),
    FieldSchema("iboxb", int, 50, 10, None),
    FieldSchema("pfact", float, 60, 10, 1.0),
    FieldSchema("srad", float, 70, 10, 1.0),
)

_DEFINESPHTOSPHCOUPLINGID_CARD1 = (
    FieldSchema("dfact", float, 0, 10, 0.0),
    FieldSchema("isoft", int, 10, 10, 0),
)

_DEFINESPHTOSPHCOUPLINGID_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphToSphCouplingId(KeywordBase):
    """DYNA DEFINE_SPH_TO_SPH_COUPLING_ID keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_TO_SPH_COUPLING_ID"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineSphToSphCouplingId class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHTOSPHCOUPLINGID_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPHTOSPHCOUPLINGID_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSphToSphCouplingId._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHTOSPHCOUPLINGID_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sida(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID for one set of particles in the contact.
        """ # nopep8
        return self._cards[0].get_value("sida")

    @sida.setter
    def sida(self, value: int) -> None:
        """Set the sida property."""
        self._cards[0].set_value("sida", value)

    @property
    def sidb(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID for the other set of particles in the contact
        """ # nopep8
        return self._cards[0].get_value("sidb")

    @sidb.setter
    def sidb(self, value: int) -> None:
        """Set the sidb property."""
        self._cards[0].set_value("sidb", value)

    @property
    def satyp(self) -> int:
        """Get or set the SIDA part type:
        EQ. 0: Part set ID,
        EQ. 1: Part ID
        ,
        """ # nopep8
        return self._cards[0].get_value("satyp")

    @satyp.setter
    def satyp(self, value: int) -> None:
        """Set the satyp property."""
        if value not in [0, 1, None]:
            raise Exception("""satyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("satyp", value)

    @property
    def sbtyp(self) -> int:
        """Get or set the SIDB part type:
        EQ. 0: Part set ID,
        EQ. 1: Part ID

        """ # nopep8
        return self._cards[0].get_value("sbtyp")

    @sbtyp.setter
    def sbtyp(self, value: int) -> None:
        """Set the sbtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sbtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sbtyp", value)

    @property
    def iboxa(self) -> typing.Optional[int]:
        """Get or set the Box ID for the A parts
        """ # nopep8
        return self._cards[0].get_value("iboxa")

    @iboxa.setter
    def iboxa(self, value: int) -> None:
        """Set the iboxa property."""
        self._cards[0].set_value("iboxa", value)

    @property
    def iboxb(self) -> typing.Optional[int]:
        """Get or set the Box ID for the B parts
        """ # nopep8
        return self._cards[0].get_value("iboxb")

    @iboxb.setter
    def iboxb(self, value: int) -> None:
        """Set the iboxb property."""
        self._cards[0].set_value("iboxb", value)

    @property
    def pfact(self) -> float:
        """Get or set the Penalty scale factor
        """ # nopep8
        return self._cards[0].get_value("pfact")

    @pfact.setter
    def pfact(self, value: float) -> None:
        """Set the pfact property."""
        self._cards[0].set_value("pfact", value)

    @property
    def srad(self) -> float:
        """Get or set the Scale factor for nodes to nodes contact criteria, See Remark 3
        """ # nopep8
        return self._cards[0].get_value("srad")

    @srad.setter
    def srad(self, value: float) -> None:
        """Set the srad property."""
        self._cards[0].set_value("srad", value)

    @property
    def dfact(self) -> float:
        """Get or set the Penalty scale factor for contact damping coefficient, See Remark 4.
        """ # nopep8
        return self._cards[1].get_value("dfact")

    @dfact.setter
    def dfact(self, value: float) -> None:
        """Set the dfact property."""
        self._cards[1].set_value("dfact", value)

    @property
    def isoft(self) -> int:
        """Get or set the Soft constraint option:
        EQ. 0: penalty formulation
        EQ. 1: soft constraint formulation
        The soft constraint may be necessary if the material constants of the parts in contact have a wide variation in the elastic bulk moduli. In the soft constraint option, the interface stiffness is based on the nodal mass and the global time step size.
        """ # nopep8
        return self._cards[1].get_value("isoft")

    @isoft.setter
    def isoft(self, value: int) -> None:
        """Set the isoft property."""
        if value not in [0, 1, None]:
            raise Exception("""isoft must be `None` or one of {0,1}.""")
        self._cards[1].set_value("isoft", value)

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

