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
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("msid", int, 10, 10, None),
    FieldSchema("sstyp", int, 20, 10, 0),
    FieldSchema("mstyp", int, 30, 10, 0),
    FieldSchema("ibox1", int, 40, 10, None),
    FieldSchema("ibox2", int, 50, 10, None),
    FieldSchema("pfact", float, 60, 10, 1.0),
    FieldSchema("srad", float, 70, 10, 1.0),
)

_DEFINESPHTOSPHCOUPLINGID_CARD1 = (
    FieldSchema("dfact", float, 0, 10, 0.0),
    FieldSchema("isoft", int, 10, 10, 0),
)

class DefineSphToSphCouplingId(KeywordBase):
    """DYNA DEFINE_SPH_TO_SPH_COUPLING_ID keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_TO_SPH_COUPLING_ID"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineSphToSphCouplingId class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHTOSPHCOUPLINGID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPHTOSPHCOUPLINGID_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSphToSphCouplingId.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Slave part or part set ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def msid(self) -> typing.Optional[int]:
        """Get or set the Master part or part set ID
        """ # nopep8
        return self._cards[0].get_value("msid")

    @msid.setter
    def msid(self, value: int) -> None:
        """Set the msid property."""
        self._cards[0].set_value("msid", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Slave part type:
        EQ. 0:	Part set ID,
        EQ. 1:	Part ID
        ,
        """ # nopep8
        return self._cards[0].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        """Set the sstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sstyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sstyp", value)

    @property
    def mstyp(self) -> int:
        """Get or set the Master part type:
        EQ. 0:	Part set ID,
        EQ. 1:	Part ID

        """ # nopep8
        return self._cards[0].get_value("mstyp")

    @mstyp.setter
    def mstyp(self, value: int) -> None:
        """Set the mstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""mstyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mstyp", value)

    @property
    def ibox1(self) -> typing.Optional[int]:
        """Get or set the Box ID for slave parts
        """ # nopep8
        return self._cards[0].get_value("ibox1")

    @ibox1.setter
    def ibox1(self, value: int) -> None:
        """Set the ibox1 property."""
        self._cards[0].set_value("ibox1", value)

    @property
    def ibox2(self) -> typing.Optional[int]:
        """Get or set the Box ID for master parts
        """ # nopep8
        return self._cards[0].get_value("ibox2")

    @ibox2.setter
    def ibox2(self, value: int) -> None:
        """Set the ibox2 property."""
        self._cards[0].set_value("ibox2", value)

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

