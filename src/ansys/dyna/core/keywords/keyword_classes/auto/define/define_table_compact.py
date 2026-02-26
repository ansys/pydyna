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

"""Module providing the DefineTableCompact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINETABLECOMPACT_CARD0 = (
    FieldSchema("tbid", int, 0, 10, None),
    FieldSchema("nvar", int, 10, 10, None),
    FieldSchema("lcint", int, 20, 10, 0),
    FieldSchema("mathis", int, 30, 10, 0),
    FieldSchema("inexect", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DEFINETABLECOMPACT_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("his1", int, 10, 10, None),
    FieldSchema("his2", int, 20, 10, None),
    FieldSchema("his3", int, 30, 10, None),
    FieldSchema("his4", int, 40, 10, None),
    FieldSchema("his5", int, 50, 10, None),
    FieldSchema("his6", int, 60, 10, None),
    FieldSchema("his7", int, 70, 10, None),
)

_DEFINETABLECOMPACT_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("his8", int, 10, 10, None),
    FieldSchema("his9", int, 20, 10, None),
)

_DEFINETABLECOMPACT_CARD3 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ixe1", int, 10, 10, None),
    FieldSchema("ixe2", int, 20, 10, None),
    FieldSchema("ixe3", int, 30, 10, None),
    FieldSchema("ixe4", int, 40, 10, None),
    FieldSchema("ixe5", int, 50, 10, None),
    FieldSchema("ixe6", int, 60, 10, None),
    FieldSchema("ixe7", int, 70, 10, None),
)

_DEFINETABLECOMPACT_CARD4 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ixe8", int, 10, 10, None),
    FieldSchema("ixe9", int, 20, 10, None),
)

_DEFINETABLECOMPACT_CARD5 = (
    FieldSchema("o1", float, 0, 10, None),
    FieldSchema("a1_1", float, 10, 10, None, "a1.1"),
    FieldSchema("a1_2", float, 20, 10, None, "a1.2"),
    FieldSchema("a1_3", float, 30, 10, None, "a1.3"),
    FieldSchema("a1_4", float, 40, 10, None, "a1.4"),
    FieldSchema("a1_5", float, 50, 10, None, "a1.5"),
    FieldSchema("a1_6", float, 60, 10, None, "a1.6"),
    FieldSchema("a1_7", float, 70, 10, None, "a1.7"),
)

_DEFINETABLECOMPACT_CARD6 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("a1_8", float, 10, 10, None, "a1.8"),
    FieldSchema("a1_9", float, 20, 10, None, "a1.9"),
)

_DEFINETABLECOMPACT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineTableCompact(KeywordBase):
    """DYNA DEFINE_TABLE_COMPACT keyword"""

    keyword = "DEFINE"
    subkeyword = "TABLE_COMPACT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineTableCompact class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINETABLECOMPACT_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineTableCompact.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINETABLECOMPACT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID. Table ID.  Tables and load curves may not share common IDs.  LS DYNA allows load curve IDs and table IDs to be used interchangeably
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[0].set_value("tbid", value)

    @property
    def nvar(self) -> typing.Optional[int]:
        """Get or set the Number of variables (dimension of the table). Current maximum is 9.
        """ # nopep8
        return self._cards[0].get_value("nvar")

    @nvar.setter
    def nvar(self, value: int) -> None:
        """Set the nvar property."""
        self._cards[0].set_value("nvar", value)

    @property
    def lcint(self) -> int:
        """Get or set the Number of discretization points.
        EQ.0:	Value of LCINT from * CONTROL_‌SOLUTION will be used.
        """ # nopep8
        return self._cards[0].get_value("lcint")

    @lcint.setter
    def lcint(self, value: int) -> None:
        """Set the lcint property."""
        self._cards[0].set_value("lcint", value)

    @property
    def mathis(self) -> int:
        """Get or set the Material history flag. Option to identify the abscissa variables as a specified  history variable number(s) (see Remarks 3 and 6). Additional Card 2 (and possibly Card 2.1) is read if this option is active.
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("mathis")

    @mathis.setter
    def mathis(self, value: int) -> None:
        """Set the mathis property."""
        if value not in [0, 1, None]:
            raise Exception("""mathis must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mathis", value)

    @property
    def inexect(self) -> int:
        """Get or set the Extra curve settings flag. Option to assign settings about curve discretization, extrapolation and interpolation for each abscissa variable. Additional Card 3 (and possibly Card 3.1) is read if this option is active.
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("inexect")

    @inexect.setter
    def inexect(self, value: int) -> None:
        """Set the inexect property."""
        if value not in [0, 1, None]:
            raise Exception("""inexect must be `None` or one of {0,1}.""")
        self._cards[0].set_value("inexect", value)

    @property
    def his1(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his1")

    @his1.setter
    def his1(self, value: int) -> None:
        """Set the his1 property."""
        self._cards[1].set_value("his1", value)

    @property
    def his2(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his2")

    @his2.setter
    def his2(self, value: int) -> None:
        """Set the his2 property."""
        self._cards[1].set_value("his2", value)

    @property
    def his3(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his3")

    @his3.setter
    def his3(self, value: int) -> None:
        """Set the his3 property."""
        self._cards[1].set_value("his3", value)

    @property
    def his4(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his4")

    @his4.setter
    def his4(self, value: int) -> None:
        """Set the his4 property."""
        self._cards[1].set_value("his4", value)

    @property
    def his5(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his5")

    @his5.setter
    def his5(self, value: int) -> None:
        """Set the his5 property."""
        self._cards[1].set_value("his5", value)

    @property
    def his6(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his6")

    @his6.setter
    def his6(self, value: int) -> None:
        """Set the his6 property."""
        self._cards[1].set_value("his6", value)

    @property
    def his7(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his7")

    @his7.setter
    def his7(self, value: int) -> None:
        """Set the his7 property."""
        self._cards[1].set_value("his7", value)

    @property
    def his8(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[2].get_value("his8")

    @his8.setter
    def his8(self, value: int) -> None:
        """Set the his8 property."""
        self._cards[2].set_value("his8", value)

    @property
    def his9(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[2].get_value("his9")

    @his9.setter
    def his9(self, value: int) -> None:
        """Set the his9 property."""
        self._cards[2].set_value("his9", value)

    @property
    def ixe1(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe1")

    @ixe1.setter
    def ixe1(self, value: int) -> None:
        """Set the ixe1 property."""
        self._cards[3].set_value("ixe1", value)

    @property
    def ixe2(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe2")

    @ixe2.setter
    def ixe2(self, value: int) -> None:
        """Set the ixe2 property."""
        self._cards[3].set_value("ixe2", value)

    @property
    def ixe3(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe3")

    @ixe3.setter
    def ixe3(self, value: int) -> None:
        """Set the ixe3 property."""
        self._cards[3].set_value("ixe3", value)

    @property
    def ixe4(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe4")

    @ixe4.setter
    def ixe4(self, value: int) -> None:
        """Set the ixe4 property."""
        self._cards[3].set_value("ixe4", value)

    @property
    def ixe5(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe5")

    @ixe5.setter
    def ixe5(self, value: int) -> None:
        """Set the ixe5 property."""
        self._cards[3].set_value("ixe5", value)

    @property
    def ixe6(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe6")

    @ixe6.setter
    def ixe6(self, value: int) -> None:
        """Set the ixe6 property."""
        self._cards[3].set_value("ixe6", value)

    @property
    def ixe7(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe7")

    @ixe7.setter
    def ixe7(self, value: int) -> None:
        """Set the ixe7 property."""
        self._cards[3].set_value("ixe7", value)

    @property
    def ixe8(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[4].get_value("ixe8")

    @ixe8.setter
    def ixe8(self, value: int) -> None:
        """Set the ixe8 property."""
        self._cards[4].set_value("ixe8", value)

    @property
    def ixe9(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[4].get_value("ixe9")

    @ixe9.setter
    def ixe9(self, value: int) -> None:
        """Set the ixe9 property."""
        self._cards[4].set_value("ixe9", value)

    @property
    def o1(self) -> typing.Optional[float]:
        """Get or set the Ordinate (function) values..
        """ # nopep8
        return self._cards[5].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        """Set the o1 property."""
        self._cards[5].set_value("o1", value)

    @property
    def a1_1(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_1")

    @a1_1.setter
    def a1_1(self, value: float) -> None:
        """Set the a1_1 property."""
        self._cards[5].set_value("a1_1", value)

    @property
    def a1_2(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_2")

    @a1_2.setter
    def a1_2(self, value: float) -> None:
        """Set the a1_2 property."""
        self._cards[5].set_value("a1_2", value)

    @property
    def a1_3(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_3")

    @a1_3.setter
    def a1_3(self, value: float) -> None:
        """Set the a1_3 property."""
        self._cards[5].set_value("a1_3", value)

    @property
    def a1_4(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_4")

    @a1_4.setter
    def a1_4(self, value: float) -> None:
        """Set the a1_4 property."""
        self._cards[5].set_value("a1_4", value)

    @property
    def a1_5(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_5")

    @a1_5.setter
    def a1_5(self, value: float) -> None:
        """Set the a1_5 property."""
        self._cards[5].set_value("a1_5", value)

    @property
    def a1_6(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_6")

    @a1_6.setter
    def a1_6(self, value: float) -> None:
        """Set the a1_6 property."""
        self._cards[5].set_value("a1_6", value)

    @property
    def a1_7(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1_7")

    @a1_7.setter
    def a1_7(self, value: float) -> None:
        """Set the a1_7 property."""
        self._cards[5].set_value("a1_7", value)

    @property
    def a1_8(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[6].get_value("a1_8")

    @a1_8.setter
    def a1_8(self, value: float) -> None:
        """Set the a1_8 property."""
        self._cards[6].set_value("a1_8", value)

    @property
    def a1_9(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[6].get_value("a1_9")

    @a1_9.setter
    def a1_9(self, value: float) -> None:
        """Set the a1_9 property."""
        self._cards[6].set_value("a1_9", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

