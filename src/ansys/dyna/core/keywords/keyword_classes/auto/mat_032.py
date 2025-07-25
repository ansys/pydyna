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

"""Module providing the Mat032 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat032(KeywordBase):
    """DYNA MAT_032 keyword"""

    keyword = "MAT"
    subkeyword = "032"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat032 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eg",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "prg",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "syg",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "etg",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "efg",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ep",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "prp",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "syp",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "etp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "f1",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f4",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f5",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f6",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f7",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "f8",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat032.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def eg(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for glass.
        """ # nopep8
        return self._cards[0].get_value("eg")

    @eg.setter
    def eg(self, value: float) -> None:
        """Set the eg property."""
        self._cards[0].set_value("eg", value)

    @property
    def prg(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for glass.
        """ # nopep8
        return self._cards[0].get_value("prg")

    @prg.setter
    def prg(self, value: float) -> None:
        """Set the prg property."""
        self._cards[0].set_value("prg", value)

    @property
    def syg(self) -> typing.Optional[float]:
        """Get or set the Yield stress for glass.
        """ # nopep8
        return self._cards[0].get_value("syg")

    @syg.setter
    def syg(self, value: float) -> None:
        """Set the syg property."""
        self._cards[0].set_value("syg", value)

    @property
    def etg(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus for glass.
        """ # nopep8
        return self._cards[0].get_value("etg")

    @etg.setter
    def etg(self, value: float) -> None:
        """Set the etg property."""
        self._cards[0].set_value("etg", value)

    @property
    def efg(self) -> typing.Optional[float]:
        """Get or set the Plastic strain at failure for glass.
        """ # nopep8
        return self._cards[0].get_value("efg")

    @efg.setter
    def efg(self, value: float) -> None:
        """Set the efg property."""
        self._cards[0].set_value("efg", value)

    @property
    def ep(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for polymer.
        """ # nopep8
        return self._cards[0].get_value("ep")

    @ep.setter
    def ep(self, value: float) -> None:
        """Set the ep property."""
        self._cards[0].set_value("ep", value)

    @property
    def prp(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for polymer.
        """ # nopep8
        return self._cards[1].get_value("prp")

    @prp.setter
    def prp(self, value: float) -> None:
        """Set the prp property."""
        self._cards[1].set_value("prp", value)

    @property
    def syp(self) -> typing.Optional[float]:
        """Get or set the Yield stress for polymer.
        """ # nopep8
        return self._cards[1].get_value("syp")

    @syp.setter
    def syp(self, value: float) -> None:
        """Set the syp property."""
        self._cards[1].set_value("syp", value)

    @property
    def etp(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus for polymer.
        """ # nopep8
        return self._cards[1].get_value("etp")

    @etp.setter
    def etp(self, value: float) -> None:
        """Set the etp property."""
        self._cards[1].set_value("etp", value)

    @property
    def f1(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass (default),
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f1")

    @f1.setter
    def f1(self, value: float) -> None:
        """Set the f1 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f1 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f1", value)

    @property
    def f2(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f2")

    @f2.setter
    def f2(self, value: float) -> None:
        """Set the f2 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f2 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f2", value)

    @property
    def f3(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f3")

    @f3.setter
    def f3(self, value: float) -> None:
        """Set the f3 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f3 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f3", value)

    @property
    def f4(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f4")

    @f4.setter
    def f4(self, value: float) -> None:
        """Set the f4 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f4 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f4", value)

    @property
    def f5(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f5")

    @f5.setter
    def f5(self, value: float) -> None:
        """Set the f5 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f5 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f5", value)

    @property
    def f6(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f6")

    @f6.setter
    def f6(self, value: float) -> None:
        """Set the f6 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f6 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f6", value)

    @property
    def f7(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f7")

    @f7.setter
    def f7(self, value: float) -> None:
        """Set the f7 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f7 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f7", value)

    @property
    def f8(self) -> float:
        """Get or set the Integration point material:
        EQ.0.0: glass,
        EQ.1.0: polymer.
        A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("f8")

    @f8.setter
    def f8(self, value: float) -> None:
        """Set the f8 property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""f8 must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("f8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

