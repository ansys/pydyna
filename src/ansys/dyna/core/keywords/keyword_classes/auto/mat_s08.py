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

"""Module providing the MatS08 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatS08(KeywordBase):
    """DYNA MAT_S08 keyword"""

    keyword = "MAT"
    subkeyword = "S08"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatS08 class."""
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
                        "lcfd",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ku",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ctf",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatS08.option_specs[0],
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
        """Get or set the Material identification. A uniques number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def lcfd(self) -> typing.Optional[int]:
        """Get or set the Load curve identification describing arbitrary force/torque versus displacement/twist relationship. This curve must be defined in the positive force-displacement quadrant regardless of whether the spring acts in tension or compression.
        """ # nopep8
        return self._cards[0].get_value("lcfd")

    @lcfd.setter
    def lcfd(self, value: int) -> None:
        """Set the lcfd property."""
        self._cards[0].set_value("lcfd", value)

    @property
    def ku(self) -> typing.Optional[float]:
        """Get or set the Unloading stiffness (optional). The maximum of KU and the maximum loading stiffness in the force/displacement or the moment/twist curve is used for unloading.
        """ # nopep8
        return self._cards[0].get_value("ku")

    @ku.setter
    def ku(self, value: float) -> None:
        """Set the ku property."""
        self._cards[0].set_value("ku", value)

    @property
    def ctf(self) -> float:
        """Get or set the Flag for compression/tension:
        EQ.-1.0: tension only,
        EQ.0.0: default is set to 1.0,
        EQ.1.0: compression only (default).
        """ # nopep8
        return self._cards[0].get_value("ctf")

    @ctf.setter
    def ctf(self, value: float) -> None:
        """Set the ctf property."""
        if value not in [1.0, -1.0, None]:
            raise Exception("""ctf must be `None` or one of {1.0,-1.0}.""")
        self._cards[0].set_value("ctf", value)

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

