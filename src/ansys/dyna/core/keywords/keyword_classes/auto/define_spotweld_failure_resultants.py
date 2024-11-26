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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineSpotweldFailureResultants(KeywordBase):
    """DYNA DEFINE_SPOTWELD_FAILURE_RESULTANTS keyword"""

    keyword = "DEFINE"
    subkeyword = "SPOTWELD_FAILURE_RESULTANTS"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "dsn",
                        float,
                        10,
                        10,
                        kwargs.get("dsn", 0.0)
                    ),
                    Field(
                        "dss",
                        float,
                        20,
                        10,
                        kwargs.get("dss", 0.0)
                    ),
                    Field(
                        "dlcidsn",
                        int,
                        30,
                        10,
                        kwargs.get("dlcidsn", 0)
                    ),
                    Field(
                        "dlcidss",
                        int,
                        40,
                        10,
                        kwargs.get("dlcidss", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid_i",
                        int,
                        0,
                        10,
                        kwargs.get("pid_i")
                    ),
                    Field(
                        "pid_j",
                        int,
                        10,
                        10,
                        kwargs.get("pid_j")
                    ),
                    Field(
                        "snij",
                        float,
                        20,
                        10,
                        kwargs.get("snij", 0.0)
                    ),
                    Field(
                        "ssij",
                        float,
                        30,
                        10,
                        kwargs.get("ssij", 0.0)
                    ),
                    Field(
                        "lcidsnij",
                        int,
                        40,
                        10,
                        kwargs.get("lcidsnij", 0)
                    ),
                    Field(
                        "lcidssij",
                        int,
                        50,
                        10,
                        kwargs.get("lcidssij", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSpotweldFailureResultants.option_specs[0],
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
    def id(self) -> int:
        """Get or set the Identification number. Only one table is allowed
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def dsn(self) -> float:
        """Get or set the Default value of the normal static stress resultant at failure
        """ # nopep8
        return self._cards[0].get_value("dsn")

    @dsn.setter
    def dsn(self, value: float) -> None:
        self._cards[0].set_value("dsn", value)

    @property
    def dss(self) -> float:
        """Get or set the Default value of the transverse static stress resultant at failure
        """ # nopep8
        return self._cards[0].get_value("dss")

    @dss.setter
    def dss(self, value: float) -> None:
        self._cards[0].set_value("dss", value)

    @property
    def dlcidsn(self) -> int:
        """Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies DSN to obtain the failure value at a given strain rate.
        """ # nopep8
        return self._cards[0].get_value("dlcidsn")

    @dlcidsn.setter
    def dlcidsn(self, value: int) -> None:
        self._cards[0].set_value("dlcidsn", value)

    @property
    def dlcidss(self) -> int:
        """Get or set the Load curve ID defining a scale factor for the static shear stress resultant as a function of strain rate. This factor multiplies DSS to obtain the failure value at a given strain rate.
        """ # nopep8
        return self._cards[0].get_value("dlcidss")

    @dlcidss.setter
    def dlcidss(self, value: int) -> None:
        self._cards[0].set_value("dlcidss", value)

    @property
    def pid_i(self) -> typing.Optional[int]:
        """Get or set the Part ID I.
        """ # nopep8
        return self._cards[1].get_value("pid_i")

    @pid_i.setter
    def pid_i(self, value: int) -> None:
        self._cards[1].set_value("pid_i", value)

    @property
    def pid_j(self) -> typing.Optional[int]:
        """Get or set the Part ID J.
        """ # nopep8
        return self._cards[1].get_value("pid_j")

    @pid_j.setter
    def pid_j(self, value: int) -> None:
        self._cards[1].set_value("pid_j", value)

    @property
    def snij(self) -> float:
        """Get or set the The normal static stress resultant at failure between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("snij")

    @snij.setter
    def snij(self, value: float) -> None:
        self._cards[1].set_value("snij", value)

    @property
    def ssij(self) -> float:
        """Get or set the The transverse shear static stress resultant at failure between parts I and J.
        """ # nopep8
        return self._cards[1].get_value("ssij")

    @ssij.setter
    def ssij(self, value: float) -> None:
        self._cards[1].set_value("ssij", value)

    @property
    def lcidsnij(self) -> int:
        """Get or set the Load curve ID defining a scale factor for the normal stress resultant as a function of strain rate. This factor multiplies SNIJ to obtain the failure value at a given strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcidsnij")

    @lcidsnij.setter
    def lcidsnij(self, value: int) -> None:
        self._cards[1].set_value("lcidsnij", value)

    @property
    def lcidssij(self) -> int:
        """Get or set the Load curve ID defining a scale factor for static shear stress resultant as a function of strain rate. This factor multiplies SSIJ to obtain the failure value at a given strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcidssij")

    @lcidssij.setter
    def lcidssij(self, value: int) -> None:
        self._cards[1].set_value("lcidssij", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

