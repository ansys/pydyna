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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ContactSpg(KeywordBase):
    """DYNA CONTACT_SPG keyword"""

    keyword = "CONTACT"
    subkeyword = "SPG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iself1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iself8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pfac1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfac8",
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
                        "fs",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fd",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dc",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nfreq",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[0].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        self._cards[0].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        self._cards[0].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        self._cards[0].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        self._cards[0].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        self._cards[0].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        self._cards[0].set_value("pid8", value)

    @property
    def iself1(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself1")

    @iself1.setter
    def iself1(self, value: int) -> None:
        self._cards[1].set_value("iself1", value)

    @property
    def iself2(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself2")

    @iself2.setter
    def iself2(self, value: int) -> None:
        self._cards[1].set_value("iself2", value)

    @property
    def iself3(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself3")

    @iself3.setter
    def iself3(self, value: int) -> None:
        self._cards[1].set_value("iself3", value)

    @property
    def iself4(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself4")

    @iself4.setter
    def iself4(self, value: int) -> None:
        self._cards[1].set_value("iself4", value)

    @property
    def iself5(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself5")

    @iself5.setter
    def iself5(self, value: int) -> None:
        self._cards[1].set_value("iself5", value)

    @property
    def iself6(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself6")

    @iself6.setter
    def iself6(self, value: int) -> None:
        self._cards[1].set_value("iself6", value)

    @property
    def iself7(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself7")

    @iself7.setter
    def iself7(self, value: int) -> None:
        self._cards[1].set_value("iself7", value)

    @property
    def iself8(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself8")

    @iself8.setter
    def iself8(self, value: int) -> None:
        self._cards[1].set_value("iself8", value)

    @property
    def pfac1(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac1")

    @pfac1.setter
    def pfac1(self, value: float) -> None:
        self._cards[2].set_value("pfac1", value)

    @property
    def pfac2(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac2")

    @pfac2.setter
    def pfac2(self, value: float) -> None:
        self._cards[2].set_value("pfac2", value)

    @property
    def pfac3(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac3")

    @pfac3.setter
    def pfac3(self, value: float) -> None:
        self._cards[2].set_value("pfac3", value)

    @property
    def pfac4(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac4")

    @pfac4.setter
    def pfac4(self, value: float) -> None:
        self._cards[2].set_value("pfac4", value)

    @property
    def pfac5(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac5")

    @pfac5.setter
    def pfac5(self, value: float) -> None:
        self._cards[2].set_value("pfac5", value)

    @property
    def pfac6(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac6")

    @pfac6.setter
    def pfac6(self, value: float) -> None:
        self._cards[2].set_value("pfac6", value)

    @property
    def pfac7(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac7")

    @pfac7.setter
    def pfac7(self, value: float) -> None:
        self._cards[2].set_value("pfac7", value)

    @property
    def pfac8(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac8")

    @pfac8.setter
    def pfac8(self, value: float) -> None:
        self._cards[2].set_value("pfac8", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction
        """ # nopep8
        return self._cards[3].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[3].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction
        """ # nopep8
        return self._cards[3].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[3].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity, v_rel , of the surfaces in contact
        """ # nopep8
        return self._cards[3].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[3].set_value("dc", value)

    @property
    def nfreq(self) -> typing.Optional[float]:
        """Get or set the Contact searching frequency
        """ # nopep8
        return self._cards[3].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: float) -> None:
        self._cards[3].set_value("nfreq", value)

