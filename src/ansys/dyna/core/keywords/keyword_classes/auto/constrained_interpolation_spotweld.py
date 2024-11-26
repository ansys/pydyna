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

class ConstrainedInterpolationSpotweld(KeywordBase):
    """DYNA CONSTRAINED_INTERPOLATION_SPOTWELD keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "INTERPOLATION_SPOTWELD"

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
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        kwargs.get("pid2")
                    ),
                    Field(
                        "nsid",
                        int,
                        20,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "thick",
                        float,
                        30,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "r",
                        float,
                        40,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "stiff",
                        float,
                        50,
                        10,
                        kwargs.get("stiff")
                    ),
                    Field(
                        "alpha1",
                        float,
                        60,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "model",
                        int,
                        70,
                        10,
                        kwargs.get("model", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rn",
                        float,
                        0,
                        10,
                        kwargs.get("rn")
                    ),
                    Field(
                        "rs",
                        float,
                        10,
                        10,
                        kwargs.get("rs")
                    ),
                    Field(
                        "beta",
                        float,
                        20,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "lcf",
                        int,
                        30,
                        10,
                        kwargs.get("lcf")
                    ),
                    Field(
                        "lcupf",
                        int,
                        40,
                        10,
                        kwargs.get("lcupf")
                    ),
                    Field(
                        "lcupr",
                        int,
                        50,
                        10,
                        kwargs.get("lcupr")
                    ),
                    Field(
                        "dens",
                        float,
                        60,
                        10,
                        kwargs.get("dens")
                    ),
                    Field(
                        "intp",
                        int,
                        70,
                        10,
                        kwargs.get("intp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "upfn",
                        float,
                        0,
                        10,
                        kwargs.get("upfn")
                    ),
                    Field(
                        "upfs",
                        float,
                        10,
                        10,
                        kwargs.get("upfs")
                    ),
                    Field(
                        "alpha2",
                        float,
                        20,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "beta2",
                        float,
                        30,
                        10,
                        kwargs.get("beta2")
                    ),
                    Field(
                        "uprn",
                        float,
                        40,
                        10,
                        kwargs.get("uprn")
                    ),
                    Field(
                        "uprs",
                        float,
                        50,
                        10,
                        kwargs.get("uprs")
                    ),
                    Field(
                        "alpha3",
                        float,
                        60,
                        10,
                        kwargs.get("alpha3")
                    ),
                    Field(
                        "beta3",
                        float,
                        70,
                        10,
                        kwargs.get("beta3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mrn",
                        float,
                        0,
                        10,
                        kwargs.get("mrn")
                    ),
                    Field(
                        "mrs",
                        float,
                        10,
                        10,
                        kwargs.get("mrs")
                    ),
                ],
            ),
        ]

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of first sheet
        GT.0:	Part ID
        LT.0 : | PID1 | is part set ID(for in - plane composed sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID of second sheet. PID2 can be identical to PID1 if the spot weld location nodes from NSID lie in between the shell elements that should be self-connected.
        GT.0:	Part ID
        LT.0 : |PID2 | is part set ID(for in - plane composed sheets sheets such as Tailored Blanks)
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[0].set_value("pid2", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of spot weld location nodes.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Total thickness of both sheets.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Spotweld radius.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[0].set_value("r", value)

    @property
    def stiff(self) -> typing.Optional[float]:
        """Get or set the Elastic stiffness. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[0].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        self._cards[0].set_value("stiff", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Scaling factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[0].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[0].set_value("alpha1", value)

    @property
    def model(self) -> int:
        """Get or set the Material behavior and damage model, see remarks.
        EQ. 1:	SPR3 (default),
        EQ. 2:	SPR4,
        EQ.11:	same as 1 with selected material parameters as functions,
        EQ.12:	same as 2 with selected material parameters as functions,
        EQ.21:	same as 11 with slight modification, see remarks,
        EQ.22:	same as 12 with slight modification, see remarks.
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        if value not in [1, 2, 11, 12, 21, 22]:
            raise Exception("""model must be one of {1,2,11,12,21,22}""")
        self._cards[0].set_value("model", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Tensile strength factor.
        GT.0.0:	Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remark 2).
        LT.0.0:	Load curve with ID | RN | giving R_n as a function of peel ratio(see Remark 5)
        """ # nopep8
        return self._cards[1].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        self._cards[1].set_value("rn", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the Shear strength factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        self._cards[1].set_value("rs", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic potential β_1. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

    @property
    def lcf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing force versus plastic displacement.
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        self._cards[1].set_value("lcf", value)

    @property
    def lcupf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[1].get_value("lcupf")

    @lcupf.setter
    def lcupf(self, value: int) -> None:
        self._cards[1].set_value("lcupf", value)

    @property
    def lcupr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
        """ # nopep8
        return self._cards[1].get_value("lcupr")

    @lcupr.setter
    def lcupr(self, value: int) -> None:
        self._cards[1].set_value("lcupr", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Spotweld density (necessary for time step calculation).
        """ # nopep8
        return self._cards[1].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        self._cards[1].set_value("dens", value)

    @property
    def intp(self) -> int:
        """Get or set the Flag for interpolation.
        EQ.0:	linear (default),
        EQ.1:	uniform,
        EQ.2:	inverse distance weighting.
        """ # nopep8
        return self._cards[1].get_value("intp")

    @intp.setter
    def intp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""intp must be one of {0,1,2}""")
        self._cards[1].set_value("intp", value)

    @property
    def upfn(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("upfn")

    @upfn.setter
    def upfn(self, value: float) -> None:
        self._cards[2].set_value("upfn", value)

    @property
    def upfs(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("upfs")

    @upfs.setter
    def upfs(self, value: float) -> None:
        self._cards[2].set_value("upfs", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[2].set_value("alpha2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic initiation displacement.
        """ # nopep8
        return self._cards[2].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        self._cards[2].set_value("beta2", value)

    @property
    def uprn(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("uprn")

    @uprn.setter
    def uprn(self, value: float) -> None:
        self._cards[2].set_value("uprn", value)

    @property
    def uprs(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("uprs")

    @uprs.setter
    def uprs(self, value: float) -> None:
        self._cards[2].set_value("uprs", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[2].set_value("alpha3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic rupture displacement.
        """ # nopep8
        return self._cards[2].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        self._cards[2].set_value("beta3", value)

    @property
    def mrn(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RN.
        """ # nopep8
        return self._cards[3].get_value("mrn")

    @mrn.setter
    def mrn(self, value: float) -> None:
        self._cards[3].set_value("mrn", value)

    @property
    def mrs(self) -> typing.Optional[float]:
        """Get or set the Proportionality factor for dependency RS.
        """ # nopep8
        return self._cards[3].get_value("mrs")

    @mrs.setter
    def mrs(self, value: float) -> None:
        self._cards[3].set_value("mrs", value)

