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

"""Module providing the Mat265 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat265(KeywordBase):
    """DYNA MAT_265 keyword"""

    keyword = "MAT"
    subkeyword = "265"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat265 class."""
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
                        "model",
                        int,
                        20,
                        10,
                        1,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "stiff",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rn",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rs",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcf",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcupf",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcupr",
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
                        "upfn",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "upfs",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha2",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta2",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "uprn",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "uprs",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha3",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta3",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat265.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Set the model property."""
        if value not in [1, 2, 11, 12, 21, 22, None]:
            raise Exception("""model must be `None` or one of {1,2,11,12,21,22}.""")
        self._cards[0].set_value("model", value)

    @property
    def stiff(self) -> typing.Optional[float]:
        """Get or set the Elastic stiffness. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("stiff")

    @stiff.setter
    def stiff(self, value: float) -> None:
        """Set the stiff property."""
        self._cards[1].set_value("stiff", value)

    @property
    def rn(self) -> typing.Optional[float]:
        """Get or set the Tensile strength factor.
        GT.0.0:	Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD).
        LT.0.0:	Load curve with ID | RN | giving R_n as a function of peel ratio(see Remarks section for* CONSTRAINED_INTERPOLATION_SPOTWELD)
        """ # nopep8
        return self._cards[1].get_value("rn")

    @rn.setter
    def rn(self, value: float) -> None:
        """Set the rn property."""
        self._cards[1].set_value("rn", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the Shear strength factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        """Set the rs property."""
        self._cards[1].set_value("rs", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Scaling factor. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[1].set_value("alpha1", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic potential. Function ID if MODEL > 10.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def lcf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing force versus plastic displacement.
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        """Set the lcf property."""
        self._cards[1].set_value("lcf", value)

    @property
    def lcupf(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD
        """ # nopep8
        return self._cards[1].get_value("lcupf")

    @lcupf.setter
    def lcupf(self, value: int) -> None:
        """Set the lcupf property."""
        self._cards[1].set_value("lcupf", value)

    @property
    def lcupr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD.
        """ # nopep8
        return self._cards[1].get_value("lcupr")

    @lcupr.setter
    def lcupr(self, value: int) -> None:
        """Set the lcupr property."""
        self._cards[1].set_value("lcupr", value)

    @property
    def upfn(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("upfn")

    @upfn.setter
    def upfn(self, value: float) -> None:
        """Set the upfn property."""
        self._cards[2].set_value("upfn", value)

    @property
    def upfs(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("upfs")

    @upfs.setter
    def upfs(self, value: float) -> None:
        """Set the upfs property."""
        self._cards[2].set_value("upfs", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Plastic initiation displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[2].set_value("alpha2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic initiation displacement.
        """ # nopep8
        return self._cards[2].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[2].set_value("beta2", value)

    @property
    def uprn(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in normal direction.
        """ # nopep8
        return self._cards[2].get_value("uprn")

    @uprn.setter
    def uprn(self, value: float) -> None:
        """Set the uprn property."""
        self._cards[2].set_value("uprn", value)

    @property
    def uprs(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement in shear direction.
        """ # nopep8
        return self._cards[2].get_value("uprs")

    @uprs.setter
    def uprs(self, value: float) -> None:
        """Set the uprs property."""
        self._cards[2].set_value("uprs", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Plastic rupture displacement scaling factor.
        """ # nopep8
        return self._cards[2].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[2].set_value("alpha3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Exponent for plastic rupture displacement.
        """ # nopep8
        return self._cards[2].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        """Set the beta3 property."""
        self._cards[2].set_value("beta3", value)

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

