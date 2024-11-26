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

class MatComprf(KeywordBase):
    """DYNA MAT_COMPRF keyword"""

    keyword = "MAT"
    subkeyword = "COMPRF"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "et",
                        float,
                        20,
                        10,
                        kwargs.get("et")
                    ),
                    Field(
                        "ec",
                        float,
                        30,
                        10,
                        kwargs.get("ec")
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "g121",
                        float,
                        50,
                        10,
                        kwargs.get("g121")
                    ),
                    Field(
                        "g122",
                        float,
                        60,
                        10,
                        kwargs.get("g122")
                    ),
                    Field(
                        "g123",
                        float,
                        70,
                        10,
                        kwargs.get("g123")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g124",
                        float,
                        0,
                        10,
                        kwargs.get("g124")
                    ),
                    Field(
                        "g125",
                        float,
                        10,
                        10,
                        kwargs.get("g125")
                    ),
                    Field(
                        "g126",
                        float,
                        20,
                        10,
                        kwargs.get("g126")
                    ),
                    Field(
                        "gammal",
                        float,
                        30,
                        10,
                        kwargs.get("gammal")
                    ),
                    Field(
                        "vf",
                        float,
                        40,
                        10,
                        kwargs.get("vf")
                    ),
                    Field(
                        "ef3",
                        float,
                        50,
                        10,
                        kwargs.get("ef3")
                    ),
                    Field(
                        "vf23",
                        float,
                        60,
                        10,
                        kwargs.get("vf23")
                    ),
                    Field(
                        "em",
                        float,
                        70,
                        10,
                        kwargs.get("em")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vm",
                        float,
                        0,
                        10,
                        kwargs.get("vm")
                    ),
                    Field(
                        "epsilon",
                        float,
                        10,
                        10,
                        kwargs.get("epsilon")
                    ),
                    Field(
                        "theta",
                        float,
                        20,
                        10,
                        kwargs.get("theta")
                    ),
                    Field(
                        "bulk",
                        float,
                        30,
                        10,
                        kwargs.get("bulk")
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        kwargs.get("g")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatComprf.option_specs[0],
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
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Continuum equivalent mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def et(self) -> typing.Optional[float]:
        """Get or set the Tensile modulus along the fiber yarns, corresponding to the slope
        of the curve in Figure M293-2 in the Stable Modulus region from a
        uniaxial tension test. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("et")

    @et.setter
    def et(self, value: float) -> None:
        self._cards[0].set_value("et", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Compression modulus along the fiber yarns, reversely calculated
        using bending tests when all the other material properties are
        determined. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def g121(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("g121")

    @g121.setter
    def g121(self, value: float) -> None:
        self._cards[0].set_value("g121", value)

    @property
    def g122(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("g122")

    @g122.setter
    def g122(self, value: float) -> None:
        self._cards[0].set_value("g122", value)

    @property
    def g123(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("g123")

    @g123.setter
    def g123(self, value: float) -> None:
        self._cards[0].set_value("g123", value)

    @property
    def g124(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("g124")

    @g124.setter
    def g124(self, value: float) -> None:
        self._cards[1].set_value("g124", value)

    @property
    def g125(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("g125")

    @g125.setter
    def g125(self, value: float) -> None:
        self._cards[1].set_value("g125", value)

    @property
    def g126(self) -> typing.Optional[float]:
        """Get or set the Coefficients for the bias-extension angle change-engineering stress
        curve in Figure M293-3. G121 to G126 corresponds to the 6th order
        to 1st order factors of the loading curve. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("g126")

    @g126.setter
    def g126(self, value: float) -> None:
        self._cards[1].set_value("g126", value)

    @property
    def gammal(self) -> typing.Optional[float]:
        """Get or set the Shear locking angle, in degrees. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("gammal")

    @gammal.setter
    def gammal(self, value: float) -> None:
        self._cards[1].set_value("gammal", value)

    @property
    def vf(self) -> typing.Optional[float]:
        """Get or set the Fiber volume fraction in the prepreg composite.
        """ # nopep8
        return self._cards[1].get_value("vf")

    @vf.setter
    def vf(self, value: float) -> None:
        self._cards[1].set_value("vf", value)

    @property
    def ef3(self) -> typing.Optional[float]:
        """Get or set the Transverse compression modulus of the dry fiber.
        """ # nopep8
        return self._cards[1].get_value("ef3")

    @ef3.setter
    def ef3(self, value: float) -> None:
        self._cards[1].set_value("ef3", value)

    @property
    def vf23(self) -> typing.Optional[float]:
        """Get or set the Transverse Poisson's ratio of the dry fiber.
        """ # nopep8
        return self._cards[1].get_value("vf23")

    @vf23.setter
    def vf23(self, value: float) -> None:
        self._cards[1].set_value("vf23", value)

    @property
    def em(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of the cured resin.
        """ # nopep8
        return self._cards[1].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        self._cards[1].set_value("em", value)

    @property
    def vm(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio of the cured resin.
        """ # nopep8
        return self._cards[2].get_value("vm")

    @vm.setter
    def vm(self, value: float) -> None:
        self._cards[2].set_value("vm", value)

    @property
    def epsilon(self) -> typing.Optional[float]:
        """Get or set the Stretch ratio at the end of undulation stage during the uniaxial
        tension test. Example shown in Figure M293-2. See Remark 5.
        """ # nopep8
        return self._cards[2].get_value("epsilon")

    @epsilon.setter
    def epsilon(self, value: float) -> None:
        self._cards[2].set_value("epsilon", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Initial angle offset between the fiber direction and the element
        direction. To reduce simulation error, when building the model,
        the elements should be aligned to the same direction as much as possible.
        """ # nopep8
        return self._cards[2].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        self._cards[2].set_value("theta", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus of the prepreg material.
        """ # nopep8
        return self._cards[2].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[2].set_value("bulk", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus of the prepreg material.
        """ # nopep8
        return self._cards[2].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[2].set_value("g", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

