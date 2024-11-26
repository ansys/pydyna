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

class Mat088(KeywordBase):
    """DYNA MAT_088 keyword"""

    keyword = "MAT"
    subkeyword = "088"
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
                        "siga",
                        float,
                        20,
                        10,
                        kwargs.get("siga")
                    ),
                    Field(
                        "sigi",
                        float,
                        30,
                        10,
                        kwargs.get("sigi")
                    ),
                    Field(
                        "sigs",
                        float,
                        40,
                        10,
                        kwargs.get("sigs")
                    ),
                    Field(
                        "sig0",
                        float,
                        50,
                        10,
                        kwargs.get("sig0")
                    ),
                    Field(
                        "bulk",
                        float,
                        60,
                        10,
                        kwargs.get("bulk")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hf0",
                        float,
                        0,
                        10,
                        kwargs.get("hf0")
                    ),
                    Field(
                        "hf1",
                        float,
                        10,
                        10,
                        kwargs.get("hf1")
                    ),
                    Field(
                        "hf2",
                        float,
                        20,
                        10,
                        kwargs.get("hf2")
                    ),
                    Field(
                        "sigs0",
                        float,
                        30,
                        10,
                        kwargs.get("sigs0")
                    ),
                    Field(
                        "edots0",
                        float,
                        40,
                        10,
                        kwargs.get("edots0")
                    ),
                    Field(
                        "burg",
                        float,
                        50,
                        10,
                        kwargs.get("burg")
                    ),
                    Field(
                        "capa",
                        float,
                        60,
                        10,
                        kwargs.get("capa")
                    ),
                    Field(
                        "boltz",
                        float,
                        70,
                        10,
                        kwargs.get("boltz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sm0",
                        float,
                        0,
                        10,
                        kwargs.get("sm0")
                    ),
                    Field(
                        "sm1",
                        float,
                        10,
                        10,
                        kwargs.get("sm1")
                    ),
                    Field(
                        "sm2",
                        float,
                        20,
                        10,
                        kwargs.get("sm2")
                    ),
                    Field(
                        "edot0",
                        float,
                        30,
                        10,
                        kwargs.get("edot0")
                    ),
                    Field(
                        "go",
                        float,
                        40,
                        10,
                        kwargs.get("go")
                    ),
                    Field(
                        "pinv",
                        float,
                        50,
                        10,
                        kwargs.get("pinv")
                    ),
                    Field(
                        "qinv",
                        float,
                        60,
                        10,
                        kwargs.get("qinv")
                    ),
                    Field(
                        "edoti",
                        float,
                        70,
                        10,
                        kwargs.get("edoti")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g0i",
                        float,
                        0,
                        10,
                        kwargs.get("g0i")
                    ),
                    Field(
                        "pinvi",
                        float,
                        10,
                        10,
                        kwargs.get("pinvi")
                    ),
                    Field(
                        "qinvi",
                        float,
                        20,
                        10,
                        kwargs.get("qinvi")
                    ),
                    Field(
                        "edots",
                        float,
                        30,
                        10,
                        kwargs.get("edots")
                    ),
                    Field(
                        "g0s",
                        float,
                        40,
                        10,
                        kwargs.get("g0s")
                    ),
                    Field(
                        "pinvs",
                        float,
                        50,
                        10,
                        kwargs.get("pinvs")
                    ),
                    Field(
                        "qinvs",
                        float,
                        60,
                        10,
                        kwargs.get("qinvs")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rhocpr",
                        float,
                        0,
                        10,
                        kwargs.get("rhocpr")
                    ),
                    Field(
                        "temprf",
                        float,
                        10,
                        10,
                        kwargs.get("temprf")
                    ),
                    Field(
                        "alpha",
                        float,
                        20,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "eps0",
                        float,
                        30,
                        10,
                        kwargs.get("eps0")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat088.option_specs[0],
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
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def siga(self) -> typing.Optional[float]:
        """Get or set the Dislocation interactions with long-range barriers (force/area).
        """ # nopep8
        return self._cards[0].get_value("siga")

    @siga.setter
    def siga(self, value: float) -> None:
        self._cards[0].set_value("siga", value)

    @property
    def sigi(self) -> typing.Optional[float]:
        """Get or set the Dislocation interactions with interstitial atoms (force/area).
        """ # nopep8
        return self._cards[0].get_value("sigi")

    @sigi.setter
    def sigi(self, value: float) -> None:
        self._cards[0].set_value("sigi", value)

    @property
    def sigs(self) -> typing.Optional[float]:
        """Get or set the Dislocation interactions with solute atoms (force/area).
        """ # nopep8
        return self._cards[0].get_value("sigs")

    @sigs.setter
    def sigs(self, value: float) -> None:
        self._cards[0].set_value("sigs", value)

    @property
    def sig0(self) -> typing.Optional[float]:
        """Get or set the Initial value of  SIGA at zero plastic strain (force/area). NOT USED.
        """ # nopep8
        return self._cards[0].get_value("sig0")

    @sig0.setter
    def sig0(self, value: float) -> None:
        self._cards[0].set_value("sig0", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus defined for shell elements only. Do not input for solid elements.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[0].set_value("bulk", value)

    @property
    def hf0(self) -> typing.Optional[float]:
        """Get or set the Dislocation generation material constant (force/area).
        """ # nopep8
        return self._cards[1].get_value("hf0")

    @hf0.setter
    def hf0(self, value: float) -> None:
        self._cards[1].set_value("hf0", value)

    @property
    def hf1(self) -> typing.Optional[float]:
        """Get or set the Dislocation generation material constant (force/area).
        """ # nopep8
        return self._cards[1].get_value("hf1")

    @hf1.setter
    def hf1(self, value: float) -> None:
        self._cards[1].set_value("hf1", value)

    @property
    def hf2(self) -> typing.Optional[float]:
        """Get or set the Dislocation generation material constant (force/area).
        """ # nopep8
        return self._cards[1].get_value("hf2")

    @hf2.setter
    def hf2(self, value: float) -> None:
        self._cards[1].set_value("hf2", value)

    @property
    def sigs0(self) -> typing.Optional[float]:
        """Get or set the Saturation threshold stress at 0  Kelvin (force/area).
        """ # nopep8
        return self._cards[1].get_value("sigs0")

    @sigs0.setter
    def sigs0(self, value: float) -> None:
        self._cards[1].set_value("sigs0", value)

    @property
    def edots0(self) -> typing.Optional[float]:
        """Get or set the Reference strain-rate (1/time).
        """ # nopep8
        return self._cards[1].get_value("edots0")

    @edots0.setter
    def edots0(self, value: float) -> None:
        self._cards[1].set_value("edots0", value)

    @property
    def burg(self) -> typing.Optional[float]:
        """Get or set the Magnitude of Burgers vector (interatomic slip distance).
        """ # nopep8
        return self._cards[1].get_value("burg")

    @burg.setter
    def burg(self, value: float) -> None:
        self._cards[1].set_value("burg", value)

    @property
    def capa(self) -> typing.Optional[float]:
        """Get or set the Material constant, A.
        """ # nopep8
        return self._cards[1].get_value("capa")

    @capa.setter
    def capa(self, value: float) -> None:
        self._cards[1].set_value("capa", value)

    @property
    def boltz(self) -> typing.Optional[float]:
        """Get or set the Boltzmann's constant ,k (energy/degree).
        """ # nopep8
        return self._cards[1].get_value("boltz")

    @boltz.setter
    def boltz(self, value: float) -> None:
        self._cards[1].set_value("boltz", value)

    @property
    def sm0(self) -> typing.Optional[float]:
        """Get or set the G0, shear modulus at zero degrees Kelvin (force/area).
        """ # nopep8
        return self._cards[2].get_value("sm0")

    @sm0.setter
    def sm0(self, value: float) -> None:
        self._cards[2].set_value("sm0", value)

    @property
    def sm1(self) -> typing.Optional[float]:
        """Get or set the b1 , shear modulus constant (force/area).
        """ # nopep8
        return self._cards[2].get_value("sm1")

    @sm1.setter
    def sm1(self, value: float) -> None:
        self._cards[2].set_value("sm1", value)

    @property
    def sm2(self) -> typing.Optional[float]:
        """Get or set the b2 , shear modulus constant (degree).
        """ # nopep8
        return self._cards[2].get_value("sm2")

    @sm2.setter
    def sm2(self, value: float) -> None:
        self._cards[2].set_value("sm2", value)

    @property
    def edot0(self) -> typing.Optional[float]:
        """Get or set the Reference strain-rate (1/time).
        """ # nopep8
        return self._cards[2].get_value("edot0")

    @edot0.setter
    def edot0(self, value: float) -> None:
        self._cards[2].set_value("edot0", value)

    @property
    def go(self) -> typing.Optional[float]:
        """Get or set the g0 , normalized activation energy for a .dislocation/dislocation interaction.
        """ # nopep8
        return self._cards[2].get_value("go")

    @go.setter
    def go(self, value: float) -> None:
        self._cards[2].set_value("go", value)

    @property
    def pinv(self) -> typing.Optional[float]:
        """Get or set the 1/p, material constant.
        """ # nopep8
        return self._cards[2].get_value("pinv")

    @pinv.setter
    def pinv(self, value: float) -> None:
        self._cards[2].set_value("pinv", value)

    @property
    def qinv(self) -> typing.Optional[float]:
        """Get or set the 1/q, material constant.
        """ # nopep8
        return self._cards[2].get_value("qinv")

    @qinv.setter
    def qinv(self, value: float) -> None:
        self._cards[2].set_value("qinv", value)

    @property
    def edoti(self) -> typing.Optional[float]:
        """Get or set the Reference strain-rate (1/time).
        """ # nopep8
        return self._cards[2].get_value("edoti")

    @edoti.setter
    def edoti(self, value: float) -> None:
        self._cards[2].set_value("edoti", value)

    @property
    def g0i(self) -> typing.Optional[float]:
        """Get or set the g0,i, normalized activation energy for a dislocation/interstitial interaction.
        """ # nopep8
        return self._cards[3].get_value("g0i")

    @g0i.setter
    def g0i(self, value: float) -> None:
        self._cards[3].set_value("g0i", value)

    @property
    def pinvi(self) -> typing.Optional[float]:
        """Get or set the 1/pi, material constant.
        """ # nopep8
        return self._cards[3].get_value("pinvi")

    @pinvi.setter
    def pinvi(self, value: float) -> None:
        self._cards[3].set_value("pinvi", value)

    @property
    def qinvi(self) -> typing.Optional[float]:
        """Get or set the 1/qi, material constant.
        """ # nopep8
        return self._cards[3].get_value("qinvi")

    @qinvi.setter
    def qinvi(self, value: float) -> None:
        self._cards[3].set_value("qinvi", value)

    @property
    def edots(self) -> typing.Optional[float]:
        """Get or set the Reference strain-rate (1/time).
        """ # nopep8
        return self._cards[3].get_value("edots")

    @edots.setter
    def edots(self, value: float) -> None:
        self._cards[3].set_value("edots", value)

    @property
    def g0s(self) -> typing.Optional[float]:
        """Get or set the g0,snormalized activation energy for a dislocation/solute interaction.
        """ # nopep8
        return self._cards[3].get_value("g0s")

    @g0s.setter
    def g0s(self, value: float) -> None:
        self._cards[3].set_value("g0s", value)

    @property
    def pinvs(self) -> typing.Optional[float]:
        """Get or set the 1/ps, material constant.
        """ # nopep8
        return self._cards[3].get_value("pinvs")

    @pinvs.setter
    def pinvs(self, value: float) -> None:
        self._cards[3].set_value("pinvs", value)

    @property
    def qinvs(self) -> typing.Optional[float]:
        """Get or set the 1/qs, material constant.
        """ # nopep8
        return self._cards[3].get_value("qinvs")

    @qinvs.setter
    def qinvs(self, value: float) -> None:
        self._cards[3].set_value("qinvs", value)

    @property
    def rhocpr(self) -> typing.Optional[float]:
        """Get or set the Product of density and specific heat.
        """ # nopep8
        return self._cards[4].get_value("rhocpr")

    @rhocpr.setter
    def rhocpr(self, value: float) -> None:
        self._cards[4].set_value("rhocpr", value)

    @property
    def temprf(self) -> typing.Optional[float]:
        """Get or set the Initial element temperature in degrees K.
        """ # nopep8
        return self._cards[4].get_value("temprf")

    @temprf.setter
    def temprf(self, value: float) -> None:
        self._cards[4].set_value("temprf", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Material constant (typical value is between 0 and 2).
        """ # nopep8
        return self._cards[4].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[4].set_value("alpha", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Factor to normalize strain rate in the calculation of Teta-0. Use 1, 1/1000, 1/1000000 for the time units of seconds, milliseconds, microseconds, respectively.
        """ # nopep8
        return self._cards[4].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        self._cards[4].set_value("eps0", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

