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

"""Module providing the Mat169 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat169(KeywordBase):
    """DYNA MAT_169 keyword"""

    keyword = "MAT"
    subkeyword = "169"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat169 class."""
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
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tenmax",
                        float,
                        40,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "gcten",
                        float,
                        50,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "shrmax",
                        float,
                        60,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "gcshr",
                        float,
                        70,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pwrt",
                        float,
                        0,
                        10,
                        2.0,
                        **kwargs,
                    ),
                    Field(
                        "pwrs",
                        float,
                        10,
                        10,
                        2.0,
                        **kwargs,
                    ),
                    Field(
                        "shrp",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sht_sl",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "edot0",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "edot2",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "thkdir",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "extra",
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
                        "tmaxe",
                        float,
                        0,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "gcte",
                        float,
                        10,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "smaxe",
                        float,
                        20,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "gcse",
                        float,
                        30,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "pwrte",
                        float,
                        40,
                        10,
                        2.0,
                        **kwargs,
                    ),
                    Field(
                        "pwrse",
                        float,
                        50,
                        10,
                        2.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "facet",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "facct",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "faces",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "faccs",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "softt",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "softs",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sdfac",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sgfac",
                        float,
                        10,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sdefac",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "sgefac",
                        float,
                        30,
                        10,
                        1.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bthk",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "outfail",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "fsip",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fbr713",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ele2ns",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat169.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def tenmax(self) -> float:
        """Get or set the Maximum through-thickness tensile stress.
        GT.0.0:	Constant value
        LT.0.0: | TENMAX | is a function ID
        """ # nopep8
        return self._cards[0].get_value("tenmax")

    @tenmax.setter
    def tenmax(self, value: float) -> None:
        """Set the tenmax property."""
        self._cards[0].set_value("tenmax", value)

    @property
    def gcten(self) -> float:
        """Get or set the Energy per unit area to fail the bond in tension.
        GT.0.0:	Constant value
        LT.0.0: | GCTEN | is a function ID
        """ # nopep8
        return self._cards[0].get_value("gcten")

    @gcten.setter
    def gcten(self, value: float) -> None:
        """Set the gcten property."""
        self._cards[0].set_value("gcten", value)

    @property
    def shrmax(self) -> float:
        """Get or set the Maximum through-thickness shear stress.
        GT.0.0:	Constant value
        LT.0.0: | SHRMAX | is a function ID
        """ # nopep8
        return self._cards[0].get_value("shrmax")

    @shrmax.setter
    def shrmax(self, value: float) -> None:
        """Set the shrmax property."""
        self._cards[0].set_value("shrmax", value)

    @property
    def gcshr(self) -> float:
        """Get or set the Energy per unit area to fail the bond in shear.
        GT.0.0:	Constant value
        LT.0.0: | GCSHR | is a function ID
        """ # nopep8
        return self._cards[0].get_value("gcshr")

    @gcshr.setter
    def gcshr(self, value: float) -> None:
        """Set the gcshr property."""
        self._cards[0].set_value("gcshr", value)

    @property
    def pwrt(self) -> float:
        """Get or set the Power law term for tension.
        """ # nopep8
        return self._cards[1].get_value("pwrt")

    @pwrt.setter
    def pwrt(self, value: float) -> None:
        """Set the pwrt property."""
        self._cards[1].set_value("pwrt", value)

    @property
    def pwrs(self) -> float:
        """Get or set the Power law term for shear.
        """ # nopep8
        return self._cards[1].get_value("pwrs")

    @pwrs.setter
    def pwrs(self, value: float) -> None:
        """Set the pwrs property."""
        self._cards[1].set_value("pwrs", value)

    @property
    def shrp(self) -> typing.Optional[float]:
        """Get or set the Shear plateau ratio (optional).
        GT.0.0:	Constant value
        LT.0.0: | SHRP | is a function ID
        """ # nopep8
        return self._cards[1].get_value("shrp")

    @shrp.setter
    def shrp(self, value: float) -> None:
        """Set the shrp property."""
        self._cards[1].set_value("shrp", value)

    @property
    def sht_sl(self) -> typing.Optional[float]:
        """Get or set the Slope (non-dimensional) of yield surface at zero tension.
        """ # nopep8
        return self._cards[1].get_value("sht_sl")

    @sht_sl.setter
    def sht_sl(self, value: float) -> None:
        """Set the sht_sl property."""
        self._cards[1].set_value("sht_sl", value)

    @property
    def edot0(self) -> float:
        """Get or set the Strain rate at which the "static" properties apply.
        """ # nopep8
        return self._cards[1].get_value("edot0")

    @edot0.setter
    def edot0(self, value: float) -> None:
        """Set the edot0 property."""
        self._cards[1].set_value("edot0", value)

    @property
    def edot2(self) -> typing.Optional[float]:
        """Get or set the Strain rate at which the "dynamic" properties apply (Card 5).
        """ # nopep8
        return self._cards[1].get_value("edot2")

    @edot2.setter
    def edot2(self, value: float) -> None:
        """Set the edot2 property."""
        self._cards[1].set_value("edot2", value)

    @property
    def thkdir(self) -> float:
        """Get or set the Through-thickness direction flag (See remarks)
        EQ.0.0: smallest element dimension (default)
        EQ.1.0: direction from nodes 1-2-3-4 to nodes 5-6-7-8
        .
        """ # nopep8
        return self._cards[1].get_value("thkdir")

    @thkdir.setter
    def thkdir(self, value: float) -> None:
        """Set the thkdir property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""thkdir must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("thkdir", value)

    @property
    def extra(self) -> typing.Optional[float]:
        """Get or set the Flag to input further data:
        EQ.1.0 interfacial failure properties (cards 3 and 4)
        EQ.2.0 bond thickness (card 6)
        EQ.3.0 both of the above
        """ # nopep8
        return self._cards[1].get_value("extra")

    @extra.setter
    def extra(self, value: float) -> None:
        """Set the extra property."""
        self._cards[1].set_value("extra", value)

    @property
    def tmaxe(self) -> float:
        """Get or set the Maximum tensile force per unit length on edges of joint.
        """ # nopep8
        return self._cards[2].get_value("tmaxe")

    @tmaxe.setter
    def tmaxe(self, value: float) -> None:
        """Set the tmaxe property."""
        self._cards[2].set_value("tmaxe", value)

    @property
    def gcte(self) -> float:
        """Get or set the Energy per unit length to fail the edge of the bond in tension.
        """ # nopep8
        return self._cards[2].get_value("gcte")

    @gcte.setter
    def gcte(self, value: float) -> None:
        """Set the gcte property."""
        self._cards[2].set_value("gcte", value)

    @property
    def smaxe(self) -> float:
        """Get or set the Maximum shear force per unit length on edges of joint.
        """ # nopep8
        return self._cards[2].get_value("smaxe")

    @smaxe.setter
    def smaxe(self, value: float) -> None:
        """Set the smaxe property."""
        self._cards[2].set_value("smaxe", value)

    @property
    def gcse(self) -> float:
        """Get or set the Energy per unit length to fail the edge of the bond in shear.
        """ # nopep8
        return self._cards[2].get_value("gcse")

    @gcse.setter
    def gcse(self, value: float) -> None:
        """Set the gcse property."""
        self._cards[2].set_value("gcse", value)

    @property
    def pwrte(self) -> float:
        """Get or set the Power law term for tension.
        """ # nopep8
        return self._cards[2].get_value("pwrte")

    @pwrte.setter
    def pwrte(self, value: float) -> None:
        """Set the pwrte property."""
        self._cards[2].set_value("pwrte", value)

    @property
    def pwrse(self) -> float:
        """Get or set the Power law term for shear.
        """ # nopep8
        return self._cards[2].get_value("pwrse")

    @pwrse.setter
    def pwrse(self, value: float) -> None:
        """Set the pwrse property."""
        self._cards[2].set_value("pwrse", value)

    @property
    def facet(self) -> float:
        """Get or set the Stiffness scaling factor for edge elements - tension.
        """ # nopep8
        return self._cards[3].get_value("facet")

    @facet.setter
    def facet(self, value: float) -> None:
        """Set the facet property."""
        self._cards[3].set_value("facet", value)

    @property
    def facct(self) -> float:
        """Get or set the Stiffness scaling factor for interior elements - tension.
        """ # nopep8
        return self._cards[3].get_value("facct")

    @facct.setter
    def facct(self, value: float) -> None:
        """Set the facct property."""
        self._cards[3].set_value("facct", value)

    @property
    def faces(self) -> float:
        """Get or set the Stiffness scaling factor for edge elements - shear.
        """ # nopep8
        return self._cards[3].get_value("faces")

    @faces.setter
    def faces(self, value: float) -> None:
        """Set the faces property."""
        self._cards[3].set_value("faces", value)

    @property
    def faccs(self) -> float:
        """Get or set the Stiffness scaling factor for interior elements - shear.
        """ # nopep8
        return self._cards[3].get_value("faccs")

    @faccs.setter
    def faccs(self, value: float) -> None:
        """Set the faccs property."""
        self._cards[3].set_value("faccs", value)

    @property
    def softt(self) -> float:
        """Get or set the Factor by which the tensile strength is reduced when a neighbor fails.
        """ # nopep8
        return self._cards[3].get_value("softt")

    @softt.setter
    def softt(self, value: float) -> None:
        """Set the softt property."""
        self._cards[3].set_value("softt", value)

    @property
    def softs(self) -> float:
        """Get or set the Factor by which the shear strength is reduced when a neighbor fails.
        """ # nopep8
        return self._cards[3].get_value("softs")

    @softs.setter
    def softs(self, value: float) -> None:
        """Set the softs property."""
        self._cards[3].set_value("softs", value)

    @property
    def sdfac(self) -> float:
        """Get or set the Factor on TENMAX and SHRMAX at strain rate EDOT2
        GT.0.0:	Constant value
        LT.0.0: | SDFAC | is a function ID.
        """ # nopep8
        return self._cards[4].get_value("sdfac")

    @sdfac.setter
    def sdfac(self, value: float) -> None:
        """Set the sdfac property."""
        self._cards[4].set_value("sdfac", value)

    @property
    def sgfac(self) -> float:
        """Get or set the Factor on GCTEN and GCSHR at strain rate EDOT2.
        GT.0.0:	Constant valu
        LT.0.0: | SGFAC | is a function ID
        """ # nopep8
        return self._cards[4].get_value("sgfac")

    @sgfac.setter
    def sgfac(self, value: float) -> None:
        """Set the sgfac property."""
        self._cards[4].set_value("sgfac", value)

    @property
    def sdefac(self) -> float:
        """Get or set the Factor on TMAXE and SMAXE at strain rate EDOT2.
        """ # nopep8
        return self._cards[4].get_value("sdefac")

    @sdefac.setter
    def sdefac(self, value: float) -> None:
        """Set the sdefac property."""
        self._cards[4].set_value("sdefac", value)

    @property
    def sgefac(self) -> float:
        """Get or set the Factor on GCTE and GCSE at strain rate EDOT2.
        """ # nopep8
        return self._cards[4].get_value("sgefac")

    @sgefac.setter
    def sgefac(self, value: float) -> None:
        """Set the sgefac property."""
        self._cards[4].set_value("sgefac", value)

    @property
    def bthk(self) -> typing.Optional[float]:
        """Get or set the Bond thickness (overrides thickness from element dimensions.
        LT.0.0: | BTHK | is bond thickness, but critical time step remains unaffected. Helps to avoid very small time steps, but it can affect stability)
        """ # nopep8
        return self._cards[5].get_value("bthk")

    @bthk.setter
    def bthk(self, value: float) -> None:
        """Set the bthk property."""
        self._cards[5].set_value("bthk", value)

    @property
    def outfail(self) -> float:
        """Get or set the Flag for additional output to messag file: Information about damageinitiation time, failure function terms and forces.
        EQ.0.0:	off
        EQ.1.0:	on
        """ # nopep8
        return self._cards[5].get_value("outfail")

    @outfail.setter
    def outfail(self, value: float) -> None:
        """Set the outfail property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""outfail must be `None` or one of {0.0,1.0}.""")
        self._cards[5].set_value("outfail", value)

    @property
    def fsip(self) -> typing.Optional[float]:
        """Get or set the Effective in-plane strain at failure.
        EQ.0.0:	Off
        EQ.1.0:	LS - DYNA release R7.1.3
        """ # nopep8
        return self._cards[5].get_value("fsip")

    @fsip.setter
    def fsip(self, value: float) -> None:
        """Set the fsip property."""
        self._cards[5].set_value("fsip", value)

    @property
    def fbr713(self) -> typing.Optional[float]:
        """Get or set the Fallback option to get results from previous version.
        """ # nopep8
        return self._cards[5].get_value("fbr713")

    @fbr713.setter
    def fbr713(self, value: float) -> None:
        """Set the fbr713 property."""
        self._cards[5].set_value("fbr713", value)

    @property
    def ele2ns(self) -> float:
        """Get or set the Volumetric smearing option for ELFORM = 2. See Remark 9.
        EQ.0.0:	Usual ELFORM = 2 behavior with volumetric smearing
        EQ.1.0 : Volumetric smearing is turned off
        """ # nopep8
        return self._cards[5].get_value("ele2ns")

    @ele2ns.setter
    def ele2ns(self, value: float) -> None:
        """Set the ele2ns property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ele2ns must be `None` or one of {0.0,1.0}.""")
        self._cards[5].set_value("ele2ns", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

