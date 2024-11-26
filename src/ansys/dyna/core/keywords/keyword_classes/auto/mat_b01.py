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

class MatB01(KeywordBase):
    """DYNA MAT_B01 keyword"""

    keyword = "MAT"
    subkeyword = "B01"
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
                        kwargs.get("mid", 0)
                    ),
                    Field(
                        "mpul",
                        float,
                        10,
                        10,
                        kwargs.get("mpul")
                    ),
                    Field(
                        "llcid",
                        int,
                        20,
                        10,
                        kwargs.get("llcid", 0)
                    ),
                    Field(
                        "ulcid",
                        int,
                        30,
                        10,
                        kwargs.get("ulcid", 0)
                    ),
                    Field(
                        "lmin",
                        float,
                        40,
                        10,
                        kwargs.get("lmin")
                    ),
                    Field(
                        "cse",
                        float,
                        50,
                        10,
                        kwargs.get("cse", 0.0)
                    ),
                    Field(
                        "damp",
                        float,
                        60,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "e",
                        float,
                        70,
                        10,
                        kwargs.get("e")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "i",
                        float,
                        10,
                        10,
                        kwargs.get("i")
                    ),
                    Field(
                        "j",
                        float,
                        20,
                        10,
                        kwargs.get("j")
                    ),
                    Field(
                        "as",
                        float,
                        30,
                        10,
                        kwargs.get("as")
                    ),
                    Field(
                        "f",
                        float,
                        40,
                        10,
                        kwargs.get("f", 1.0e20)
                    ),
                    Field(
                        "m",
                        float,
                        50,
                        10,
                        kwargs.get("m", 1.0e20)
                    ),
                    Field(
                        "r",
                        float,
                        60,
                        10,
                        kwargs.get("r", 0.05)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1doff",
                        int,
                        0,
                        10,
                        kwargs.get("p1doff")
                    ),
                    Field(
                        "form",
                        int,
                        10,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "ecoat",
                        float,
                        20,
                        10,
                        kwargs.get("ecoat")
                    ),
                    Field(
                        "tcoat",
                        float,
                        30,
                        10,
                        kwargs.get("tcoat")
                    ),
                    Field(
                        "scoat",
                        float,
                        40,
                        10,
                        kwargs.get("scoat")
                    ),
                    Field(
                        "eb",
                        float,
                        50,
                        10,
                        kwargs.get("eb", -0.1)
                    ),
                    Field(
                        "prba",
                        float,
                        60,
                        10,
                        kwargs.get("prba", 0.3)
                    ),
                    Field(
                        "prba",
                        float,
                        70,
                        10,
                        kwargs.get("prba", 0.3)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gab",
                        float,
                        0,
                        10,
                        kwargs.get("gab")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatB01.option_specs[0],
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
    def mid(self) -> int:
        """Get or set the Belt material number. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def mpul(self) -> typing.Optional[float]:
        """Get or set the Mass per unit length.
        """ # nopep8
        return self._cards[0].get_value("mpul")

    @mpul.setter
    def mpul(self, value: float) -> None:
        self._cards[0].set_value("mpul", value)

    @property
    def llcid(self) -> int:
        """Get or set the Load curve identification for loading (strain/force with engineering strain).
        """ # nopep8
        return self._cards[0].get_value("llcid")

    @llcid.setter
    def llcid(self, value: int) -> None:
        self._cards[0].set_value("llcid", value)

    @property
    def ulcid(self) -> int:
        """Get or set the Load curve identification for unloading (strain/force with engineering strain).
        """ # nopep8
        return self._cards[0].get_value("ulcid")

    @ulcid.setter
    def ulcid(self, value: int) -> None:
        self._cards[0].set_value("ulcid", value)

    @property
    def lmin(self) -> typing.Optional[float]:
        """Get or set the Minimum length (for elements connected to slip rings and retractors).
        """ # nopep8
        return self._cards[0].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        self._cards[0].set_value("lmin", value)

    @property
    def cse(self) -> float:
        """Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
        EQ.0.0:	eliminate compressive stresses in shell fabric
        EQ.1.0:	don't eliminate compressive stresses.  This option should not be used if retractors and sliprings are present in the model.
        EQ.2.0:	whether or not compressive stress is eliminated is decided by ls-dyna automatically, recommended for shell belt.
        """ # nopep8
        return self._cards[0].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""cse must be one of {0.0,1.0,2.0}""")
        self._cards[0].set_value("cse", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Cross sectional area for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def i(self) -> typing.Optional[float]:
        """Get or set the Area moment of inertia for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("i")

    @i.setter
    def i(self, value: float) -> None:
        self._cards[1].set_value("i", value)

    @property
    def j(self) -> typing.Optional[float]:
        """Get or set the Torsional constant for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("j")

    @j.setter
    def j(self, value: float) -> None:
        self._cards[1].set_value("j", value)

    @property
    def as_(self) -> typing.Optional[float]:
        """Get or set the Shear area for bending/compression stiffness
        """ # nopep8
        return self._cards[1].get_value("as")

    @as_.setter
    def as_(self, value: float) -> None:
        self._cards[1].set_value("as", value)

    @property
    def f(self) -> float:
        """Get or set the Maximum force in compression/tension.
        """ # nopep8
        return self._cards[1].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        self._cards[1].set_value("f", value)

    @property
    def m(self) -> float:
        """Get or set the Maximum torque
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def r(self) -> float:
        """Get or set the Rotational mass scaling factor
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def p1doff(self) -> typing.Optional[int]:
        """Get or set the Part ID offset for internally created 1D, bar-type, belt parts for 2D seatbelt of this material, i.e., the IDs of newly created 1d belt parts will be P1DOFF+1, P1DOFF+2, ....  If zero, the maximum ID of user-defined parts is used as the part ID offset.
        """ # nopep8
        return self._cards[2].get_value("p1doff")

    @p1doff.setter
    def p1doff(self, value: int) -> None:
        self._cards[2].set_value("p1doff", value)

    @property
    def form(self) -> int:
        """Get or set the Formulation of the translated fabric material, see FORM of *MAT_FABRIC for details.  FORM=0 was used since R8 and non-zero FORM is available since r137418/dev
        """ # nopep8
        return self._cards[2].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        self._cards[2].set_value("form", value)

    @property
    def ecoat(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of coat material for FORM=-14, see *MAT_FABRIC for details.
        The coat material is assumed to be elastic, therefore there is no need to define its yield stress, see SCOAT of *MAT_FABRIC.
        EQ.0.0:	ECOAT is the Young's modulus determined by LS-DYNA.
        GT.0.0:	ECOAT is the Young's modulus to be used for coat material.
        LT.0.0:	|ECOAT| is the ratio of coat material's Young's modulus to that of the fabric shell which is determined by LS-DYNA.
        """ # nopep8
        return self._cards[2].get_value("ecoat")

    @ecoat.setter
    def ecoat(self, value: float) -> None:
        self._cards[2].set_value("ecoat", value)

    @property
    def tcoat(self) -> typing.Optional[float]:
        """Get or set the Thickness of coat material for FORM=-14, see *MAT_FABRIC for details.
        GT.0.0:	TCOAT is the thickness of the coat material.
        LT.0.0:	|TCOAT| is the ratio of coat material's thickness to that of the fabric shell defined in the related *SECTION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("tcoat")

    @tcoat.setter
    def tcoat(self, value: float) -> None:
        self._cards[2].set_value("tcoat", value)

    @property
    def scoat(self) -> typing.Optional[float]:
        """Get or set the Yield stress of coat material for FORM=-14, see *MAT_FABRIC for details.
        If not defined, the coat material is assumed to be elastic.
        """ # nopep8
        return self._cards[2].get_value("scoat")

    @scoat.setter
    def scoat(self, value: float) -> None:
        self._cards[2].set_value("scoat", value)

    @property
    def eb(self) -> float:
        """Get or set the Young’s modulus along transverse direction, see *MAT_FABRIC for details.
        EQ.0.0: The Young’s modulus along transverse direction is 10 % of
        the Young’s determined by LS - DYNA based on the loading curve, LLCID.
        LT.0.0 : | EB | is the ratio of Young’s modulus along transverse
        direction to the Young’s determined by LS - DYNA based on the loading curve, LLCID.
        GT.0.0 : EB is the Young's modulus along the transverse direction
        """ # nopep8
        return self._cards[2].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[2].set_value("eb", value)

    @property
    def prba(self) -> float:
        """Get or set the Minor (Major) Poisson's ratioba (ab) direction
        """ # nopep8
        return self._cards[2].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[2].set_value("prba", value)

    @property
    def prba(self) -> float:
        """Get or set the Minor (Major) Poisson's ratioba (ab) direction
        """ # nopep8
        return self._cards[2].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[2].set_value("prba", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in the ab direction. Set to a very small value for an
        isotropic elastic material, see * MAT_FABRIC.If defined to be zero, a
        default value of EA / (2 * (1 + PRBA)) will be used where EA is the
        Young's modulus along the longitudinal direction and is set to 1 % of the
        Young's modulus determined by LS - DYNA according to the loading curve, LLCID
        """ # nopep8
        return self._cards[3].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[3].set_value("gab", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

