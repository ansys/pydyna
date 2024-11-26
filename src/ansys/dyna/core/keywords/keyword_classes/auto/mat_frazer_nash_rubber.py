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

class MatFrazerNashRubber(KeywordBase):
    """DYNA MAT_FRAZER-NASH_RUBBER keyword"""

    keyword = "MAT"
    subkeyword = "FRAZER-NASH_RUBBER"
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
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "c100",
                        float,
                        30,
                        10,
                        kwargs.get("c100")
                    ),
                    Field(
                        "c200",
                        float,
                        40,
                        10,
                        kwargs.get("c200")
                    ),
                    Field(
                        "c300",
                        float,
                        50,
                        10,
                        kwargs.get("c300")
                    ),
                    Field(
                        "c400",
                        float,
                        60,
                        10,
                        kwargs.get("c400")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c110",
                        float,
                        0,
                        10,
                        kwargs.get("c110")
                    ),
                    Field(
                        "c210",
                        float,
                        10,
                        10,
                        kwargs.get("c210")
                    ),
                    Field(
                        "c010",
                        float,
                        20,
                        10,
                        kwargs.get("c010")
                    ),
                    Field(
                        "c020",
                        float,
                        30,
                        10,
                        kwargs.get("c020")
                    ),
                    Field(
                        "exit",
                        float,
                        40,
                        10,
                        kwargs.get("exit")
                    ),
                    Field(
                        "emax",
                        float,
                        50,
                        10,
                        kwargs.get("emax")
                    ),
                    Field(
                        "emin",
                        float,
                        60,
                        10,
                        kwargs.get("emin")
                    ),
                    Field(
                        "ref",
                        float,
                        70,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sgl",
                        float,
                        0,
                        10,
                        kwargs.get("sgl")
                    ),
                    Field(
                        "sw",
                        float,
                        10,
                        10,
                        kwargs.get("sw")
                    ),
                    Field(
                        "st",
                        float,
                        20,
                        10,
                        kwargs.get("st")
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatFrazerNashRubber.option_specs[0],
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio. Values between .49 and .50 are suggested.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def c100(self) -> typing.Optional[float]:
        """Get or set the C100 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[0].get_value("c100")

    @c100.setter
    def c100(self, value: float) -> None:
        self._cards[0].set_value("c100", value)

    @property
    def c200(self) -> typing.Optional[float]:
        """Get or set the C200 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[0].get_value("c200")

    @c200.setter
    def c200(self, value: float) -> None:
        self._cards[0].set_value("c200", value)

    @property
    def c300(self) -> typing.Optional[float]:
        """Get or set the C300 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[0].get_value("c300")

    @c300.setter
    def c300(self, value: float) -> None:
        self._cards[0].set_value("c300", value)

    @property
    def c400(self) -> typing.Optional[float]:
        """Get or set the C400 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[0].get_value("c400")

    @c400.setter
    def c400(self, value: float) -> None:
        self._cards[0].set_value("c400", value)

    @property
    def c110(self) -> typing.Optional[float]:
        """Get or set the C110 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[1].get_value("c110")

    @c110.setter
    def c110(self, value: float) -> None:
        self._cards[1].set_value("c110", value)

    @property
    def c210(self) -> typing.Optional[float]:
        """Get or set the C210 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[1].get_value("c210")

    @c210.setter
    def c210(self, value: float) -> None:
        self._cards[1].set_value("c210", value)

    @property
    def c010(self) -> typing.Optional[float]:
        """Get or set the C010 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[1].get_value("c010")

    @c010.setter
    def c010(self, value: float) -> None:
        self._cards[1].set_value("c010", value)

    @property
    def c020(self) -> typing.Optional[float]:
        """Get or set the C020 (EQ.1.0 if term is in the least squares fit.).
        """ # nopep8
        return self._cards[1].get_value("c020")

    @c020.setter
    def c020(self, value: float) -> None:
        self._cards[1].set_value("c020", value)

    @property
    def exit(self) -> typing.Optional[float]:
        """Get or set the Exit option:
        EQ.0.0: stop if strain limits are exceeded (recommended),
        NE.0.0: continue if strain limits are exceeded. The curve is then extrapolated.
        """ # nopep8
        return self._cards[1].get_value("exit")

    @exit.setter
    def exit(self, value: float) -> None:
        self._cards[1].set_value("exit", value)

    @property
    def emax(self) -> typing.Optional[float]:
        """Get or set the Maximum strain limit, (Green-St, Venant Strain).
        """ # nopep8
        return self._cards[1].get_value("emax")

    @emax.setter
    def emax(self, value: float) -> None:
        self._cards[1].set_value("emax", value)

    @property
    def emin(self) -> typing.Optional[float]:
        """Get or set the Minimum strain limit, (Green-St, Venant Strain).
        """ # nopep8
        return self._cards[1].get_value("emin")

    @emin.setter
    def emin(self, value: float) -> None:
        self._cards[1].set_value("emin", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor ,see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded-solid elements with on integration point):
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[1].set_value("ref", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[2].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        self._cards[2].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[2].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        self._cards[2].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[2].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[2].set_value("st", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID, see DEFINE_CURVE, giving the force versus actual change in gauge length.
        """ # nopep8
        return self._cards[2].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[2].set_value("lcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

