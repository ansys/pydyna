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

class DefineElementGeneralizedShell(KeywordBase):
    """DYNA DEFINE_ELEMENT_GENERALIZED_SHELL keyword"""

    keyword = "DEFINE"
    subkeyword = "ELEMENT_GENERALIZED_SHELL"
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
                        "elform",
                        int,
                        0,
                        10,
                        kwargs.get("elform")
                    ),
                    Field(
                        "nip",
                        int,
                        10,
                        10,
                        kwargs.get("nip")
                    ),
                    Field(
                        "nmnp",
                        int,
                        20,
                        10,
                        kwargs.get("nmnp")
                    ),
                    Field(
                        "imass",
                        int,
                        30,
                        10,
                        kwargs.get("imass", 0)
                    ),
                    Field(
                        "form",
                        int,
                        40,
                        10,
                        kwargs.get("form", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "wi",
                        float,
                        0,
                        20,
                        kwargs.get("wi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nki",
                        float,
                        0,
                        20,
                        kwargs.get("nki")
                    ),
                    Field(
                        "dnkidr",
                        float,
                        20,
                        20,
                        kwargs.get("dnkidr")
                    ),
                    Field(
                        "dnkids",
                        float,
                        40,
                        20,
                        kwargs.get("dnkids")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dnkldr",
                        float,
                        0,
                        20,
                        kwargs.get("dnkldr")
                    ),
                    Field(
                        "dnklds",
                        float,
                        20,
                        20,
                        kwargs.get("dnklds")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d2nkidr2",
                        float,
                        0,
                        20,
                        kwargs.get("d2nkidr2")
                    ),
                    Field(
                        "d2nkidrds",
                        float,
                        20,
                        20,
                        kwargs.get("d2nkidrds")
                    ),
                    Field(
                        "d2nkids2",
                        float,
                        40,
                        20,
                        kwargs.get("d2nkids2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineElementGeneralizedShell.option_specs[0],
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
    def elform(self) -> typing.Optional[int]:
        """Get or set the Element Formulation ID referenced via *SECTION_SHELL to connect
        *ELEMENT_GENERALIZED_SHELL with the appropriate shell
        formulation. The chosen number needs to be greater or equal than 1000.
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        self._cards[0].set_value("elform", value)

    @property
    def nip(self) -> typing.Optional[int]:
        """Get or set the Number of in-plane integration points.
        """ # nopep8
        return self._cards[0].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[0].set_value("nip", value)

    @property
    def nmnp(self) -> typing.Optional[int]:
        """Get or set the Number of nodes for this element formulation.
        """ # nopep8
        return self._cards[0].get_value("nmnp")

    @nmnp.setter
    def nmnp(self, value: int) -> None:
        self._cards[0].set_value("nmnp", value)

    @property
    def imass(self) -> int:
        """Get or set the Option for lumping of mass matrix:
        EQ.0: row sum
        EQ.1: diagonal weighting.
        """ # nopep8
        return self._cards[0].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""imass must be one of {0,1}""")
        self._cards[0].set_value("imass", value)

    @property
    def form(self) -> int:
        """Get or set the Shell formulation to be used
        EQ.0: shear deformable shell theory with rotational DOFs (shell normal evaluated at the nodes)
        EQ.1: shear deformable shell theory without rotational DOFs (shell	normal evaluated at the nodes)
        EQ.2: thin shell theory without rotational DOFs (shell normal evaluated at the integration points)
        EQ.3: thin shell theory with rotational DOFs (shell normal evaluated at the integration points).
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""form must be one of {0,1,2,3}""")
        self._cards[0].set_value("form", value)

    @property
    def wi(self) -> typing.Optional[float]:
        """Get or set the Integration weight at integration point i.
        """ # nopep8
        return self._cards[1].get_value("wi")

    @wi.setter
    def wi(self, value: float) -> None:
        self._cards[1].set_value("wi", value)

    @property
    def nki(self) -> typing.Optional[float]:
        """Get or set the Value of the shape function N k evaluated at integration point i.
        """ # nopep8
        return self._cards[2].get_value("nki")

    @nki.setter
    def nki(self, value: float) -> None:
        self._cards[2].set_value("nki", value)

    @property
    def dnkidr(self) -> typing.Optional[float]:
        """Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
        """ # nopep8
        return self._cards[2].get_value("dnkidr")

    @dnkidr.setter
    def dnkidr(self, value: float) -> None:
        self._cards[2].set_value("dnkidr", value)

    @property
    def dnkids(self) -> typing.Optional[float]:
        """Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point i.
        """ # nopep8
        return self._cards[2].get_value("dnkids")

    @dnkids.setter
    def dnkids(self, value: float) -> None:
        self._cards[2].set_value("dnkids", value)

    @property
    def dnkldr(self) -> typing.Optional[float]:
        """Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point l.
        """ # nopep8
        return self._cards[3].get_value("dnkldr")

    @dnkldr.setter
    def dnkldr(self, value: float) -> None:
        self._cards[3].set_value("dnkldr", value)

    @property
    def dnklds(self) -> typing.Optional[float]:
        """Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point l.
        """ # nopep8
        return self._cards[3].get_value("dnklds")

    @dnklds.setter
    def dnklds(self, value: float) -> None:
        self._cards[3].set_value("dnklds", value)

    @property
    def d2nkidr2(self) -> typing.Optional[float]:
        """Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
        """ # nopep8
        return self._cards[4].get_value("d2nkidr2")

    @d2nkidr2.setter
    def d2nkidr2(self, value: float) -> None:
        self._cards[4].set_value("d2nkidr2", value)

    @property
    def d2nkidrds(self) -> typing.Optional[float]:
        """Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinates r and s at the integration point i.
        """ # nopep8
        return self._cards[4].get_value("d2nkidrds")

    @d2nkidrds.setter
    def d2nkidrds(self, value: float) -> None:
        self._cards[4].set_value("d2nkidrds", value)

    @property
    def d2nkids2(self) -> typing.Optional[float]:
        """Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate s at the integration point  i.
        """ # nopep8
        return self._cards[4].get_value("d2nkids2")

    @d2nkids2.setter
    def d2nkids2(self, value: float) -> None:
        self._cards[4].set_value("d2nkids2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

