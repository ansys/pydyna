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

class DefineDeMeshBeam(KeywordBase):
    """DYNA DEFINE_DE_MESH_BEAM keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_MESH_BEAM"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid", 0)
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "nquad",
                        int,
                        20,
                        10,
                        kwargs.get("nquad", 1)
                    ),
                    Field(
                        "despid",
                        int,
                        30,
                        10,
                        kwargs.get("despid", 0)
                    ),
                    Field(
                        "desxid",
                        int,
                        40,
                        10,
                        kwargs.get("desxid", 0)
                    ),
                    Field(
                        "nsid",
                        int,
                        50,
                        10,
                        kwargs.get("nsid", 0)
                    ),
                    Field(
                        "rsf",
                        float,
                        60,
                        10,
                        kwargs.get("rsf", 1.0)
                    ),
                    Field(
                        "iactive",
                        int,
                        70,
                        10,
                        kwargs.get("iactive", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mass",
                        float,
                        0,
                        10,
                        kwargs.get("mass")
                    ),
                    Field(
                        "inertia",
                        float,
                        10,
                        10,
                        kwargs.get("inertia")
                    ),
                    Field(
                        "radius",
                        float,
                        20,
                        10,
                        kwargs.get("radius")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeMeshBeam.option_specs[0],
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
    def sid(self) -> int:
        """Get or set the Part or part set ID for the region of the mesh upon which the DES elements will be placed
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def type(self) -> int:
        """Get or set the EQ.0:	Part set
        EQ.1:	Part
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""type must be one of {0,1}""")
        self._cards[0].set_value("type", value)

    @property
    def nquad(self) -> int:
        """Get or set the Number of equally spaced DES elements created along the axis of beam element. (Maximum NQUAD=4)
        """ # nopep8
        return self._cards[0].get_value("nquad")

    @nquad.setter
    def nquad(self, value: int) -> None:
        self._cards[0].set_value("nquad", value)

    @property
    def despid(self) -> int:
        """Get or set the Part ID for generated DES elements
        """ # nopep8
        return self._cards[0].get_value("despid")

    @despid.setter
    def despid(self, value: int) -> None:
        self._cards[0].set_value("despid", value)

    @property
    def desxid(self) -> int:
        """Get or set the Section ID for generated DES elements
        """ # nopep8
        return self._cards[0].get_value("desxid")

    @desxid.setter
    def desxid(self, value: int) -> None:
        self._cards[0].set_value("desxid", value)

    @property
    def nsid(self) -> int:
        """Get or set the Creates a node set with ID NSID (see *SET_NODE) for the nodes generated by this keyword. By default, no node set is created )
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def rsf(self) -> float:
        """Get or set the Scale factor of DES radius. By default, the DES radius is the shell thickness (RSF = 1.0).
        """ # nopep8
        return self._cards[0].get_value("rsf")

    @rsf.setter
    def rsf(self, value: float) -> None:
        self._cards[0].set_value("rsf", value)

    @property
    def iactive(self) -> int:
        """Get or set the Activate DES:
        EQ.0: DES is inactive and used as a shadow (default)
        EQ.1: DES is active
        """ # nopep8
        return self._cards[0].get_value("iactive")

    @iactive.setter
    def iactive(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iactive must be one of {0,1}""")
        self._cards[0].set_value("iactive", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the DES Mass:
        GT.0:	DES mass
        EQ. - 1 : The DES particle radius(r) is 0.5×Beam Length / NQUAD , the DES mass(m) is(4πρr ^ 3)⁄3,and the moment of inertia is 2mr ^ 2 / 5. Input fields INERTIA and RADIUS are ignored.
        """ # nopep8
        return self._cards[1].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[1].set_value("mass", value)

    @property
    def inertia(self) -> typing.Optional[float]:
        """Get or set the Mass moment of ineritia (ignored if MASS = -1)
        """ # nopep8
        return self._cards[1].get_value("inertia")

    @inertia.setter
    def inertia(self, value: float) -> None:
        self._cards[1].set_value("inertia", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Particle radius (ignored if MASS = -1)
        """ # nopep8
        return self._cards[1].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[1].set_value("radius", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

