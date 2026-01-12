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

"""Module providing the DefineDeInjectBondedEllipse class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDEINJECTBONDEDELLIPSE_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("xc", float, 20, 10, 0.0),
    FieldSchema("yc", float, 30, 10, 0.0),
    FieldSchema("zc", float, 40, 10, 0.0),
    FieldSchema("xl", float, 50, 10, 0.0),
    FieldSchema("yl", float, 60, 10, 0.0),
    FieldSchema("cid", int, 70, 10, None),
)

_DEFINEDEINJECTBONDEDELLIPSE_CARD1 = (
    FieldSchema("rmass", float, 0, 10, 0.0),
    FieldSchema("vx", float, 10, 10, 0.0),
    FieldSchema("xy", float, 20, 10, 0.0),
    FieldSchema("vz", float, 30, 10, 0.0),
    FieldSchema("tbeg", float, 40, 10, 0.0),
    FieldSchema("tend", float, 50, 10, 0.0),
)

_DEFINEDEINJECTBONDEDELLIPSE_CARD2 = (
    FieldSchema("pbn", float, 0, 10, None),
    FieldSchema("pbs", float, 10, 10, None),
    FieldSchema("pbn_s", float, 20, 10, 0.0),
    FieldSchema("pbs_s", float, 30, 10, 0.0),
    FieldSchema("sfa", float, 40, 10, 0.0),
    FieldSchema("alpha", float, 50, 10, 0.0),
    FieldSchema("maxgap", float, 60, 10, 0.0001),
)

_DEFINEDEINJECTBONDEDELLIPSE_CARD3 = (
    FieldSchema("nshape", int, 0, 10, 0),
)

_DEFINEDEINJECTBONDEDELLIPSE_CARD4 = (
    FieldSchema("ishape", int, 0, 10, None),
    FieldSchema("ishape", int, 10, 10, None),
    FieldSchema("ishape", int, 20, 10, None),
    FieldSchema("ishape", int, 30, 10, None),
    FieldSchema("ishape", int, 40, 10, None),
    FieldSchema("ishape", int, 50, 10, None),
    FieldSchema("ishape", int, 60, 10, None),
    FieldSchema("ishape", int, 70, 10, None),
)

class DefineDeInjectBondedEllipse(KeywordBase):
    """DYNA DEFINE_DE_INJECT_BONDED_ELLIPSE keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECT_BONDED_ELLIPSE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeInjectBondedEllipse class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTBONDEDELLIPSE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTBONDEDELLIPSE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTBONDEDELLIPSE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTBONDEDELLIPSE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTBONDEDELLIPSE_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeInjectBondedEllipse.option_specs[0],
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
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID.  Nodes and DES properties are generated automatically during input phase based on the user input and assigned to this SID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def xc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def xl(self) -> float:
        """Get or set the For rectangular planes XL specifies the planar length along the x-axis in the coordinate system specified by CID.  For elliptical planes XL specifies the length of the major axis
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the For rectangular planes YL specifies the planar length along the y-axis in the coordinate system specified by CID.  For elliptical planes YL specifies the length of the minor axis
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[0].set_value("yl", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Optional local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def rmass(self) -> float:
        """Get or set the Mass flow rate
        GE.0.0:	Constant mass flow rate
        LT.0.0 : RMASS is a curve ID defining the mass flow rate as a function of time.
        """ # nopep8
        return self._cards[1].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        """Set the rmass property."""
        self._cards[1].set_value("rmass", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def xy(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("xy")

    @xy.setter
    def xy(self, value: float) -> None:
        """Set the xy property."""
        self._cards[1].set_value("xy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time; time for injection to begin
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Death time; time for injection to end
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def pbn(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond modulus [Pa].
        """ # nopep8
        return self._cards[2].get_value("pbn")

    @pbn.setter
    def pbn(self, value: float) -> None:
        """Set the pbn property."""
        self._cards[2].set_value("pbn", value)

    @property
    def pbs(self) -> typing.Optional[float]:
        """Get or set the Parallel-bond stiffness ratio.  Shear stiffness/normal stiffness
        """ # nopep8
        return self._cards[2].get_value("pbs")

    @pbs.setter
    def pbs(self, value: float) -> None:
        """Set the pbs property."""
        self._cards[2].set_value("pbs", value)

    @property
    def pbn_s(self) -> float:
        """Get or set the Parallel-bond maximum normal stress.  A zero value defines an infinite maximum normal stress
        """ # nopep8
        return self._cards[2].get_value("pbn_s")

    @pbn_s.setter
    def pbn_s(self, value: float) -> None:
        """Set the pbn_s property."""
        self._cards[2].set_value("pbn_s", value)

    @property
    def pbs_s(self) -> float:
        """Get or set the Parallel-bond maximum shear stress.  A zero value defines an infinite maximum shear stress
        """ # nopep8
        return self._cards[2].get_value("pbs_s")

    @pbs_s.setter
    def pbs_s(self, value: float) -> None:
        """Set the pbs_s property."""
        self._cards[2].set_value("pbs_s", value)

    @property
    def sfa(self) -> float:
        """Get or set the Bond radius multiplier
        """ # nopep8
        return self._cards[2].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        """Set the sfa property."""
        self._cards[2].set_value("sfa", value)

    @property
    def alpha(self) -> float:
        """Get or set the Numerical damping
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[2].set_value("alpha", value)

    @property
    def maxgap(self) -> float:
        """Get or set the Maximum gap between two bonded spheres
        GT.0.0:	When MAXGAP is positive, the maximum allowed gap is determined on a bond - by - bond basis as a function of the radii of the two involved spheres.The maximum gap is determined by multiplying the minimum of the two radii by the value of MAXGAP.
        LT.0.0 : Absolute value is used as the maximum gap
        """ # nopep8
        return self._cards[2].get_value("maxgap")

    @maxgap.setter
    def maxgap(self, value: float) -> None:
        """Set the maxgap property."""
        self._cards[2].set_value("maxgap", value)

    @property
    def nshape(self) -> int:
        """Get or set the Number of shape patterns
        """ # nopep8
        return self._cards[3].get_value("nshape")

    @nshape.setter
    def nshape(self, value: int) -> None:
        """Set the nshape property."""
        self._cards[3].set_value("nshape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
        """ # nopep8
        return self._cards[4].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[4].set_value("ishape", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

