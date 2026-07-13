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

"""Module providing the DefineSphInjectionSimplifiedFlowrate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("flwrt", float, 10, 10, None),
    FieldSchema("radius", float, 20, 10, None),
    FieldSchema("psize", float, 30, 10, None),
    FieldSchema("offst", float, 40, 10, None),
    FieldSchema("cid", int, 50, 10, None),
    FieldSchema("nid", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_CARD1 = (
    FieldSchema("xc", float, 0, 10, 0.0),
    FieldSchema("yc", float, 10, 10, 1e+20),
    FieldSchema("zc", float, 20, 10, None),
    FieldSchema("dnx", float, 30, 10, None),
    FieldSchema("dny", float, 40, 10, None),
    FieldSchema("dnz", float, 50, 10, None),
    FieldSchema("tbeg", float, 60, 10, None),
    FieldSchema("tend", float, 70, 10, None),
)

_DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphInjectionSimplifiedFlowrate(KeywordBase):
    """DYNA DEFINE_SPH_INJECTION_SIMPLIFIED_FLOWRATE keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_INJECTION_SIMPLIFIED_FLOWRATE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "nid": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSphInjectionSimplifiedFlowrate class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSphInjectionSimplifiedFlowrate._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHINJECTIONSIMPLIFIEDFLOWRATE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of newly generated SPH elements.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def flwrt(self) -> typing.Optional[float]:
        """Get or set the this field is interpreted as the volumetric flow rate of injection.
        GT.0: The volumetric flowrate is constant, equal to FLWRT.
        LT.0: | FLWRT | is a curve ID defining the volumetric flowrate with respect to time, for variable injection flowrate.
        """ # nopep8
        return self._cards[0].get_value("flwrt")

    @flwrt.setter
    def flwrt(self, value: float) -> None:
        """Set the flwrt property."""
        self._cards[0].set_value("flwrt", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Radius of the circular injection Particles are uniformly generated with a spacing of PSIZE
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[0].set_value("radius", value)

    @property
    def psize(self) -> typing.Optional[float]:
        """Get or set the Spacing between particles on the injection surface.
        """ # nopep8
        return self._cards[0].get_value("psize")

    @psize.setter
    def psize(self, value: float) -> None:
        """Set the psize property."""
        self._cards[0].set_value("psize", value)

    @property
    def offst(self) -> typing.Optional[float]:
        """Get or set the Option to offset the centroid of the injection surface along the normal vector by a distance of OFFST. Only valid when CID is zero.
        """ # nopep8
        return self._cards[0].get_value("offst")

    @offst.setter
    def offst(self, value: float) -> None:
        """Set the offst property."""
        self._cards[0].set_value("offst", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID; see *DEFINE_COORDINATE_SYSTEM for example. The origin of this local coordinate system defines the centroid of the injection area, and the local z-direction defines the normal to the injection area, which is the direction in which particles will be injected.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Optional Node ID. If defined, the injection plane will follow the motion of this node.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def xc(self) -> float:
        """Get or set the Centroid of the injection surface. Ignored if CID is nonzero
        """ # nopep8
        return self._cards[1].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[1].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Centroid of the injection surface. Ignored if CID is nonzero
        """ # nopep8
        return self._cards[1].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[1].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the Centroid of the injection surface. Ignored if CID is nonzero
        """ # nopep8
        return self._cards[1].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[1].set_value("zc", value)

    @property
    def dnx(self) -> typing.Optional[float]:
        """Get or set the Components of the normal vector to the injection surface. Ignored if CID is nonzero.
        """ # nopep8
        return self._cards[1].get_value("dnx")

    @dnx.setter
    def dnx(self, value: float) -> None:
        """Set the dnx property."""
        self._cards[1].set_value("dnx", value)

    @property
    def dny(self) -> typing.Optional[float]:
        """Get or set the Components of the normal vector to the injection surface. Ignored if CID is nonzero.
        """ # nopep8
        return self._cards[1].get_value("dny")

    @dny.setter
    def dny(self, value: float) -> None:
        """Set the dny property."""
        self._cards[1].set_value("dny", value)

    @property
    def dnz(self) -> typing.Optional[float]:
        """Get or set the Components of the normal vector to the injection surface. Ignored if CID is nonzero.
        """ # nopep8
        return self._cards[1].get_value("dnz")

    @dnz.setter
    def dnz(self, value: float) -> None:
        """Set the dnz property."""
        self._cards[1].set_value("dnz", value)

    @property
    def tbeg(self) -> typing.Optional[float]:
        """Get or set the Birth time
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Death time
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

