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

"""Module providing the DefineIspgToSurfaceCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEISPGTOSURFACECOUPLING_CARD0 = (
    FieldSchema("fp", int, 0, 10, None),
    FieldSchema("surf", int, 10, 10, None),
    FieldSchema("fptype", int, 20, 10, 0),
    FieldSchema("surftype", int, 30, 10, None),
    FieldSchema("cpl_id", int, 40, 10, None),
)

_DEFINEISPGTOSURFACECOUPLING_CARD1 = (
    FieldSchema("sbc", int, 0, 10, 0),
    FieldSchema("sca", float, 10, 10, None),
    FieldSchema("aca", float, 20, 10, None),
    FieldSchema("rca", float, 30, 10, None),
    FieldSchema("vca", float, 40, 10, 1.0),
    FieldSchema("sfpn", float, 50, 10, 1.0),
    FieldSchema("thk", float, 60, 10, 0.5),
    FieldSchema("lslp", float, 70, 10, 1.0),
)

_DEFINEISPGTOSURFACECOUPLING_CARD2 = (
    FieldSchema("htc", float, 0, 10, 2e-05),
)

_DEFINEISPGTOSURFACECOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineIspgToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_ISPG_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "ISPG_TO_SURFACE_COUPLING"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "surf": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineIspgToSurfaceCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEISPGTOSURFACECOUPLING_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEISPGTOSURFACECOUPLING_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEISPGTOSURFACECOUPLING_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineIspgToSurfaceCoupling._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEISPGTOSURFACECOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def fp(self) -> typing.Optional[int]:
        """Get or set the Part ID/Part set ID for the fluid particles.
        """ # nopep8
        return self._cards[0].get_value("fp")

    @fp.setter
    def fp(self, value: int) -> None:
        """Set the fp property."""
        self._cards[0].set_value("fp", value)

    @property
    def surf(self) -> typing.Optional[int]:
        """Get or set the Segment set ID specifying the surface. Currently the segment set should be generated from the 8-noded hexahedral elements.
        """ # nopep8
        return self._cards[0].get_value("surf")

    @surf.setter
    def surf(self, value: int) -> None:
        """Set the surf property."""
        self._cards[0].set_value("surf", value)

    @property
    def fptype(self) -> int:
        """Get or set the Type for FP:
        EQ.0: Part set ID
        EQ.1: Part ID
        """ # nopep8
        return self._cards[0].get_value("fptype")

    @fptype.setter
    def fptype(self, value: int) -> None:
        """Set the fptype property."""
        if value not in [0, 1, None]:
            raise Exception("""fptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fptype", value)

    @property
    def surftype(self) -> typing.Optional[int]:
        """Get or set the Type for SURF:
        EQ.0: Segment set ID
        """ # nopep8
        return self._cards[0].get_value("surftype")

    @surftype.setter
    def surftype(self, value: int) -> None:
        """Set the surftype property."""
        self._cards[0].set_value("surftype", value)

    @property
    def cpl_id(self) -> typing.Optional[int]:
        """Get or set the ID of the coupling
        """ # nopep8
        return self._cards[0].get_value("cpl_id")

    @cpl_id.setter
    def cpl_id(self, value: int) -> None:
        """Set the cpl_id property."""
        self._cards[0].set_value("cpl_id", value)

    @property
    def sbc(self) -> int:
        """Get or set the Boundary condition type for the contact edges (see Figure 0-1) and contact treatment for the non-contact-edge ISPG nodes:
        EQ.0:Contact edges can move on the structure�s surface, while non - contact - edge ISPG nodes in contact with the structure are tied to the structure, modeling a non - slip boundary condition.
        EQ.1:All ISPG nodes in contact with the structure, including those on the contact edges, are tied to the structure.
        EQ.2: A free slip condition applies to all ISPG nodes in contact with the structure, including those on the contact edges,
        """ # nopep8
        return self._cards[1].get_value("sbc")

    @sbc.setter
    def sbc(self, value: int) -> None:
        """Set the sbc property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sbc must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("sbc", value)

    @property
    def sca(self) -> typing.Optional[float]:
        """Get or set the Static (equilibrium) contact angle in radians
        """ # nopep8
        return self._cards[1].get_value("sca")

    @sca.setter
    def sca(self, value: float) -> None:
        """Set the sca property."""
        self._cards[1].set_value("sca", value)

    @property
    def aca(self) -> typing.Optional[float]:
        """Get or set the Advancing contact angle in radians. If set to zero, then the value of SCA is used
        """ # nopep8
        return self._cards[1].get_value("aca")

    @aca.setter
    def aca(self, value: float) -> None:
        """Set the aca property."""
        self._cards[1].set_value("aca", value)

    @property
    def rca(self) -> typing.Optional[float]:
        """Get or set the Receding contact angle in radians. If set to zero, then the value of SCA is used
        """ # nopep8
        return self._cards[1].get_value("rca")

    @rca.setter
    def rca(self, value: float) -> None:
        """Set the rca property."""
        self._cards[1].set_value("rca", value)

    @property
    def vca(self) -> float:
        """Get or set the Velocity of the fluid contact edge for measurement of the contact angles.
        """ # nopep8
        return self._cards[1].get_value("vca")

    @vca.setter
    def vca(self, value: float) -> None:
        """Set the vca property."""
        self._cards[1].set_value("vca", value)

    @property
    def sfpn(self) -> float:
        """Get or set the Scale factor for the thickness of the solid elements coupled with the  ISPG fluid. This parameter can increase the contact detection thickness if the length of the solid element along the contact normal direction is much less than the length in the in-plane direction.
        """ # nopep8
        return self._cards[1].get_value("sfpn")

    @sfpn.setter
    def sfpn(self, value: float) -> None:
        """Set the sfpn property."""
        self._cards[1].set_value("sfpn", value)

    @property
    def thk(self) -> float:
        """Get or set the Scale factor on the average ISPG element edge length for contact detection
        """ # nopep8
        return self._cards[1].get_value("thk")

    @thk.setter
    def thk(self, value: float) -> None:
        """Set the thk property."""
        self._cards[1].set_value("thk", value)

    @property
    def lslp(self) -> float:
        """Get or set the Slip length, Lslip, for the Navier slip model (see Figure 0-2). A smaller slip length causes more friction. By default, LSLP = 0.0, meaning no Navier slip force is calculated. This field is ignored for SBC = 1.
        """ # nopep8
        return self._cards[1].get_value("lslp")

    @lslp.setter
    def lslp(self, value: float) -> None:
        """Set the lslp property."""
        self._cards[1].set_value("lslp", value)

    @property
    def htc(self) -> float:
        """Get or set the Heat transfer coefficient for the thermal boundary condition between the ISPG nodes and the structural surface if performing a coupled thermal-flow analysis. The default value is 2.0 � 10-5 kW/(mm2K) with its base units assumed to be kg, mm, ms, and K.
        """ # nopep8
        return self._cards[2].get_value("htc")

    @htc.setter
    def htc(self, value: float) -> None:
        """Set the htc property."""
        self._cards[2].set_value("htc", value)

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

    @property
    def surf_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for surf."""
        return self._get_set_link("SEGMENT", self.surf)

    @surf_link.setter
    def surf_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for surf."""
        self.surf = value.sid

