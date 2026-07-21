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

"""Module providing the InterfaceCompensation3DMultiSteps class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INTERFACECOMPENSATION3DMULTISTEPS_CARD0 = (
    FieldSchema("method", int, 0, 10, 6),
    FieldSchema("sl", float, 10, 10, 5.0),
    FieldSchema("sf", float, 20, 10, 0.75),
    FieldSchema("elref", int, 30, 10, 1),
    FieldSchema("psidp", float, 40, 10, None),
    FieldSchema("undct", float, 50, 10, None),
    FieldSchema("angle", float, 60, 10, 0.0),
    FieldSchema("nlinear", int, 70, 10, 1),
)

_INTERFACECOMPENSATION3DMULTISTEPS_CARD1 = (
    FieldSchema("tangent", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("fxnsid", int, 60, 10, None),
    FieldSchema("trrnsid", int, 70, 10, None),
)

_INTERFACECOMPENSATION3DMULTISTEPS_CARD2 = (
    FieldSchema("vx", float, 0, 10, None),
    FieldSchema("vy", float, 10, 10, None),
    FieldSchema("vz", float, 20, 10, None),
    FieldSchema("gap0", float, 30, 10, None),
)

class InterfaceCompensation3DMultiSteps(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_3D_MULTI_STEPS keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_3D_MULTI_STEPS"
    _link_fields = {
        "fxnsid": LinkType.SET_NODE,
        "trrnsid": LinkType.SET_NODE,
        "psidp": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InterfaceCompensation3DMultiSteps class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION3DMULTISTEPS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION3DMULTISTEPS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _INTERFACECOMPENSATION3DMULTISTEPS_CARD2,
                **kwargs,
            ),
        ]
    @property
    def method(self) -> int:
        """Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, . See Remark 1 for a discussion of available method
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        """Set the method property."""
        self._cards[0].set_value("method", value)

    @property
    def sl(self) -> float:
        """Get or set the The smooth level parameter controls the smoothness of the modified
        surfaces. A large value makes the surface smoother. Typically the value ranges from 5 to 10. If spring back is large, the transition
        region is expected to be large. However, by using a smaller value of SL, the region of transition can be reduced.
        """ # nopep8
        return self._cards[0].get_value("sl")

    @sl.setter
    def sl(self, value: float) -> None:
        """Set the sl property."""
        self._cards[0].set_value("sl", value)

    @property
    def sf(self) -> float:
        """Get or set the Shape compensation scale factor. The value scales the spring back
        amount of the blank and the scaled amount is used to compensate the tooling.
        GT.0: compensate in the opposite direction of the spring back;
        LT.0: compensate in the punch moving direction (for undercut).
        This scale factor scales how much of the shape deviation is
        compensated. For example, if 10 mm of spring back is predicted,
        and the scale factor is chosen as 0.75, then the compensation in the
        opposite direction will only be 7.5 mm.
        Experience shows that the best scale factor for reaching a converged
        solution (within part tolerance) is case dependent. In some cases, a
        scale factor range of 0.5 to 0.75 is best; while in others, larger values
        are indicated. Sometimes, the best value can be larger than 1.1.
        Note that within an automatic compensation loop, this factor does  not need to be varied.
        Since it is impossible to choose the best value for each application up
        front 0.75 is recommended for the first attempt. If the spring back
        cannot be effectively compensated and the calculation diverges, the
        factor can be moved upward or downward to obtain a converged
        solution, or more iterations must be used with the initial trial value
        to compensate the remaining shape deviation.
        For channel shaped parts that have a twisting mode of spring back,
        the scale factor is more important. It was found that a small change
        of the tool shape might change the twisting mode. If this occurs,
        using a small value (<0.5) is suggested.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def elref(self) -> int:
        """Get or set the Element refinement option:
        EQ.1: special element refinement is used with the tool elements (default);
        EQ.2: special element refinement is turned off.
        """ # nopep8
        return self._cards[0].get_value("elref")

    @elref.setter
    def elref(self, value: int) -> None:
        """Set the elref property."""
        if value not in [1, 2, None]:
            raise Exception("""elref must be `None` or one of {1,2}.""")
        self._cards[0].set_value("elref", value)

    @property
    def psidp(self) -> typing.Optional[float]:
        """Get or set the Define the part set ID for primary parts of the tooling.  Properly choosing the parts for the primary side is important, as it affects the kinds of modifications made to the tooling. Usually, only one side of the tool is chosen as the primary side, and modifications to the other side (the secondary side) depend solely on changes to the primary side.  This specification allows the two sides to be coupled while maintaining a constant (tool) gap between them.  If both sides are chosen as primary, the gap between them might change and become inhomogeneous.
        When using METHOD 7, the choice of primary side affects results for three - piece draw models.At this time, when the punch and binder are chosen as the primary side, the binder region is not changed.Otherwise, when the die is chosen as the primary side, the binder is changed since the changes extend to the edges of the primary tool.
        """ # nopep8
        return self._cards[0].get_value("psidp")

    @psidp.setter
    def psidp(self, value: float) -> None:
        """Set the psidp property."""
        self._cards[0].set_value("psidp", value)

    @property
    def undct(self) -> typing.Optional[float]:
        """Get or set the Tool undercut treatment option:
        EQ.0: no check (default);
        EQ.1: check and fix undercut.
        """ # nopep8
        return self._cards[0].get_value("undct")

    @undct.setter
    def undct(self, value: float) -> None:
        """Set the undct property."""
        self._cards[0].set_value("undct", value)

    @property
    def angle(self) -> float:
        """Get or set the An angle defining the undercut.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        """Set the angle property."""
        self._cards[0].set_value("angle", value)

    @property
    def nlinear(self) -> int:
        """Get or set the Activate nonlinear extrapolation.
        """ # nopep8
        return self._cards[0].get_value("nlinear")

    @nlinear.setter
    def nlinear(self, value: int) -> None:
        """Set the nlinear property."""
        self._cards[0].set_value("nlinear", value)

    @property
    def tangent(self) -> int:
        """Get or set the A flag to maintain tangency during the compensation.  Set TANGENT = 1 to maintain the tangential transition between the compensated and non-compensated areas of the rigid tool (for example, between addendum and binder in METHOD 7), and between the rigid tool area at the trim curves and the addendum part of the tool.  See Remark
        """ # nopep8
        return self._cards[1].get_value("tangent")

    @tangent.setter
    def tangent(self, value: int) -> None:
        """Set the tangent property."""
        self._cards[1].set_value("tangent", value)

    @property
    def fxnsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID specifying the nodes that are fixed during springback compensation. If 0, these nodes are internally determined.
        """ # nopep8
        return self._cards[1].get_value("fxnsid")

    @fxnsid.setter
    def fxnsid(self, value: int) -> None:
        """Set the fxnsid property."""
        self._cards[1].set_value("fxnsid", value)

    @property
    def trrnsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID specifying the transitional region during compensation. If 0, these nodes are internally determined.
        """ # nopep8
        return self._cards[1].get_value("trrnsid")

    @trrnsid.setter
    def trrnsid(self, value: int) -> None:
        """Set the trrnsid property."""
        self._cards[1].set_value("trrnsid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Components of the direction the blank needs to move to sit on the post
        """ # nopep8
        return self._cards[2].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[2].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Components of the direction the blank needs to move to sit on the post
        """ # nopep8
        return self._cards[2].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[2].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Components of the direction the blank needs to move to sit on the post
        """ # nopep8
        return self._cards[2].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[2].set_value("vz", value)

    @property
    def gap0(self) -> typing.Optional[float]:
        """Get or set the Distance between the initial position of the blank and the post
        """ # nopep8
        return self._cards[2].get_value("gap0")

    @gap0.setter
    def gap0(self, value: float) -> None:
        """Set the gap0 property."""
        self._cards[2].set_value("gap0", value)

    @property
    def fxnsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for fxnsid."""
        return self._get_set_link("NODE", self.fxnsid)

    @fxnsid_link.setter
    def fxnsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for fxnsid."""
        self.fxnsid = value.sid

    @property
    def trrnsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for trrnsid."""
        return self._get_set_link("NODE", self.trrnsid)

    @trrnsid_link.setter
    def trrnsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for trrnsid."""
        self.trrnsid = value.sid

    @property
    def psidp_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psidp."""
        return self._get_set_link("PART", self.psidp)

    @psidp_link.setter
    def psidp_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psidp."""
        self.psidp = value.sid

