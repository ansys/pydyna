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

"""Module providing the ContactRigidSurface class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTRIGIDSURFACE_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("psid", int, 10, 10, None),
    FieldSchema("boxid", int, 20, 10, 0),
    FieldSchema("segid", int, 30, 10, None),
    FieldSchema("fs", float, 40, 10, 0.0),
    FieldSchema("fd", float, 50, 10, 0.0),
    FieldSchema("dc", float, 60, 10, 0.0),
    FieldSchema("vc", float, 70, 10, 0.0),
)

_CONTACTRIGIDSURFACE_CARD1 = (
    FieldSchema("lcidx", int, 0, 10, 0),
    FieldSchema("lcidy", int, 10, 10, 0),
    FieldSchema("lcidz", int, 20, 10, 0),
    FieldSchema("fslcid", int, 30, 10, 0),
    FieldSchema("fdlcid", int, 40, 10, 0),
)

_CONTACTRIGIDSURFACE_CARD2 = (
    FieldSchema("sfs", float, 0, 10, 1.0),
    FieldSchema("stthk", float, 10, 10, 0.0),
    FieldSchema("sfthk", float, 20, 10, 1.0),
    FieldSchema("xpene", float, 30, 10, 4.0),
    FieldSchema("bsort", float, 40, 10, 10.0),
    FieldSchema("ctype", int, 50, 10, 0),
)

class ContactRigidSurface(KeywordBase):
    """DYNA CONTACT_RIGID_SURFACE keyword"""

    keyword = "CONTACT"
    subkeyword = "RIGID_SURFACE"

    def __init__(self, **kwargs):
        """Initialize the ContactRigidSurface class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTRIGIDSURFACE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTRIGIDSURFACE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTRIGIDSURFACE_CARD2,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact interface ID. This must be a unique number.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of all parts that may contact the rigid surface. See *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Include only nodes of the part set that are within the specified box, see *DEFINE_BOX, in contact definition.
        EQ.0: all nodes from the part set, PSID, will be included in the contact (default).
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def segid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID defining the rigid surface. See *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("segid")

    @segid.setter
    def segid(self, value: int) -> None:
        """Set the segid property."""
        self._cards[0].set_value("segid", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[0].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient.
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[0].set_value("dc", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction.
        """ # nopep8
        return self._cards[0].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        """Set the vc property."""
        self._cards[0].set_value("vc", value)

    @property
    def lcidx(self) -> int:
        """Get or set the Load curve ID defining x-direction motion.
        EQ.0: There is no motion in the x-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        """Set the lcidx property."""
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> int:
        """Get or set the Load curve ID defining y-direction motion.
        EQ.0: There is no motion in the y-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> int:
        """Get or set the Load curve ID defining z-direction motion.
        EQ.0: There is no motion in the z-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[1].set_value("lcidz", value)

    @property
    def fslcid(self) -> int:
        """Get or set the Load curve ID defining the static coefficient of friction as a function of interface pressure. This option applies to shell segments only.
        """ # nopep8
        return self._cards[1].get_value("fslcid")

    @fslcid.setter
    def fslcid(self, value: int) -> None:
        """Set the fslcid property."""
        self._cards[1].set_value("fslcid", value)

    @property
    def fdlcid(self) -> int:
        """Get or set the Load curve ID defining the dynamic coefficient of friction as a function of interface pressure. This option applies to shell segments only.
        """ # nopep8
        return self._cards[1].get_value("fdlcid")

    @fdlcid.setter
    def fdlcid(self, value: int) -> None:
        """Set the fdlcid property."""
        self._cards[1].set_value("fdlcid", value)

    @property
    def sfs(self) -> float:
        """Get or set the Scale factor on default slave penalty stiffness, see also *CONTROL_ CONTACT.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[2].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        """Set the sfs property."""
        self._cards[2].set_value("sfs", value)

    @property
    def stthk(self) -> float:
        """Get or set the Optional thickness for slave surface (overrides true thickness). This option applies to contact with shell, solid, and beam elements. True thickness is the element thickness of the shell elements. Thickness offsets are not used for solid element unless this option is specified.
        Default is set to 0.0.
        """ # nopep8
        return self._cards[2].get_value("stthk")

    @stthk.setter
    def stthk(self, value: float) -> None:
        """Set the stthk property."""
        self._cards[2].set_value("stthk", value)

    @property
    def sfthk(self) -> float:
        """Get or set the Scale factor for slave surface thickness (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
        Default is set to 1.0
        """ # nopep8
        return self._cards[2].get_value("sfthk")

    @sfthk.setter
    def sfthk(self, value: float) -> None:
        """Set the sfthk property."""
        self._cards[2].set_value("sfthk", value)

    @property
    def xpene(self) -> float:
        """Get or set the Contact surface maximum penetration check multiplier. If the penetration of a node through the rigid surface exceeds the product of XPENE and the slave node thickness, the node is set free.
        Default is set to 4.0.
        """ # nopep8
        return self._cards[2].get_value("xpene")

    @xpene.setter
    def xpene(self, value: float) -> None:
        """Set the xpene property."""
        self._cards[2].set_value("xpene", value)

    @property
    def bsort(self) -> float:
        """Get or set the Number of cycles between bucket sorts. The default value is set to 10.0 but can be much larger, e.g., 50-100, for fully connected surfaces.
        """ # nopep8
        return self._cards[2].get_value("bsort")

    @bsort.setter
    def bsort(self, value: float) -> None:
        """Set the bsort property."""
        self._cards[2].set_value("bsort", value)

    @property
    def ctype(self) -> int:
        """Get or set the The contact formulation. The default, CTYPE=0, is equivalent to the ONE_WAY_SURFACE_TO_SURFACE formulation, and CTYPE=1 is a penalty formulation. If the slave surface belongs to a rigid body, CTYPE=1 must be used.
        """ # nopep8
        return self._cards[2].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [0, 1, None]:
            raise Exception("""ctype must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ctype", value)

