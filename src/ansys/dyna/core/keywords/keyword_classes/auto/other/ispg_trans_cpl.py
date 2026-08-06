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

"""Module providing the IspgTransCpl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ISPGTRANSCPL_CARD0 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("pid3", int, 20, 10, None),
    FieldSchema("pid4", int, 30, 10, None),
    FieldSchema("pid5", int, 40, 10, None),
    FieldSchema("pid6", int, 50, 10, None),
    FieldSchema("pid7", int, 60, 10, None),
    FieldSchema("pid8", int, 70, 10, None),
)

_ISPGTRANSCPL_CARD1 = (
    FieldSchema("dofx", int, 0, 10, 0),
    FieldSchema("dofy", int, 10, 10, 0),
    FieldSchema("dofz", int, 20, 10, 0),
    FieldSchema("dofrx", int, 30, 10, 0),
    FieldSchema("dofry", int, 40, 10, 0),
    FieldSchema("dofrz", int, 50, 10, 0),
    FieldSchema("birth", float, 60, 10, None),
    FieldSchema("thk_min", float, 70, 10, None),
)

class IspgTransCpl(KeywordBase):
    """DYNA ISPG_TRANS_CPL keyword"""

    keyword = "ISPG"
    subkeyword = "TRANS_CPL"
    _link_fields = {
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
        "pid3": LinkType.PART,
        "pid4": LinkType.PART,
        "pid5": LinkType.PART,
        "pid6": LinkType.PART,
        "pid7": LinkType.PART,
        "pid8": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IspgTransCpl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGTRANSCPL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ISPGTRANSCPL_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        """Set the pid3 property."""
        self._cards[0].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        """Set the pid4 property."""
        self._cards[0].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        """Set the pid5 property."""
        self._cards[0].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        """Set the pid6 property."""
        self._cards[0].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        """Set the pid7 property."""
        self._cards[0].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Part IDs for the rigid bodies that are coupled to the ISPG fluid
        """ # nopep8
        return self._cards[0].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        """Set the pid8 property."""
        self._cards[0].set_value("pid8", value)

    @property
    def dofx(self) -> int:
        """Get or set the Flag to specify if the translational X degree of freedom is fixed for the rigid body motion:
        EQ.0:Not fixed
        EQ.1:Fixed
        """ # nopep8
        return self._cards[1].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        """Set the dofx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofx must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the Flag to specify if the translational Y degree of freedom is fixed for the rigid body motion:
        EQ.0:Not fixed
        EQ.1:Fixed
        """ # nopep8
        return self._cards[1].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        """Set the dofy property."""
        if value not in [0, 1, None]:
            raise Exception("""dofy must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the Flag to specify if the translational Z degree of freedom is fixed for the rigid body motion:
        EQ.0:Not fixed
        EQ.1:Fixed
        """ # nopep8
        return self._cards[1].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        """Set the dofz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofz must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofz", value)

    @property
    def dofrx(self) -> int:
        """Get or set the Flag to specify if the rotational X degree of freedom is fixed for the rigid body motion:
        EQ.0:	Not fixed
        EQ.1:	Fixed
        """ # nopep8
        return self._cards[1].get_value("dofrx")

    @dofrx.setter
    def dofrx(self, value: int) -> None:
        """Set the dofrx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrx must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofrx", value)

    @property
    def dofry(self) -> int:
        """Get or set the Flag to specify if the rotational Y degree of freedom is fixed for the rigid body motion:
        EQ.0:	Not fixed
        EQ.1:	Fixed
        """ # nopep8
        return self._cards[1].get_value("dofry")

    @dofry.setter
    def dofry(self, value: int) -> None:
        """Set the dofry property."""
        if value not in [0, 1, None]:
            raise Exception("""dofry must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofry", value)

    @property
    def dofrz(self) -> int:
        """Get or set the Flag to specify if the rotational Z degree of freedom is fixed for the rigid body motion:
        EQ.0:	Not fixed
        EQ.1:	Fixed
        """ # nopep8
        return self._cards[1].get_value("dofrz")

    @dofrz.setter
    def dofrz(self, value: int) -> None:
        """Set the dofrz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrz must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dofrz", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Birth time used to activate the coupling algorithm. After BIRTH, the general rigid motion defined via *BOUNDARY_PRESCRIBED_MOTION_RIGID and *BOUNDARY_SPC_OPTION is deactivated. Do not use the DEATH option in the keyword *BOUNDARY_PRESCRIBED_MOTION_RIGID because it can lead to unpredictable behavior.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def thk_min(self) -> typing.Optional[float]:
        """Get or set the Variable used to define the minimum thickness of the fluid under the compression of rigid bodies. If the thickness of the adhesive reaches THK_MIN anywhere in the adhesive due to compression from the rigid parts, the motion of the rigid part is stopped. This variable can be used to avoid the penetration among rigid parts that are in contact with the ISPG fluid. If THK_MIN < 1.0 �10-20, the penetration detection algorithm is deactivated.
        """ # nopep8
        return self._cards[1].get_value("thk_min")

    @thk_min.setter
    def thk_min(self, value: float) -> None:
        """Set the thk_min property."""
        self._cards[1].set_value("thk_min", value)

    @property
    def pid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

    @property
    def pid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid3."""
        return self._get_link_by_attr("PART", "pid", self.pid3, "parts")

    @property
    def pid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid4."""
        return self._get_link_by_attr("PART", "pid", self.pid4, "parts")

    @property
    def pid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid5."""
        return self._get_link_by_attr("PART", "pid", self.pid5, "parts")

    @property
    def pid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid6."""
        return self._get_link_by_attr("PART", "pid", self.pid6, "parts")

    @property
    def pid7_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid7."""
        return self._get_link_by_attr("PART", "pid", self.pid7, "parts")

    @property
    def pid8_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid8."""
        return self._get_link_by_attr("PART", "pid", self.pid8, "parts")

