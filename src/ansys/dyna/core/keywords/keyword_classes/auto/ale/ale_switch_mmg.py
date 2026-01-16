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

"""Module providing the AleSwitchMmg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ALESWITCHMMG_CARD0 = (
    FieldSchema("fr_mmg", int, 0, 10, None),
    FieldSchema("to_mmg", int, 10, 10, None),
    FieldSchema("idfunc", int, 20, 10, None),
    FieldSchema("idsegset", int, 30, 10, 0),
    FieldSchema("idsldset", int, 40, 10, 0),
    FieldSchema("ncycseg", int, 50, 10, 50),
    FieldSchema("ncycsld", int, 60, 10, 50),
)

_ALESWITCHMMG_CARD1 = (
    FieldSchema("var1", int, 0, 10, 0),
    FieldSchema("var2", int, 10, 10, 0),
    FieldSchema("var3", int, 20, 10, 0),
    FieldSchema("var4", int, 30, 10, 0),
    FieldSchema("var5", int, 40, 10, 0),
    FieldSchema("var6", int, 50, 10, 0),
    FieldSchema("var7", int, 60, 10, 0),
    FieldSchema("var8", int, 70, 10, 0),
)

class AleSwitchMmg(KeywordBase):
    """DYNA ALE_SWITCH_MMG keyword"""

    keyword = "ALE"
    subkeyword = "SWITCH_MMG"
    _link_fields = {
        "idsegset": LinkType.SET_SEGMENT,
        "idsldset": LinkType.SET_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the AleSwitchMmg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESWITCHMMG_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALESWITCHMMG_CARD1,
                **kwargs,
            ),        ]
    @property
    def fr_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMG-SID before the switch. The AMMG-SID
        corresponds to the SID defined on a *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.
        This SID refers to one or more AMMGs.
        """ # nopep8
        return self._cards[0].get_value("fr_mmg")

    @fr_mmg.setter
    def fr_mmg(self, value: int) -> None:
        """Set the fr_mmg property."""
        self._cards[0].set_value("fr_mmg", value)

    @property
    def to_mmg(self) -> typing.Optional[int]:
        """Get or set the This is the AMMG-SID after the switch. The AMMG-SID
        corresponds to the SID defined on a *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.
        This SID refers to one or more AMMGs.
        """ # nopep8
        return self._cards[0].get_value("to_mmg")

    @to_mmg.setter
    def to_mmg(self, value: int) -> None:
        """Set the to_mmg property."""
        self._cards[0].set_value("to_mmg", value)

    @property
    def idfunc(self) -> typing.Optional[int]:
        """Get or set the ID of a *DEFINE_FUNCTION function. This function determines
        the material fraction to be switched.
        """ # nopep8
        return self._cards[0].get_value("idfunc")

    @idfunc.setter
    def idfunc(self, value: int) -> None:
        """Set the idfunc property."""
        self._cards[0].set_value("idfunc", value)

    @property
    def idsegset(self) -> int:
        """Get or set the ID of *SEGMENT_SET that is used to pass geometric properties to
        the function specified by IDFUNC. This field is optional.
        The segment center positions and normal vectors are computed.
        For each ALE element, this data is passed to the function
        IDFUNC for the segment the closest to the element center.
        """ # nopep8
        return self._cards[0].get_value("idsegset")

    @idsegset.setter
    def idsegset(self, value: int) -> None:
        """Set the idsegset property."""
        self._cards[0].set_value("idsegset", value)

    @property
    def idsldset(self) -> int:
        """Get or set the The ID of a *SOLID_SET specifying which elements are affected
        by this particular instance of the *ALE_SWITCH_MMG keyword.
        This field is optional. If undefined, *ALE_SWITCH_MMG affects
        all ALE elements. The element centers are computed and can be
        used as variables in the function IDFUNC.
        """ # nopep8
        return self._cards[0].get_value("idsldset")

    @idsldset.setter
    def idsldset(self, value: int) -> None:
        """Set the idsldset property."""
        self._cards[0].set_value("idsldset", value)

    @property
    def ncycseg(self) -> int:
        """Get or set the Number of cycles between each update of the segment centers
        and normal vectors (if a segment set is defined). For each update,
        a bucket sort is applied to find the closest segment to each ALE
        element. If the segment nodes are fully constrained, the segment
        centers and normal vectors are computed only one time.
        """ # nopep8
        return self._cards[0].get_value("ncycseg")

    @ncycseg.setter
    def ncycseg(self, value: int) -> None:
        """Set the ncycseg property."""
        self._cards[0].set_value("ncycseg", value)

    @property
    def ncycsld(self) -> int:
        """Get or set the Number of cycles between each update of the ALE element
        centers. For each update, a bucket sort is applied to find the
        closest segment to each ALE element. If the element nodes does
        not move (as with AFAC = -1 in *CONTROL_ALE) the element
        centers are computed exactly once.
        """ # nopep8
        return self._cards[0].get_value("ncycsld")

    @ncycsld.setter
    def ncycsld(self, value: int) -> None:
        """Set the ncycsld property."""
        self._cards[0].set_value("ncycsld", value)

    @property
    def var1(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var1")

    @var1.setter
    def var1(self, value: int) -> None:
        """Set the var1 property."""
        self._cards[1].set_value("var1", value)

    @property
    def var2(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var2")

    @var2.setter
    def var2(self, value: int) -> None:
        """Set the var2 property."""
        self._cards[1].set_value("var2", value)

    @property
    def var3(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var3")

    @var3.setter
    def var3(self, value: int) -> None:
        """Set the var3 property."""
        self._cards[1].set_value("var3", value)

    @property
    def var4(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var4")

    @var4.setter
    def var4(self, value: int) -> None:
        """Set the var4 property."""
        self._cards[1].set_value("var4", value)

    @property
    def var5(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var5")

    @var5.setter
    def var5(self, value: int) -> None:
        """Set the var5 property."""
        self._cards[1].set_value("var5", value)

    @property
    def var6(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var6")

    @var6.setter
    def var6(self, value: int) -> None:
        """Set the var6 property."""
        self._cards[1].set_value("var6", value)

    @property
    def var7(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var7")

    @var7.setter
    def var7(self, value: int) -> None:
        """Set the var7 property."""
        self._cards[1].set_value("var7", value)

    @property
    def var8(self) -> int:
        """Get or set the Variable rank in the following list (See Remark 2):
        EQ.0: See Remark 3
        EQ.1: ....-stress for FR_MMG
        EQ.2: ....-stress for FR_MMG
        EQ.3: ....-stress for FR_MMG
        EQ.4: ....-stress for FR_MMG
        EQ.5: ....-stress for FR_MMG
        EQ.6: ....-stress for FR_MMG
        EQ.7: plastic strain for FR_MMG
        EQ.8: internal energy for FR_MMG
        EQ.9: bulk viscosity for FR_MMG
        EQ.10: volume from previous cycle for FR_MMG
        GE.11 and LE.20: other auxiliary variables for FR_MMG
        GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
        EQ.41: mass for FR_MMG
        EQ.42: mass for TO_MMG
        EQ.43: volume fraction for FR_MMG
        EQ.44: volume fraction for TO_MMG
        EQ.45: material volume for FR_MMG
        EQ.46: material volume for TO_MMG
        EQ.47: time
        EQ.48: cycle
        EQ.49: x-position of the ALE element center
        EQ.50: y-position of the ALE element center
        EQ.51: z-position of the ALE element center
        EQ.52: x-position of the segment center
        EQ.53: y-position of the segment center
        EQ.54: 𝑧-position of the segment center
        EQ.55: x-component of the segment normal
        EQ.56: y-component of the segment normal
        EQ.57: z-component of the segment normal
        GE.58 and LE.65: x-positions of the ALE nodes
        GE.66 and LE.69: x-positions of the segment nodes
        GE.70 and LE.77: y-positions of the ALE nodes
        GE.79 and LE.81: y-positions of the segment nodes
        GE.83 and LE.89: z-positions of the ALE nodes
        GE.90 and LE.93: z-positions of the segment nodes
        GE.94 and LE.101: x-velocities of the ALE nodes
        GE.102 and LE.105: ..-velocities of the segment nodes
        GE.106 and LE.113: ..-velocities of the ALE nodes
        GE.114 and LE.117: ..-velocities of the segment nodes
        GE.118 and LE.125: ..-velocities of the ALE nodes
        GE.126 and LE.129: ..-velocities of the segment nodes
        GE.130 and LE.137: x-accelerations of the ALE nodes
        GE.138 and LE.141: x-accelerations of the segment nodes
        GE.142 and LE.149: y-accelerations of the ALE nodes
        GE.150 and LE.153: y-accelerations of the segment nodes
        GE.154 and LE.161: z-accelerations of the ALE nodes
        GE.162 and LE.165: z-accelerations of the segment nodes
        GE.166 and LE.173: masses of the ALE nodes
        GE.174 and LE.177: masses of the segment nodes
        EQ.178: rank of the variable updated by the function (See Remark 4)
        EQ.179: rank of the multi-material group in the set
        EQ.180: time step.
        """ # nopep8
        return self._cards[1].get_value("var8")

    @var8.setter
    def var8(self, value: int) -> None:
        """Set the var8 property."""
        self._cards[1].set_value("var8", value)

    @property
    def idsegset_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for idsegset."""
        return self._get_set_link("SEGMENT", self.idsegset)

    @idsegset_link.setter
    def idsegset_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for idsegset."""
        self.idsegset = value.sid

    @property
    def idsldset_link(self) -> KeywordBase:
        """Get the SET_SOLID_* keyword for idsldset."""
        return self._get_set_link("SOLID", self.idsldset)

    @idsldset_link.setter
    def idsldset_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for idsldset."""
        self.idsldset = value.sid

