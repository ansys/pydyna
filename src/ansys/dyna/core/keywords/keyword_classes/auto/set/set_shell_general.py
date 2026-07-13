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

"""Module providing the SetShellGeneral class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETSHELLGENERAL_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
)

_SETSHELLGENERAL_CARD1 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

_SETSHELLGENERAL_CARD2 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("mshid", int, 10, 10, None),
    FieldSchema("imin", int, 20, 10, None),
    FieldSchema("imax", int, 30, 10, None),
    FieldSchema("jmin", int, 40, 10, None),
    FieldSchema("jmax", int, 50, 10, None),
    FieldSchema("__", int, 60, 10, None, "--"),
    FieldSchema("__", int, 70, 10, None, "--"),
)

_SETSHELLGENERAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetShellGeneral(KeywordBase):
    """DYNA SET_SHELL_GENERAL keyword"""

    keyword = "SET"
    subkeyword = "SHELL_GENERAL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetShellGeneral class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETSHELLGENERAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETSHELLGENERAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETSHELLGENERAL_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SetShellGeneral._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETSHELLGENERAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Shell element set ID. All shell sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def option(self) -> str:
        """Get or set the OPTION.EQ.ALL: All shell elements will be included in the set,
        OPTION.EQ.ELEM: Shell elements E1...E7 will be included in the current set,
        OPTION.EQ.DELEM: Shell elements E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from parts E1...E7 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from parts E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside boxes E1...E7 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside boxes E1...E7 previously added will be excluded from the current set.
        OPTION.EQ.SALECPT:Elements inside a box for a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4,and E5 correspond to IMIN, IMAX, JMIN,and JMAX, respectively.They are the minimumand the maximum nodal indices along each direction in the S - ALE mesh. To include all shells in the S-ALE mesh defined by E1, set values E2�E7 to zero or leave them blank. This option is only to be used for a Structured ALE mesh.It can be used with SALEFAC to generate a shell set but should not be used with other "_GENERAL" options.
        OPTION.EQ.SALEFAC:Elements on the face of a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, and E5 correspond to the - X, +X, -Y, and +Y faces, respectively.Assigning 1 to these 4 values would include all the boundary elements at these faces in the shell element set.This option is only to be used for a Structured ALE mesh.It can be used with SALECPT to generate a shell set but should not be used with other "_GENERAL" options.
        Note on trimmed mesh using *ALE_STRUCTURED_MESH_TRIM: An element is taken as a surface element as long as it has no neighboring element along the specified direction.
        For trimmed mesh, all those surface elements facing that direction are picked up, at both exterior and interior boundaries.
        Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details
        OPTION.EQ.SET:Elements of beam element sets E1, E2, E3, ... will be included
        OPTION.EQ.DSET: Previously added elements that are members of beam element sets E1, E2, E3, ... will be excluded.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "ELEM", "DELEM", "PART", "DPART", "BOX", "DBOX", "SALECPT", "SALEFAC", "SALEBOXL", "SET", "DSET", None]:
            raise Exception("""option must be `None` or one of {"ALL","ELEM","DELEM","PART","DPART","BOX","DBOX","SALECPT","SALEFAC","SALEBOXL","SET","DSET"}.""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E1 not used,
        OPTION.EQ.ELEM: Shell element E1 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E1 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E1 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E1 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E1 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E1 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E2 not used,
        OPTION.EQ.ELEM: Shell element E2 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E2 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E2 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E2 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E2 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E2 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E3 not used,
        OPTION.EQ.ELEM: Shell element E3 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E3 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E3 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E3 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E3 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E3 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E4 not used,
        OPTION.EQ.ELEM: Shell element E4 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E4 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E4 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E4 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E4 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E4 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E5 not used,
        OPTION.EQ.ELEM: Shell element E5 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E5 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E5 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E5 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E5 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E5 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E6 not used,
        OPTION.EQ.ELEM: Shell element E6 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E6 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E6 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E6 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E6 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E6 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E7 not used,
        OPTION.EQ.ELEM: Shell element E7 will be included in the current set,
        OPTION.EQ.DELEM: Shell element E7 will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from part E7 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from part E7 will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside box E7 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside box E7 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

    @property
    def option(self) -> str:
        """Get or set the OPTION.EQ.ALL: All shell elements will be included in the set,
        OPTION.EQ.ELEM: Shell elements E1...E7 will be included in the current set,
        OPTION.EQ.DELEM: Shell elements E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.PART: Shell elements from parts E1...E7 will be included in the current set,
        OPTION.EQ.DPART: Shell elements from parts E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.BOX: Shell elements inside boxes E1...E7 will be included in the current set,
        OPTION.EQ.DBOX: Shell elements inside boxes E1...E7 previously added will be excluded from the current set.
        OPTION.EQ.SALECPT:Elements inside a box for a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4,and E5 correspond to IMIN, IMAX, JMIN,and JMAX, respectively.They are the minimumand the maximum nodal indices along each direction in the S - ALE mesh.This option is only to be used for a Structured ALE mesh.It can be used with SALEFAC to generate a shell set but should not be used with other "_GENERAL" options.
        OPTION.EQ.SALEFAC:Elements on the face of a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, and E5 correspond to the - X, +X, -Y, and +Y faces, respectively.Assigning 1 to these 4 values would include all the boundary elements at these faces in the shell element set.This option is only to be used for a Structured ALE mesh.It can be used with SALECPT to generate a shell set but should not be used with other "_GENERAL" options.
        Note on trimmed mesh using *ALE_STRUCTURED_MESH_TRIM: An element is taken as a surface element as long as it has no neighboring element along the specified direction.
        For trimmed mesh, all those surface elements facing that direction are picked up, at both exterior and interior boundaries.
        Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details
        OPTION.EQ.SET:Elements of beam element sets E1, E2, E3, ... will be included
        OPTION.EQ.DSET: Previously added elements that are members of beam element sets E1, E2, E3, ... will be excluded.
        """ # nopep8
        return self._cards[2].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "ELEM", "DELEM", "PART", "DPART", "BOX", "DBOX", "SALECPT", "SALEFAC", "SALEBOXL", "SET", "DSET", None]:
            raise Exception("""option must be `None` or one of {"ALL","ELEM","DELEM","PART","DPART","BOX","DBOX","SALECPT","SALEFAC","SALEBOXL","SET","DSET"}.""")
        self._cards[2].set_value("option", value)

    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE mesh ID.
        """ # nopep8
        return self._cards[2].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[2].set_value("mshid", value)

    @property
    def imin(self) -> typing.Optional[int]:
        """Get or set the The minimum nodal indices along X in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("imin")

    @imin.setter
    def imin(self, value: int) -> None:
        """Set the imin property."""
        self._cards[2].set_value("imin", value)

    @property
    def imax(self) -> typing.Optional[int]:
        """Get or set the The maximum nodal indices along X in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("imax")

    @imax.setter
    def imax(self, value: int) -> None:
        """Set the imax property."""
        self._cards[2].set_value("imax", value)

    @property
    def jmin(self) -> typing.Optional[int]:
        """Get or set the The minimum nodal indices along Y in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("jmin")

    @jmin.setter
    def jmin(self, value: int) -> None:
        """Set the jmin property."""
        self._cards[2].set_value("jmin", value)

    @property
    def jmax(self) -> typing.Optional[int]:
        """Get or set the The maximum nodal indices along Y in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("jmax")

    @jmax.setter
    def jmax(self, value: int) -> None:
        """Set the jmax property."""
        self._cards[2].set_value("jmax", value)

    @property
    def __(self) -> typing.Optional[int]:
        """Get or set the -.
        """ # nopep8
        return self._cards[2].get_value("__")

    @__.setter
    def __(self, value: int) -> None:
        """Set the __ property."""
        self._cards[2].set_value("__", value)

    @property
    def __(self) -> typing.Optional[int]:
        """Get or set the -.
        """ # nopep8
        return self._cards[2].get_value("__")

    @__.setter
    def __(self, value: int) -> None:
        """Set the __ property."""
        self._cards[2].set_value("__", value)

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

