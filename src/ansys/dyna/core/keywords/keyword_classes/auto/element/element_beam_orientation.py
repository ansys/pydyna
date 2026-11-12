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

"""Module providing the ElementBeamOrientation class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

class ElementBeamOrientation(KeywordBase):
    """DYNA ELEMENT_BEAM_ORIENTATION keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEAM_ORIENTATION"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementBeamOrientation class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCardGroup(
                [
                    (
                        FieldSchema("eid", int, 0, 8, None),
                        FieldSchema("pid", int, 8, 8, None),
                        FieldSchema("n1", int, 16, 8, None),
                        FieldSchema("n2", int, 24, 8, None),
                        FieldSchema("n3", int, 32, 8, None),
                        FieldSchema("rt1", int, 40, 8, 0),
                        FieldSchema("rr1", int, 48, 8, 0),
                        FieldSchema("rt2", int, 56, 8, 0),
                        FieldSchema("rr2", int, 64, 8, 0),
                        FieldSchema("local", int, 72, 8, 2),
                    ),
                    (
                        FieldSchema("vx", float, 0, 10, 0.0),
                        FieldSchema("vy", float, 10, 10, 0.0),
                        FieldSchema("vz", float, 20, 10, 0.0),
                    ),
                ],
                None,
                None,
                "elements",
                **kwargs,
            ),
        ]
    @property
    def elements(self) -> pd.DataFrame:
        """Gets the full table of elements."""
        return self._cards[0].table

    @elements.setter
    def elements(self, df: pd.DataFrame):
        """sets elements from the dataframe df."""
        self._cards[0].table = df

    @property
    def eid(self) -> pd.Series:
        """Get or set the Element ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_column("eid")

    @eid.setter
    def eid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the eid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("eid", value)

    @property
    def pid(self) -> pd.Series:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def n1(self) -> pd.Series:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_column("n1")

    @n1.setter
    def n1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n1", value)

    @property
    def n2(self) -> pd.Series:
        """Get or set the Nodal point 2. This node is optional for the spot weld, beam type 9, since if it not defined it will be created automatically and given a nonconfliciting nodal point ID. Nodes N1 and N2 are automatically positioned for the spot weld beam element.
        """ # nopep8
        return self._cards[0].get_column("n2")

    @n2.setter
    def n2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n2", value)

    @property
    def n3(self) -> pd.Series:
        """Get or set the Nodal point 3, the third node, N3, is optional for beam type 3,6,7,8,and 9, if the latter, type 9, has a non-circular cross section. The third node is used for the discrete beam, type 6, if and only if SCOOR is set to 2.0 in the *SECTION_BEAM input, but even in this case it is optional.
        """ # nopep8
        return self._cards[0].get_column("n3")

    @n3.setter
    def n3(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the n3 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("n3", value)

    @property
    def rt1(self) -> pd.Series:
        """Get or set the Release conditions for translations at node N1.
        EQ.0: no translational degrees-of-freedom are released,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x and y-translational degrees-of-freedom,
        EQ.5: y and z-translational degrees-of-freedom,
        EQ.6: z and x-translational degrees-of-freedom,
        EQ.7: x, y, and z-translational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_column("rt1")

    @rt1.setter
    def rt1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rt1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rt1", value)

    @property
    def rr1(self) -> pd.Series:
        """Get or set the Release conditions for rotations at node N1.
        EQ.0: no rotational degrees-of-freedom are released,
        EQ.1: x-rotational degree-of-freedom,
        EQ.2: y-rotational degree-of-freedom,
        EQ.3: z-rotational degree-of-freedom,
        EQ.4: x and y-rotational degrees-of-freedom,
        EQ.5: y and z-rotational degrees-of-freedom,
        EQ.6: z and x-rotational degrees-of-freedom,
        EQ.7: x, y, and z-rotational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_column("rr1")

    @rr1.setter
    def rr1(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rr1 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rr1", value)

    @property
    def rt2(self) -> pd.Series:
        """Get or set the Release conditions for translations at node N2.
        EQ.0: no translational degrees-of-freedom are released,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x and y-translational degrees-of-freedom,
        EQ.5: y and z-translational degrees-of-freedom,
        EQ.6: z and x-translational degrees-of-freedom,
        EQ.7: x, y, and z-translational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_column("rt2")

    @rt2.setter
    def rt2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rt2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rt2", value)

    @property
    def rr2(self) -> pd.Series:
        """Get or set the Release conditions for rotations at node N2.
        EQ.0: no rotational degrees-of-freedom are released,
        EQ.1: x-rotational degree-of-freedom,
        EQ.2: y-rotational degree-of-freedom,
        EQ.3: z-rotational degree-of-freedom,
        EQ.4: x and y-rotational degrees-of-freedom,
        EQ.5: y and z-rotational degrees-of-freedom,
        EQ.6: z and x-rotational degrees-of-freedom,
        EQ.7: x, y, and z-rotational degrees-of-freedom.
        This option does not apply to the spot weld, beam type 9.
        """ # nopep8
        return self._cards[0].get_column("rr2")

    @rr2.setter
    def rr2(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rr2 column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rr2", value)

    @property
    def local(self) -> pd.Series:
        """Get or set the Coordinate system
        EQ.1-global system
        EQ.2-Local system (default).
        """ # nopep8
        return self._cards[0].get_column("local")

    @local.setter
    def local(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the local column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("local", value)

    @property
    def vx(self) -> pd.Series:
        """Get or set the Components of an orientation vector relative to node N1. In this
        case, the orientation vector points to a virtual third node, so the
        field N3 should be left undefined.
        """ # nopep8
        return self._cards[0].get_column("vx")

    @vx.setter
    def vx(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the vx column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("vx", value)

    @property
    def vy(self) -> pd.Series:
        """Get or set the Components of an orientation vector relative to node N1. In this
        case, the orientation vector points to a virtual third node, so the
        field N3 should be left undefined.
        """ # nopep8
        return self._cards[0].get_column("vy")

    @vy.setter
    def vy(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the vy column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("vy", value)

    @property
    def vz(self) -> pd.Series:
        """Get or set the Components of an orientation vector relative to node N1. In this
        case, the orientation vector points to a virtual third node, so the
        field N3 should be left undefined.
        """ # nopep8
        return self._cards[0].get_column("vz")

    @vz.setter
    def vz(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the vz column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("vz", value)

