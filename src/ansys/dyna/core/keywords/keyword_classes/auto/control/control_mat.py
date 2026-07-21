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

"""Module providing the ControlMat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLMAT_CARD0 = (
    FieldSchema("maef", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("umchk", int, 20, 10, 0),
    FieldSchema("oldintp", int, 30, 10, 0),
    FieldSchema("impcont", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("psmaef", int, 60, 10, None),
)

class ControlMat(KeywordBase):
    """DYNA CONTROL_MAT keyword"""

    keyword = "CONTROL"
    subkeyword = "MAT"

    def __init__(self, **kwargs):
        """Initialize the ControlMat class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLMAT_CARD0,
                **kwargs,
            ),
        ]
    @property
    def maef(self) -> int:
        """Get or set the Failure options:
        EQ.0: All *MAT_ADD_EROSION, *MAT_ADD_DAMAGE_DIEM,and *MAT_ADD_DAMAGE_GISSMO definitions are active.
        EQ.1: Switch off all *MAT_ADD_EROSION, *MAT_ADD_DAMAGE_DIEM,and *MAT_ADD_DAMAGE_GISSMO definitions globally.This feature is helpful for larger models where removing those cards is inconvenient.
        EQ.2:	Same as 1 but also deactivate plastic strain to failure in some material models, In other words, FAIL is internally set to zero in materials 24, 103, 105, 106, 114, 123, 124, 155, 195, 225, 251, and 255.
        """ # nopep8
        return self._cards[0].get_value("maef")

    @maef.setter
    def maef(self, value: int) -> None:
        """Set the maef property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""maef must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("maef", value)

    @property
    def umchk(self) -> int:
        """Get or set the User material check. Initially in the first calculation cycle, it is checked if true user-defined material models are applied or whether only the default, unmodified subroutines already present in the native dyn21 files are called. it also works for user-defined friction in MPP.
        EQ.0: Warning is issued.
        EQ.1: Error termination occurs.
        """ # nopep8
        return self._cards[0].get_value("umchk")

    @umchk.setter
    def umchk(self, value: int) -> None:
        """Set the umchk property."""
        if value not in [0, 1, None]:
            raise Exception("""umchk must be `None` or one of {0,1}.""")
        self._cards[0].set_value("umchk", value)

    @property
    def oldintp(self) -> int:
        """Get or set the Because of changes in results when correcting the interpolation of tables in the piecewise plasticity model (*MAT_024), you can set this flag to 1 to invoke the old behavior. Note that the old behavior may result in an incorrect stress response, so we generally do not recommend it.
        """ # nopep8
        return self._cards[0].get_value("oldintp")

    @oldintp.setter
    def oldintp(self, value: int) -> None:
        """Set the oldintp property."""
        self._cards[0].set_value("oldintp", value)

    @property
    def impcont(self) -> int:
        """Get or set the Flag to ignore issues coming from the material models, such as the plasticity routine not converging, when using implicit (see Remark 1):
        EQ.0: When issues arise in the material model routines, the job error terminates unless IAUTO is greater than 0 on *CONTROL_IMPLICIT_AUTO. If IAUTO > 0, the time step size reduces.
        EQ.1: Continue the job, ignoring the issue coming from the material model routine.
        """ # nopep8
        return self._cards[0].get_value("impcont")

    @impcont.setter
    def impcont(self, value: int) -> None:
        """Set the impcont property."""
        if value not in [0, 1, None]:
            raise Exception("""impcont must be `None` or one of {0,1}.""")
        self._cards[0].set_value("impcont", value)

    @property
    def psmaef(self) -> typing.Optional[int]:
        """Get or set the Part set ID for option MAEF:
        EQ.0: MAEF is applied to all parts in the model(default).
        GT.0: MAEF is applied to all parts in this set.
        LT.0: MAEF is applied to all parts not in set |PSMAEF|.
        """ # nopep8
        return self._cards[0].get_value("psmaef")

    @psmaef.setter
    def psmaef(self, value: int) -> None:
        """Set the psmaef property."""
        self._cards[0].set_value("psmaef", value)

