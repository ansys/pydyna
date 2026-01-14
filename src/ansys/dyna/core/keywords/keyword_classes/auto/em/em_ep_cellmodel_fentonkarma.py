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

"""Module providing the EmEpCellmodelFentonkarma class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMEPCELLMODELFENTONKARMA_CARD0 = (
    FieldSchema("matid", int, 0, 10, None),
)

_EMEPCELLMODELFENTONKARMA_CARD1 = (
    FieldSchema("taud", float, 0, 10, None),
    FieldSchema("taur", float, 10, 10, None),
    FieldSchema("tausi", float, 20, 10, None),
    FieldSchema("tauo", float, 30, 10, None),
    FieldSchema("tauvp", float, 40, 10, None),
    FieldSchema("tauvm", float, 50, 10, None),
    FieldSchema("tauwp", float, 60, 10, None),
    FieldSchema("tauwm", float, 70, 10, None),
)

_EMEPCELLMODELFENTONKARMA_CARD2 = (
    FieldSchema("uc", float, 0, 10, None),
    FieldSchema("ucsi", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
)

_EMEPCELLMODELFENTONKARMA_CARD3 = (
    FieldSchema("u0", float, 0, 10, None),
    FieldSchema("v0", float, 10, 10, None),
    FieldSchema("w0", float, 20, 10, None),
)

class EmEpCellmodelFentonkarma(KeywordBase):
    """DYNA EM_EP_CELLMODEL_FENTONKARMA keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_FENTONKARMA"

    def __init__(self, **kwargs):
        """Initialize the EmEpCellmodelFentonkarma class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELFENTONKARMA_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELFENTONKARMA_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELFENTONKARMA_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPCELLMODELFENTONKARMA_CARD3,
                **kwargs,
            ),        ]
    @property
    def matid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT_.

        """ # nopep8
        return self._cards[0].get_value("matid")

    @matid.setter
    def matid(self, value: int) -> None:
        """Set the matid property."""
        self._cards[0].set_value("matid", value)

    @property
    def taud(self) -> typing.Optional[float]:
        """Get or set the Time constant td described in Equation(5).
        """ # nopep8
        return self._cards[1].get_value("taud")

    @taud.setter
    def taud(self, value: float) -> None:
        """Set the taud property."""
        self._cards[1].set_value("taud", value)

    @property
    def taur(self) -> typing.Optional[float]:
        """Get or set the Time constant tr described in Equation (6).
        """ # nopep8
        return self._cards[1].get_value("taur")

    @taur.setter
    def taur(self, value: float) -> None:
        """Set the taur property."""
        self._cards[1].set_value("taur", value)

    @property
    def tausi(self) -> typing.Optional[float]:
        """Get or set the Time constant tsi described in Equation (7).
        """ # nopep8
        return self._cards[1].get_value("tausi")

    @tausi.setter
    def tausi(self, value: float) -> None:
        """Set the tausi property."""
        self._cards[1].set_value("tausi", value)

    @property
    def tauo(self) -> typing.Optional[float]:
        """Get or set the Time constant t0 described in Equation(6).
        """ # nopep8
        return self._cards[1].get_value("tauo")

    @tauo.setter
    def tauo(self, value: float) -> None:
        """Set the tauo property."""
        self._cards[1].set_value("tauo", value)

    @property
    def tauvp(self) -> typing.Optional[float]:
        """Get or set the Time constant tvp described in Equation(3).
        """ # nopep8
        return self._cards[1].get_value("tauvp")

    @tauvp.setter
    def tauvp(self, value: float) -> None:
        """Set the tauvp property."""
        self._cards[1].set_value("tauvp", value)

    @property
    def tauvm(self) -> typing.Optional[float]:
        """Get or set the Time constant tvm described in Equation(3)
        """ # nopep8
        return self._cards[1].get_value("tauvm")

    @tauvm.setter
    def tauvm(self, value: float) -> None:
        """Set the tauvm property."""
        self._cards[1].set_value("tauvm", value)

    @property
    def tauwp(self) -> typing.Optional[float]:
        """Get or set the Time constant twp described in Equation(4).
        """ # nopep8
        return self._cards[1].get_value("tauwp")

    @tauwp.setter
    def tauwp(self, value: float) -> None:
        """Set the tauwp property."""
        self._cards[1].set_value("tauwp", value)

    @property
    def tauwm(self) -> typing.Optional[float]:
        """Get or set the Time constant twm described in Equation(4).
        """ # nopep8
        return self._cards[1].get_value("tauwm")

    @tauwm.setter
    def tauwm(self, value: float) -> None:
        """Set the tauwm property."""
        self._cards[1].set_value("tauwm", value)

    @property
    def uc(self) -> typing.Optional[float]:
        """Get or set the Threshold potential Uc for activation of Jfi (the fast inward current) Equation (3, 4, 5, 6).
        """ # nopep8
        return self._cards[2].get_value("uc")

    @uc.setter
    def uc(self, value: float) -> None:
        """Set the uc property."""
        self._cards[2].set_value("uc", value)

    @property
    def ucsi(self) -> typing.Optional[float]:
        """Get or set the Threshold potential Ucsi for activation of Jsi (the slow inward current) in Equation (7).
        """ # nopep8
        return self._cards[2].get_value("ucsi")

    @ucsi.setter
    def ucsi(self, value: float) -> None:
        """Set the ucsi property."""
        self._cards[2].set_value("ucsi", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Constant k in Equation(7).
        """ # nopep8
        return self._cards[2].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[2].set_value("k", value)

    @property
    def u0(self) -> typing.Optional[float]:
        """Get or set the Initial value of U respectively.
        """ # nopep8
        return self._cards[3].get_value("u0")

    @u0.setter
    def u0(self, value: float) -> None:
        """Set the u0 property."""
        self._cards[3].set_value("u0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial value of V respectively.
        """ # nopep8
        return self._cards[3].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[3].set_value("v0", value)

    @property
    def w0(self) -> typing.Optional[float]:
        """Get or set the Initial value of W respectively.
        """ # nopep8
        return self._cards[3].get_value("w0")

    @w0.setter
    def w0(self, value: float) -> None:
        """Set the w0 property."""
        self._cards[3].set_value("w0", value)

