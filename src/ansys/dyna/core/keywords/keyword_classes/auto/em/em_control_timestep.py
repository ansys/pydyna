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

"""Module providing the EmControlTimestep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMCONTROLTIMESTEP_CARD0 = (
    FieldSchema("tstype", int, 0, 10, 1),
    FieldSchema("dtcons", float, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("factor", float, 30, 10, 1.0),
    FieldSchema("tsmin", float, 40, 10, None),
    FieldSchema("tsmas", float, 50, 10, None),
    FieldSchema("rlcsf", int, 60, 10, 25),
    FieldSchema("mecats", int, 70, 10, 0),
)

class EmControlTimestep(KeywordBase):
    """DYNA EM_CONTROL_TIMESTEP keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_TIMESTEP"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmControlTimestep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLTIMESTEP_CARD0,
                **kwargs,
            ),        ]
    @property
    def tstype(self) -> int:
        """Get or set the Time Step type
        EQ.1 : constant time step given in DTCONST
        EQ.2 : time step as a function of time given by a load curve specified in LCID
        EQ.3 : Automatic time step computation, depending on the solver type. This time step is then multiplied by FACTOR
        """ # nopep8
        return self._cards[0].get_value("tstype")

    @tstype.setter
    def tstype(self, value: int) -> None:
        """Set the tstype property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""tstype must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("tstype", value)

    @property
    def dtcons(self) -> typing.Optional[float]:
        """Get or set the Constant value for the time step for TSTYPE=1
        """ # nopep8
        return self._cards[0].get_value("dtcons")

    @dtcons.setter
    def dtcons(self, value: float) -> None:
        """Set the dtcons property."""
        self._cards[0].set_value("dtcons", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the time step vs time for TSTYPE=2
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def factor(self) -> float:
        """Get or set the Multiplicative factor applied to the time step for TSTYPE=3
        """ # nopep8
        return self._cards[0].get_value("factor")

    @factor.setter
    def factor(self, value: float) -> None:
        """Set the factor property."""
        self._cards[0].set_value("factor", value)

    @property
    def tsmin(self) -> typing.Optional[float]:
        """Get or set the Minimum time step. When TSMIN is defined, the EM time step cannot drop below TSMIN. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("tsmin")

    @tsmin.setter
    def tsmin(self, value: float) -> None:
        """Set the tsmin property."""
        self._cards[0].set_value("tsmin", value)

    @property
    def tsmas(self) -> typing.Optional[float]:
        """Get or set the Maximum time step. When TSMAX is defined, the EM time step cannot increase beyond TSMAX. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("tsmas")

    @tsmas.setter
    def tsmas(self, value: float) -> None:
        """Set the tsmas property."""
        self._cards[0].set_value("tsmas", value)

    @property
    def rlcsf(self) -> int:
        """Get or set the RLC Circuit time step scale factor.
        """ # nopep8
        return self._cards[0].get_value("rlcsf")

    @rlcsf.setter
    def rlcsf(self, value: int) -> None:
        """Set the rlcsf property."""
        self._cards[0].set_value("rlcsf", value)

    @property
    def mecats(self) -> int:
        """Get or set the Mechanical time step handling in cases where the EM solver time step becomes smaller (see Remark 3):
        EQ.0: Default.The EM time step will go below the solid mechanics timestep,and several EM solves will occur between two solid mechanics time steps to ensure time consistency.
        EQ.1: The solid mechanics time step will adapt and decrease to the EM time step value so that only one EM solve occurs between two solid mechanics solves.
        """ # nopep8
        return self._cards[0].get_value("mecats")

    @mecats.setter
    def mecats(self, value: int) -> None:
        """Set the mecats property."""
        if value not in [0, 1, None]:
            raise Exception("""mecats must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mecats", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

