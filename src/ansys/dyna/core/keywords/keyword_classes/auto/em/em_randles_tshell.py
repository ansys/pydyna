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

"""Module providing the EmRandlesTshell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMRANDLESTSHELL_CARD0 = (
    FieldSchema("rdlid", int, 0, 10, None),
    FieldSchema("rdltype", int, 10, 10, None),
    FieldSchema("rdlarea", int, 20, 10, 2),
    FieldSchema("psid", int, 30, 10, None),
)

_EMRANDLESTSHELL_CARD1 = (
    FieldSchema("q", float, 0, 10, None),
    FieldSchema("cq", float, 10, 10, None),
    FieldSchema("socinit", float, 20, 10, None),
    FieldSchema("soctou", float, 30, 10, None),
)

_EMRANDLESTSHELL_CARD2 = (
    FieldSchema("r0cha", float, 0, 10, None),
    FieldSchema("r0dis", float, 10, 10, None),
    FieldSchema("r10cha", float, 20, 10, None),
    FieldSchema("r10dis", float, 30, 10, None),
    FieldSchema("c10cha", float, 40, 10, None),
    FieldSchema("c10dis", float, 50, 10, None),
)

_EMRANDLESTSHELL_CARD3 = (
    FieldSchema("r20cha", float, 0, 10, None),
    FieldSchema("r20dis", float, 10, 10, None),
    FieldSchema("c20cha", float, 20, 10, None),
    FieldSchema("c20dis", float, 30, 10, None),
    FieldSchema("r30cha", float, 40, 10, None),
    FieldSchema("r30dis", float, 50, 10, None),
    FieldSchema("c30cha", float, 60, 10, None),
    FieldSchema("c30dis", float, 70, 10, None),
)

_EMRANDLESTSHELL_CARD4 = (
    FieldSchema("temp", float, 0, 10, 0.0),
    FieldSchema("frther", int, 10, 10, 0),
    FieldSchema("r0toth", int, 20, 10, 0),
    FieldSchema("dudt", float, 30, 10, 0.0),
    FieldSchema("tempu", int, 40, 10, 0),
)

_EMRANDLESTSHELL_CARD5 = (
    FieldSchema("usesocs", int, 0, 10, 0),
    FieldSchema("tau", float, 10, 10, None),
    FieldSchema("flcid", int, 20, 10, None),
)

class EmRandlesTshell(KeywordBase):
    """DYNA EM_RANDLES_TSHELL keyword"""

    keyword = "EM"
    subkeyword = "RANDLES_TSHELL"
    _link_fields = {
        "flcid": LinkType.DEFINE_CURVE,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmRandlesTshell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD3,
                active_func=lambda: self.rdltype > 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMRANDLESTSHELL_CARD5,
                **kwargs,
            ),        ]
    @property
    def rdlid(self) -> typing.Optional[int]:
        """Get or set the Id of the Randles Cell.
        """ # nopep8
        return self._cards[0].get_value("rdlid")

    @rdlid.setter
    def rdlid(self, value: int) -> None:
        """Set the rdlid property."""
        self._cards[0].set_value("rdlid", value)

    @property
    def rdltype(self) -> typing.Optional[int]:
        """Get or set the Type of Randles Cell
        EQ.-1:User defined equivalent circuit model.
        EQ.0:0-order Randles Cell.
        EQ.1:1-order Randles Cell.
        EQ.2:2-order Randles Cell.
        EQ.3:3-order Randles Cell.
        """ # nopep8
        return self._cards[0].get_value("rdltype")

    @rdltype.setter
    def rdltype(self, value: int) -> None:
        """Set the rdltype property."""
        self._cards[0].set_value("rdltype", value)

    @property
    def rdlarea(self) -> int:
        """Get or set the Randle Area:
        EQ.1: The parameters are per unit area and will be scaled in each Randle circuit by a factor depending on the local area of the circuit.
        EQ.2: Default. The parameters are defined for the whole cell and will be scaled in each Randle circuit by a factor depending on the local area of the circuit and the global area of the cell.
        EQ.3:The parameters are not scaled by area factors.
        """ # nopep8
        return self._cards[0].get_value("rdlarea")

    @rdlarea.setter
    def rdlarea(self, value: int) -> None:
        """Set the rdlarea property."""
        if value not in [2, 1, 3, None]:
            raise Exception("""rdlarea must be `None` or one of {2,1,3}.""")
        self._cards[0].set_value("rdlarea", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID of all the parts composing the cell.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Cell capacity.
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[1].set_value("q", value)

    @property
    def cq(self) -> typing.Optional[float]:
        """Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
        """ # nopep8
        return self._cards[1].get_value("cq")

    @cq.setter
    def cq(self, value: float) -> None:
        """Set the cq property."""
        self._cards[1].set_value("cq", value)

    @property
    def socinit(self) -> typing.Optional[float]:
        """Get or set the Initial state of charge of the cell.
        """ # nopep8
        return self._cards[1].get_value("socinit")

    @socinit.setter
    def socinit(self, value: float) -> None:
        """Set the socinit property."""
        self._cards[1].set_value("socinit", value)

    @property
    def soctou(self) -> typing.Optional[float]:
        """Get or set the Equilibrium voltage (OCV):
        GE.0.0:constant value.
        LT.0.0:|SOCTOU| is a load curve ID defining equilibrium voltage(OCV) as a function of the state of charge(SOC).
        """ # nopep8
        return self._cards[1].get_value("soctou")

    @soctou.setter
    def soctou(self, value: float) -> None:
        """Set the soctou property."""
        self._cards[1].set_value("soctou", value)

    @property
    def r0cha(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the charge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the charge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("r0cha")

    @r0cha.setter
    def r0cha(self, value: float) -> None:
        """Set the r0cha property."""
        self._cards[2].set_value("r0cha", value)

    @property
    def r0dis(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the discharge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the discharge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("r0dis")

    @r0dis.setter
    def r0dis(self, value: float) -> None:
        """Set the r0dis property."""
        self._cards[2].set_value("r0dis", value)

    @property
    def r10cha(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the charge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the charge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("r10cha")

    @r10cha.setter
    def r10cha(self, value: float) -> None:
        """Set the r10cha property."""
        self._cards[2].set_value("r10cha", value)

    @property
    def r10dis(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the discharge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the discharge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("r10dis")

    @r10dis.setter
    def r10dis(self, value: float) -> None:
        """Set the r10dis property."""
        self._cards[2].set_value("r10dis", value)

    @property
    def c10cha(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the charge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the charge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("c10cha")

    @c10cha.setter
    def c10cha(self, value: float) -> None:
        """Set the c10cha property."""
        self._cards[2].set_value("c10cha", value)

    @property
    def c10dis(self) -> typing.Optional[float]:
        """Get or set the r0/r10/c10 when the current flows in the discharge direction:
        GE.0.0: constant value.
        LT.0.0: absolute value is a load curve or table ID, defining r0/r10/c10 when the current flows in the discharge direction as a function of - SOC for a load curve and as a function - SOC and temperature for a table.
        """ # nopep8
        return self._cards[2].get_value("c10dis")

    @c10dis.setter
    def c10dis(self, value: float) -> None:
        """Set the c10dis property."""
        self._cards[2].set_value("c10dis", value)

    @property
    def r20cha(self) -> typing.Optional[float]:
        """Get or set the r20 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r20cha")

    @r20cha.setter
    def r20cha(self, value: float) -> None:
        """Set the r20cha property."""
        self._cards[3].set_value("r20cha", value)

    @property
    def r20dis(self) -> typing.Optional[float]:
        """Get or set the r20 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r20dis")

    @r20dis.setter
    def r20dis(self, value: float) -> None:
        """Set the r20dis property."""
        self._cards[3].set_value("r20dis", value)

    @property
    def c20cha(self) -> typing.Optional[float]:
        """Get or set the c20 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c20cha")

    @c20cha.setter
    def c20cha(self, value: float) -> None:
        """Set the c20cha property."""
        self._cards[3].set_value("c20cha", value)

    @property
    def c20dis(self) -> typing.Optional[float]:
        """Get or set the c20 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c20dis")

    @c20dis.setter
    def c20dis(self, value: float) -> None:
        """Set the c20dis property."""
        self._cards[3].set_value("c20dis", value)

    @property
    def r30cha(self) -> typing.Optional[float]:
        """Get or set the r30 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r30cha")

    @r30cha.setter
    def r30cha(self, value: float) -> None:
        """Set the r30cha property."""
        self._cards[3].set_value("r30cha", value)

    @property
    def r30dis(self) -> typing.Optional[float]:
        """Get or set the r30 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r30dis")

    @r30dis.setter
    def r30dis(self, value: float) -> None:
        """Set the r30dis property."""
        self._cards[3].set_value("r30dis", value)

    @property
    def c30cha(self) -> typing.Optional[float]:
        """Get or set the c30 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c30cha")

    @c30cha.setter
    def c30cha(self, value: float) -> None:
        """Set the c30cha property."""
        self._cards[3].set_value("c30cha", value)

    @property
    def c30dis(self) -> typing.Optional[float]:
        """Get or set the c30 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c30dis")

    @c30dis.setter
    def c30dis(self, value: float) -> None:
        """Set the c30dis property."""
        self._cards[3].set_value("c30dis", value)

    @property
    def temp(self) -> float:
        """Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver(FRTHER = 0).
        """ # nopep8
        return self._cards[4].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        """Set the temp property."""
        self._cards[4].set_value("temp", value)

    @property
    def frther(self) -> int:
        """Get or set the From Thermal:
        EQ.0:The temperature used in the Randles circuit parameters is TEMP.
        EQ.1: The temperature used in the Randles circuit parameter is the temperature from the thermal solver.
        """ # nopep8
        return self._cards[4].get_value("frther")

    @frther.setter
    def frther(self, value: int) -> None:
        """Set the frther property."""
        if value not in [0, 1, None]:
            raise Exception("""frther must be `None` or one of {0,1}.""")
        self._cards[4].set_value("frther", value)

    @property
    def r0toth(self) -> int:
        """Get or set the R0 to Thermal:
        EQ.0:The joule heating in the resistance r0 is not added to the thermal solver.
        EQ.1:The joule heating in the resistance r0 is added to the thermal solver.
        """ # nopep8
        return self._cards[4].get_value("r0toth")

    @r0toth.setter
    def r0toth(self, value: int) -> None:
        """Set the r0toth property."""
        if value not in [0, 1, None]:
            raise Exception("""r0toth must be `None` or one of {0,1}.""")
        self._cards[4].set_value("r0toth", value)

    @property
    def dudt(self) -> float:
        """Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
        """ # nopep8
        return self._cards[4].get_value("dudt")

    @dudt.setter
    def dudt(self, value: float) -> None:
        """Set the dudt property."""
        self._cards[4].set_value("dudt", value)

    @property
    def tempu(self) -> int:
        """Get or set the Temperature Unit :
        EQ.0:The temperature is in Celsius
        EQ.1:The Temperature is in Kelvin.
        """ # nopep8
        return self._cards[4].get_value("tempu")

    @tempu.setter
    def tempu(self, value: int) -> None:
        """Set the tempu property."""
        if value not in [0, 1, None]:
            raise Exception("""tempu must be `None` or one of {0,1}.""")
        self._cards[4].set_value("tempu", value)

    @property
    def usesocs(self) -> int:
        """Get or set the Use SOC shift:
        EQ.0:Don't use the added SOCshift
        EQ.1:Use the added SOCshift.
        """ # nopep8
        return self._cards[5].get_value("usesocs")

    @usesocs.setter
    def usesocs(self, value: int) -> None:
        """Set the usesocs property."""
        if value not in [0, 1, None]:
            raise Exception("""usesocs must be `None` or one of {0,1}.""")
        self._cards[5].set_value("usesocs", value)

    @property
    def tau(self) -> typing.Optional[float]:
        """Get or set the Damping time in the SOCshift equation.
        """ # nopep8
        return self._cards[5].get_value("tau")

    @tau.setter
    def tau(self, value: float) -> None:
        """Set the tau property."""
        self._cards[5].set_value("tau", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Load curve giving f(i) where I is the total current in the unit cell.
        """ # nopep8
        return self._cards[5].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        """Set the flcid property."""
        self._cards[5].set_value("flcid", value)

    @property
    def flcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for flcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.flcid:
                return kwd
        return None

    @flcid_link.setter
    def flcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for flcid."""
        self.flcid = value.lcid

    @property
    def psid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

