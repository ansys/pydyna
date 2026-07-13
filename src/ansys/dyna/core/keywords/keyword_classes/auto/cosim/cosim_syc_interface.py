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

"""Module providing the CosimSycInterface class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_COSIMSYCINTERFACE_CARD0 = (
    FieldSchema("sycid", str, 0, 20, None),
    FieldSchema("nreg", int, 20, 10, None),
)

_COSIMSYCINTERFACE_CARD1 = (
    FieldSchema("regname", str, 0, 20, None),
    FieldSchema("regtyp", str, 20, 10, None),
    FieldSchema("regid", int, 30, 10, None),
    FieldSchema("nexp", int, 40, 10, None),
    FieldSchema("nimp", int, 50, 10, None),
)

_COSIMSYCINTERFACE_CARD2 = (
    FieldSchema("varname", str, 0, 20, None),
    FieldSchema("vartyp", str, 20, 10, None),
    FieldSchema("sctyp", str, 30, 10, None),
    FieldSchema("scext", str, 40, 10, None),
    FieldSchema("scloc", str, 50, 10, None),
    FieldSchema("scvs", str, 60, 10, None),
)

class CosimSycInterface(KeywordBase):
    """DYNA COSIM_SYC_INTERFACE keyword"""

    keyword = "COSIM"
    subkeyword = "SYC_INTERFACE"

    def __init__(self, **kwargs):
        """Initialize the CosimSycInterface class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _COSIMSYCINTERFACE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _COSIMSYCINTERFACE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _COSIMSYCINTERFACE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def sycid(self) -> typing.Optional[str]:
        """Get or set the LS-DYNA�s System Coupling identification. System Coupling uses this ID to identify and connect the participants.
        """ # nopep8
        return self._cards[0].get_value("sycid")

    @sycid.setter
    def sycid(self, value: str) -> None:
        """Set the sycid property."""
        self._cards[0].set_value("sycid", value)

    @property
    def nreg(self) -> typing.Optional[int]:
        """Get or set the Number of interface regions to be defined
        """ # nopep8
        return self._cards[0].get_value("nreg")

    @nreg.setter
    def nreg(self, value: int) -> None:
        """Set the nreg property."""
        self._cards[0].set_value("nreg", value)

    @property
    def regname(self) -> typing.Optional[str]:
        """Get or set the Name of this region
        """ # nopep8
        return self._cards[1].get_value("regname")

    @regname.setter
    def regname(self, value: str) -> None:
        """Set the regname property."""
        self._cards[1].set_value("regname", value)

    @property
    def regtyp(self) -> typing.Optional[str]:
        """Get or set the Type of interfae region:
        EQ.SETSEG: Set of segments defined in *SET_SEGMENT_N8
        """ # nopep8
        return self._cards[1].get_value("regtyp")

    @regtyp.setter
    def regtyp(self, value: str) -> None:
        """Set the regtyp property."""
        self._cards[1].set_value("regtyp", value)

    @property
    def regid(self) -> typing.Optional[int]:
        """Get or set the ID of the corresponding region type in LS-DYNA
        """ # nopep8
        return self._cards[1].get_value("regid")

    @regid.setter
    def regid(self, value: int) -> None:
        """Set the regid property."""
        self._cards[1].set_value("regid", value)

    @property
    def nexp(self) -> typing.Optional[int]:
        """Get or set the Number of variables to be exported by LS-DYNA for this region
        """ # nopep8
        return self._cards[1].get_value("nexp")

    @nexp.setter
    def nexp(self, value: int) -> None:
        """Set the nexp property."""
        self._cards[1].set_value("nexp", value)

    @property
    def nimp(self) -> typing.Optional[int]:
        """Get or set the Number of variables to be imported into LS-DYNA for this region
        """ # nopep8
        return self._cards[1].get_value("nimp")

    @nimp.setter
    def nimp(self, value: int) -> None:
        """Set the nimp property."""
        self._cards[1].set_value("nimp", value)

    @property
    def varname(self) -> typing.Optional[str]:
        """Get or set the Name of the variable to be used in the .scp file and .py file
        """ # nopep8
        return self._cards[2].get_value("varname")

    @varname.setter
    def varname(self, value: str) -> None:
        """Set the varname property."""
        self._cards[2].set_value("varname", value)

    @property
    def vartyp(self) -> typing.Optional[str]:
        """Get or set the Field data type, for ls-dyna, to describe the characteristics of the variable.
        EQ.FORCE: Nodal force.
        EQ.INCD:	Incremental nodal displacement from the last System Coupling time step
        EQ.PRESS : pressure for segments.
        EQ.VEL : nodal velocity, export only
        The following data are for coupled structural - thermal analysis, SOLN of* CONTROL_SOLUTION is set to 2.
        EQ.TEMP : nodal temperature, export only
        EQ.HTRATE : nodal heat flow rate, import only
        EQ.CONVH : nodal heat transfer coefficient for convection, import only
        EQ.CONVTR : nodal reference temperature for convection, import only.
        Note that these thermal data types are available only in R18 and later
        """ # nopep8
        return self._cards[2].get_value("vartyp")

    @vartyp.setter
    def vartyp(self, value: str) -> None:
        """Set the vartyp property."""
        self._cards[2].set_value("vartyp", value)

    @property
    def sctyp(self) -> typing.Optional[str]:
        """Get or set the Field data type defined in System Level Coupling, see �System Coupling Settings and Commands Reference� for details.SCTYP can be set to VARTYP for thermal data, TEMP, HTRATE, CONVH and CONVTR.
        EQ.FORCE: for force or pressure
        EQ.INCDIS : for incremental displacement
        EQ.UNSPEC : Unspecified
        """ # nopep8
        return self._cards[2].get_value("sctyp")

    @sctyp.setter
    def sctyp(self, value: str) -> None:
        """Set the sctyp property."""
        self._cards[2].set_value("sctyp", value)

    @property
    def scext(self) -> typing.Optional[str]:
        """Get or set the Flag to specify if the variable to be transferred is extensive. This flag is only needed when SCTYP is set to UNSPEC.  A variable is extensive if its value depends on the system size.
        EQ.FALSE:	Variable is not extensive
        EQ.TRUE: Variable is extensive
        """ # nopep8
        return self._cards[2].get_value("scext")

    @scext.setter
    def scext(self, value: str) -> None:
        """Set the scext property."""
        self._cards[2].set_value("scext", value)

    @property
    def scloc(self) -> typing.Optional[str]:
        """Get or set the Location of variable being transferred, NODE or ELEMENT, needed only when SCTYP is set to UNSPEC
        """ # nopep8
        return self._cards[2].get_value("scloc")

    @scloc.setter
    def scloc(self, value: str) -> None:
        """Set the scloc property."""
        self._cards[2].set_value("scloc", value)

    @property
    def scvs(self) -> typing.Optional[str]:
        """Get or set the Tensor type of the variable, SCALAR or VECTOR, needed only when SCTYP is set to UNSPEC
        """ # nopep8
        return self._cards[2].get_value("scvs")

    @scvs.setter
    def scvs(self, value: str) -> None:
        """Set the scvs property."""
        self._cards[2].set_value("scvs", value)

