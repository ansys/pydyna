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

"""Module providing the ConstrainedBeamInSolid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDBEAMINSOLID_CARD0 = (
    FieldSchema("bside", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
    FieldSchema("bstyp", int, 20, 10, 0),
    FieldSchema("sstyp", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("ncoup", int, 60, 10, None),
    FieldSchema("cdir", int, 70, 10, None),
)

_CONSTRAINEDBEAMINSOLID_CARD1 = (
    FieldSchema("start", float, 0, 10, 0.0),
    FieldSchema("end", float, 10, 10, 1e+21),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("axfor_", int, 30, 10, None, "axfor "),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("pssf", float, 50, 10, 0.1),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("xint", int, 70, 10, None),
)

_CONSTRAINEDBEAMINSOLID_OPTION0_CARD0 = (
    FieldSchema("coupid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_CONSTRAINEDBEAMINSOLID_OPTION1_CARD0 = (
    FieldSchema("coupid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

class ConstrainedBeamInSolid(KeywordBase):
    """DYNA CONSTRAINED_BEAM_IN_SOLID keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "BEAM_IN_SOLID"
    option_specs = [
        OptionSpec("ID", -1, 1),
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the ConstrainedBeamInSolid class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLID_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = ConstrainedBeamInSolid.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONSTRAINEDBEAMINSOLID_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ConstrainedBeamInSolid.option_specs[1],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONSTRAINEDBEAMINSOLID_OPTION1_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def bside(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian beam structure(see *PART,* SET_PART)
        """ # nopep8
        return self._cards[0].get_value("bside")

    @bside.setter
    def bside(self, value: int) -> None:
        """Set the bside property."""
        self._cards[0].set_value("bside", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian solid elements or thick shell element(see *PART,* SET_PART)
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def bstyp(self) -> int:
        """Get or set the Set type of BSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[0].get_value("bstyp")

    @bstyp.setter
    def bstyp(self, value: int) -> None:
        """Set the bstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""bstyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("bstyp", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Set type of SSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[0].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        """Set the sstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sstyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sstyp", value)

    @property
    def ncoup(self) -> typing.Optional[int]:
        """Get or set the Number of coupling points generated in one beam element. If set to 0, coupling only happens at beam nodes. Otherwise, coupling is done at both the beam nodes and those automatically generated coupling points
        """ # nopep8
        return self._cards[0].get_value("ncoup")

    @ncoup.setter
    def ncoup(self, value: int) -> None:
        """Set the ncoup property."""
        self._cards[0].set_value("ncoup", value)

    @property
    def cdir(self) -> typing.Optional[int]:
        """Get or set the Coupling direction.
        EQ.0: default, constraint applied along all directions.
        EQ.1: Constraint only applied along normal directions; along the beam axial direction there is no constraint
        """ # nopep8
        return self._cards[0].get_value("cdir")

    @cdir.setter
    def cdir(self, value: int) -> None:
        """Set the cdir property."""
        self._cards[0].set_value("cdir", value)

    @property
    def start(self) -> float:
        """Get or set the Start time to activate the coupling
        LT.0:	Start time is set to |START|.  When negative, start time is followed during the dynamic relaxation phase of the calculation.  After dynamic relaxation has completed, coupling is activated regardless of the value of END.EQ.0:	Start time is inactive, meaning coupling is always active
        GT.0 : If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start - time and end - time.Otherwise, if END > 0, start time applies both duringand after dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[1].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time to deactive the coupling
        LT.0:	If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start-time and end-time.  Otherwise, negative END indicates that coupling is inactive during dynamic relaxation.  After dynamic relaxation the start and end times are followed and set to |START| and |END|, respectively.EQ.0:	END defaults to 1020.
        GT.0 : END sets the time at which the coupling is deactivated.
        """ # nopep8
        return self._cards[1].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        """Set the end property."""
        self._cards[1].set_value("end", value)

    @property
    def axfor_(self) -> typing.Optional[int]:
        """Get or set the ID of a user defined function describes coupling force versus slip along beam axial direction.
        GE.0: OFF
        EQ.-n: n is the function ID in *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[1].get_value("axfor_")

    @axfor_.setter
    def axfor_(self, value: int) -> None:
        """Set the axfor_ property."""
        self._cards[1].set_value("axfor_", value)

    @property
    def pssf(self) -> float:
        """Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
        """ # nopep8
        return self._cards[1].get_value("pssf")

    @pssf.setter
    def pssf(self, value: float) -> None:
        """Set the pssf property."""
        self._cards[1].set_value("pssf", value)

    @property
    def xint(self) -> typing.Optional[int]:
        """Get or set the Interval distance. This field is designed to deal with beam elements having a wide variation in lengths.
        Coupling points are generated at an interval of length equal to XINT.
        Hence the number of coupling points in a beam element is no longer a fixed number (NCOUP),
        but rather variable, depending on the length of the beam element.
        This field can be used together with NCOUP.
        In that case, in each element, we will take the larger number of coupling points from these two options.
        """ # nopep8
        return self._cards[1].get_value("xint")

    @xint.setter
    def xint(self, value: int) -> None:
        """Set the xint property."""
        self._cards[1].set_value("xint", value)

    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the Coupling card ID number
        """ # nopep8
        return self._cards[2].cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        """Set the coupid property."""
        self._cards[2].cards[0].set_value("coupid", value)
        self._cards[3].cards[0].set_value("coupid", value)

        if value:
            self.activate_option("COUPID")

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the A description of this coupling definition
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

