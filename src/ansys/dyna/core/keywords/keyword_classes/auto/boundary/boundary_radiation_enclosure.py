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

"""Module providing the BoundaryRadiationEnclosure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYRADIATIONENCLOSURE_CARD0 = (
    FieldSchema("brencid", int, 0, 10, None),
    FieldSchema("encname", str, 10, 70, None),
)

_BOUNDARYRADIATIONENCLOSURE_CARD1 = (
    FieldSchema("calopt", int, 0, 10, 0),
    FieldSchema("outopt", int, 10, 10, 0),
    FieldSchema("conopt", int, 20, 10, 0),
)

_BOUNDARYRADIATIONENCLOSURE_CARD2 = (
    FieldSchema("encname", str, 0, 80, None),
)

_BOUNDARYRADIATIONENCLOSURE_CARD3 = (
    FieldSchema("smflag", int, 0, 10, 0),
    FieldSchema("smmaxi", int, 10, 10, 500),
    FieldSchema("smabst", float, 20, 10, 1e-10),
    FieldSchema("smrelt", float, 30, 10, 1e-06),
)

_BOUNDARYRADIATIONENCLOSURE_CARD4 = (
    FieldSchema("stype", int, 0, 10, 0),
    FieldSchema("slmaxi", int, 10, 10, 500),
    FieldSchema("slabst", float, 20, 10, 1e-10),
    FieldSchema("slrelt", float, 30, 10, 1e-06),
    FieldSchema("slmlev", int, 40, 10, 0),
)

_BOUNDARYRADIATIONENCLOSURE_CARD5 = (
    FieldSchema("ssid", int, 0, 10, None),
)

_BOUNDARYRADIATIONENCLOSURE_CARD6 = (
    FieldSchema("nint", int, 0, 10, None),
    FieldSchema("block", int, 10, 10, 0),
    FieldSchema("selcid", int, 20, 10, 0),
    FieldSchema("semult", float, 30, 10, 0.0),
    FieldSchema("loc", int, 40, 10, 0),
)

class BoundaryRadiationEnclosure(KeywordBase):
    """DYNA BOUNDARY_RADIATION_ENCLOSURE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_ENCLOSURE"
    _link_fields = {
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryRadiationEnclosure class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYRADIATIONENCLOSURE_CARD6,
                **kwargs,
            ),        ]
    @property
    def brencid(self) -> typing.Optional[int]:
        """Get or set the Boundary radiation ID for this enclosure
        """ # nopep8
        return self._cards[0].get_value("brencid")

    @brencid.setter
    def brencid(self, value: int) -> None:
        """Set the brencid property."""
        self._cards[0].set_value("brencid", value)

    @property
    def encname(self) -> typing.Optional[str]:
        """Get or set the Name of enclosure, used for output purposes
        """ # nopep8
        return self._cards[0].get_value("encname")

    @encname.setter
    def encname(self, value: str) -> None:
        """Set the encname property."""
        self._cards[0].set_value("encname", value)

    @property
    def calopt(self) -> int:
        """Get or set the Calculation option:
        EQ.0:	view factors
        """ # nopep8
        return self._cards[1].get_value("calopt")

    @calopt.setter
    def calopt(self, value: int) -> None:
        """Set the calopt property."""
        self._cards[1].set_value("calopt", value)

    @property
    def outopt(self) -> int:
        """Get or set the Output option:
        EQ.0:	no output
        EQ.1 : output in LSDA format
        """ # nopep8
        return self._cards[1].get_value("outopt")

    @outopt.setter
    def outopt(self, value: int) -> None:
        """Set the outopt property."""
        if value not in [0, 1, None]:
            raise Exception("""outopt must be `None` or one of {0,1}.""")
        self._cards[1].set_value("outopt", value)

    @property
    def conopt(self) -> int:
        """Get or set the Control option:
        EQ.0:	calculate view factors matrix and preform thermal analysis
        """ # nopep8
        return self._cards[1].get_value("conopt")

    @conopt.setter
    def conopt(self, value: int) -> None:
        """Set the conopt property."""
        self._cards[1].set_value("conopt", value)

    @property
    def encname(self) -> typing.Optional[str]:
        """Get or set the Name of view factor output file
        """ # nopep8
        return self._cards[2].get_value("encname")

    @encname.setter
    def encname(self, value: str) -> None:
        """Set the encname property."""
        self._cards[2].set_value("encname", value)

    @property
    def smflag(self) -> int:
        """Get or set the View factor matrix smoothing flag:
        EQ.0:	no smoothing
        EQ.1 : smoothing
        """ # nopep8
        return self._cards[3].get_value("smflag")

    @smflag.setter
    def smflag(self, value: int) -> None:
        """Set the smflag property."""
        if value not in [0, 1, None]:
            raise Exception("""smflag must be `None` or one of {0,1}.""")
        self._cards[3].set_value("smflag", value)

    @property
    def smmaxi(self) -> int:
        """Get or set the Maximum number of iterations for view factor matrix smoothing (default = 500)
        """ # nopep8
        return self._cards[3].get_value("smmaxi")

    @smmaxi.setter
    def smmaxi(self, value: int) -> None:
        """Set the smmaxi property."""
        self._cards[3].set_value("smmaxi", value)

    @property
    def smabst(self) -> float:
        """Get or set the Absolute convergence tolerance for view factor matrix smoothing (default = 10-10)
        """ # nopep8
        return self._cards[3].get_value("smabst")

    @smabst.setter
    def smabst(self, value: float) -> None:
        """Set the smabst property."""
        self._cards[3].set_value("smabst", value)

    @property
    def smrelt(self) -> float:
        """Get or set the Relative convergence tolerance for view factor matrix smoothing (default = 10-6)
        """ # nopep8
        return self._cards[3].get_value("smrelt")

    @smrelt.setter
    def smrelt(self, value: float) -> None:
        """Set the smrelt property."""
        self._cards[3].set_value("smrelt", value)

    @property
    def stype(self) -> int:
        """Get or set the Solver type:
        EQ.0:	reverse conjugated gradient
        """ # nopep8
        return self._cards[4].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        self._cards[4].set_value("stype", value)

    @property
    def slmaxi(self) -> int:
        """Get or set the Maximum number of iterations for radiosity solver (default = 500)
        """ # nopep8
        return self._cards[4].get_value("slmaxi")

    @slmaxi.setter
    def slmaxi(self, value: int) -> None:
        """Set the slmaxi property."""
        self._cards[4].set_value("slmaxi", value)

    @property
    def slabst(self) -> float:
        """Get or set the Absolute convergence tolerance for radiosity solver (default is 10-10)
        """ # nopep8
        return self._cards[4].get_value("slabst")

    @slabst.setter
    def slabst(self, value: float) -> None:
        """Set the slabst property."""
        self._cards[4].set_value("slabst", value)

    @property
    def slrelt(self) -> float:
        """Get or set the Relative convergence tolerance for radiosity solver (default = 10-6)
        """ # nopep8
        return self._cards[4].get_value("slrelt")

    @slrelt.setter
    def slrelt(self, value: float) -> None:
        """Set the slrelt property."""
        self._cards[4].set_value("slrelt", value)

    @property
    def slmlev(self) -> int:
        """Get or set the Radiosity solver message level:
        EQ.0:	no output
        EQ.1 : debug output level I
        EQ.2 : debug output level II
        EQ.3 : debug output level III
        """ # nopep8
        return self._cards[4].get_value("slmlev")

    @slmlev.setter
    def slmlev(self, value: int) -> None:
        """Set the slmlev property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""slmlev must be `None` or one of {0,1,2,3}.""")
        self._cards[4].set_value("slmlev", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the SSID specifies the ID for a set of segments that comprise a portion of, or possibly, the entire enclosure. See *SET_‌SEGMENT.
        """ # nopep8
        return self._cards[5].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[5].set_value("ssid", value)

    @property
    def nint(self) -> typing.Optional[int]:
        """Get or set the Number of integration points for view factor calculation, 1 ≤ NINT ≤ 10
        EQ.0:	LS - DYNA determines the number of integration points based on the segment size and separation distance.
        """ # nopep8
        return self._cards[6].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        """Set the nint property."""
        self._cards[6].set_value("nint", value)

    @property
    def block(self) -> int:
        """Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces:
        EQ.0:	no blocking(default)
        EQ.1 : blocking
        """ # nopep8
        return self._cards[6].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        """Set the block property."""
        self._cards[6].set_value("block", value)

    @property
    def selcid(self) -> int:
        """Get or set the Load curve ID for surface emissivity (see *DEFINE_‌CURVE):
        GT.0:	surface emissivity as a function of time
        EQ.0 : use constant multiplier value, SEMULT
        LT.0 : surface emissivity as a function of temperature.The value of –SELCID must be an integer,and it is interpreted as a load curve ID.
        """ # nopep8
        return self._cards[6].get_value("selcid")

    @selcid.setter
    def selcid(self, value: int) -> None:
        """Set the selcid property."""
        self._cards[6].set_value("selcid", value)

    @property
    def semult(self) -> float:
        """Get or set the Curve multiplier for surface emissivity; see *DEFINE_‌CURVE
        """ # nopep8
        return self._cards[6].get_value("semult")

    @semult.setter
    def semult(self, value: float) -> None:
        """Set the semult property."""
        self._cards[6].set_value("semult", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements (see THSHEL in the *CONTROL_‌SHELL input):
        EQ. - 1:	lower surface of thermal shell element
        EQ.0 : middle surface of thermal shell element
        EQ.1 : upper surface of thermal shell element
        """ # nopep8
        return self._cards[6].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        """Set the loc property."""
        self._cards[6].set_value("loc", value)

    @property
    def ssid_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

