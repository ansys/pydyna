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

"""Module providing the DefineSpotweldFailurePid class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPOTWELDFAILUREPID_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("tflag", int, 10, 10, 0),
    FieldSchema("dc1", float, 20, 10, 1.183),
    FieldSchema("dc2", float, 30, 10, 0.002963),
    FieldSchema("dc3", float, 40, 10, 0.0458),
    FieldSchema("dc4", float, 50, 10, 0.1),
    FieldSchema("exn", float, 60, 10, 2.0),
    FieldSchema("exs", float, 70, 10, 2.0),
)

_DEFINESPOTWELDFAILUREPID_CARD1 = (
    FieldSchema("navg", int, 0, 10, 0),
    FieldSchema("d_sn", float, 10, 10, 0.0),
    FieldSchema("d_ss", float, 20, 10, 0.0),
    FieldSchema("r_sult", float, 30, 10, 0.0),
    FieldSchema("tscale", float, 40, 10, 1.0),
)

_DEFINESPOTWELDFAILUREPID_CARD2 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("sn", float, 20, 10, None),
    FieldSchema("ss", float, 30, 10, None),
)

_DEFINESPOTWELDFAILUREPID_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSpotweldFailurePid(KeywordBase):
    """DYNA DEFINE_SPOTWELD_FAILURE_PID keyword"""

    keyword = "DEFINE"
    subkeyword = "SPOTWELD_FAILURE_PID"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "pid1": LinkType.PART,
        "pid2": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSpotweldFailurePid class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDFAILUREPID_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDFAILUREPID_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDFAILUREPID_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSpotweldFailurePid.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPOTWELDFAILUREPID_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identification number of data set, input as FVAL on *MAT_SPOTWELD.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def tflag(self) -> int:
        """Get or set the Thickness flag for nominal stress calculation
        EQ.0:	Use minimum sheet thickness
        EQ.1:	Use average sheet thickness
        EQ.2	Use maximum sheet thickness
        EQ.3:	Use sum of sheet thicknesses.
        """ # nopep8
        return self._cards[0].get_value("tflag")

    @tflag.setter
    def tflag(self, value: int) -> None:
        """Set the tflag property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""tflag must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("tflag", value)

    @property
    def dc1(self) -> float:
        """Get or set the Dynamic coefficient C1.
        """ # nopep8
        return self._cards[0].get_value("dc1")

    @dc1.setter
    def dc1(self, value: float) -> None:
        """Set the dc1 property."""
        self._cards[0].set_value("dc1", value)

    @property
    def dc2(self) -> float:
        """Get or set the Dynamic coefficient C2.
        """ # nopep8
        return self._cards[0].get_value("dc2")

    @dc2.setter
    def dc2(self, value: float) -> None:
        """Set the dc2 property."""
        self._cards[0].set_value("dc2", value)

    @property
    def dc3(self) -> float:
        """Get or set the Dynamic coefficient C3.
        """ # nopep8
        return self._cards[0].get_value("dc3")

    @dc3.setter
    def dc3(self, value: float) -> None:
        """Set the dc3 property."""
        self._cards[0].set_value("dc3", value)

    @property
    def dc4(self) -> float:
        """Get or set the Dynamic coefficient C4.
        """ # nopep8
        return self._cards[0].get_value("dc4")

    @dc4.setter
    def dc4(self, value: float) -> None:
        """Set the dc4 property."""
        self._cards[0].set_value("dc4", value)

    @property
    def exn(self) -> float:
        """Get or set the Exponent on the normal term.
        """ # nopep8
        return self._cards[0].get_value("exn")

    @exn.setter
    def exn(self, value: float) -> None:
        """Set the exn property."""
        self._cards[0].set_value("exn", value)

    @property
    def exs(self) -> float:
        """Get or set the Exponent on the shear term.
        """ # nopep8
        return self._cards[0].get_value("exs")

    @exs.setter
    def exs(self, value: float) -> None:
        """Set the exs property."""
        self._cards[0].set_value("exs", value)

    @property
    def navg(self) -> int:
        """Get or set the Number of points in the time average of the load rates.
        """ # nopep8
        return self._cards[1].get_value("navg")

    @navg.setter
    def navg(self, value: int) -> None:
        """Set the navg property."""
        self._cards[1].set_value("navg", value)

    @property
    def d_sn(self) -> float:
        """Get or set the Reference value of the static normal strength.
        """ # nopep8
        return self._cards[1].get_value("d_sn")

    @d_sn.setter
    def d_sn(self, value: float) -> None:
        """Set the d_sn property."""
        self._cards[1].set_value("d_sn", value)

    @property
    def d_ss(self) -> float:
        """Get or set the Reference value of the static shear strength.
        """ # nopep8
        return self._cards[1].get_value("d_ss")

    @d_ss.setter
    def d_ss(self, value: float) -> None:
        """Set the d_ss property."""
        self._cards[1].set_value("d_ss", value)

    @property
    def r_sult(self) -> float:
        """Get or set the Reference ultimate strength .
        """ # nopep8
        return self._cards[1].get_value("r_sult")

    @r_sult.setter
    def r_sult(self, value: float) -> None:
        """Set the r_sult property."""
        self._cards[1].set_value("r_sult", value)

    @property
    def tscale(self) -> float:
        """Get or set the Scale factor for thickness used in nominal stress calculations.
        """ # nopep8
        return self._cards[1].get_value("tscale")

    @tscale.setter
    def tscale(self, value: float) -> None:
        """Set the tscale property."""
        self._cards[1].set_value("tscale", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part ID of welded shell part.
        """ # nopep8
        return self._cards[2].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[2].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part ID of part welded to PID1.
        """ # nopep8
        return self._cards[2].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[2].set_value("pid2", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Static normal strength of material MID.
        """ # nopep8
        return self._cards[2].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[2].set_value("sn", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Static shear strength of material MID.
        """ # nopep8
        return self._cards[2].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[2].set_value("ss", value)

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

    @property
    def pid1_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid1."""
        return self._get_link_by_attr("PART", "pid", self.pid1, "parts")

    @property
    def pid2_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid2."""
        return self._get_link_by_attr("PART", "pid", self.pid2, "parts")

