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

"""Module providing the Mat071 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT071_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("lcid", int, 30, 10, 0),
    FieldSchema("f0", float, 40, 10, None),
    FieldSchema("tmaxf0", float, 50, 10, None),
    FieldSchema("tramp", float, 60, 10, None),
    FieldSchema("iread", int, 70, 10, 0),
)

_MAT071_CARD1 = (
    FieldSchema("output", int, 0, 10, 0),
    FieldSchema("tstart", float, 10, 10, 0.0),
    FieldSchema("fracl0", float, 20, 10, 0.0),
    FieldSchema("mxeps", float, 30, 10, 1e+20),
    FieldSchema("mxfrc", float, 40, 10, 1e+20),
)

_MAT071_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat071(KeywordBase):
    """DYNA MAT_071 keyword"""

    keyword = "MAT"
    subkeyword = "071"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat071 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT071_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT071_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat071.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT071_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the GT. 0.0: Young's modulus,
        LT. 0.0: Stiffness.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining the stress versus engineering strain. (optional).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def f0(self) -> typing.Optional[float]:
        """Get or set the Initial tensile force. If F0 is defined, an offset is not needed for an initial tensile force.
        """ # nopep8
        return self._cards[0].get_value("f0")

    @f0.setter
    def f0(self, value: float) -> None:
        """Set the f0 property."""
        self._cards[0].set_value("f0", value)

    @property
    def tmaxf0(self) -> typing.Optional[float]:
        """Get or set the Time for which pre-tension force will be held.
        """ # nopep8
        return self._cards[0].get_value("tmaxf0")

    @tmaxf0.setter
    def tmaxf0(self, value: float) -> None:
        """Set the tmaxf0 property."""
        self._cards[0].set_value("tmaxf0", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Ramp-up time for pre-tension force.
        """ # nopep8
        return self._cards[0].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[0].set_value("tramp", value)

    @property
    def iread(self) -> int:
        """Get or set the Set to 1 to read second line of input
        """ # nopep8
        return self._cards[0].get_value("iread")

    @iread.setter
    def iread(self, value: int) -> None:
        """Set the iread property."""
        self._cards[0].set_value("iread", value)

    @property
    def output(self) -> int:
        """Get or set the Flag =1 to output axial strain.
        """ # nopep8
        return self._cards[1].get_value("output")

    @output.setter
    def output(self, value: int) -> None:
        """Set the output property."""
        self._cards[1].set_value("output", value)

    @property
    def tstart(self) -> float:
        """Get or set the TSTART is the time at which the ramp-up of preload begins - it is relevant only if TRAMP is also non-zero.
        """ # nopep8
        return self._cards[1].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        """Set the tstart property."""
        self._cards[1].set_value("tstart", value)

    @property
    def fracl0(self) -> float:
        """Get or set the Fraction of initial length that should be reached over time period of
        TRAMP. Corresponding tensile force builds up as necessary to reach
        cable length = FRACL0*L0 at time t = TRAMP..
        """ # nopep8
        return self._cards[1].get_value("fracl0")

    @fracl0.setter
    def fracl0(self, value: float) -> None:
        """Set the fracl0 property."""
        self._cards[1].set_value("fracl0", value)

    @property
    def mxeps(self) -> float:
        """Get or set the Maximum strain at failure.
        """ # nopep8
        return self._cards[1].get_value("mxeps")

    @mxeps.setter
    def mxeps(self, value: float) -> None:
        """Set the mxeps property."""
        self._cards[1].set_value("mxeps", value)

    @property
    def mxfrc(self) -> float:
        """Get or set the Maximum force at failure.
        """ # nopep8
        return self._cards[1].get_value("mxfrc")

    @mxfrc.setter
    def mxfrc(self, value: float) -> None:
        """Set the mxfrc property."""
        self._cards[1].set_value("mxfrc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

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

