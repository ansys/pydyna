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

"""Module providing the SectionSphEllipse class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONSPHELLIPSE_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("cslh", float, 10, 10, 1.2),
    FieldSchema("hmin", float, 20, 10, 0.2),
    FieldSchema("hmax", float, 30, 10, 2.0),
    FieldSchema("sphini", float, 40, 10, 0.0),
    FieldSchema("death", float, 50, 10, 1e+20),
    FieldSchema("start", float, 60, 10, 0.0),
    FieldSchema("sphkern", int, 70, 10, 0),
)

_SECTIONSPHELLIPSE_CARD1 = (
    FieldSchema("hxcslh", float, 0, 10, None),
    FieldSchema("hycslh", float, 10, 10, None),
    FieldSchema("hzcslh", float, 20, 10, None),
    FieldSchema("hxini", float, 30, 10, None),
    FieldSchema("hyini", float, 40, 10, None),
    FieldSchema("hzini", float, 50, 10, None),
)

_SECTIONSPHELLIPSE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionSphEllipse(KeywordBase):
    """DYNA SECTION_SPH_ELLIPSE keyword"""

    keyword = "SECTION"
    subkeyword = "SPH_ELLIPSE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionSphEllipse class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONSPHELLIPSE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONSPHELLIPSE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionSphEllipse.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONSPHELLIPSE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def cslh(self) -> float:
        """Get or set the Constant applied to the smoothing length of the particles.
        The default is set to 1.2. This value applies for most problems.
        Values between 1.05 and 1.3 are acceptable.  Taking a value less than 1 is inadmissible. Values larger than 1.3 will increase the computational time.
        """ # nopep8
        return self._cards[0].get_value("cslh")

    @cslh.setter
    def cslh(self, value: float) -> None:
        """Set the cslh property."""
        self._cards[0].set_value("cslh", value)

    @property
    def hmin(self) -> float:
        """Get or set the Scale factor for the minimum smoothing length.
        """ # nopep8
        return self._cards[0].get_value("hmin")

    @hmin.setter
    def hmin(self, value: float) -> None:
        """Set the hmin property."""
        self._cards[0].set_value("hmin", value)

    @property
    def hmax(self) -> float:
        """Get or set the Scale factor for the maximum smoothing length.
        """ # nopep8
        return self._cards[0].get_value("hmax")

    @hmax.setter
    def hmax(self, value: float) -> None:
        """Set the hmax property."""
        self._cards[0].set_value("hmax", value)

    @property
    def sphini(self) -> float:
        """Get or set the Optional initial smoothing length (overrides true smoothing length). This option applies to avoid LS-DYNA to calculate the smoothing length during initialization. In this case, the variable CSLH doesn't apply.
        """ # nopep8
        return self._cards[0].get_value("sphini")

    @sphini.setter
    def sphini(self, value: float) -> None:
        """Set the sphini property."""
        self._cards[0].set_value("sphini", value)

    @property
    def death(self) -> float:
        """Get or set the Time imposed SPH approximation is stopped.
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[0].set_value("death", value)

    @property
    def start(self) -> float:
        """Get or set the Time imposed SPH approximation is activated.
        """ # nopep8
        return self._cards[0].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[0].set_value("start", value)

    @property
    def sphkern(self) -> int:
        """Get or set the Option for SPH kernel functions (smoothing functions):
        EQ.0: Cubic spline kernel function (default).
        EQ.1: Quintic spline kernel function: higher order smoothing function with bigger support size (recommend to use
        HMAX = 3.0 or bigger value, only available for FORM = 0, 1, 9 and 10).
        """ # nopep8
        return self._cards[0].get_value("sphkern")

    @sphkern.setter
    def sphkern(self, value: int) -> None:
        """Set the sphkern property."""
        if value not in [0, 1, None]:
            raise Exception("""sphkern must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sphkern", value)

    @property
    def hxcslh(self) -> typing.Optional[float]:
        """Get or set the Constant applied for the smoothing length in the X direction for the ellipse case.
        """ # nopep8
        return self._cards[1].get_value("hxcslh")

    @hxcslh.setter
    def hxcslh(self, value: float) -> None:
        """Set the hxcslh property."""
        self._cards[1].set_value("hxcslh", value)

    @property
    def hycslh(self) -> typing.Optional[float]:
        """Get or set the Constant applied for the smoothing length in the Y direction for the ellipse case.
        """ # nopep8
        return self._cards[1].get_value("hycslh")

    @hycslh.setter
    def hycslh(self, value: float) -> None:
        """Set the hycslh property."""
        self._cards[1].set_value("hycslh", value)

    @property
    def hzcslh(self) -> typing.Optional[float]:
        """Get or set the Constant applied for the smoothing length in the Z direction for the ellipse case.
        """ # nopep8
        return self._cards[1].get_value("hzcslh")

    @hzcslh.setter
    def hzcslh(self, value: float) -> None:
        """Set the hzcslh property."""
        self._cards[1].set_value("hzcslh", value)

    @property
    def hxini(self) -> typing.Optional[float]:
        """Get or set the Optional initial smoothing length in the X direction for the ellipse case, overrides true smoothing length
        """ # nopep8
        return self._cards[1].get_value("hxini")

    @hxini.setter
    def hxini(self, value: float) -> None:
        """Set the hxini property."""
        self._cards[1].set_value("hxini", value)

    @property
    def hyini(self) -> typing.Optional[float]:
        """Get or set the Optional initial smoothing length in the Y direction for the ellipse case, overrides true smoothing length
        """ # nopep8
        return self._cards[1].get_value("hyini")

    @hyini.setter
    def hyini(self, value: float) -> None:
        """Set the hyini property."""
        self._cards[1].set_value("hyini", value)

    @property
    def hzini(self) -> typing.Optional[float]:
        """Get or set the Optional initial smoothing length in the Z direction for the ellipse case, overrides true smoothing length
        """ # nopep8
        return self._cards[1].get_value("hzini")

    @hzini.setter
    def hzini(self, value: float) -> None:
        """Set the hzini property."""
        self._cards[1].set_value("hzini", value)

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

