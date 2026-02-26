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

"""Module providing the Mat197 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT197_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("a", float, 20, 10, 1.0),
    FieldSchema("beta", float, 30, 10, 0.5),
    FieldSchema("gamma", float, 40, 10, 0.5),
    FieldSchema("dispy", float, 50, 10, None),
    FieldSchema("stiffv", float, 60, 10, None),
    FieldSchema("itype", int, 70, 10, 0),
)

_MAT197_CARD1 = (
    FieldSchema("preload", float, 0, 10, 0.0),
    FieldSchema("damp", float, 10, 10, 1.0),
    FieldSchema("mx1", float, 20, 10, 0.0),
    FieldSchema("mx2", float, 30, 10, 0.0),
    FieldSchema("my1", float, 40, 10, 0.0),
    FieldSchema("my2", float, 50, 10, 0.0),
    FieldSchema("cde", float, 60, 10, 0.0),
    FieldSchema("iextra", int, 70, 10, 0),
)

_MAT197_CARD2 = (
    FieldSchema("fmax", float, 0, 10, 0.0),
    FieldSchema("delf", float, 10, 10, 0.0),
    FieldSchema("afric", float, 20, 10, 0.0),
    FieldSchema("radx", float, 30, 10, 1e+20),
    FieldSchema("rady", float, 40, 10, 1e+20),
    FieldSchema("radb", float, 50, 10, 1e+20),
    FieldSchema("stiffl", float, 60, 10, None),
    FieldSchema("stiffts", float, 70, 10, 0.0),
)

_MAT197_CARD3 = (
    FieldSchema("forcey", float, 0, 10, 0.0),
    FieldSchema("alpha", float, 10, 10, 0.0),
    FieldSchema("stifft", float, 20, 10, None),
    FieldSchema("dfail", float, 30, 10, 1e+20),
    FieldSchema("fmaxyc", float, 40, 10, None),
    FieldSchema("fmaxxt", float, 50, 10, None),
    FieldSchema("fmaxyt", float, 60, 10, None),
    FieldSchema("ylock", float, 70, 10, None),
)

_MAT197_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat197(KeywordBase):
    """DYNA MAT_197 keyword"""

    keyword = "MAT"
    subkeyword = "197"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat197 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT197_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT197_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT197_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT197_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat197.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT197_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def a(self) -> float:
        """Get or set the Nondimensional variable.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def beta(self) -> float:
        """Get or set the Nondimensional variable.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def gamma(self) -> float:
        """Get or set the Nondimensional variable.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def dispy(self) -> typing.Optional[float]:
        """Get or set the Yield displacement (length units - must be > 0.0).
        """ # nopep8
        return self._cards[0].get_value("dispy")

    @dispy.setter
    def dispy(self, value: float) -> None:
        """Set the dispy property."""
        self._cards[0].set_value("dispy", value)

    @property
    def stiffv(self) -> typing.Optional[float]:
        """Get or set the Vertical stiffness (force/length units).
        """ # nopep8
        return self._cards[0].get_value("stiffv")

    @stiffv.setter
    def stiffv(self, value: float) -> None:
        """Set the stiffv property."""
        self._cards[0].set_value("stiffv", value)

    @property
    def itype(self) -> int:
        """Get or set the Type:
        EQ.0:	sliding (spherical or cylindrical)
        EQ.1:	elastomeric
        EQ.2:	sliding (two perpendicular curved beams)
        EQ.3:	lead rubber bearing
        EQ.4: high damping rubber bearing.
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""itype must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("itype", value)

    @property
    def preload(self) -> float:
        """Get or set the Vertical preload not explicitly modelled (force units).
        """ # nopep8
        return self._cards[1].get_value("preload")

    @preload.setter
    def preload(self, value: float) -> None:
        """Set the preload property."""
        self._cards[1].set_value("preload", value)

    @property
    def damp(self) -> float:
        """Get or set the Damping ratio (nondimensional).
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def mx1(self) -> float:
        """Get or set the Moment factor at ends 1 and 2 in local X-direction.
        """ # nopep8
        return self._cards[1].get_value("mx1")

    @mx1.setter
    def mx1(self, value: float) -> None:
        """Set the mx1 property."""
        self._cards[1].set_value("mx1", value)

    @property
    def mx2(self) -> float:
        """Get or set the Moment factor at ends 1 and 2 in local X-direction.
        """ # nopep8
        return self._cards[1].get_value("mx2")

    @mx2.setter
    def mx2(self, value: float) -> None:
        """Set the mx2 property."""
        self._cards[1].set_value("mx2", value)

    @property
    def my1(self) -> float:
        """Get or set the Moment factor at ends 1 and 2 in local Y-direction.
        """ # nopep8
        return self._cards[1].get_value("my1")

    @my1.setter
    def my1(self, value: float) -> None:
        """Set the my1 property."""
        self._cards[1].set_value("my1", value)

    @property
    def my2(self) -> float:
        """Get or set the Moment factor at ends 1 and 2 in local Y-direction.
        """ # nopep8
        return self._cards[1].get_value("my2")

    @my2.setter
    def my2(self, value: float) -> None:
        """Set the my2 property."""
        self._cards[1].set_value("my2", value)

    @property
    def cde(self) -> float:
        """Get or set the Viscous damping coefficient (ITYPE=1, 3 or 4).
        """ # nopep8
        return self._cards[1].get_value("cde")

    @cde.setter
    def cde(self, value: float) -> None:
        """Set the cde property."""
        self._cards[1].set_value("cde", value)

    @property
    def iextra(self) -> int:
        """Get or set the If IEXTRA = 1, optional Card 8 will be read.
        """ # nopep8
        return self._cards[1].get_value("iextra")

    @iextra.setter
    def iextra(self, value: int) -> None:
        """Set the iextra property."""
        self._cards[1].set_value("iextra", value)

    @property
    def fmax(self) -> float:
        """Get or set the Maximum friction coefficient (dynamic).
        """ # nopep8
        return self._cards[2].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[2].set_value("fmax", value)

    @property
    def delf(self) -> float:
        """Get or set the Difference between maximum friction and static friction coefficient.
        """ # nopep8
        return self._cards[2].get_value("delf")

    @delf.setter
    def delf(self, value: float) -> None:
        """Set the delf property."""
        self._cards[2].set_value("delf", value)

    @property
    def afric(self) -> float:
        """Get or set the Velocity multiplier in sliding friction equation (time/length units).
        """ # nopep8
        return self._cards[2].get_value("afric")

    @afric.setter
    def afric(self, value: float) -> None:
        """Set the afric property."""
        self._cards[2].set_value("afric", value)

    @property
    def radx(self) -> float:
        """Get or set the Radius for sliding in local X direction.
        """ # nopep8
        return self._cards[2].get_value("radx")

    @radx.setter
    def radx(self, value: float) -> None:
        """Set the radx property."""
        self._cards[2].set_value("radx", value)

    @property
    def rady(self) -> float:
        """Get or set the Radius for sliding in local Y direction.
        """ # nopep8
        return self._cards[2].get_value("rady")

    @rady.setter
    def rady(self, value: float) -> None:
        """Set the rady property."""
        self._cards[2].set_value("rady", value)

    @property
    def radb(self) -> float:
        """Get or set the Radius of retaining ring.
        """ # nopep8
        return self._cards[2].get_value("radb")

    @radb.setter
    def radb(self, value: float) -> None:
        """Set the radb property."""
        self._cards[2].set_value("radb", value)

    @property
    def stiffl(self) -> typing.Optional[float]:
        """Get or set the Stiffness for lateral contact against the retaining ring, default is STIFFV.
        """ # nopep8
        return self._cards[2].get_value("stiffl")

    @stiffl.setter
    def stiffl(self, value: float) -> None:
        """Set the stiffl property."""
        self._cards[2].set_value("stiffl", value)

    @property
    def stiffts(self) -> float:
        """Get or set the Stiffness for tensile vertical response (sliding isolator - default = 0).
        """ # nopep8
        return self._cards[2].get_value("stiffts")

    @stiffts.setter
    def stiffts(self, value: float) -> None:
        """Set the stiffts property."""
        self._cards[2].set_value("stiffts", value)

    @property
    def forcey(self) -> float:
        """Get or set the Yield force.
        """ # nopep8
        return self._cards[3].get_value("forcey")

    @forcey.setter
    def forcey(self, value: float) -> None:
        """Set the forcey property."""
        self._cards[3].set_value("forcey", value)

    @property
    def alpha(self) -> float:
        """Get or set the Ratio of postyielding stiffness to preyielding stiffness.
        """ # nopep8
        return self._cards[3].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[3].set_value("alpha", value)

    @property
    def stifft(self) -> typing.Optional[float]:
        """Get or set the Stiffness for tensile vertical response (elastomeric isolator), default is 0.5STIFF.
        """ # nopep8
        return self._cards[3].get_value("stifft")

    @stifft.setter
    def stifft(self, value: float) -> None:
        """Set the stifft property."""
        self._cards[3].set_value("stifft", value)

    @property
    def dfail(self) -> float:
        """Get or set the Lateral displacement at which the isolator fails.
        """ # nopep8
        return self._cards[3].get_value("dfail")

    @dfail.setter
    def dfail(self, value: float) -> None:
        """Set the dfail property."""
        self._cards[3].set_value("dfail", value)

    @property
    def fmaxyc(self) -> typing.Optional[float]:
        """Get or set the Max friction coefficient (dynamic) for local Y-axis (compression).  (ITYPE=2 only).
        """ # nopep8
        return self._cards[3].get_value("fmaxyc")

    @fmaxyc.setter
    def fmaxyc(self, value: float) -> None:
        """Set the fmaxyc property."""
        self._cards[3].set_value("fmaxyc", value)

    @property
    def fmaxxt(self) -> typing.Optional[float]:
        """Get or set the Max friction coefficient (dynamic) for local X-axis (tension). (ITYPE=2 only)..
        """ # nopep8
        return self._cards[3].get_value("fmaxxt")

    @fmaxxt.setter
    def fmaxxt(self, value: float) -> None:
        """Set the fmaxxt property."""
        self._cards[3].set_value("fmaxxt", value)

    @property
    def fmaxyt(self) -> typing.Optional[float]:
        """Get or set the Max friction coefficient (dynamic) for local Y-axis (tension). (ITYPE=2 only).
        """ # nopep8
        return self._cards[3].get_value("fmaxyt")

    @fmaxyt.setter
    def fmaxyt(self, value: float) -> None:
        """Set the fmaxyt property."""
        self._cards[3].set_value("fmaxyt", value)

    @property
    def ylock(self) -> typing.Optional[float]:
        """Get or set the Stiffness locking the local Y-displacement (optional -single-axis sliding).  (ITYPE=2 only).
        """ # nopep8
        return self._cards[3].get_value("ylock")

    @ylock.setter
    def ylock(self, value: float) -> None:
        """Set the ylock property."""
        self._cards[3].set_value("ylock", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

