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

"""Module providing the MatJointedRock class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATJOINTEDROCK_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("gmod", float, 20, 10, None),
    FieldSchema("rnu", float, 30, 10, None),
    FieldSchema("rkf", float, 40, 10, 1.0),
    FieldSchema("phi", float, 50, 10, None),
    FieldSchema("cval", float, 60, 10, None),
    FieldSchema("psi", float, 70, 10, 0.0),
)

_MATJOINTEDROCK_CARD1 = (
    FieldSchema("str_lim", float, 0, 10, 0.005),
    FieldSchema("nplanes", int, 10, 10, 0),
    FieldSchema("elastic", int, 20, 10, 0),
    FieldSchema("lccpdr", int, 30, 10, 0),
    FieldSchema("lccpt", int, 40, 10, 0),
    FieldSchema("lccjdr", int, 50, 10, 0),
    FieldSchema("lccjt", int, 60, 10, 0),
    FieldSchema("lcsfac", int, 70, 10, 0),
)

_MATJOINTEDROCK_CARD2 = (
    FieldSchema("gmoddp", float, 0, 10, None),
    FieldSchema("phidp", float, 10, 10, None),
    FieldSchema("cvaldp", float, 20, 10, None),
    FieldSchema("psidp", float, 30, 10, None),
    FieldSchema("gmodgr", float, 40, 10, None),
    FieldSchema("phigr", float, 50, 10, None),
    FieldSchema("cvalgr", float, 60, 10, None),
    FieldSchema("psigr", float, 70, 10, None),
)

_MATJOINTEDROCK_CARD3 = (
    FieldSchema("dip", float, 0, 10, None),
    FieldSchema("strike", float, 10, 10, None),
    FieldSchema("cplane", float, 20, 10, None),
    FieldSchema("frplane", float, 30, 10, None),
    FieldSchema("tplane", float, 40, 10, None),
    FieldSchema("shrmax", float, 50, 10, 1e+20),
    FieldSchema("local", float, 60, 10, None),
)

_MATJOINTEDROCK_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatJointedRock(KeywordBase):
    """DYNA MAT_JOINTED_ROCK keyword"""

    keyword = "MAT"
    subkeyword = "JOINTED_ROCK"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatJointedRock class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATJOINTEDROCK_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOINTEDROCK_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOINTEDROCK_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOINTEDROCK_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatJointedRock._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATJOINTEDROCK_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
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
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        """Set the gmod property."""
        self._cards[0].set_value("gmod", value)

    @property
    def rnu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("rnu")

    @rnu.setter
    def rnu(self, value: float) -> None:
        """Set the rnu property."""
        self._cards[0].set_value("rnu", value)

    @property
    def rkf(self) -> float:
        """Get or set the Failture surface shape parameter.
        """ # nopep8
        return self._cards[0].get_value("rkf")

    @rkf.setter
    def rkf(self, value: float) -> None:
        """Set the rkf property."""
        self._cards[0].set_value("rkf", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Angle of friction (radians).
        """ # nopep8
        return self._cards[0].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        """Set the phi property."""
        self._cards[0].set_value("phi", value)

    @property
    def cval(self) -> typing.Optional[float]:
        """Get or set the Cohesion value (shear strength at zero normal stress).
        """ # nopep8
        return self._cards[0].get_value("cval")

    @cval.setter
    def cval(self, value: float) -> None:
        """Set the cval property."""
        self._cards[0].set_value("cval", value)

    @property
    def psi(self) -> float:
        """Get or set the Dilation angle (radians).
        """ # nopep8
        return self._cards[0].get_value("psi")

    @psi.setter
    def psi(self, value: float) -> None:
        """Set the psi property."""
        self._cards[0].set_value("psi", value)

    @property
    def str_lim(self) -> float:
        """Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
        """ # nopep8
        return self._cards[1].get_value("str_lim")

    @str_lim.setter
    def str_lim(self, value: float) -> None:
        """Set the str_lim property."""
        self._cards[1].set_value("str_lim", value)

    @property
    def nplanes(self) -> int:
        """Get or set the Number of jointed planes (maximum 3).
        """ # nopep8
        return self._cards[1].get_value("nplanes")

    @nplanes.setter
    def nplanes(self, value: int) -> None:
        """Set the nplanes property."""
        self._cards[1].set_value("nplanes", value)

    @property
    def elastic(self) -> int:
        """Get or set the Behavior of base material (see Remark 3):
        EQ.0: Nonlinear using all parameters on Cards 1 through 3
        EQ.1: Linear elastic; only the joint planes are nonlinear
        """ # nopep8
        return self._cards[1].get_value("elastic")

    @elastic.setter
    def elastic(self, value: int) -> None:
        """Set the elastic property."""
        if value not in [0, 1, None]:
            raise Exception("""elastic must be `None` or one of {0,1}.""")
        self._cards[1].set_value("elastic", value)

    @property
    def lccpdr(self) -> int:
        """Get or set the Loadcurve for extra cohesion for base material (dynamic relaxation) as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lccpdr")

    @lccpdr.setter
    def lccpdr(self, value: int) -> None:
        """Set the lccpdr property."""
        self._cards[1].set_value("lccpdr", value)

    @property
    def lccpt(self) -> int:
        """Get or set the Loadcurve for extra cohesion for base material (transient) as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lccpt")

    @lccpt.setter
    def lccpt(self, value: int) -> None:
        """Set the lccpt property."""
        self._cards[1].set_value("lccpt", value)

    @property
    def lccjdr(self) -> int:
        """Get or set the Loadcurve for extra cohesion for joints (dynamic relaxation) as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lccjdr")

    @lccjdr.setter
    def lccjdr(self, value: int) -> None:
        """Set the lccjdr property."""
        self._cards[1].set_value("lccjdr", value)

    @property
    def lccjt(self) -> int:
        """Get or set the Loadcurve for extra cohesion for joints (transient) as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lccjt")

    @lccjt.setter
    def lccjt(self, value: int) -> None:
        """Set the lccjt property."""
        self._cards[1].set_value("lccjt", value)

    @property
    def lcsfac(self) -> int:
        """Get or set the Loadcurve giving factor on strength as a function of time.
        """ # nopep8
        return self._cards[1].get_value("lcsfac")

    @lcsfac.setter
    def lcsfac(self, value: int) -> None:
        """Set the lcsfac property."""
        self._cards[1].set_value("lcsfac", value)

    @property
    def gmoddp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate at which GMOD is correct
        """ # nopep8
        return self._cards[2].get_value("gmoddp")

    @gmoddp.setter
    def gmoddp(self, value: float) -> None:
        """Set the gmoddp property."""
        self._cards[2].set_value("gmoddp", value)

    @property
    def phidp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate at which PHI is correct
        """ # nopep8
        return self._cards[2].get_value("phidp")

    @phidp.setter
    def phidp(self, value: float) -> None:
        """Set the phidp property."""
        self._cards[2].set_value("phidp", value)

    @property
    def cvaldp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate at which CVAL is correct
        """ # nopep8
        return self._cards[2].get_value("cvaldp")

    @cvaldp.setter
    def cvaldp(self, value: float) -> None:
        """Set the cvaldp property."""
        self._cards[2].set_value("cvaldp", value)

    @property
    def psidp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate at which PSI is correct
        """ # nopep8
        return self._cards[2].get_value("psidp")

    @psidp.setter
    def psidp(self, value: float) -> None:
        """Set the psidp property."""
        self._cards[2].set_value("psidp", value)

    @property
    def gmodgr(self) -> typing.Optional[float]:
        """Get or set the Gradient of GMOD as a function of Z-coordinate (usually negative)
        """ # nopep8
        return self._cards[2].get_value("gmodgr")

    @gmodgr.setter
    def gmodgr(self, value: float) -> None:
        """Set the gmodgr property."""
        self._cards[2].set_value("gmodgr", value)

    @property
    def phigr(self) -> typing.Optional[float]:
        """Get or set the Gradient of PHI as a function of Z-coordinate.
        """ # nopep8
        return self._cards[2].get_value("phigr")

    @phigr.setter
    def phigr(self, value: float) -> None:
        """Set the phigr property."""
        self._cards[2].set_value("phigr", value)

    @property
    def cvalgr(self) -> typing.Optional[float]:
        """Get or set the Gradient of CVAL as a function of Z-coordinate (usually negative).
        """ # nopep8
        return self._cards[2].get_value("cvalgr")

    @cvalgr.setter
    def cvalgr(self, value: float) -> None:
        """Set the cvalgr property."""
        self._cards[2].set_value("cvalgr", value)

    @property
    def psigr(self) -> typing.Optional[float]:
        """Get or set the Gradient of PSI as a function of Z-coordinate .
        """ # nopep8
        return self._cards[2].get_value("psigr")

    @psigr.setter
    def psigr(self, value: float) -> None:
        """Set the psigr property."""
        self._cards[2].set_value("psigr", value)

    @property
    def dip(self) -> typing.Optional[float]:
        """Get or set the Angle of the plane in degrees below the horizontal.
        """ # nopep8
        return self._cards[3].get_value("dip")

    @dip.setter
    def dip(self, value: float) -> None:
        """Set the dip property."""
        self._cards[3].set_value("dip", value)

    @property
    def strike(self) -> typing.Optional[float]:
        """Get or set the Plan view angle (degrees) of downhill vector drawn on the plane.
        """ # nopep8
        return self._cards[3].get_value("strike")

    @strike.setter
    def strike(self, value: float) -> None:
        """Set the strike property."""
        self._cards[3].set_value("strike", value)

    @property
    def cplane(self) -> typing.Optional[float]:
        """Get or set the Cohesion for shear behaviour on plane.
        """ # nopep8
        return self._cards[3].get_value("cplane")

    @cplane.setter
    def cplane(self, value: float) -> None:
        """Set the cplane property."""
        self._cards[3].set_value("cplane", value)

    @property
    def frplane(self) -> typing.Optional[float]:
        """Get or set the Friction angle for shear behaviour on plane (degrees).
        """ # nopep8
        return self._cards[3].get_value("frplane")

    @frplane.setter
    def frplane(self, value: float) -> None:
        """Set the frplane property."""
        self._cards[3].set_value("frplane", value)

    @property
    def tplane(self) -> typing.Optional[float]:
        """Get or set the Tensile strength across plane (generally zero or very small).
        """ # nopep8
        return self._cards[3].get_value("tplane")

    @tplane.setter
    def tplane(self, value: float) -> None:
        """Set the tplane property."""
        self._cards[3].set_value("tplane", value)

    @property
    def shrmax(self) -> float:
        """Get or set the Max shear stress on plane (upper limit, independent of compression).
        """ # nopep8
        return self._cards[3].get_value("shrmax")

    @shrmax.setter
    def shrmax(self, value: float) -> None:
        """Set the shrmax property."""
        self._cards[3].set_value("shrmax", value)

    @property
    def local(self) -> typing.Optional[float]:
        """Get or set the Axes (see Remark 10)
        EQ=0: DIP and STRIKE are with respect to the global axes.
        EQ=1: DIP and STRIKE are with respect to the local element axes.
        """ # nopep8
        return self._cards[3].get_value("local")

    @local.setter
    def local(self, value: float) -> None:
        """Set the local property."""
        self._cards[3].set_value("local", value)

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

