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

"""Module providing the MatSoilBrick class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATSOILBRICK_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("rlamda", float, 20, 10, None),
    FieldSchema("rkappa", float, 30, 10, None),
    FieldSchema("riota", float, 40, 10, None),
    FieldSchema("rbeta1", float, 50, 10, None),
    FieldSchema("rbeta2", float, 60, 10, None),
    FieldSchema("rmu", float, 70, 10, 1.0),
)

_MATSOILBRICK_CARD1 = (
    FieldSchema("rnu", float, 0, 10, None),
    FieldSchema("rlcid", float, 10, 10, None),
    FieldSchema("tol", float, 20, 10, 0.0005),
    FieldSchema("pgcl", float, 30, 10, None),
    FieldSchema("sub_inc", float, 40, 10, None, "sub-inc"),
    FieldSchema("blk", float, 50, 10, None),
    FieldSchema("grav", float, 60, 10, 9.807),
    FieldSchema("theory", int, 70, 10, 0),
)

_MATSOILBRICK_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSoilBrick(KeywordBase):
    """DYNA MAT_SOIL_BRICK keyword"""

    keyword = "MAT"
    subkeyword = "SOIL_BRICK"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "rlcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatSoilBrick class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSOILBRICK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSOILBRICK_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSoilBrick.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSOILBRICK_OPTION0_CARD0,
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def rlamda(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("rlamda")

    @rlamda.setter
    def rlamda(self, value: float) -> None:
        """Set the rlamda property."""
        self._cards[0].set_value("rlamda", value)

    @property
    def rkappa(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("rkappa")

    @rkappa.setter
    def rkappa(self, value: float) -> None:
        """Set the rkappa property."""
        self._cards[0].set_value("rkappa", value)

    @property
    def riota(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("riota")

    @riota.setter
    def riota(self, value: float) -> None:
        """Set the riota property."""
        self._cards[0].set_value("riota", value)

    @property
    def rbeta1(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("rbeta1")

    @rbeta1.setter
    def rbeta1(self, value: float) -> None:
        """Set the rbeta1 property."""
        self._cards[0].set_value("rbeta1", value)

    @property
    def rbeta2(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("rbeta2")

    @rbeta2.setter
    def rbeta2(self, value: float) -> None:
        """Set the rbeta2 property."""
        self._cards[0].set_value("rbeta2", value)

    @property
    def rmu(self) -> float:
        """Get or set the Shape factor coefficient. This parameter will modify the shape of the yield surface used. 1.0 implies a von mises type surface, but 1.1 to 1.25 is more indicative of soils.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("rmu")

    @rmu.setter
    def rmu(self, value: float) -> None:
        """Set the rmu property."""
        self._cards[0].set_value("rmu", value)

    @property
    def rnu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[1].get_value("rnu")

    @rnu.setter
    def rnu(self, value: float) -> None:
        """Set the rnu property."""
        self._cards[1].set_value("rnu", value)

    @property
    def rlcid(self) -> typing.Optional[float]:
        """Get or set the Load curve identification number referring to a curve defining up to 10 pairs of 'string-length' vs G/Gmax points.
        """ # nopep8
        return self._cards[1].get_value("rlcid")

    @rlcid.setter
    def rlcid(self, value: float) -> None:
        """Set the rlcid property."""
        self._cards[1].set_value("rlcid", value)

    @property
    def tol(self) -> float:
        """Get or set the User defined tolerance for convergence checking.
        Default is set to 0.02.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[1].set_value("tol", value)

    @property
    def pgcl(self) -> typing.Optional[float]:
        """Get or set the Pre-consolidation ground level. This parameter defines the maximum surface level (relative to z = 0.0 in the model) of the soil throughout geological history. This is used calculate the maxuimum over buden pressure on the soil elements.
        """ # nopep8
        return self._cards[1].get_value("pgcl")

    @pgcl.setter
    def pgcl(self, value: float) -> None:
        """Set the pgcl property."""
        self._cards[1].set_value("pgcl", value)

    @property
    def sub_inc(self) -> typing.Optional[float]:
        """Get or set the User defined strain increment size. This is the maximum strain increment that the material model can normally cope with.
        If the value is exceeded a warning is echoed to the d3hsp file.
        """ # nopep8
        return self._cards[1].get_value("sub_inc")

    @sub_inc.setter
    def sub_inc(self, value: float) -> None:
        """Set the sub_inc property."""
        self._cards[1].set_value("sub_inc", value)

    @property
    def blk(self) -> typing.Optional[float]:
        """Get or set the The elastic bulk stiffness of the soil. This is used for the contact stiffness only.
        """ # nopep8
        return self._cards[1].get_value("blk")

    @blk.setter
    def blk(self, value: float) -> None:
        """Set the blk property."""
        self._cards[1].set_value("blk", value)

    @property
    def grav(self) -> float:
        """Get or set the The gravitational acceleration. This is used to calculate the element stresses due the overlying soil.
        Default is set to 9.807 m/s2.
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[1].set_value("grav", value)

    @property
    def theory(self) -> int:
        """Get or set the Version of material subroutines used (see Remarks 7 and 8):
        EQ.0:	1995 version(default)
        EQ.4 : 2003 version, load / unload initialization
        EQ.7 : 2003 version, load / unload initialization, anisotropy from Ellison et al(2012)
        EQ.104 : 2003 version, load / unload / reload initialization
        EQ.107 : 2003 version, load / unload / reload initialization, anisotropy from Ellison et al(2012)
        EQ.204 : 2015 version, load / unload initialization
        EQ.304 : 2015 version, load / unload / reload initialization.
        """ # nopep8
        return self._cards[1].get_value("theory")

    @theory.setter
    def theory(self, value: int) -> None:
        """Set the theory property."""
        if value not in [0, 4, 7, 104, 107, 204, 304, None]:
            raise Exception("""theory must be `None` or one of {0,4,7,104,107,204,304}.""")
        self._cards[1].set_value("theory", value)

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
    def rlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for rlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.rlcid:
                return kwd
        return None

    @rlcid_link.setter
    def rlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for rlcid."""
        self.rlcid = value.lcid

