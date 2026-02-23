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

"""Module providing the PerturbationMat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_PERTURBATIONMAT_CARD0 = (
    FieldSchema("type", int, 0, 10, 1),
    FieldSchema("pid", int, 10, 10, 0),
    FieldSchema("scl", float, 20, 10, 1.0),
    FieldSchema("cmp", int, 30, 10, None),
    FieldSchema("icoord", int, 40, 10, 0),
    FieldSchema("cid", int, 50, 10, 0),
)

_PERTURBATIONMAT_CARD1 = (
    FieldSchema("ampl", float, 0, 10, 1.0),
    FieldSchema("xwl", float, 10, 10, 0.0),
    FieldSchema("xoff", float, 20, 10, 0.0),
    FieldSchema("ywl", float, 30, 10, 0.0),
    FieldSchema("yoff", float, 40, 10, 0.0),
    FieldSchema("zwl", float, 50, 10, 0.0),
    FieldSchema("zoff", float, 60, 10, 0.0),
)

_PERTURBATIONMAT_CARD2 = (
    FieldSchema("fade", float, 0, 10, 1.0),
)

_PERTURBATIONMAT_CARD3 = (
    FieldSchema("fname", str, 0, 80, None),
)

_PERTURBATIONMAT_CARD4 = (
    FieldSchema("cstype", int, 0, 10, 1),
    FieldSchema("ellip1", float, 10, 10, 0.0),
    FieldSchema("ellip2", float, 20, 10, 0.0),
    FieldSchema("rnd", int, 30, 10, 0),
)

_PERTURBATIONMAT_CARD5 = (
    FieldSchema("cftype", int, 0, 10, 1),
    FieldSchema("cfc1", float, 10, 10, 1.0),
    FieldSchema("cfc2", float, 20, 10, 1.0),
    FieldSchema("cfc3", float, 30, 10, 1.0),
)

_PERTURBATIONMAT_CARD6 = (
    FieldSchema("ampl", int, 0, 10, 1),
    FieldSchema("dtype", float, 10, 10, 0.0),
)

class PerturbationMat(KeywordBase):
    """DYNA PERTURBATION_MAT keyword"""

    keyword = "PERTURBATION"
    subkeyword = "MAT"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the PerturbationMat class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _PERTURBATIONMAT_CARD6,
                **kwargs,
            ),        ]
    @property
    def type(self) -> int:
        """Get or set the Type of perturbation.
        EQ.1:	Harmonic Field (see Remark 3)
        EQ.2:unused
        EQ.3:	Read perturbations from a file
        EQ.4 : Spectral field
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""type must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("type", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def scl(self) -> float:
        """Get or set the Scale factor.
        """ # nopep8
        return self._cards[0].get_value("scl")

    @scl.setter
    def scl(self, value: float) -> None:
        """Set the scl property."""
        self._cards[0].set_value("scl", value)

    @property
    def cmp(self) -> typing.Optional[int]:
        """Get or set the Component.
        """ # nopep8
        return self._cards[0].get_value("cmp")

    @cmp.setter
    def cmp(self, value: int) -> None:
        """Set the cmp property."""
        self._cards[0].set_value("cmp", value)

    @property
    def icoord(self) -> int:
        """Get or set the Coordinate system to use;
        EQ.0: Global Cartesian
        EQ.1: Cartesian
        EQ.2: Cylindrical (computed and applied)
        EQ.3: Spherical (computed and applied)
        EQ.-2: Computed in cartesian but applied in cylindrical
        EQ.-3  Computed in cartesian but applied in spherical.
        """ # nopep8
        return self._cards[0].get_value("icoord")

    @icoord.setter
    def icoord(self, value: int) -> None:
        """Set the icoord property."""
        if value not in [0, 1, 2, 3, -2, -3, None]:
            raise Exception("""icoord must be `None` or one of {0,1,2,3,-2,-3}.""")
        self._cards[0].set_value("icoord", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID,see *DEFINE_‌COORDINATE_‌NODES.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def ampl(self) -> float:
        """Get or set the Amplitude of the harmonic perturbation.
        """ # nopep8
        return self._cards[1].get_value("ampl")

    @ampl.setter
    def ampl(self, value: float) -> None:
        """Set the ampl property."""
        self._cards[1].set_value("ampl", value)

    @property
    def xwl(self) -> float:
        """Get or set the x wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("xwl")

    @xwl.setter
    def xwl(self, value: float) -> None:
        """Set the xwl property."""
        self._cards[1].set_value("xwl", value)

    @property
    def xoff(self) -> float:
        """Get or set the x offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("xoff")

    @xoff.setter
    def xoff(self, value: float) -> None:
        """Set the xoff property."""
        self._cards[1].set_value("xoff", value)

    @property
    def ywl(self) -> float:
        """Get or set the y wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("ywl")

    @ywl.setter
    def ywl(self, value: float) -> None:
        """Set the ywl property."""
        self._cards[1].set_value("ywl", value)

    @property
    def yoff(self) -> float:
        """Get or set the y offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("yoff")

    @yoff.setter
    def yoff(self, value: float) -> None:
        """Set the yoff property."""
        self._cards[1].set_value("yoff", value)

    @property
    def zwl(self) -> float:
        """Get or set the z wavelength of the harmonic field.
        """ # nopep8
        return self._cards[1].get_value("zwl")

    @zwl.setter
    def zwl(self, value: float) -> None:
        """Set the zwl property."""
        self._cards[1].set_value("zwl", value)

    @property
    def zoff(self) -> float:
        """Get or set the z offset of harmonic field.
        """ # nopep8
        return self._cards[1].get_value("zoff")

    @zoff.setter
    def zoff(self, value: float) -> None:
        """Set the zoff property."""
        self._cards[1].set_value("zoff", value)

    @property
    def fade(self) -> float:
        """Get or set the Distance over which all *PERTURBATION_NODE are faded to zero.
        """ # nopep8
        return self._cards[2].get_value("fade")

    @fade.setter
    def fade(self, value: float) -> None:
        """Set the fade property."""
        self._cards[2].set_value("fade", value)

    @property
    def fname(self) -> typing.Optional[str]:
        """Get or set the Name of file containing the perturbation definitions.
        """ # nopep8
        return self._cards[3].get_value("fname")

    @fname.setter
    def fname(self, value: str) -> None:
        """Set the fname property."""
        self._cards[3].set_value("fname", value)

    @property
    def cstype(self) -> int:
        """Get or set the Correlation structure:
        EQ.1: 3D isotropic. The X, Y and Z correlations are described using one correlation function.
        EQ.2: 3D product. The X, Y and Z correlations are described using a correlation function each.
        EQ.3: 2D isotropic. A correlation function describes the X correlation while the YZ isotropic relationship is described using another correlation function.
        EQ.4: 2D isotropic. A correlation function describes the Y correlation while the XZ isotropic relationship is described using another correlation function.
        EQ.5: 2D isotropic. A correlation function describes the Z correlation while the XY isotropic relationship is described using another correlation function.
        EQ.6: 3D elliptic. Define CSE1 and CSE2.
        EQ.7: 2D elliptic. A correlation function describes the X correlation while the YZ elliptic relationship is described using another correlation function.
        EQ.8: 2D elliptic. A correlation function describes the Y correlation while the ZX elliptic relationship is described using another correlation function.
        EQ.9: 2D elliptic. A correlation function describes the Z correlation while the XY elliptic relationship is described using another correlation function.
        """ # nopep8
        return self._cards[4].get_value("cstype")

    @cstype.setter
    def cstype(self, value: int) -> None:
        """Set the cstype property."""
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9, None]:
            raise Exception("""cstype must be `None` or one of {1,2,3,4,5,6,7,8,9}.""")
        self._cards[4].set_value("cstype", value)

    @property
    def ellip1(self) -> float:
        """Get or set the Elliptic constant for 2D and 3D elliptic fields.
        """ # nopep8
        return self._cards[4].get_value("ellip1")

    @ellip1.setter
    def ellip1(self, value: float) -> None:
        """Set the ellip1 property."""
        self._cards[4].set_value("ellip1", value)

    @property
    def ellip2(self) -> float:
        """Get or set the Elliptic constant for 3D elliptic field.
        """ # nopep8
        return self._cards[4].get_value("ellip2")

    @ellip2.setter
    def ellip2(self, value: float) -> None:
        """Set the ellip2 property."""
        self._cards[4].set_value("ellip2", value)

    @property
    def rnd(self) -> int:
        """Get or set the Seed for random number generator.
        EQ.0: LS-DYNA will generate a random seed
        GT.0: Value to be used as seed.
        """ # nopep8
        return self._cards[4].get_value("rnd")

    @rnd.setter
    def rnd(self, value: int) -> None:
        """Set the rnd property."""
        self._cards[4].set_value("rnd", value)

    @property
    def cftype(self) -> int:
        """Get or set the Correlation function
        EQ.1: Gaussian
        EQ.2: Exponential
        EQ.3: Exponential Cosine
        EQ.4: Rational
        EQ.5: Linear.
        """ # nopep8
        return self._cards[5].get_value("cftype")

    @cftype.setter
    def cftype(self, value: int) -> None:
        """Set the cftype property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""cftype must be `None` or one of {1,2,3,4,5}.""")
        self._cards[5].set_value("cftype", value)

    @property
    def cfc1(self) -> float:
        """Get or set the Correlation function constant 1.
        """ # nopep8
        return self._cards[5].get_value("cfc1")

    @cfc1.setter
    def cfc1(self, value: float) -> None:
        """Set the cfc1 property."""
        self._cards[5].set_value("cfc1", value)

    @property
    def cfc2(self) -> float:
        """Get or set the Correlation function constant 2.
        """ # nopep8
        return self._cards[5].get_value("cfc2")

    @cfc2.setter
    def cfc2(self, value: float) -> None:
        """Set the cfc2 property."""
        self._cards[5].set_value("cfc2", value)

    @property
    def cfc3(self) -> float:
        """Get or set the Correlation function constant 3.
        """ # nopep8
        return self._cards[5].get_value("cfc3")

    @cfc3.setter
    def cfc3(self, value: float) -> None:
        """Set the cfc3 property."""
        self._cards[5].set_value("cfc3", value)

    @property
    def ampl(self) -> int:
        """Get or set the Amplitude of the random perturbation.
        """ # nopep8
        return self._cards[6].get_value("ampl")

    @ampl.setter
    def ampl(self, value: int) -> None:
        """Set the ampl property."""
        self._cards[6].set_value("ampl", value)

    @property
    def dtype(self) -> float:
        """Get or set the Distribution type:
        EQ.0.0:	Uniform distribution between SCL×[0,AMPL]
        EQ.1.0 : Uniform distribution between SCL×[-AMPL ,AMPL]
        """ # nopep8
        return self._cards[6].get_value("dtype")

    @dtype.setter
    def dtype(self, value: float) -> None:
        """Set the dtype property."""
        self._cards[6].set_value("dtype", value)

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

