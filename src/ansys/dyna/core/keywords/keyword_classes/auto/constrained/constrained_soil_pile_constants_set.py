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

"""Module providing the ConstrainedSoilPileConstantsSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDSOILPILECONSTANTSSET_CARD0 = (
    FieldSchema("pbsid", int, 0, 10, None),
    FieldSchema("diam", float, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("pidns", int, 30, 10, None),
    FieldSchema("pidnb", int, 40, 10, None),
    FieldSchema("error", int, 50, 10, 0),
    FieldSchema("nring", int, 60, 10, 1),
    FieldSchema("nringb", int, 70, 10, None),
)

_CONSTRAINEDSOILPILECONSTANTSSET_CARD1 = (
    FieldSchema("damp", float, 0, 10, 0.0),
    FieldSchema("local", int, 10, 10, 1),
)

_CONSTRAINEDSOILPILECONSTANTSSET_CARD2 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("acu", float, 10, 10, None),
    FieldSchema("bcu", float, 20, 10, None),
    FieldSchema("lccu", int, 30, 10, None),
    FieldSchema("astiffs", float, 40, 10, None),
    FieldSchema("bstiffs", float, 50, 10, None),
    FieldSchema("astiffb", float, 60, 10, None),
    FieldSchema("zref", float, 70, 10, None),
)

_CONSTRAINEDSOILPILECONSTANTSSET_CARD3 = (
    FieldSchema("kbcon", float, 0, 10, None),
    FieldSchema("kbcu", float, 10, 10, None),
    FieldSchema("kbsx", float, 20, 10, None),
    FieldSchema("kbsy", float, 30, 10, None),
    FieldSchema("kbsz", float, 40, 10, None),
    FieldSchema("bstfac", float, 50, 10, 1.0),
    FieldSchema("bhyper", float, 60, 10, None),
    FieldSchema("blc", int, 70, 10, None),
)

_CONSTRAINEDSOILPILECONSTANTSSET_CARD4 = (
    FieldSchema("kvcon", float, 0, 10, None),
    FieldSchema("kvcu", float, 10, 10, None),
    FieldSchema("kvsx", float, 20, 10, None),
    FieldSchema("kvsy", float, 30, 10, None),
    FieldSchema("kvsz", float, 40, 10, None),
    FieldSchema("vstfac", float, 50, 10, 1.0),
    FieldSchema("vhyper", float, 60, 10, None),
    FieldSchema("vlc", int, 70, 10, None),
)

_CONSTRAINEDSOILPILECONSTANTSSET_CARD5 = (
    FieldSchema("khcon", float, 0, 10, None),
    FieldSchema("khcu", float, 10, 10, None),
    FieldSchema("khsx", float, 20, 10, None),
    FieldSchema("khsy", float, 30, 10, None),
    FieldSchema("khsz", float, 40, 10, None),
    FieldSchema("hstfac", float, 50, 10, 1.0),
    FieldSchema("hhyper", float, 60, 10, None),
    FieldSchema("hlc", int, 70, 10, None),
)

class ConstrainedSoilPileConstantsSet(KeywordBase):
    """DYNA CONSTRAINED_SOIL_PILE_CONSTANTS_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SOIL_PILE_CONSTANTS_SET"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedSoilPileConstantsSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDSOILPILECONSTANTSSET_CARD5,
                **kwargs,
            ),        ]
    @property
    def pbsid(self) -> typing.Optional[int]:
        """Get or set the Part set ID containing beam elements for coupling (the piles).
        """ # nopep8
        return self._cards[0].get_value("pbsid")

    @pbsid.setter
    def pbsid(self, value: int) -> None:
        """Set the pbsid property."""
        self._cards[0].set_value("pbsid", value)

    @property
    def diam(self) -> typing.Optional[float]:
        """Get or set the Pile diameter (optional). If zero or blank, the pile diameter will be taken automatically from the section properties of the beam element.
        """ # nopep8
        return self._cards[0].get_value("diam")

    @diam.setter
    def diam(self, value: float) -> None:
        """Set the diam property."""
        self._cards[0].set_value("diam", value)

    @property
    def pidns(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for perpendicular and axial coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidns")

    @pidns.setter
    def pidns(self, value: int) -> None:
        """Set the pidns property."""
        self._cards[0].set_value("pidns", value)

    @property
    def pidnb(self) -> typing.Optional[int]:
        """Get or set the ID for automatically generated part containing visualization elements for base coupling.
        If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
        """ # nopep8
        return self._cards[0].get_value("pidnb")

    @pidnb.setter
    def pidnb(self, value: int) -> None:
        """Set the pidnb property."""
        self._cards[0].set_value("pidnb", value)

    @property
    def error(self) -> int:
        """Get or set the Action taken if any coupling point is not constrained within a soil element:
        EQ.0:	Stop with an error message.
        EQ.1 : Warn and continue..
        """ # nopep8
        return self._cards[0].get_value("error")

    @error.setter
    def error(self, value: int) -> None:
        """Set the error property."""
        if value not in [0, 1, None]:
            raise Exception("""error must be `None` or one of {0,1}.""")
        self._cards[0].set_value("error", value)

    @property
    def nring(self) -> int:
        """Get or set the Number of coupling points around circumference at each pile node:
        EQ.1:	One coupling point coincident with pile node
        GT.1 : NRING coupling points equally spaced around the circumference of the pile.
        """ # nopep8
        return self._cards[0].get_value("nring")

    @nring.setter
    def nring(self, value: int) -> None:
        """Set the nring property."""
        self._cards[0].set_value("nring", value)

    @property
    def nringb(self) -> typing.Optional[int]:
        """Get or set the Number of extra rings of coupling points on base, in addition to those around the pile circumference. By default, NRINGB is chosen automatically to distribute the base stress as uniformly as possible .
        """ # nopep8
        return self._cards[0].get_value("nringb")

    @nringb.setter
    def nringb(self, value: int) -> None:
        """Set the nringb property."""
        self._cards[0].set_value("nringb", value)

    @property
    def damp(self) -> float:
        """Get or set the Optional damping coefficient for Axial coupling (stress/velocity units). An additional axial coupling shear stress equal to DAMP times the axial velocity of the pile relative to the soil will be generated.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to identify which free end of a pile is treated as the Base:
        EQ.1:	End with the most negative global Z - coordinate
        EQ.2 : End which is Node 1 of the attached beam element topology.
        """ # nopep8
        return self._cards[1].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        """Set the local property."""
        if value not in [1, 2, None]:
            raise Exception("""local must be `None` or one of {1,2}.""")
        self._cards[1].set_value("local", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part SET ID (depending on OPTION2) containing solid elements for coupling (the soil).
        """ # nopep8
        return self._cards[2].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[2].set_value("pid", value)

    @property
    def acu(self) -> typing.Optional[float]:
        """Get or set the Constant term in depth-dependence formula. Units of stress..
        """ # nopep8
        return self._cards[2].get_value("acu")

    @acu.setter
    def acu(self, value: float) -> None:
        """Set the acu property."""
        self._cards[2].set_value("acu", value)

    @property
    def bcu(self) -> typing.Optional[float]:
        """Get or set the Coefficient on relative Z-coordinate in depth-dependence formula. Units of stress/length. Note that soil strengths (and therefore coupling properties) generally increase with depth, meaning they increase with an increasingly negative Z-coordinate. Therefore, this term is usually negative..
        """ # nopep8
        return self._cards[2].get_value("bcu")

    @bcu.setter
    def bcu(self, value: float) -> None:
        """Set the bcu property."""
        self._cards[2].set_value("bcu", value)

    @property
    def lccu(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID giving stress (stress units) as a function of relative Z-coordinate (length units). If defined, LCCU overrides ACU and BCU. Note that “increasing depth” corresponds to “increasingly negative relative Z-coordinate”..
        """ # nopep8
        return self._cards[2].get_value("lccu")

    @lccu.setter
    def lccu(self, value: int) -> None:
        """Set the lccu property."""
        self._cards[2].set_value("lccu", value)

    @property
    def astiffs(self) -> typing.Optional[float]:
        """Get or set the Generic stiffness term. Units of stress / length.
        """ # nopep8
        return self._cards[2].get_value("astiffs")

    @astiffs.setter
    def astiffs(self, value: float) -> None:
        """Set the astiffs property."""
        self._cards[2].set_value("astiffs", value)

    @property
    def bstiffs(self) -> typing.Optional[float]:
        """Get or set the Generic Z-coordinate-dependent stiffness term. Units of stress / length2.
        """ # nopep8
        return self._cards[2].get_value("bstiffs")

    @bstiffs.setter
    def bstiffs(self, value: float) -> None:
        """Set the bstiffs property."""
        self._cards[2].set_value("bstiffs", value)

    @property
    def astiffb(self) -> typing.Optional[float]:
        """Get or set the Base stiffness. Units of stress / length.
        """ # nopep8
        return self._cards[2].get_value("astiffb")

    @astiffb.setter
    def astiffb(self, value: float) -> None:
        """Set the astiffb property."""
        self._cards[2].set_value("astiffb", value)

    @property
    def zref(self) -> typing.Optional[float]:
        """Get or set the Reference Z-coordinate to calculate “relative Z-coordinate”.
        """ # nopep8
        return self._cards[2].get_value("zref")

    @zref.setter
    def zref(self, value: float) -> None:
        """Set the zref property."""
        self._cards[2].set_value("zref", value)

    @property
    def kbcon(self) -> typing.Optional[float]:
        """Get or set the Base coupling, constant term (stress units)
        """ # nopep8
        return self._cards[3].get_value("kbcon")

    @kbcon.setter
    def kbcon(self, value: float) -> None:
        """Set the kbcon property."""
        self._cards[3].set_value("kbcon", value)

    @property
    def kbcu(self) -> typing.Optional[float]:
        """Get or set the Base coupling, coefficient for Cu  (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("kbcu")

    @kbcu.setter
    def kbcu(self, value: float) -> None:
        """Set the kbcu property."""
        self._cards[3].set_value("kbcu", value)

    @property
    def kbsx(self) -> typing.Optional[float]:
        """Get or set the Base coupling, coefficient for effective global X-stress (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("kbsx")

    @kbsx.setter
    def kbsx(self, value: float) -> None:
        """Set the kbsx property."""
        self._cards[3].set_value("kbsx", value)

    @property
    def kbsy(self) -> typing.Optional[float]:
        """Get or set the Base coupling, coefficient for effective global Y-stress (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("kbsy")

    @kbsy.setter
    def kbsy(self, value: float) -> None:
        """Set the kbsy property."""
        self._cards[3].set_value("kbsy", value)

    @property
    def kbsz(self) -> typing.Optional[float]:
        """Get or set the Base coupling, coefficient for effective global Z-stress (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("kbsz")

    @kbsz.setter
    def kbsz(self, value: float) -> None:
        """Set the kbsz property."""
        self._cards[3].set_value("kbsz", value)

    @property
    def bstfac(self) -> float:
        """Get or set the Base coupling, factor on elastic stiffness (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("bstfac")

    @bstfac.setter
    def bstfac(self, value: float) -> None:
        """Set the bstfac property."""
        self._cards[3].set_value("bstfac", value)

    @property
    def bhyper(self) -> typing.Optional[float]:
        """Get or set the Base coupling, hyperbolic curve limit (dimensionless).
        """ # nopep8
        return self._cards[3].get_value("bhyper")

    @bhyper.setter
    def bhyper(self, value: float) -> None:
        """Set the bhyper property."""
        self._cards[3].set_value("bhyper", value)

    @property
    def blc(self) -> typing.Optional[int]:
        """Get or set the Base coupling, load curve ID for dimensionless factor on stress as a function of displacement .
        """ # nopep8
        return self._cards[3].get_value("blc")

    @blc.setter
    def blc(self, value: int) -> None:
        """Set the blc property."""
        self._cards[3].set_value("blc", value)

    @property
    def kvcon(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, constant term (stress units)
        """ # nopep8
        return self._cards[4].get_value("kvcon")

    @kvcon.setter
    def kvcon(self, value: float) -> None:
        """Set the kvcon property."""
        self._cards[4].set_value("kvcon", value)

    @property
    def kvcu(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, coefficient for Cu  (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("kvcu")

    @kvcu.setter
    def kvcu(self, value: float) -> None:
        """Set the kvcu property."""
        self._cards[4].set_value("kvcu", value)

    @property
    def kvsx(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, coefficient for effective global X-stress (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("kvsx")

    @kvsx.setter
    def kvsx(self, value: float) -> None:
        """Set the kvsx property."""
        self._cards[4].set_value("kvsx", value)

    @property
    def kvsy(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, coefficient for effective global Y-stress (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("kvsy")

    @kvsy.setter
    def kvsy(self, value: float) -> None:
        """Set the kvsy property."""
        self._cards[4].set_value("kvsy", value)

    @property
    def kvsz(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, coefficient for effective global Z-stress (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("kvsz")

    @kvsz.setter
    def kvsz(self, value: float) -> None:
        """Set the kvsz property."""
        self._cards[4].set_value("kvsz", value)

    @property
    def vstfac(self) -> float:
        """Get or set the Axial coupling, factor on elastic stiffness (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("vstfac")

    @vstfac.setter
    def vstfac(self, value: float) -> None:
        """Set the vstfac property."""
        self._cards[4].set_value("vstfac", value)

    @property
    def vhyper(self) -> typing.Optional[float]:
        """Get or set the Axial coupling, hyperbolic curve limit (dimensionless).
        """ # nopep8
        return self._cards[4].get_value("vhyper")

    @vhyper.setter
    def vhyper(self, value: float) -> None:
        """Set the vhyper property."""
        self._cards[4].set_value("vhyper", value)

    @property
    def vlc(self) -> typing.Optional[int]:
        """Get or set the Axial coupling, load curve ID for dimensionless factor on stress as a function of displacement .
        """ # nopep8
        return self._cards[4].get_value("vlc")

    @vlc.setter
    def vlc(self, value: int) -> None:
        """Set the vlc property."""
        self._cards[4].set_value("vlc", value)

    @property
    def khcon(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, constant term (stress units)
        """ # nopep8
        return self._cards[5].get_value("khcon")

    @khcon.setter
    def khcon(self, value: float) -> None:
        """Set the khcon property."""
        self._cards[5].set_value("khcon", value)

    @property
    def khcu(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, coefficient for Cu  (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("khcu")

    @khcu.setter
    def khcu(self, value: float) -> None:
        """Set the khcu property."""
        self._cards[5].set_value("khcu", value)

    @property
    def khsx(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, coefficient for effective global X-stress (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("khsx")

    @khsx.setter
    def khsx(self, value: float) -> None:
        """Set the khsx property."""
        self._cards[5].set_value("khsx", value)

    @property
    def khsy(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, coefficient for effective global Y-stress (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("khsy")

    @khsy.setter
    def khsy(self, value: float) -> None:
        """Set the khsy property."""
        self._cards[5].set_value("khsy", value)

    @property
    def khsz(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, coefficient for effective global Z-stress (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("khsz")

    @khsz.setter
    def khsz(self, value: float) -> None:
        """Set the khsz property."""
        self._cards[5].set_value("khsz", value)

    @property
    def hstfac(self) -> float:
        """Get or set the Perpendicular coupling, factor on elastic stiffness (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("hstfac")

    @hstfac.setter
    def hstfac(self, value: float) -> None:
        """Set the hstfac property."""
        self._cards[5].set_value("hstfac", value)

    @property
    def hhyper(self) -> typing.Optional[float]:
        """Get or set the Perpendicular coupling, hyperbolic curve limit (dimensionless).
        """ # nopep8
        return self._cards[5].get_value("hhyper")

    @hhyper.setter
    def hhyper(self, value: float) -> None:
        """Set the hhyper property."""
        self._cards[5].set_value("hhyper", value)

    @property
    def hlc(self) -> typing.Optional[int]:
        """Get or set the Perpendicular coupling, load curve ID for dimensionless factor on stress as a function of displacement .
        """ # nopep8
        return self._cards[5].get_value("hlc")

    @hlc.setter
    def hlc(self, value: int) -> None:
        """Set the hlc property."""
        self._cards[5].set_value("hlc", value)

