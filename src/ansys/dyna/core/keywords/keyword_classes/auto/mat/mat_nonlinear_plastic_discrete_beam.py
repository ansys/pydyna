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

"""Module providing the MatNonlinearPlasticDiscreteBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATNONLINEARPLASTICDISCRETEBEAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("tkr", float, 20, 10, None),
    FieldSchema("tks", float, 30, 10, None),
    FieldSchema("tkt", float, 40, 10, None),
    FieldSchema("rkr", float, 50, 10, None),
    FieldSchema("rks", float, 60, 10, None),
    FieldSchema("rkt", float, 70, 10, None),
)

_MATNONLINEARPLASTICDISCRETEBEAM_CARD1 = (
    FieldSchema("tdr", float, 0, 10, None),
    FieldSchema("tds", float, 10, 10, None),
    FieldSchema("tdt", float, 20, 10, None),
    FieldSchema("rdr", float, 30, 10, None),
    FieldSchema("rds", float, 40, 10, None),
    FieldSchema("rdt", float, 50, 10, None),
)

_MATNONLINEARPLASTICDISCRETEBEAM_CARD2 = (
    FieldSchema("lcpdr", int, 0, 10, 0),
    FieldSchema("lcpds", int, 10, 10, 0),
    FieldSchema("lcpdt", int, 20, 10, 0),
    FieldSchema("lcpmr", int, 30, 10, 0),
    FieldSchema("lcpms", int, 40, 10, 0),
    FieldSchema("lcpmt", int, 50, 10, 0),
)

_MATNONLINEARPLASTICDISCRETEBEAM_CARD3 = (
    FieldSchema("ffailr", float, 0, 10, None),
    FieldSchema("ffails", float, 10, 10, None),
    FieldSchema("ffailt", float, 20, 10, None),
    FieldSchema("mfailr", float, 30, 10, None),
    FieldSchema("mfails", float, 40, 10, None),
    FieldSchema("mfailt", float, 50, 10, None),
)

_MATNONLINEARPLASTICDISCRETEBEAM_CARD4 = (
    FieldSchema("ufailr", float, 0, 10, None),
    FieldSchema("ufails", float, 10, 10, None),
    FieldSchema("ufailt", float, 20, 10, None),
    FieldSchema("tfailr", float, 30, 10, None),
    FieldSchema("tfails", float, 40, 10, None),
    FieldSchema("tfailt", float, 50, 10, None),
)

_MATNONLINEARPLASTICDISCRETEBEAM_CARD5 = (
    FieldSchema("for_", float, 0, 10, None, "for"),
    FieldSchema("fos", float, 10, 10, None),
    FieldSchema("fot", float, 20, 10, None),
    FieldSchema("mor", float, 30, 10, None),
    FieldSchema("mos", float, 40, 10, None),
    FieldSchema("mot", float, 50, 10, None),
)

_MATNONLINEARPLASTICDISCRETEBEAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatNonlinearPlasticDiscreteBeam(KeywordBase):
    """DYNA MAT_NONLINEAR_PLASTIC_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "NONLINEAR_PLASTIC_DISCRETE_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatNonlinearPlasticDiscreteBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARPLASTICDISCRETEBEAM_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatNonlinearPlasticDiscreteBeam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATNONLINEARPLASTICDISCRETEBEAM_OPTION0_CARD0,
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
        """Get or set the Mass density, see also volume on *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def tkr(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local r-axis.
        LT.0.0:	|TKR| is the load curve ID defining the elastic translational force along local r-axis as a function of relative translational displacement. Useful for nonlinear elastic behavior.
        """ # nopep8
        return self._cards[0].get_value("tkr")

    @tkr.setter
    def tkr(self, value: float) -> None:
        """Set the tkr property."""
        self._cards[0].set_value("tkr", value)

    @property
    def tks(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local s-axis.
        LT.0.0:	|TKS| is the load curve ID defining the elastic translational force along local s-axis as a function of relative translational displacement. Useful for nonlinear elastic behavior.
        """ # nopep8
        return self._cards[0].get_value("tks")

    @tks.setter
    def tks(self, value: float) -> None:
        """Set the tks property."""
        self._cards[0].set_value("tks", value)

    @property
    def tkt(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local t-axis.
        LT.0.0:	|TKT| is the load curve ID defining the elastic translational force along local t-axis as a function of relative translational displacement. Useful for nonlinear elastic behavior.
        """ # nopep8
        return self._cards[0].get_value("tkt")

    @tkt.setter
    def tkt(self, value: float) -> None:
        """Set the tkt property."""
        self._cards[0].set_value("tkt", value)

    @property
    def rkr(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local r-axis.
        """ # nopep8
        return self._cards[0].get_value("rkr")

    @rkr.setter
    def rkr(self, value: float) -> None:
        """Set the rkr property."""
        self._cards[0].set_value("rkr", value)

    @property
    def rks(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local s-axis.
        """ # nopep8
        return self._cards[0].get_value("rks")

    @rks.setter
    def rks(self, value: float) -> None:
        """Set the rks property."""
        self._cards[0].set_value("rks", value)

    @property
    def rkt(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local t-axis.
        """ # nopep8
        return self._cards[0].get_value("rkt")

    @rkt.setter
    def rkt(self, value: float) -> None:
        """Set the rkt property."""
        self._cards[0].set_value("rkt", value)

    @property
    def tdr(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local r-axis.
        """ # nopep8
        return self._cards[1].get_value("tdr")

    @tdr.setter
    def tdr(self, value: float) -> None:
        """Set the tdr property."""
        self._cards[1].set_value("tdr", value)

    @property
    def tds(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local s-axis.
        """ # nopep8
        return self._cards[1].get_value("tds")

    @tds.setter
    def tds(self, value: float) -> None:
        """Set the tds property."""
        self._cards[1].set_value("tds", value)

    @property
    def tdt(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local t-axis.
        """ # nopep8
        return self._cards[1].get_value("tdt")

    @tdt.setter
    def tdt(self, value: float) -> None:
        """Set the tdt property."""
        self._cards[1].set_value("tdt", value)

    @property
    def rdr(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local r-axis.
        """ # nopep8
        return self._cards[1].get_value("rdr")

    @rdr.setter
    def rdr(self, value: float) -> None:
        """Set the rdr property."""
        self._cards[1].set_value("rdr", value)

    @property
    def rds(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local s-axis.
        """ # nopep8
        return self._cards[1].get_value("rds")

    @rds.setter
    def rds(self, value: float) -> None:
        """Set the rds property."""
        self._cards[1].set_value("rds", value)

    @property
    def rdt(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local t-axis.
        """ # nopep8
        return self._cards[1].get_value("rdt")

    @rdt.setter
    def rdt(self, value: float) -> None:
        """Set the rdt property."""
        self._cards[1].set_value("rdt", value)

    @property
    def lcpdr(self) -> int:
        """Get or set the Load curve ID-yield force versus plastic displacement r-axis. If the curve ID zero, and if TKR is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpdr")

    @lcpdr.setter
    def lcpdr(self, value: int) -> None:
        """Set the lcpdr property."""
        self._cards[2].set_value("lcpdr", value)

    @property
    def lcpds(self) -> int:
        """Get or set the Load curve ID-yield force versus plastic displacement s-axis. If the curve ID zero, and if TKS is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpds")

    @lcpds.setter
    def lcpds(self, value: int) -> None:
        """Set the lcpds property."""
        self._cards[2].set_value("lcpds", value)

    @property
    def lcpdt(self) -> int:
        """Get or set the Load curve ID-yield force versus plastic displacement t-axis. If the curve ID zero, and if TKT is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpdt")

    @lcpdt.setter
    def lcpdt(self, value: int) -> None:
        """Set the lcpdt property."""
        self._cards[2].set_value("lcpdt", value)

    @property
    def lcpmr(self) -> int:
        """Get or set the Load curve ID-yield moment versus plastic rotation r-axis. If the curve ID zero, and if RKR is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpmr")

    @lcpmr.setter
    def lcpmr(self, value: int) -> None:
        """Set the lcpmr property."""
        self._cards[2].set_value("lcpmr", value)

    @property
    def lcpms(self) -> int:
        """Get or set the Load curve ID-yield moment versus plastic rotation s-axis. If the curve ID zero, and if RKS is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpms")

    @lcpms.setter
    def lcpms(self, value: int) -> None:
        """Set the lcpms property."""
        self._cards[2].set_value("lcpms", value)

    @property
    def lcpmt(self) -> int:
        """Get or set the Load curve ID-yield moment versus plastic rotation t-axis. If the curve ID zero, and if RKT is nonzero, then nonlinear elastic behavior is obtained for this component.
        """ # nopep8
        return self._cards[2].get_value("lcpmt")

    @lcpmt.setter
    def lcpmt(self, value: int) -> None:
        """Set the lcpmt property."""
        self._cards[2].set_value("lcpmt", value)

    @property
    def ffailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding force Fr is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("ffailr")

    @ffailr.setter
    def ffailr(self, value: float) -> None:
        """Set the ffailr property."""
        self._cards[3].set_value("ffailr", value)

    @property
    def ffails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding force Fs is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("ffails")

    @ffails.setter
    def ffails(self, value: float) -> None:
        """Set the ffails property."""
        self._cards[3].set_value("ffails", value)

    @property
    def ffailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding force Ft is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("ffailt")

    @ffailt.setter
    def ffailt(self, value: float) -> None:
        """Set the ffailt property."""
        self._cards[3].set_value("ffailt", value)

    @property
    def mfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding moment Mr is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("mfailr")

    @mfailr.setter
    def mfailr(self, value: float) -> None:
        """Set the mfailr property."""
        self._cards[3].set_value("mfailr", value)

    @property
    def mfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding moment Ms is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("mfails")

    @mfails.setter
    def mfails(self, value: float) -> None:
        """Set the mfails property."""
        self._cards[3].set_value("mfails", value)

    @property
    def mfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding moment Mt is not considered in the failure calculation.
        """ # nopep8
        return self._cards[3].get_value("mfailt")

    @mfailt.setter
    def mfailt(self, value: float) -> None:
        """Set the mfailt property."""
        self._cards[3].set_value("mfailt", value)

    @property
    def ufailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding displacement ur is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("ufailr")

    @ufailr.setter
    def ufailr(self, value: float) -> None:
        """Set the ufailr property."""
        self._cards[4].set_value("ufailr", value)

    @property
    def ufails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0:the corresponding displacement us is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("ufails")

    @ufails.setter
    def ufails(self, value: float) -> None:
        """Set the ufails property."""
        self._cards[4].set_value("ufails", value)

    @property
    def ufailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding displacement ut is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("ufailt")

    @ufailt.setter
    def ufailt(self, value: float) -> None:
        """Set the ufailt property."""
        self._cards[4].set_value("ufailt", value)

    @property
    def tfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding rotation qr is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("tfailr")

    @tfailr.setter
    def tfailr(self, value: float) -> None:
        """Set the tfailr property."""
        self._cards[4].set_value("tfailr", value)

    @property
    def tfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding rotation qs is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("tfails")

    @tfails.setter
    def tfails(self, value: float) -> None:
        """Set the tfails property."""
        self._cards[4].set_value("tfails", value)

    @property
    def tfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter.
        EQ.0: the corresponding rotation qt is not considered in the failure calculation.
        """ # nopep8
        return self._cards[4].get_value("tfailt")

    @tfailt.setter
    def tfailt(self, value: float) -> None:
        """Set the tfailt property."""
        self._cards[4].set_value("tfailt", value)

    @property
    def for_(self) -> typing.Optional[float]:
        """Get or set the Preload force in r-direction
        """ # nopep8
        return self._cards[5].get_value("for_")

    @for_.setter
    def for_(self, value: float) -> None:
        """Set the for_ property."""
        self._cards[5].set_value("for_", value)

    @property
    def fos(self) -> typing.Optional[float]:
        """Get or set the Preload force in s-direction
        """ # nopep8
        return self._cards[5].get_value("fos")

    @fos.setter
    def fos(self, value: float) -> None:
        """Set the fos property."""
        self._cards[5].set_value("fos", value)

    @property
    def fot(self) -> typing.Optional[float]:
        """Get or set the Preload force in t-direction
        """ # nopep8
        return self._cards[5].get_value("fot")

    @fot.setter
    def fot(self, value: float) -> None:
        """Set the fot property."""
        self._cards[5].set_value("fot", value)

    @property
    def mor(self) -> typing.Optional[float]:
        """Get or set the Preload moment about r-axis
        """ # nopep8
        return self._cards[5].get_value("mor")

    @mor.setter
    def mor(self, value: float) -> None:
        """Set the mor property."""
        self._cards[5].set_value("mor", value)

    @property
    def mos(self) -> typing.Optional[float]:
        """Get or set the Preload moment about s-axis
        """ # nopep8
        return self._cards[5].get_value("mos")

    @mos.setter
    def mos(self, value: float) -> None:
        """Set the mos property."""
        self._cards[5].set_value("mos", value)

    @property
    def mot(self) -> typing.Optional[float]:
        """Get or set the Preload moment about t-axis
        """ # nopep8
        return self._cards[5].get_value("mot")

    @mot.setter
    def mot(self, value: float) -> None:
        """Set the mot property."""
        self._cards[5].set_value("mot", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

