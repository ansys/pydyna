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

"""Module providing the ElementShellBextPatch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTSHELLBEXTPATCH_CARD0 = (
    FieldSchema("epid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("nel", int, 20, 10, None),
)

_ELEMENTSHELLBEXTPATCH_CARD1 = (
    FieldSchema("wfl", int, 0, 10, None),
    FieldSchema("form", int, 10, 10, 0),
    FieldSchema("int_", int, 20, 10, 0, "int"),
    FieldSchema("nisr", int, 30, 10, None),
    FieldSchema("niss", int, 40, 10, None),
    FieldSchema("imass", int, 50, 10, 0),
    FieldSchema("nl", int, 60, 10, None),
)

_ELEMENTSHELLBEXTPATCH_CARD2 = (
    FieldSchema("shpe", int, 0, 10, 0),
    FieldSchema("pr", int, 10, 10, None),
    FieldSchema("ps", int, 20, 10, None),
    FieldSchema("bdry", int, 30, 10, 0),
    FieldSchema("trm", int, 40, 10, 0),
    FieldSchema("smth", int, 50, 10, 0),
)

_ELEMENTSHELLBEXTPATCH_CARD3 = (
    FieldSchema("b1", int, 0, 10, 0),
    FieldSchema("b2", int, 10, 10, 0),
    FieldSchema("b3", int, 20, 10, 0),
    FieldSchema("b4", int, 30, 10, 0),
    FieldSchema("b5", int, 40, 10, 0),
    FieldSchema("b6", int, 50, 10, 0),
    FieldSchema("b7", int, 60, 10, 0),
    FieldSchema("b8", int, 70, 10, 0),
)

_ELEMENTSHELLBEXTPATCH_CARD4 = (
    FieldSchema("s1", int, 0, 10, 0),
    FieldSchema("s2", int, 10, 10, 0),
    FieldSchema("s3", int, 20, 10, 0),
    FieldSchema("s4", int, 30, 10, 0),
    FieldSchema("s5", int, 40, 10, 0),
    FieldSchema("s6", int, 50, 10, 0),
    FieldSchema("s7", int, 60, 10, 0),
    FieldSchema("s8", int, 70, 10, 0),
)

_ELEMENTSHELLBEXTPATCH_CARD5 = (
    FieldSchema("n1", int, 0, 10, 0),
    FieldSchema("n2", int, 10, 10, 0),
    FieldSchema("n3", int, 20, 10, 0),
    FieldSchema("n4", int, 30, 10, 0),
    FieldSchema("n5", int, 40, 10, 0),
    FieldSchema("n6", int, 50, 10, 0),
    FieldSchema("n7", int, 60, 10, 0),
    FieldSchema("n8", int, 70, 10, 0),
)

_ELEMENTSHELLBEXTPATCH_CARD6 = (
    FieldSchema("w1", float, 0, 10, 0.0),
    FieldSchema("w2", float, 10, 10, 0.0),
    FieldSchema("w3", float, 20, 10, 0.0),
    FieldSchema("w4", float, 30, 10, 0.0),
    FieldSchema("w5", float, 40, 10, 0.0),
    FieldSchema("w6", float, 50, 10, 0.0),
    FieldSchema("w7", float, 60, 10, 0.0),
    FieldSchema("w8", float, 70, 10, 0.0),
)

_ELEMENTSHELLBEXTPATCH_CARD7 = (
    FieldSchema("o1", float, 0, 10, 0.0),
    FieldSchema("o2", float, 10, 10, 0.0),
    FieldSchema("o3", float, 20, 10, 0.0),
    FieldSchema("o4", float, 30, 10, 0.0),
    FieldSchema("o5", float, 40, 10, 0.0),
    FieldSchema("o6", float, 50, 10, 0.0),
    FieldSchema("o7", float, 60, 10, 0.0),
    FieldSchema("o8", float, 70, 10, 0.0),
)

class ElementShellBextPatch(KeywordBase):
    """DYNA ELEMENT_SHELL_BEXT_PATCH keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_BEXT_PATCH"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "n7": LinkType.NODE,
        "n8": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementShellBextPatch class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLBEXTPATCH_CARD7,
                **kwargs,
            ),        ]
    @property
    def epid(self) -> typing.Optional[int]:
        """Get or set the Extraction patch element ID. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("epid")

    @epid.setter
    def epid(self, value: int) -> None:
        """Set the epid property."""
        self._cards[0].set_value("epid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. See *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nel(self) -> typing.Optional[int]:
        """Get or set the Number of Bezier elements in the patch.
        """ # nopep8
        return self._cards[0].get_value("nel")

    @nel.setter
    def nel(self, value: int) -> None:
        """Set the nel property."""
        self._cards[0].set_value("nel", value)

    @property
    def wfl(self) -> typing.Optional[int]:
        """Get or set the Flag for weighting factors of the control points.
        WFL = 0 : All weights at the control points are set to 1 and no optional	cards D are allowed.
        WFL != 0 : The weights at the control points are de
        ned in optional cards
        D which must be de
        ned after cards C..
        """ # nopep8
        return self._cards[1].get_value("wfl")

    @wfl.setter
    def wfl(self, value: int) -> None:
        """Set the wfl property."""
        self._cards[1].set_value("wfl", value)

    @property
    def form(self) -> int:
        """Get or set the Shell formulation to be used.
        FORM = 0 : Shear deformable shell theory with rotational DOFs.
        FORM = 1 : Shear deformable shell theory without rotational DOFs.
        FORM = 2 : Thin shell theory without rotational DOFs.
        FORM = 4 : Combination of FORM = 0 and FORM = 1.
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        if value not in [0, 1, 2, 4, None]:
            raise Exception("""form must be `None` or one of {0,1,2,4}.""")
        self._cards[1].set_value("form", value)

    @property
    def int_(self) -> int:
        """Get or set the In-plane numerical integration rule.
        INT = 0 : Uniformly reduced Gauss integration, NIP = PR  PS. Note
        that the number of integration points may change from element
        to element depending on local element degree.
        INT = 1 : Full Gauss integration, NIP = (PR + 1)  (PS + 1). Note that
        the number of integration points may change from element to
        element depending on local element degree.
        """ # nopep8
        return self._cards[1].get_value("int_")

    @int_.setter
    def int_(self, value: int) -> None:
        """Set the int_ property."""
        if value not in [0, 1, None]:
            raise Exception("""int_ must be `None` or one of {0,1}.""")
        self._cards[1].set_value("int_", value)

    @property
    def nisr(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) interpolation shell elements in the local r-direction of each Bezier element for visualization (post processing) and contact.
        """ # nopep8
        return self._cards[1].get_value("nisr")

    @nisr.setter
    def nisr(self, value: int) -> None:
        """Set the nisr property."""
        self._cards[1].set_value("nisr", value)

    @property
    def niss(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) interpolation shell elements in the local s-direction of each Bezier element for visualization (post processing) and contact.
        """ # nopep8
        return self._cards[1].get_value("niss")

    @niss.setter
    def niss(self, value: int) -> None:
        """Set the niss property."""
        self._cards[1].set_value("niss", value)

    @property
    def imass(self) -> int:
        """Get or set the Option for lumping of mass matrix.
        IMASS = 0 : Row sum.
        IMASS = 1 : Diagonal weighting.
        """ # nopep8
        return self._cards[1].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        """Set the imass property."""
        if value not in [0, 1, None]:
            raise Exception("""imass must be `None` or one of {0,1}.""")
        self._cards[1].set_value("imass", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of trimming loops
        NL = 0 : No trimming loops (i.e., untrimmed U-spline).
        NL > 0 : Trimmed U-spline with NL trimming loops.
        """ # nopep8
        return self._cards[1].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        """Set the nl property."""
        self._cards[1].set_value("nl", value)

    @property
    def shpe(self) -> int:
        """Get or set the The Bezier element shape
        SHPE = 0 : Quadrilateral Bezier element.
        SHPE = 1 : Triangular Bezier element.
        """ # nopep8
        return self._cards[2].get_value("shpe")

    @shpe.setter
    def shpe(self, value: int) -> None:
        """Set the shpe property."""
        if value not in [0, 1, None]:
            raise Exception("""shpe must be `None` or one of {0,1}.""")
        self._cards[2].set_value("shpe", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the The Bezier element degree in the local r-direction.
        """ # nopep8
        return self._cards[2].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        """Set the pr property."""
        self._cards[2].set_value("pr", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the The Bezier element degree in the local s-direction.
        """ # nopep8
        return self._cards[2].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        """Set the ps property."""
        self._cards[2].set_value("ps", value)

    @property
    def bdry(self) -> int:
        """Get or set the A boolean indicating if the Bezier is on the patch boundary.
        """ # nopep8
        return self._cards[2].get_value("bdry")

    @bdry.setter
    def bdry(self, value: int) -> None:
        """Set the bdry property."""
        self._cards[2].set_value("bdry", value)

    @property
    def trm(self) -> int:
        """Get or set the A boolean indicating if the Bezier element is trimmed.
        """ # nopep8
        return self._cards[2].get_value("trm")

    @trm.setter
    def trm(self, value: int) -> None:
        """Set the trm property."""
        self._cards[2].set_value("trm", value)

    @property
    def smth(self) -> int:
        """Get or set the A boolean indicating whether smoothness information will be specifed for the element.
        """ # nopep8
        return self._cards[2].get_value("smth")

    @smth.setter
    def smth(self, value: int) -> None:
        """Set the smth property."""
        self._cards[2].set_value("smth", value)

    @property
    def b1(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: int) -> None:
        """Set the b1 property."""
        self._cards[3].set_value("b1", value)

    @property
    def b2(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b2")

    @b2.setter
    def b2(self, value: int) -> None:
        """Set the b2 property."""
        self._cards[3].set_value("b2", value)

    @property
    def b3(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b3")

    @b3.setter
    def b3(self, value: int) -> None:
        """Set the b3 property."""
        self._cards[3].set_value("b3", value)

    @property
    def b4(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b4")

    @b4.setter
    def b4(self, value: int) -> None:
        """Set the b4 property."""
        self._cards[3].set_value("b4", value)

    @property
    def b5(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary..
        """ # nopep8
        return self._cards[3].get_value("b5")

    @b5.setter
    def b5(self, value: int) -> None:
        """Set the b5 property."""
        self._cards[3].set_value("b5", value)

    @property
    def b6(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b6")

    @b6.setter
    def b6(self, value: int) -> None:
        """Set the b6 property."""
        self._cards[3].set_value("b6", value)

    @property
    def b7(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b7")

    @b7.setter
    def b7(self, value: int) -> None:
        """Set the b7 property."""
        self._cards[3].set_value("b7", value)

    @property
    def b8(self) -> int:
        """Get or set the The Bezier element (format A) borders which touch the patch boundary.
        """ # nopep8
        return self._cards[3].get_value("b8")

    @b8.setter
    def b8(self, value: int) -> None:
        """Set the b8 property."""
        self._cards[3].set_value("b8", value)

    @property
    def s1(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s1")

    @s1.setter
    def s1(self, value: int) -> None:
        """Set the s1 property."""
        self._cards[4].set_value("s1", value)

    @property
    def s2(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s2")

    @s2.setter
    def s2(self, value: int) -> None:
        """Set the s2 property."""
        self._cards[4].set_value("s2", value)

    @property
    def s3(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s3")

    @s3.setter
    def s3(self, value: int) -> None:
        """Set the s3 property."""
        self._cards[4].set_value("s3", value)

    @property
    def s4(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s4")

    @s4.setter
    def s4(self, value: int) -> None:
        """Set the s4 property."""
        self._cards[4].set_value("s4", value)

    @property
    def s5(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A)..
        """ # nopep8
        return self._cards[4].get_value("s5")

    @s5.setter
    def s5(self, value: int) -> None:
        """Set the s5 property."""
        self._cards[4].set_value("s5", value)

    @property
    def s6(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s6")

    @s6.setter
    def s6(self, value: int) -> None:
        """Set the s6 property."""
        self._cards[4].set_value("s6", value)

    @property
    def s7(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s7")

    @s7.setter
    def s7(self, value: int) -> None:
        """Set the s7 property."""
        self._cards[4].set_value("s7", value)

    @property
    def s8(self) -> int:
        """Get or set the The smoothness levels on each side of a Bezier element (format A).
        """ # nopep8
        return self._cards[4].get_value("s8")

    @s8.setter
    def s8(self, value: int) -> None:
        """Set the s8 property."""
        self._cards[4].set_value("s8", value)

    @property
    def n1(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)..
        """ # nopep8
        return self._cards[5].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[5].set_value("n1", value)

    @property
    def n2(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[5].set_value("n2", value)

    @property
    def n3(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[5].set_value("n3", value)

    @property
    def n4(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[5].set_value("n4", value)

    @property
    def n5(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A).
        """ # nopep8
        return self._cards[5].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[5].set_value("n5", value)

    @property
    def n6(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[5].set_value("n6", value)

    @property
    def n7(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[5].set_value("n7", value)

    @property
    def n8(self) -> int:
        """Get or set the Control points i (de
        ned via *NODE) which de
        ne the Bezier element (format	A)
        """ # nopep8
        return self._cards[5].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[5].set_value("n8", value)

    @property
    def w1(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[6].set_value("w1", value)

    @property
    def w2(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[6].set_value("w2", value)

    @property
    def w3(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[6].set_value("w3", value)

    @property
    def w4(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[6].set_value("w4", value)

    @property
    def w5(self) -> float:
        """Get or set the Weighting factor of control point i.
        """ # nopep8
        return self._cards[6].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[6].set_value("w5", value)

    @property
    def w6(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[6].set_value("w6", value)

    @property
    def w7(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[6].set_value("w7", value)

    @property
    def w8(self) -> float:
        """Get or set the Weighting factor of control point i
        """ # nopep8
        return self._cards[6].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[6].set_value("w8", value)

    @property
    def o1(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        """Set the o1 property."""
        self._cards[7].set_value("o1", value)

    @property
    def o2(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o2")

    @o2.setter
    def o2(self, value: float) -> None:
        """Set the o2 property."""
        self._cards[7].set_value("o2", value)

    @property
    def o3(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o3")

    @o3.setter
    def o3(self, value: float) -> None:
        """Set the o3 property."""
        self._cards[7].set_value("o3", value)

    @property
    def o4(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o4")

    @o4.setter
    def o4(self, value: float) -> None:
        """Set the o4 property."""
        self._cards[7].set_value("o4", value)

    @property
    def o5(self) -> float:
        """Get or set the The extraction operator values for a Bezier element.
        """ # nopep8
        return self._cards[7].get_value("o5")

    @o5.setter
    def o5(self, value: float) -> None:
        """Set the o5 property."""
        self._cards[7].set_value("o5", value)

    @property
    def o6(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o6")

    @o6.setter
    def o6(self, value: float) -> None:
        """Set the o6 property."""
        self._cards[7].set_value("o6", value)

    @property
    def o7(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o7")

    @o7.setter
    def o7(self, value: float) -> None:
        """Set the o7 property."""
        self._cards[7].set_value("o7", value)

    @property
    def o8(self) -> float:
        """Get or set the The extraction operator values for a Bezier element
        """ # nopep8
        return self._cards[7].get_value("o8")

    @o8.setter
    def o8(self, value: float) -> None:
        """Set the o8 property."""
        self._cards[7].set_value("o8", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def n5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def n7_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n7."""
        return self._get_link_by_attr("NODE", "nid", self.n7, "parts")

    @property
    def n8_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n8."""
        return self._get_link_by_attr("NODE", "nid", self.n8, "parts")

