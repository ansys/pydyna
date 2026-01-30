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

"""Module providing the IncludeStampedSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INCLUDESTAMPEDSET_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_INCLUDESTAMPEDSET_CARD1 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("thick", int, 10, 10, 0),
    FieldSchema("pstrn", int, 20, 10, 0),
    FieldSchema("strain", int, 30, 10, 0),
    FieldSchema("stress", int, 40, 10, 0),
    FieldSchema("incout", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("rmax", float, 70, 10, 20.0),
)

_INCLUDESTAMPEDSET_CARD2 = (
    FieldSchema("n1s", int, 0, 10, 0),
    FieldSchema("n2s", int, 10, 10, 0),
    FieldSchema("n3s", int, 20, 10, 0),
    FieldSchema("n1c", int, 30, 10, 0),
    FieldSchema("n2c", int, 40, 10, 0),
    FieldSchema("n3c", int, 50, 10, 0),
    FieldSchema("tensor", int, 60, 10, 0),
    FieldSchema("thkscl", float, 70, 10, 1.0),
)

_INCLUDESTAMPEDSET_CARD3 = (
    FieldSchema("isym", int, 0, 10, 0),
    FieldSchema("iafter", int, 10, 10, 0),
    FieldSchema("percele", float, 20, 10, None),
    FieldSchema("iortho", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("isrocut", int, 50, 10, None),
)

_INCLUDESTAMPEDSET_CARD4 = (
    FieldSchema("x01", float, 0, 10, None),
    FieldSchema("y01", float, 10, 10, None),
    FieldSchema("z01", float, 20, 10, None),
)

_INCLUDESTAMPEDSET_CARD5 = (
    FieldSchema("x02", float, 0, 10, None),
    FieldSchema("y02", float, 10, 10, None),
    FieldSchema("z02", float, 20, 10, None),
    FieldSchema("x03", float, 30, 10, None),
    FieldSchema("y03", float, 40, 10, None),
    FieldSchema("z03", float, 50, 10, None),
)

class IncludeStampedSet(KeywordBase):
    """DYNA INCLUDE_STAMPED_SET keyword"""

    keyword = "INCLUDE"
    subkeyword = "STAMPED_SET"
    _link_fields = {
        "n1s": LinkType.NODE,
        "n2s": LinkType.NODE,
        "n3s": LinkType.NODE,
        "n1c": LinkType.NODE,
        "n2c": LinkType.NODE,
        "n3c": LinkType.NODE,
        "psid": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IncludeStampedSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDESTAMPEDSET_CARD5,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID of crash part for remapping.
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[1].set_value("psid", value)

    @property
    def thick(self) -> int:
        """Get or set the Thickness remap:
        EQ.0: map thickness
        EQ.1: do not map thickness
        EQ.2:	Average value inside a circle defined by RMAX
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: int) -> None:
        """Set the thick property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""thick must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("thick", value)

    @property
    def pstrn(self) -> int:
        """Get or set the Plastic strain remap:
        EQ.0: map plastic strain
        EQ.1: do not plastic strain
        EQ.2:	Average value inside a circle defined by RMAX
        """ # nopep8
        return self._cards[1].get_value("pstrn")

    @pstrn.setter
    def pstrn(self, value: int) -> None:
        """Set the pstrn property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""pstrn must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("pstrn", value)

    @property
    def strain(self) -> int:
        """Get or set the Strain remap:
        EQ.0: map strains
        EQ.1: do not map strains
        """ # nopep8
        return self._cards[1].get_value("strain")

    @strain.setter
    def strain(self, value: int) -> None:
        """Set the strain property."""
        if value not in [0, 1, None]:
            raise Exception("""strain must be `None` or one of {0,1}.""")
        self._cards[1].set_value("strain", value)

    @property
    def stress(self) -> int:
        """Get or set the Stress tensor remap:
        EQ.0: map stress tensorand history variables
        EQ.1:do not map stress tensor. only history varibales
        EQ.2:	Do not map stress tensor or history variables
        EQ. - 1:	Map stress tensor in an internal large format(binary files)
        EQ. - 3 : Do not map stress tensor in an internal large format, only history variables(binary files)
        """ # nopep8
        return self._cards[1].get_value("stress")

    @stress.setter
    def stress(self, value: int) -> None:
        """Set the stress property."""
        if value not in [0, 1, 2, -1, -3, None]:
            raise Exception("""stress must be `None` or one of {0,1,2,-1,-3}.""")
        self._cards[1].set_value("stress", value)

    @property
    def incout(self) -> int:
        """Get or set the Save mapped data:
        EQ.1:	Save the mapped data for the part / part set(PID) to a file called dyna.inc.This option is useful for when the mapped data may be required in a future simulation.
        EQ.2 : Save the mapped data for the specified part or part set(PID) to a file called dynain_‌xx(xx is the part or part set ID).
        EQ.3 : Save the mapped data for the specified part or part set(PID) to a file called nastran_‌xx(in nastran format).xx is the part or part set ID.
        """ # nopep8
        return self._cards[1].get_value("incout")

    @incout.setter
    def incout(self, value: int) -> None:
        """Set the incout property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""incout must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("incout", value)

    @property
    def rmax(self) -> float:
        """Get or set the Search radius.  LS-DYNA remaps history variables from the mesh of the stamped part to the mesh of the crash part with a spatial tolerance of RMAX.  If an element in the crash part lies within RMAX of the stamped part, data will be mapped to that element.  If set less than 0.001, RMAX automatically assumes the default value of 20.
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[1].set_value("rmax", value)

    @property
    def n1s(self) -> int:
        """Get or set the First of 3 nodes need to reorient the stamped part.
        """ # nopep8
        return self._cards[2].get_value("n1s")

    @n1s.setter
    def n1s(self, value: int) -> None:
        """Set the n1s property."""
        self._cards[2].set_value("n1s", value)

    @property
    def n2s(self) -> int:
        """Get or set the Second of 3 nodes need to reorient the stamped part.
        """ # nopep8
        return self._cards[2].get_value("n2s")

    @n2s.setter
    def n2s(self, value: int) -> None:
        """Set the n2s property."""
        self._cards[2].set_value("n2s", value)

    @property
    def n3s(self) -> int:
        """Get or set the Third of 3 nodes need to reorient the stamped part.
        """ # nopep8
        return self._cards[2].get_value("n3s")

    @n3s.setter
    def n3s(self, value: int) -> None:
        """Set the n3s property."""
        self._cards[2].set_value("n3s", value)

    @property
    def n1c(self) -> int:
        """Get or set the First of 3 nodes need to reorient the crash model part.
        """ # nopep8
        return self._cards[2].get_value("n1c")

    @n1c.setter
    def n1c(self, value: int) -> None:
        """Set the n1c property."""
        self._cards[2].set_value("n1c", value)

    @property
    def n2c(self) -> int:
        """Get or set the Second of 3 nodes need to reorient the crash model part.
        """ # nopep8
        return self._cards[2].get_value("n2c")

    @n2c.setter
    def n2c(self, value: int) -> None:
        """Set the n2c property."""
        self._cards[2].set_value("n2c", value)

    @property
    def n3c(self) -> int:
        """Get or set the Third of 3 nodes need to reorient the crash model part.
        """ # nopep8
        return self._cards[2].get_value("n3c")

    @n3c.setter
    def n3c(self, value: int) -> None:
        """Set the n3c property."""
        self._cards[2].set_value("n3c", value)

    @property
    def tensor(self) -> int:
        """Get or set the Tensor remap:
        EQ.0: map tensor data from history variables.
        EQ.1: Do not map tensor data from history variables.
        """ # nopep8
        return self._cards[2].get_value("tensor")

    @tensor.setter
    def tensor(self, value: int) -> None:
        """Set the tensor property."""
        if value not in [0, 1, None]:
            raise Exception("""tensor must be `None` or one of {0,1}.""")
        self._cards[2].set_value("tensor", value)

    @property
    def thkscl(self) -> float:
        """Get or set the Thickness scale factor.
        """ # nopep8
        return self._cards[2].get_value("thkscl")

    @thkscl.setter
    def thkscl(self, value: float) -> None:
        """Set the thkscl property."""
        self._cards[2].set_value("thkscl", value)

    @property
    def isym(self) -> int:
        """Get or set the Symmetric switch
        EQ.0:no symetric mapping
        EQ.1: yz plane symmetric mapping
        EQ.2: zx plane symmetric mapping
        EQ.3: zx and yz planes symmetric mapping
        EQ.4: user defined symmetric plane mapping
        """ # nopep8
        return self._cards[3].get_value("isym")

    @isym.setter
    def isym(self, value: int) -> None:
        """Set the isym property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""isym must be `None` or one of {0,1,2,3,4}.""")
        self._cards[3].set_value("isym", value)

    @property
    def iafter(self) -> int:
        """Get or set the Mirroring sequence switch
        EQ.0: generate a symmetric part before transformation
        EQ.1: generate a symmetric part after transformation
        """ # nopep8
        return self._cards[3].get_value("iafter")

    @iafter.setter
    def iafter(self, value: int) -> None:
        """Set the iafter property."""
        if value not in [0, 1, None]:
            raise Exception("""iafter must be `None` or one of {0,1}.""")
        self._cards[3].set_value("iafter", value)

    @property
    def percele(self) -> typing.Optional[float]:
        """Get or set the Percentage of elements that should be mapped for the simulation to proceed (default = 0); otherwise an error termination occurs. See Remark 6
        """ # nopep8
        return self._cards[3].get_value("percele")

    @percele.setter
    def percele(self, value: float) -> None:
        """Set the percele property."""
        self._cards[3].set_value("percele", value)

    @property
    def iortho(self) -> typing.Optional[int]:
        """Get or set the Location of the material direction cosine in the array of history variables of an orthotropic material
        """ # nopep8
        return self._cards[3].get_value("iortho")

    @iortho.setter
    def iortho(self, value: int) -> None:
        """Set the iortho property."""
        self._cards[3].set_value("iortho", value)

    @property
    def isrocut(self) -> typing.Optional[int]:
        """Get or set the Optional output of stamped part after transformation(s)
        EQ.0:	No output is written.
        NE.0 : Keyword output file “srcmsh_‌<ISRCOUT>” is created
        """ # nopep8
        return self._cards[3].get_value("isrocut")

    @isrocut.setter
    def isrocut(self, value: int) -> None:
        """Set the isrocut property."""
        self._cards[3].set_value("isrocut", value)

    @property
    def x01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[4].get_value("x01")

    @x01.setter
    def x01(self, value: float) -> None:
        """Set the x01 property."""
        self._cards[4].set_value("x01", value)

    @property
    def y01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[4].get_value("y01")

    @y01.setter
    def y01(self, value: float) -> None:
        """Set the y01 property."""
        self._cards[4].set_value("y01", value)

    @property
    def z01(self) -> typing.Optional[float]:
        """Get or set the First point in the symmetric plane (required if ISYM.NE.0)
        """ # nopep8
        return self._cards[4].get_value("z01")

    @z01.setter
    def z01(self, value: float) -> None:
        """Set the z01 property."""
        self._cards[4].set_value("z01", value)

    @property
    def x02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("x02")

    @x02.setter
    def x02(self, value: float) -> None:
        """Set the x02 property."""
        self._cards[5].set_value("x02", value)

    @property
    def y02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("y02")

    @y02.setter
    def y02(self, value: float) -> None:
        """Set the y02 property."""
        self._cards[5].set_value("y02", value)

    @property
    def z02(self) -> typing.Optional[float]:
        """Get or set the Second point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("z02")

    @z02.setter
    def z02(self, value: float) -> None:
        """Set the z02 property."""
        self._cards[5].set_value("z02", value)

    @property
    def x03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("x03")

    @x03.setter
    def x03(self, value: float) -> None:
        """Set the x03 property."""
        self._cards[5].set_value("x03", value)

    @property
    def y03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("y03")

    @y03.setter
    def y03(self, value: float) -> None:
        """Set the y03 property."""
        self._cards[5].set_value("y03", value)

    @property
    def z03(self) -> typing.Optional[float]:
        """Get or set the Third point in the symmetric plane
        """ # nopep8
        return self._cards[5].get_value("z03")

    @z03.setter
    def z03(self, value: float) -> None:
        """Set the z03 property."""
        self._cards[5].set_value("z03", value)

    @property
    def n1s_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1s."""
        return self._get_link_by_attr("NODE", "nid", self.n1s, "parts")

    @property
    def n2s_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2s."""
        return self._get_link_by_attr("NODE", "nid", self.n2s, "parts")

    @property
    def n3s_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3s."""
        return self._get_link_by_attr("NODE", "nid", self.n3s, "parts")

    @property
    def n1c_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1c."""
        return self._get_link_by_attr("NODE", "nid", self.n1c, "parts")

    @property
    def n2c_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2c."""
        return self._get_link_by_attr("NODE", "nid", self.n2c, "parts")

    @property
    def n3c_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3c."""
        return self._get_link_by_attr("NODE", "nid", self.n3c, "parts")

    @property
    def psid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid."""
        return self._get_set_link("PART", self.psid)

    @psid_link.setter
    def psid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid."""
        self.psid = value.sid

