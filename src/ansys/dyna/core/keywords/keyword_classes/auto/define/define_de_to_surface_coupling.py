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

"""Module providing the DefineDeToSurfaceCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDETOSURFACECOUPLING_CARD0 = (
    FieldSchema("slave", int, 0, 10, None),
    FieldSchema("master", int, 10, 10, None),
    FieldSchema("stype", int, 20, 10, 0),
    FieldSchema("mtype", int, 30, 10, 0),
)

_DEFINEDETOSURFACECOUPLING_CARD1 = (
    FieldSchema("frics", float, 0, 10, None),
    FieldSchema("fricd", float, 10, 10, None),
    FieldSchema("damp", float, 20, 10, None),
    FieldSchema("bsort", int, 30, 10, 100),
    FieldSchema("lcvx", int, 40, 10, 0),
    FieldSchema("lcvy", int, 50, 10, 0),
    FieldSchema("lcvz", int, 60, 10, 0),
    FieldSchema("wearc", float, 70, 10, 0.0),
)

_DEFINEDETOSURFACECOUPLING_CARD2 = (
    FieldSchema("w1", float, 0, 10, None),
    FieldSchema("w2", float, 10, 10, None),
    FieldSchema("w3", float, 20, 10, None),
    FieldSchema("w4", float, 30, 10, None),
    FieldSchema("w5", float, 40, 10, None),
    FieldSchema("w6", float, 50, 10, None),
    FieldSchema("w7", float, 60, 10, None),
    FieldSchema("w8", float, 70, 10, None),
)

_DEFINEDETOSURFACECOUPLING_CARD3 = (
    FieldSchema("sfp", float, 0, 10, 1.0),
    FieldSchema("sft", float, 10, 10, 1.0),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("cid_rcf", int, 50, 10, 0),
    FieldSchema("bt", float, 60, 10, 0.0),
    FieldSchema("dt", float, 70, 10, 1e+20),
)

class DefineDeToSurfaceCoupling(KeywordBase):
    """DYNA DEFINE_DE_TO_SURFACE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_TO_SURFACE_COUPLING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeToSurfaceCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDETOSURFACECOUPLING_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeToSurfaceCoupling.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def slave(self) -> typing.Optional[int]:
        """Get or set the Slave Set ID
        """ # nopep8
        return self._cards[0].get_value("slave")

    @slave.setter
    def slave(self, value: int) -> None:
        """Set the slave property."""
        self._cards[0].set_value("slave", value)

    @property
    def master(self) -> typing.Optional[int]:
        """Get or set the Master shell Set ID
        """ # nopep8
        return self._cards[0].get_value("master")

    @master.setter
    def master(self, value: int) -> None:
        """Set the master property."""
        self._cards[0].set_value("master", value)

    @property
    def stype(self) -> int:
        """Get or set the EQ.0: Slave node set
        EQ.1: Slave node
        EQ.2: Slave part set
        EQ.3: Slave part
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("stype", value)

    @property
    def mtype(self) -> int:
        """Get or set the EQ.0: Part set
        EQ.1: Part
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, None]:
            raise Exception("""mtype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def frics(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient
        """ # nopep8
        return self._cards[1].get_value("frics")

    @frics.setter
    def frics(self, value: float) -> None:
        """Set the frics property."""
        self._cards[1].set_value("frics", value)

    @property
    def fricd(self) -> typing.Optional[float]:
        """Get or set the Rolling friction coefficient
        """ # nopep8
        return self._cards[1].get_value("fricd")

    @fricd.setter
    def fricd(self, value: float) -> None:
        """Set the fricd property."""
        self._cards[1].set_value("fricd", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def bsort(self) -> int:
        """Get or set the Number of cycle between bucket sort. (Default=100) .LT.0: ABS(BSORT) is the minimum number of cycle between bucket sort.  This value can be increased during runtime by tracking the velocity of potential coupling pair.  This feature only works with MPP currently.
        """ # nopep8
        return self._cards[1].get_value("bsort")

    @bsort.setter
    def bsort(self, value: int) -> None:
        """Set the bsort property."""
        self._cards[1].set_value("bsort", value)

    @property
    def lcvx(self) -> int:
        """Get or set the Load curve defines surface velocity in X direction.
        """ # nopep8
        return self._cards[1].get_value("lcvx")

    @lcvx.setter
    def lcvx(self, value: int) -> None:
        """Set the lcvx property."""
        self._cards[1].set_value("lcvx", value)

    @property
    def lcvy(self) -> int:
        """Get or set the Load curve defines surface velocity in Y direction.
        """ # nopep8
        return self._cards[1].get_value("lcvy")

    @lcvy.setter
    def lcvy(self, value: int) -> None:
        """Set the lcvy property."""
        self._cards[1].set_value("lcvy", value)

    @property
    def lcvz(self) -> int:
        """Get or set the Load curve defines surface velocity in Z direction.
        """ # nopep8
        return self._cards[1].get_value("lcvz")

    @lcvz.setter
    def lcvz(self, value: int) -> None:
        """Set the lcvz property."""
        self._cards[1].set_value("lcvz", value)

    @property
    def wearc(self) -> float:
        """Get or set the WEARC is the wear coefficient..
        """ # nopep8
        return self._cards[1].get_value("wearc")

    @wearc.setter
    def wearc(self, value: float) -> None:
        """Set the wearc property."""
        self._cards[1].set_value("wearc", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[2].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[2].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[2].set_value("w3", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[2].set_value("w4", value)

    @property
    def w5(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[2].set_value("w5", value)

    @property
    def w6(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[2].set_value("w6", value)

    @property
    def w7(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[2].set_value("w7", value)

    @property
    def w8(self) -> typing.Optional[float]:
        """Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
        """ # nopep8
        return self._cards[2].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[2].set_value("w8", value)

    @property
    def sfp(self) -> float:
        """Get or set the Scale factor on contact stiffness. By default, SFP = 1.0
        """ # nopep8
        return self._cards[3].get_value("sfp")

    @sfp.setter
    def sfp(self, value: float) -> None:
        """Set the sfp property."""
        self._cards[3].set_value("sfp", value)

    @property
    def sft(self) -> float:
        """Get or set the Scale factor for surface thickness (scales true thickness). This option
        applies only to contact with shell elements. True thickness is the	element thickness of the shell elements
        """ # nopep8
        return self._cards[3].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        """Set the sft property."""
        self._cards[3].set_value("sft", value)

    @property
    def cid_rcf(self) -> int:
        """Get or set the Coordinate system ID to output demrcf force resultants in a local system.
        """ # nopep8
        return self._cards[3].get_value("cid_rcf")

    @cid_rcf.setter
    def cid_rcf(self, value: int) -> None:
        """Set the cid_rcf property."""
        self._cards[3].set_value("cid_rcf", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time
        """ # nopep8
        return self._cards[3].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[3].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time
        """ # nopep8
        return self._cards[3].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[3].set_value("dt", value)

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

