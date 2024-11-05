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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SensorDefineElementSetUpdate(KeywordBase):
    """DYNA SENSOR_DEFINE_ELEMENT_SET_UPDATE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_ELEMENT_SET_UPDATE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sensid",
                        int,
                        0,
                        10,
                        kwargs.get("sensid")
                    ),
                    Field(
                        "etype",
                        str,
                        10,
                        10,
                        kwargs.get("etype", "BEAM")
                    ),
                    Field(
                        "elemid",
                        int,
                        20,
                        10,
                        kwargs.get("elemid")
                    ),
                    Field(
                        "comp",
                        str,
                        30,
                        10,
                        kwargs.get("comp", "XX")
                    ),
                    Field(
                        "ctype",
                        str,
                        40,
                        10,
                        kwargs.get("ctype", "STRAIN")
                    ),
                    Field(
                        "layer",
                        str,
                        50,
                        10,
                        kwargs.get("layer", "BOT")
                    ),
                    Field(
                        "sf",
                        float,
                        60,
                        10,
                        kwargs.get("sf")
                    ),
                    Field(
                        "pwr",
                        float,
                        70,
                        10,
                        kwargs.get("pwr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "setopt",
                        str,
                        0,
                        10,
                        kwargs.get("setopt", "AVG")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "birth",
                        float,
                        0,
                        10,
                        kwargs.get("birth")
                    ),
                    Field(
                        "death",
                        float,
                        10,
                        10,
                        kwargs.get("death")
                    ),
                    Field(
                        "dtupd",
                        float,
                        20,
                        10,
                        kwargs.get("dtupd")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorDefineElementSetUpdate.option_specs[0],
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
    def sensid(self) -> typing.Optional[int]:
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        self._cards[0].set_value("sensid", value)

    @property
    def etype(self) -> str:
        """Get or set the Element type:
        EQ.BEAM	: 	beam element set.
        EQ.SHELL:	shell element set
        EQ.SOLID:	solid element set
        EQ.DISC-ELE:	discrete element set.
        """ # nopep8
        return self._cards[0].get_value("etype")

    @etype.setter
    def etype(self, value: str) -> None:
        if value not in ["BEAM", "SHELL", "SOLID", "DISC-ELE"]:
            raise Exception("""etype must be one of {"BEAM","SHELL","SOLID","DISC-ELE"}""")
        self._cards[0].set_value("etype", value)

    @property
    def elemid(self) -> typing.Optional[int]:
        """Get or set the Element ID or element set ID when the SET keyword option is active.
        In the case of the SET keyword option with SETOPT not defined, determining the status of a related* SENSOR_SWITCH depends on the sign of ELEMID
        """ # nopep8
        return self._cards[0].get_value("elemid")

    @elemid.setter
    def elemid(self, value: int) -> None:
        self._cards[0].set_value("elemid", value)

    @property
    def comp(self) -> str:
        """Get or set the Element type:
        EQ.XX: 		x-normal component for shells and solids
        EQ.YY:		y-normal component for shells and solids
        EQ.ZZ:		z-normal component for shells and solids
        EQ.XY:		xy-shear component for shells and solids
        EQ.YZ:		yz-shear component for shells and solids
        EQ.ZX:		zx-shear component for shells and solids
        EQ:AXIAL:	axial
        EQ:SHEARS:	local s-direction
        EQ:SHEART:	local t-direction
        EQ:               : 	leave blank for discrete elements
        """ # nopep8
        return self._cards[0].get_value("comp")

    @comp.setter
    def comp(self, value: str) -> None:
        if value not in ["XX", "YY", "ZZ", "XY", "YZ", "ZX", "AXIAL", "  ", "SHEARS", "SHEART"]:
            raise Exception("""comp must be one of {"XX","YY","ZZ","XY","YZ","ZX","AXIAL","  ","SHEARS","SHEART"}""")
        self._cards[0].set_value("comp", value)

    @property
    def ctype(self) -> str:
        """Get or set the Component type:
        EQ.STRAIN: 	strain component for shells and solids
        EQ.STRESS:	stress component for shells and solids
        EQ.FORCE:	force resultants for beams
        EQ.MOMENT:	moment resultants for beams
        EQ.FORCE:	discrete element force
        EQ.DLEN:	change in length for discrete element
        EQ.FAIL:	failure of element, sensor value = 1 when element fails, = 0 otherwise.
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: str) -> None:
        if value not in ["STRAIN", "STRESS", "FORCE", "MOMENT", "DLEN", "FAIL"]:
            raise Exception("""ctype must be one of {"STRAIN","STRESS","FORCE","MOMENT","DLEN","FAIL"}""")
        self._cards[0].set_value("ctype", value)

    @property
    def layer(self) -> str:
        """Get or set the Layer of integration point in shell element
        EQ.BOT: component at lower surface
        EQ.TOP: component at upper surface
        """ # nopep8
        return self._cards[0].get_value("layer")

    @layer.setter
    def layer(self, value: str) -> None:
        if value not in ["BOT", "TOP"]:
            raise Exception("""layer must be one of {"BOT","TOP"}""")
        self._cards[0].set_value("layer", value)

    @property
    def sf(self) -> typing.Optional[float]:
        """Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def pwr(self) -> typing.Optional[float]:
        """Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
        """ # nopep8
        return self._cards[0].get_value("pwr")

    @pwr.setter
    def pwr(self, value: float) -> None:
        self._cards[0].set_value("pwr", value)

    @property
    def setopt(self) -> str:
        """Get or set the Option to process set of data when SET option is specified.If you set SETOPT, then ELEMID must be greater than 0. When SETOPT is defined, a single value will be reported.  The single reported value could be:
        EQ.AVG: the average value of the dataset
        EQ.MAX: the maximum value of the dataset
        EQ.MIN: the minimum value of the dataset
        EQ.SUM: the sum of the dataset.
        """ # nopep8
        return self._cards[1].get_value("setopt")

    @setopt.setter
    def setopt(self, value: str) -> None:
        if value not in ["AVG", "MAX", "MIN", "SUM"]:
            raise Exception("""setopt must be one of {"AVG","MAX","MIN","SUM"}""")
        self._cards[1].set_value("setopt", value)

    @property
    def birth(self) -> typing.Optional[float]:
        """Get or set the Sensor IBirth time of this sensor.
        """ # nopep8
        return self._cards[2].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[2].set_value("birth", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time of this sensor.
        """ # nopep8
        return self._cards[2].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[2].set_value("death", value)

    @property
    def dtupd(self) -> typing.Optional[float]:
        """Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
        """ # nopep8
        return self._cards[2].get_value("dtupd")

    @dtupd.setter
    def dtupd(self, value: float) -> None:
        self._cards[2].set_value("dtupd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

