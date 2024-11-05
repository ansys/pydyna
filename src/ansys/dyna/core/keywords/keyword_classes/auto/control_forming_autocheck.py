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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlFormingAutocheck(KeywordBase):
    """DYNA CONTROL_FORMING_AUTOCHECK keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_AUTOCHECK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "icheck",
                        int,
                        0,
                        10,
                        kwargs.get("icheck", 0)
                    ),
                    Field(
                        "igd",
                        int,
                        10,
                        10,
                        kwargs.get("igd")
                    ),
                    Field(
                        "ioffset",
                        int,
                        20,
                        10,
                        kwargs.get("ioffset", 0)
                    ),
                    Field(
                        "ioutputp",
                        int,
                        30,
                        10,
                        kwargs.get("ioutputp")
                    ),
                ],
            ),
        ]

    @property
    def icheck(self) -> int:
        """Get or set the Tool mesh checking/fixing flag:
        ICHECK.EQ.0:	Do not activate mesh checking/fixing feature.
        ICHECK.EQ.1:	Activate comprehensive mesh check and fix those problematic tool meshes which cause unreasonable forming results and/or error termination.  The keyword with this variable only can be inserted in any regular forming simulation; the simulation will continue after tool mesh checking/fixing is done, see Example 1.  The fixed tool meshes can be viewed and recovered from the resulting D3PLOT files.  If the termination time is set to “0.0” or the keyword *CONTROL_TERMINATION is absent all together, the simulation will terminate as soon as  “checking/fixing” is completed, and fixed tool meshes can be extracted from the D3PLOT files.
        """ # nopep8
        return self._cards[0].get_value("icheck")

    @icheck.setter
    def icheck(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icheck must be one of {0,1}""")
        self._cards[0].set_value("icheck", value)

    @property
    def igd(self) -> typing.Optional[int]:
        """Get or set the Not used.
        """ # nopep8
        return self._cards[0].get_value("igd")

    @igd.setter
    def igd(self, value: int) -> None:
        self._cards[0].set_value("igd", value)

    @property
    def ioffset(self) -> int:
        """Get or set the Tool mesh offset flag. Note this variable works only when IOUTPUT is defined.  When ICHECK is set to “1”:
        IOFFSET.EQ.0:	Do not offset rigid tool mesh.  The sheet blank does not need to be present.  IOUTPUT must be defined.  All corresponding files for defined IOUTPUT=1~4 will still be output; however, both rigid_offset.inc and rigid_offset_before.inc will be the same – the checked and fixed tool mesh file without offset.  See Example 2.
        IOFFSET.EQ.1:	Perform rigid tool mesh offset using the variable MST (see Figure 0-1) defined in *CONTACT_FORMING.... The blank must be defined and positioned completely above or below the rigid tool to be offset.  Both part ID and part SID (MSTYP) can be used in defining the MSID.  IOUTPUT must also be defined.
        """ # nopep8
        return self._cards[0].get_value("ioffset")

    @ioffset.setter
    def ioffset(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ioffset must be one of {0,1}""")
        self._cards[0].set_value("ioffset", value)

    @property
    def ioutputp(self) -> typing.Optional[int]:
        """Get or set the Output option flag:
        IOUTPUT.EQ.1:	Output offset rigid tool meshes into a keyword file rigid_offset.inc, and terminates the simulation.
        IOUTPUT.EQ.2:	Output offset rigid tool meshes as well as nodes used to define draw beads into a keyword file rigid_offset.inc, and terminates the simulation.  See Example 4.
        IOUTPUT.EQ.3:	Output checked/fixed tool as well as offset rigid tool meshes into two separate keyword files, rigid_offset_before.inc, and rigid_offset.inc, respectively, and terminates the simulation.  See Example 3.
        IOUTPUT.EQ.4:	Output checked/fixed tool meshes, offset rigid tool meshes as well as the nodes used to define draw beads into two separate keyword files, rigid_offset_before.inc, and rigid_offset.inc, respectively, and terminates the simulation.
        """ # nopep8
        return self._cards[0].get_value("ioutputp")

    @ioutputp.setter
    def ioutputp(self, value: int) -> None:
        self._cards[0].set_value("ioutputp", value)

