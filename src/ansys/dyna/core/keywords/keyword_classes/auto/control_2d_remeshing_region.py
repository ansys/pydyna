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

class Control2DRemeshingRegion(KeywordBase):
    """DYNA CONTROL_2D_REMESHING_REGION keyword"""

    keyword = "CONTROL"
    subkeyword = "2D_REMESHING_REGION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ltyp",
                        int,
                        0,
                        10,
                        kwargs.get("ltyp")
                    ),
                    Field(
                        "par1",
                        float,
                        10,
                        10,
                        kwargs.get("par1", 0.0)
                    ),
                    Field(
                        "par2",
                        float,
                        20,
                        10,
                        kwargs.get("par2", 0.0)
                    ),
                    Field(
                        "par3",
                        float,
                        30,
                        10,
                        kwargs.get("par3", 0.0)
                    ),
                    Field(
                        "par4",
                        float,
                        40,
                        10,
                        kwargs.get("par4", 0.0)
                    ),
                    Field(
                        "par5",
                        float,
                        50,
                        10,
                        kwargs.get("par5", 0.0)
                    ),
                    Field(
                        "par6",
                        float,
                        60,
                        10,
                        kwargs.get("par6", 0.0)
                    ),
                    Field(
                        "par7",
                        float,
                        70,
                        10,
                        kwargs.get("par7", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ltyp(self) -> typing.Optional[int]:
        """Get or set the Type of regions defined by the parameters PARi:
        EQ.1:	Box.PAR1 is the ID of * DEFINE_BOX that defines the region to remesh.The other parameters are not used.
        EQ.2 : Part set.PAR1 is the ID of * SET_PART that selects the parts to remesh.The other parameters are not used.
        EQ.3 : Region around elements in contact from a specified contact.To specify the desired contact, set PAR1 to its order of appearance in the input deck.For instance, PAR1 = 5 if the desired contact is the 5th contact keyword to appear in the input deck.A box is created around each element in contact for the remeshing.PAR2 through PAR4 add padding around the box to increase the remeshing region.PAR2 > 0 is the length subtracted from the x - coordinate of the box’s lower corner.PAR3 > 0 is the length added to the x - coordinate of the box’s upper corner.PAR4 > 0 is the length subtracted from the y - coordinate of the box’s lower corner.PAR5 > 0 is the length added to the y - coordinate of the box’s upper corner.The last 2 parameters are not used.
        EQ.4:	PAR1 is the ID of * DEFINE_BOX that selects mesh boundaries (edges) of remeshing regions along which nodes added after remeshing(Hanging nodes between edge ends) keep their initial parametric positions between the boundary corner nodesedge-end constraining nodes).The other parameters are not used.By default, when nodes are added to the edges of elements in the regionand the edges are not on the mesh boundary, the positionsand velocities of the hanging nodes are interpolated from the positionsand velocities of the original constraining nodes along these edges(constraining nodes are nodes that existed before remeshing).If the edge of a remeshed element is on the mesh boundaries, the positionsand velocities of the hanging nodes on the edge are, by default, not interpolated because they are likely to be subject to boundary conditions.With LTYP = 4, the hanging nodes on the boundary of the mesh are interpolated.
        EQ.5:	PAR1 is the ID of *SET_NODE that selects nodes along shell edges. After remeshing, the node set is recreated with nodes along the same shell edges. PAR2>0 is a thickness for the shell edges to select the new nodes after remeshing. The other parameters are not used.
        EQ.6:	PAR1 is the ID of * SET_PART that selects the parts for which the total displacements are output in D3PLOT after remeshings.
        EQ.7 : PAR1 is the ID of * SET_SHELL that selects the shells to remesh.The other parameters are not used
        """ # nopep8
        return self._cards[0].get_value("ltyp")

    @ltyp.setter
    def ltyp(self, value: int) -> None:
        self._cards[0].set_value("ltyp", value)

    @property
    def par1(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par1")

    @par1.setter
    def par1(self, value: float) -> None:
        self._cards[0].set_value("par1", value)

    @property
    def par2(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par2")

    @par2.setter
    def par2(self, value: float) -> None:
        self._cards[0].set_value("par2", value)

    @property
    def par3(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par3")

    @par3.setter
    def par3(self, value: float) -> None:
        self._cards[0].set_value("par3", value)

    @property
    def par4(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par4")

    @par4.setter
    def par4(self, value: float) -> None:
        self._cards[0].set_value("par4", value)

    @property
    def par5(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par5")

    @par5.setter
    def par5(self, value: float) -> None:
        self._cards[0].set_value("par5", value)

    @property
    def par6(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par6")

    @par6.setter
    def par6(self, value: float) -> None:
        self._cards[0].set_value("par6", value)

    @property
    def par7(self) -> float:
        """Get or set the Parameters defined by LTYP
        """ # nopep8
        return self._cards[0].get_value("par7")

    @par7.setter
    def par7(self, value: float) -> None:
        self._cards[0].set_value("par7", value)

