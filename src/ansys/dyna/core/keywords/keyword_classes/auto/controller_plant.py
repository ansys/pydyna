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

class ControllerPlant(KeywordBase):
    """DYNA CONTROLLER_PLANT keyword"""

    keyword = "CONTROLLER"
    subkeyword = "PLANT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "plntid",
                        int,
                        0,
                        10,
                        kwargs.get("plntid")
                    ),
                    Field(
                        "nin",
                        int,
                        10,
                        10,
                        kwargs.get("nin")
                    ),
                    Field(
                        "nout",
                        int,
                        20,
                        10,
                        kwargs.get("nout")
                    ),
                    Field(
                        "nmode",
                        int,
                        30,
                        10,
                        kwargs.get("nmode")
                    ),
                    Field(
                        "mtxq",
                        int,
                        40,
                        10,
                        kwargs.get("mtxq")
                    ),
                    Field(
                        "mtxr",
                        int,
                        50,
                        10,
                        kwargs.get("mtxr")
                    ),
                    Field(
                        "mopt",
                        int,
                        60,
                        10,
                        kwargs.get("mopt")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fscilab",
                        str,
                        0,
                        20,
                        kwargs.get("fscilab")
                    ),
                    Field(
                        "flsdyna",
                        str,
                        20,
                        20,
                        kwargs.get("flsdyna")
                    ),
                    Field(
                        "fmatlab",
                        str,
                        40,
                        20,
                        kwargs.get("fmatlab")
                    ),
                    Field(
                        "unused",
                        str,
                        60,
                        20,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodi1",
                        int,
                        0,
                        10,
                        kwargs.get("nodi1")
                    ),
                    Field(
                        "dofi1",
                        int,
                        10,
                        10,
                        kwargs.get("dofi1", 1)
                    ),
                    Field(
                        "nodi2",
                        int,
                        20,
                        10,
                        kwargs.get("nodi2")
                    ),
                    Field(
                        "dofi2",
                        int,
                        30,
                        10,
                        kwargs.get("dofi2", 1)
                    ),
                    Field(
                        "nodi3",
                        int,
                        40,
                        10,
                        kwargs.get("nodi3")
                    ),
                    Field(
                        "dofi3",
                        int,
                        50,
                        10,
                        kwargs.get("dofi3", 1)
                    ),
                    Field(
                        "nodi4",
                        int,
                        60,
                        10,
                        kwargs.get("nodi4")
                    ),
                    Field(
                        "dofi4",
                        int,
                        70,
                        10,
                        kwargs.get("dofi4", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodo1",
                        int,
                        0,
                        10,
                        kwargs.get("nodo1")
                    ),
                    Field(
                        "dofo1",
                        int,
                        10,
                        10,
                        kwargs.get("dofo1", 1)
                    ),
                    Field(
                        "nodo2",
                        int,
                        20,
                        10,
                        kwargs.get("nodo2")
                    ),
                    Field(
                        "dofo2",
                        int,
                        30,
                        10,
                        kwargs.get("dofo2", 1)
                    ),
                    Field(
                        "nodo3",
                        int,
                        40,
                        10,
                        kwargs.get("nodo3")
                    ),
                    Field(
                        "dofo3",
                        int,
                        50,
                        10,
                        kwargs.get("dofo3", 1)
                    ),
                    Field(
                        "nodo4",
                        int,
                        60,
                        10,
                        kwargs.get("nodo4")
                    ),
                    Field(
                        "dofo4",
                        int,
                        70,
                        10,
                        kwargs.get("dofo4", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nfeq",
                        int,
                        0,
                        10,
                        kwargs.get("nfeq", 1)
                    ),
                    Field(
                        "deftol",
                        float,
                        10,
                        10,
                        kwargs.get("deftol", 1.0e-9)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mod1",
                        int,
                        0,
                        10,
                        kwargs.get("mod1")
                    ),
                    Field(
                        "mod2",
                        int,
                        10,
                        10,
                        kwargs.get("mod2")
                    ),
                    Field(
                        "mod3",
                        int,
                        20,
                        10,
                        kwargs.get("mod3")
                    ),
                    Field(
                        "mod4",
                        int,
                        30,
                        10,
                        kwargs.get("mod4")
                    ),
                    Field(
                        "mod5",
                        int,
                        40,
                        10,
                        kwargs.get("mod5")
                    ),
                    Field(
                        "mod6",
                        int,
                        50,
                        10,
                        kwargs.get("mod6")
                    ),
                    Field(
                        "mod7",
                        int,
                        60,
                        10,
                        kwargs.get("mod7")
                    ),
                    Field(
                        "mod8",
                        int,
                        70,
                        10,
                        kwargs.get("mod8")
                    ),
                ],
            ),
        ]

    @property
    def plntid(self) -> typing.Optional[int]:
        """Get or set the Plant ID
        """ # nopep8
        return self._cards[0].get_value("plntid")

    @plntid.setter
    def plntid(self, value: int) -> None:
        self._cards[0].set_value("plntid", value)

    @property
    def nin(self) -> typing.Optional[int]:
        """Get or set the Number of input DOFs, such as nodal force or voltage. If all nodes within a set share a single input variable, together they account for one DOF. For example, all nodes within a set share a single input voltage for a piezo actuator.
        """ # nopep8
        return self._cards[0].get_value("nin")

    @nin.setter
    def nin(self, value: int) -> None:
        self._cards[0].set_value("nin", value)

    @property
    def nout(self) -> typing.Optional[int]:
        """Get or set the Number of output DOFs, such as nodal displacement or voltage. Note that the same node velocity will be automatically exported as well
        """ # nopep8
        return self._cards[0].get_value("nout")

    @nout.setter
    def nout(self, value: int) -> None:
        self._cards[0].set_value("nout", value)

    @property
    def nmode(self) -> typing.Optional[int]:
        """Get or set the Number of modes for the modal truncation, or number of base vectors for the Krylov method. If zero, all active DOFs will be used (not recommended). The reduced system will have a dimension of 2NMODE for the modal truncation method, and NMODE for the Krylov method
        """ # nopep8
        return self._cards[0].get_value("nmode")

    @nmode.setter
    def nmode(self, value: int) -> None:
        self._cards[0].set_value("nmode", value)

    @property
    def mtxq(self) -> typing.Optional[int]:
        """Get or set the Q matrix for linear-quadratic-regular (LQR) method (unused currently)
        """ # nopep8
        return self._cards[0].get_value("mtxq")

    @mtxq.setter
    def mtxq(self, value: int) -> None:
        self._cards[0].set_value("mtxq", value)

    @property
    def mtxr(self) -> typing.Optional[int]:
        """Get or set the R matrix for linear-quadratic-regular (LQR) method (unused currently)
        """ # nopep8
        return self._cards[0].get_value("mtxr")

    @mtxr.setter
    def mtxr(self, value: int) -> None:
        self._cards[0].set_value("mtxr", value)

    @property
    def mopt(self) -> typing.Optional[int]:
        """Get or set the Modal order reduction method (see Remark 1):
        EQ.0:	Modal truncation method
        EQ.1 : Krylov subspace method
        """ # nopep8
        return self._cards[0].get_value("mopt")

    @mopt.setter
    def mopt(self, value: int) -> None:
        self._cards[0].set_value("mopt", value)

    @property
    def fscilab(self) -> typing.Optional[str]:
        """Get or set the File name in LSDYNA format “SCI”. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
        """ # nopep8
        return self._cards[1].get_value("fscilab")

    @fscilab.setter
    def fscilab(self, value: str) -> None:
        self._cards[1].set_value("fscilab", value)

    @property
    def flsdyna(self) -> typing.Optional[str]:
        """Get or set the File name in LS-DYNA format .k. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated
        """ # nopep8
        return self._cards[1].get_value("flsdyna")

    @flsdyna.setter
    def flsdyna(self, value: str) -> None:
        self._cards[1].set_value("flsdyna", value)

    @property
    def fmatlab(self) -> typing.Optional[str]:
        """Get or set the File name in MATLAB format .m. If specified, the reduced matrices will be written accordingly. If left blank, no such file will be generated.
        """ # nopep8
        return self._cards[1].get_value("fmatlab")

    @fmatlab.setter
    def fmatlab(self, value: str) -> None:
        self._cards[1].set_value("fmatlab", value)

    @property
    def nodi1(self) -> typing.Optional[int]:
        """Get or set the Node or node set index for the input channel.
        GT.0:Nnode index
        LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
        """ # nopep8
        return self._cards[2].get_value("nodi1")

    @nodi1.setter
    def nodi1(self, value: int) -> None:
        self._cards[2].set_value("nodi1", value)

    @property
    def dofi1(self) -> int:
        """Get or set the Degree-of-freedom for input:
        EQ.1:	Nodal force in the x - direction, f_x
        EQ.2 : Nodal force in the y - direction, f_y
        EQ.3 : Nodal force in the z - direction, f_z
        EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("dofi1")

    @dofi1.setter
    def dofi1(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofi1 must be one of {1,2,3,7}""")
        self._cards[2].set_value("dofi1", value)

    @property
    def nodi2(self) -> typing.Optional[int]:
        """Get or set the Node or node set index for the input channel.
        GT.0:Nnode index
        LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
        """ # nopep8
        return self._cards[2].get_value("nodi2")

    @nodi2.setter
    def nodi2(self, value: int) -> None:
        self._cards[2].set_value("nodi2", value)

    @property
    def dofi2(self) -> int:
        """Get or set the Degree-of-freedom for input:
        EQ.1:	Nodal force in the x - direction, f_x
        EQ.2 : Nodal force in the y - direction, f_y
        EQ.3 : Nodal force in the z - direction, f_z
        EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("dofi2")

    @dofi2.setter
    def dofi2(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofi2 must be one of {1,2,3,7}""")
        self._cards[2].set_value("dofi2", value)

    @property
    def nodi3(self) -> typing.Optional[int]:
        """Get or set the Node or node set index for the input channel.
        GT.0:Nnode index
        LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
        """ # nopep8
        return self._cards[2].get_value("nodi3")

    @nodi3.setter
    def nodi3(self, value: int) -> None:
        self._cards[2].set_value("nodi3", value)

    @property
    def dofi3(self) -> int:
        """Get or set the Degree-of-freedom for input:
        EQ.1:	Nodal force in the x - direction, f_x
        EQ.2 : Nodal force in the y - direction, f_y
        EQ.3 : Nodal force in the z - direction, f_z
        EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("dofi3")

    @dofi3.setter
    def dofi3(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofi3 must be one of {1,2,3,7}""")
        self._cards[2].set_value("dofi3", value)

    @property
    def nodi4(self) -> typing.Optional[int]:
        """Get or set the Node or node set index for the input channel.
        GT.0:Nnode index
        LT.0:Node set index, within which all nodes share the same input variable, e.g., force, voltage, see Remark 2 below.
        """ # nopep8
        return self._cards[2].get_value("nodi4")

    @nodi4.setter
    def nodi4(self, value: int) -> None:
        self._cards[2].set_value("nodi4", value)

    @property
    def dofi4(self) -> int:
        """Get or set the Degree-of-freedom for input:
        EQ.1:	Nodal force in the x - direction, f_x
        EQ.2 : Nodal force in the y - direction, f_y
        EQ.3 : Nodal force in the z - direction, f_z
        EQ.7 : Voltage if piezoelectric materials are defined.See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("dofi4")

    @dofi4.setter
    def dofi4(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofi4 must be one of {1,2,3,7}""")
        self._cards[2].set_value("dofi4", value)

    @property
    def nodo1(self) -> typing.Optional[int]:
        """Get or set the Node index for output
        """ # nopep8
        return self._cards[3].get_value("nodo1")

    @nodo1.setter
    def nodo1(self, value: int) -> None:
        self._cards[3].set_value("nodo1", value)

    @property
    def dofo1(self) -> int:
        """Get or set the Degree-of-freedom for output:
        EQ.1:	Displacement along the x - direction
        EQ.2 : Displacement along the y - direction
        EQ.3 : Displacement along the z - direction
        EQ.7 : Voltage output if piezoelectric materials are defined.
        """ # nopep8
        return self._cards[3].get_value("dofo1")

    @dofo1.setter
    def dofo1(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofo1 must be one of {1,2,3,7}""")
        self._cards[3].set_value("dofo1", value)

    @property
    def nodo2(self) -> typing.Optional[int]:
        """Get or set the Node index for output
        """ # nopep8
        return self._cards[3].get_value("nodo2")

    @nodo2.setter
    def nodo2(self, value: int) -> None:
        self._cards[3].set_value("nodo2", value)

    @property
    def dofo2(self) -> int:
        """Get or set the Degree-of-freedom for output:
        EQ.1:	Displacement along the x - direction
        EQ.2 : Displacement along the y - direction
        EQ.3 : Displacement along the z - direction
        EQ.7 : Voltage output if piezoelectric materials are defined.
        """ # nopep8
        return self._cards[3].get_value("dofo2")

    @dofo2.setter
    def dofo2(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofo2 must be one of {1,2,3,7}""")
        self._cards[3].set_value("dofo2", value)

    @property
    def nodo3(self) -> typing.Optional[int]:
        """Get or set the Node index for output
        """ # nopep8
        return self._cards[3].get_value("nodo3")

    @nodo3.setter
    def nodo3(self, value: int) -> None:
        self._cards[3].set_value("nodo3", value)

    @property
    def dofo3(self) -> int:
        """Get or set the Degree-of-freedom for output:
        EQ.1:	Displacement along the x - direction
        EQ.2 : Displacement along the y - direction
        EQ.3 : Displacement along the z - direction
        EQ.7 : Voltage output if piezoelectric materials are defined.
        """ # nopep8
        return self._cards[3].get_value("dofo3")

    @dofo3.setter
    def dofo3(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofo3 must be one of {1,2,3,7}""")
        self._cards[3].set_value("dofo3", value)

    @property
    def nodo4(self) -> typing.Optional[int]:
        """Get or set the Node index for output
        """ # nopep8
        return self._cards[3].get_value("nodo4")

    @nodo4.setter
    def nodo4(self, value: int) -> None:
        self._cards[3].set_value("nodo4", value)

    @property
    def dofo4(self) -> int:
        """Get or set the Degree-of-freedom for output:
        EQ.1:	Displacement along the x - direction
        EQ.2 : Displacement along the y - direction
        EQ.3 : Displacement along the z - direction
        EQ.7 : Voltage output if piezoelectric materials are defined.
        """ # nopep8
        return self._cards[3].get_value("dofo4")

    @dofo4.setter
    def dofo4(self, value: int) -> None:
        if value not in [1, 2, 3, 7]:
            raise Exception("""dofo4 must be one of {1,2,3,7}""")
        self._cards[3].set_value("dofo4", value)

    @property
    def nfeq(self) -> int:
        """Get or set the Number of shifted frequencies to generate the Krylov base vectors. In most cases, a single frequency at zero rad/s works. For the modal truncation method, just leave as it is.
        """ # nopep8
        return self._cards[4].get_value("nfeq")

    @nfeq.setter
    def nfeq(self, value: int) -> None:
        self._cards[4].set_value("nfeq", value)

    @property
    def deftol(self) -> float:
        """Get or set the Deflation tolerance for the Krylov method. The default value of 10E-9 works in most cases. For the modal truncation method, just leave as it is.
        """ # nopep8
        return self._cards[4].get_value("deftol")

    @deftol.setter
    def deftol(self, value: float) -> None:
        self._cards[4].set_value("deftol", value)

    @property
    def mod1(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod1")

    @mod1.setter
    def mod1(self, value: int) -> None:
        self._cards[5].set_value("mod1", value)

    @property
    def mod2(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod2")

    @mod2.setter
    def mod2(self, value: int) -> None:
        self._cards[5].set_value("mod2", value)

    @property
    def mod3(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod3")

    @mod3.setter
    def mod3(self, value: int) -> None:
        self._cards[5].set_value("mod3", value)

    @property
    def mod4(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod4")

    @mod4.setter
    def mod4(self, value: int) -> None:
        self._cards[5].set_value("mod4", value)

    @property
    def mod5(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod5")

    @mod5.setter
    def mod5(self, value: int) -> None:
        self._cards[5].set_value("mod5", value)

    @property
    def mod6(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod6")

    @mod6.setter
    def mod6(self, value: int) -> None:
        self._cards[5].set_value("mod6", value)

    @property
    def mod7(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod7")

    @mod7.setter
    def mod7(self, value: int) -> None:
        self._cards[5].set_value("mod7", value)

    @property
    def mod8(self) -> typing.Optional[int]:
        """Get or set the List all NMODE mode indexes for the modal truncation method, or NFEQ shifting frequencies (unit: rad/s) for the Krylov method. The default setting of a single frequency at 0 rad/s works in most Krylov cases.For the model truncation method, a negative MODx triggers mode generation between MODx-1 and -MODx, meaning all modes between MODx-1 and -MODx will be considered.
        """ # nopep8
        return self._cards[5].get_value("mod8")

    @mod8.setter
    def mod8(self, value: int) -> None:
        self._cards[5].set_value("mod8", value)

