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

class ControlOutput(KeywordBase):
    """DYNA CONTROL_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "npopt",
                        int,
                        0,
                        10,
                        kwargs.get("npopt", 0)
                    ),
                    Field(
                        "neecho",
                        int,
                        10,
                        10,
                        kwargs.get("neecho", 0)
                    ),
                    Field(
                        "nrefup",
                        int,
                        20,
                        10,
                        kwargs.get("nrefup", 0)
                    ),
                    Field(
                        "iaccop",
                        int,
                        30,
                        10,
                        kwargs.get("iaccop", 0)
                    ),
                    Field(
                        "opifs",
                        float,
                        40,
                        10,
                        kwargs.get("opifs", 0.0)
                    ),
                    Field(
                        "ipnint",
                        int,
                        50,
                        10,
                        kwargs.get("ipnint", 0)
                    ),
                    Field(
                        "ikedit",
                        int,
                        60,
                        10,
                        kwargs.get("ikedit", 100)
                    ),
                    Field(
                        "iflush",
                        int,
                        70,
                        10,
                        kwargs.get("iflush", 5000)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iprtf",
                        int,
                        0,
                        10,
                        kwargs.get("iprtf", 0)
                    ),
                    Field(
                        "ierode",
                        int,
                        10,
                        10,
                        kwargs.get("ierode", 0)
                    ),
                    Field(
                        "tet10s8",
                        int,
                        20,
                        10,
                        kwargs.get("tet10s8", 2)
                    ),
                    Field(
                        "msgmax",
                        int,
                        30,
                        10,
                        kwargs.get("msgmax", 50)
                    ),
                    Field(
                        "ipcurv",
                        int,
                        40,
                        10,
                        kwargs.get("ipcurv", 0)
                    ),
                    Field(
                        "gmdt",
                        float,
                        50,
                        10,
                        kwargs.get("gmdt", 0.0)
                    ),
                    Field(
                        "ip1dblt",
                        int,
                        60,
                        10,
                        kwargs.get("ip1dblt", 0)
                    ),
                    Field(
                        "eocs",
                        int,
                        70,
                        10,
                        kwargs.get("eocs", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tolev",
                        int,
                        0,
                        10,
                        kwargs.get("tolev", 2)
                    ),
                    Field(
                        "newleg",
                        int,
                        10,
                        10,
                        kwargs.get("newleg", 0)
                    ),
                    Field(
                        "frfreq",
                        int,
                        20,
                        10,
                        kwargs.get("frfreq", 1)
                    ),
                    Field(
                        "minfo",
                        int,
                        30,
                        10,
                        kwargs.get("minfo", 0)
                    ),
                    Field(
                        "solsig",
                        int,
                        40,
                        10,
                        kwargs.get("solsig", 0)
                    ),
                    Field(
                        "msgflg",
                        int,
                        50,
                        10,
                        kwargs.get("msgflg", 0)
                    ),
                    Field(
                        "cdetol",
                        float,
                        60,
                        10,
                        kwargs.get("cdetol", 10.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "phschng",
                        int,
                        0,
                        10,
                        kwargs.get("phschng", 0)
                    ),
                    Field(
                        "demden",
                        int,
                        10,
                        10,
                        kwargs.get("demden", 0)
                    ),
                    Field(
                        "icrfile",
                        int,
                        20,
                        10,
                        kwargs.get("icrfile", 0)
                    ),
                    Field(
                        "spc2bnd",
                        int,
                        30,
                        10,
                        kwargs.get("spc2bnd")
                    ),
                    Field(
                        "penout",
                        int,
                        40,
                        10,
                        kwargs.get("penout", 0)
                    ),
                    Field(
                        "shlsig",
                        int,
                        50,
                        10,
                        kwargs.get("shlsig", 0)
                    ),
                    Field(
                        "hisnout",
                        int,
                        60,
                        10,
                        kwargs.get("hisnout", 0)
                    ),
                    Field(
                        "engout",
                        int,
                        70,
                        10,
                        kwargs.get("engout", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "insf",
                        int,
                        0,
                        10,
                        kwargs.get("insf", 0)
                    ),
                    Field(
                        "isolsf",
                        int,
                        10,
                        10,
                        kwargs.get("isolsf", 0)
                    ),
                    Field(
                        "ibsf",
                        int,
                        20,
                        10,
                        kwargs.get("ibsf", 0)
                    ),
                    Field(
                        "issf",
                        int,
                        30,
                        10,
                        kwargs.get("issf", 0)
                    ),
                    Field(
                        "mlkbag",
                        int,
                        40,
                        10,
                        kwargs.get("mlkbag", 0)
                    ),
                ],
            ),
        ]

    @property
    def npopt(self) -> int:
        """Get or set the Print suppression during input phase flag for the printed output file:
        EQ.0: no suppression,
        EQ.1: nodal coordinates, element connectivities, rigid wall definitions and initial velocities are not printed.
        """ # nopep8
        return self._cards[0].get_value("npopt")

    @npopt.setter
    def npopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""npopt must be one of {0,1}""")
        self._cards[0].set_value("npopt", value)

    @property
    def neecho(self) -> int:
        """Get or set the Print suppression during input phase flag for echo file:
        EQ.0: all data printed,
        EQ.1: nodal printing is suppressed,
        EQ.2: element printing is suppressed,
        EQ.3: both node and element printing is suppressed.
        """ # nopep8
        return self._cards[0].get_value("neecho")

    @neecho.setter
    def neecho(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""neecho must be one of {0,1,2,3}""")
        self._cards[0].set_value("neecho", value)

    @property
    def nrefup(self) -> int:
        """Get or set the Flag to update reference node coordinates for beam elements.
        EQ.0: no update(default),
        EQ.1: update.
        """ # nopep8
        return self._cards[0].get_value("nrefup")

    @nrefup.setter
    def nrefup(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nrefup must be one of {0,1}""")
        self._cards[0].set_value("nrefup", value)

    @property
    def iaccop(self) -> int:
        """Get or set the Averaged accelerations from velocities in file NODOUT and the time history database file d3thdt:
        EQ.0: no average (default),
        EQ.1: averaged between output intervals.
        EQ.2: Built-in, user-defined filtering. With this option the keyword parameter DT2MS on *CONTROL_TIMESTEP must be defined. All data points between output intervals are stored and used to obtain the filtered output values. The user defined filter must be provided and linked. The procedure for handling is not yet defined.
        """ # nopep8
        return self._cards[0].get_value("iaccop")

    @iaccop.setter
    def iaccop(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""iaccop must be one of {0,1,2}""")
        self._cards[0].set_value("iaccop", value)

    @property
    def opifs(self) -> float:
        """Get or set the Output interval for interface file (Dt).
        """ # nopep8
        return self._cards[0].get_value("opifs")

    @opifs.setter
    def opifs(self, value: float) -> None:
        self._cards[0].set_value("opifs", value)

    @property
    def ipnint(self) -> int:
        """Get or set the Print initial time step sizes for all elements on the first cycle:
        EQ.0: 100 elements with the smallest time step sizes are printed.
        EQ.1: the governing time step sizes for each element are printed.
        """ # nopep8
        return self._cards[0].get_value("ipnint")

    @ipnint.setter
    def ipnint(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ipnint must be one of {0,1}""")
        self._cards[0].set_value("ipnint", value)

    @property
    def ikedit(self) -> int:
        """Get or set the Problem status report interval steps to the D3HSP (printed output)
        """ # nopep8
        return self._cards[0].get_value("ikedit")

    @ikedit.setter
    def ikedit(self, value: int) -> None:
        self._cards[0].set_value("ikedit", value)

    @property
    def iflush(self) -> int:
        """Get or set the Number of time steps interval for flushing I/O buffers (default =5000).
        """ # nopep8
        return self._cards[0].get_value("iflush")

    @iflush.setter
    def iflush(self, value: int) -> None:
        self._cards[0].set_value("iflush", value)

    @property
    def iprtf(self) -> int:
        """Get or set the Default print flag for RBDOUT and MATSUM files.
        EQ.0: write part data into both MATSUM and RBDOUT
        EQ.1: write data into RBDOUT file only
        EQ.2: write data into MATSUM file only
        EQ.3: do not write data into RBDOUT and MATSUM
        """ # nopep8
        return self._cards[1].get_value("iprtf")

    @iprtf.setter
    def iprtf(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""iprtf must be one of {0,1,2,3}""")
        self._cards[1].set_value("iprtf", value)

    @property
    def ierode(self) -> int:
        """Get or set the Output eroded internal and kinetic energy into the matsum file.  Also, (1) under the heading of part ID 0 in matsum, output the kinetic energy from nonstructural mass, lumped mass elements, and lumped inertia elements, and (2) under the heading of part ID -1in matsum, output the kinetic energy associated with distributed mass from *ELEMENT_MASS_PART..
        EQ.0: do not output extra data.
        EQ.1: output the eroded internal and kinetic energy
        """ # nopep8
        return self._cards[1].get_value("ierode")

    @ierode.setter
    def ierode(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ierode must be one of {0,1}""")
        self._cards[1].set_value("ierode", value)

    @property
    def tet10s8(self) -> int:
        """Get or set the Output ten connectivity nodes for the 10-node solid tetrahedral and the eight connectivity nodes for the 8-node shell into “d3plot” database.  The current default is set to 2 since this change in the database may make the data unreadable for many popular post-processors and older versions of LS-PrePost.  The default will change to 1 later.
        EQ.1:	write the full node connectivity into the “d3plot” database
        EQ.2:	write only the corner nodes of the elements into the “d3plot” database
        """ # nopep8
        return self._cards[1].get_value("tet10s8")

    @tet10s8.setter
    def tet10s8(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""tet10s8 must be one of {2,1}""")
        self._cards[1].set_value("tet10s8", value)

    @property
    def msgmax(self) -> int:
        """Get or set the Maximum number of each error/warning message
        """ # nopep8
        return self._cards[1].get_value("msgmax")

    @msgmax.setter
    def msgmax(self, value: int) -> None:
        self._cards[1].set_value("msgmax", value)

    @property
    def ipcurv(self) -> int:
        """Get or set the Flag to output digitized curve data to d3msg and d3hsp files.
        EQ.0: off
        EQ.1: on
        """ # nopep8
        return self._cards[1].get_value("ipcurv")

    @ipcurv.setter
    def ipcurv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ipcurv must be one of {0,1}""")
        self._cards[1].set_value("ipcurv", value)

    @property
    def gmdt(self) -> float:
        """Get or set the Output interval for recorded motions from *INTERFACE_SSI_AUX
        """ # nopep8
        return self._cards[1].get_value("gmdt")

    @gmdt.setter
    def gmdt(self, value: float) -> None:
        self._cards[1].set_value("gmdt", value)

    @property
    def ip1dblt(self) -> int:
        """Get or set the Output information of 1D (bar-type) seatbelt created for 2D (shell-type) seatbelt to sbtout.
        EQ.0: the analysis results of internally created 1D seatbelts are extracted and processed to yield the 2D belt information. The 2D belt information is stored in sbtout,
        EQ.1: the analysis results of internally created 1D retractors and sliprings are stored in sbtout. Belt load can be yielded by *DATABASE_CROSS_SECTION.This might lead to different results from that of IP1DBLT=0 in MPP, if the model it not robust.
        EQ.2:	Same as IP1DBLT = 1, but the model is decomposed in the same way of IP1DBLT = 0 in MPPand, therefore, guarantee result consistency.
        """ # nopep8
        return self._cards[1].get_value("ip1dblt")

    @ip1dblt.setter
    def ip1dblt(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ip1dblt must be one of {0,1,2}""")
        self._cards[1].set_value("ip1dblt", value)

    @property
    def eocs(self) -> int:
        """Get or set the elout Coordinate System: controls the coordinate system to be used when writing out shell data to the elout file.  EOCS has no affect on eloutdet.:
        EQ.0:	default (local element coordinate system, or if an orthotropic material model and CMPFLG=1, then material coordinate system)
        EQ.1: local element coordinate system
        EQ.2: global coordinate system
        """ # nopep8
        return self._cards[1].get_value("eocs")

    @eocs.setter
    def eocs(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""eocs must be one of {0,1,2}""")
        self._cards[1].set_value("eocs", value)

    @property
    def tolev(self) -> int:
        """Get or set the Timing Output Levels: controls the # of levels output in the timing summary at termination. The default is 2.
        """ # nopep8
        return self._cards[2].get_value("tolev")

    @tolev.setter
    def tolev(self, value: int) -> None:
        self._cards[2].set_value("tolev", value)

    @property
    def newleg(self) -> int:
        """Get or set the New Legends: controls the format of the LEGEND section of various ascii output files.
        EQ.0: use the normal format
        EQ.1: use the optional format with extra fields.
        """ # nopep8
        return self._cards[2].get_value("newleg")

    @newleg.setter
    def newleg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""newleg must be one of {0,1}""")
        self._cards[2].set_value("newleg", value)

    @property
    def frfreq(self) -> int:
        """Get or set the Output frequency for failed elemetn report, in cycles. The default is to report the summary every cycle on which an element fails. If > 1, the summary will be reported every FRFREQ cycles whether an element fails that cycle or not, provided some element has failed since the last summary report. Individual element failure is still reported as it occurs.
        """ # nopep8
        return self._cards[2].get_value("frfreq")

    @frfreq.setter
    def frfreq(self, value: int) -> None:
        self._cards[2].set_value("frfreq", value)

    @property
    def minfo(self) -> int:
        """Get or set the Output penetration information for mortar contact after each implicit step, not applicable in explicit analysis.
        EQ.0: No information
        EQ.1: Penetrations reported for each contact interface
        """ # nopep8
        return self._cards[2].get_value("minfo")

    @minfo.setter
    def minfo(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""minfo must be one of {0,1,2}""")
        self._cards[2].set_value("minfo", value)

    @property
    def solsig(self) -> int:
        """Get or set the Flag to extrapolate stresses and other history variables for multi-integration point solids from integration points to nodes.   These extrapolated nodal values replace the integration point values normally stored in d3plot.  When a nonzero SOLSIG is invoked, NINTSLD in *DATABASE_EXTENT_BINARY should be set to 8 as any other value of NINTSLD will result in only one value being reported for each element.  Supported solid formulations are: -1, -2, 2, 3, 4, 18, 16, 17, and 23.
        EQ.0:	No extrapolation.
        EQ.1:	Extrapolate the stress for linear materials only.
        EQ.2:	Extrapolate the stress if plastic strain is zero.
        EQ.3:	Extrapolate the stress always.
        EQ.4:	Extrapolate all history variables.
        """ # nopep8
        return self._cards[2].get_value("solsig")

    @solsig.setter
    def solsig(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""solsig must be one of {0,1,2,3,4}""")
        self._cards[2].set_value("solsig", value)

    @property
    def msgflg(self) -> int:
        """Get or set the Option for printing detail message rto d3msg
        EQ.0:	No detail message
        EQ.1:	Print detail message to d3msg at the termination time
        """ # nopep8
        return self._cards[2].get_value("msgflg")

    @msgflg.setter
    def msgflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""msgflg must be one of {0,1}""")
        self._cards[2].set_value("msgflg", value)

    @property
    def cdetol(self) -> float:
        """Get or set the Tolerance for output of *DEFINE_CURVE discretization warnings.  After each curve is discretized, the resulting curve is evaluated at each of the original definition points, and the values compared.  A warning will be issued for any curve where this comparison results in an error of more than CDETOL/100*M, where the curve specific value M is computed as the median of the absolute values of the non-zero curve values.
        """ # nopep8
        return self._cards[2].get_value("cdetol")

    @cdetol.setter
    def cdetol(self, value: float) -> None:
        self._cards[2].set_value("cdetol", value)

    @property
    def phschng(self) -> int:
        """Get or set the Message to messag file when materials 216, 217, and 218 change phase..
        EQ.0: (default) no message.
        EQ.1: The time and element ID are written..
        """ # nopep8
        return self._cards[3].get_value("phschng")

    @phschng.setter
    def phschng(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""phschng must be one of {0,1}""")
        self._cards[3].set_value("phschng", value)

    @property
    def demden(self) -> int:
        """Get or set the Output DEM density data to d3plot database..
        EQ.0: (default) no output.
        EQ.1: output data.
        """ # nopep8
        return self._cards[3].get_value("demden")

    @demden.setter
    def demden(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""demden must be one of {0,1}""")
        self._cards[3].set_value("demden", value)

    @property
    def icrfile(self) -> int:
        """Get or set the Flag to output node sets and element sets used in computing secforc data; see *DATABASE_CROSS_SECTION_OPTION and *DATABASE_SECFORC.  These sets are written in keyword format (*SET_...) and thus can be displayed using LS-PrePost.  The assigned set IDs are the same as the ID of the cross-section.
        EQ.0:	Do not write sets (default).
        EQ.1:	Write a separate file for each cross-section called cross_section_# where # is the cross-section ID.
        EQ.2:	Write sets for all cross-sections to a file called cross_sections
        """ # nopep8
        return self._cards[3].get_value("icrfile")

    @icrfile.setter
    def icrfile(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""icrfile must be one of {0,1,2}""")
        self._cards[3].set_value("icrfile", value)

    @property
    def spc2bnd(self) -> typing.Optional[int]:
        """Get or set the converts all constraints on MAT_RIGID (see CMO, CON1, CON2)
        to corresponding BOUNDARY_PRESCRIBED_MOTION_RIGID with a zero curve,
        which allows the reaction force associated with this constraint to be monitored in bndout
        """ # nopep8
        return self._cards[3].get_value("spc2bnd")

    @spc2bnd.setter
    def spc2bnd(self, value: int) -> None:
        self._cards[3].set_value("spc2bnd", value)

    @property
    def penout(self) -> int:
        """Get or set the Flag to output contact penetration to sleout (binout format only) and d3plot for Mortar contact.
        In sleout the maximum absolute and/or relative penetration per interface is output, in magnutide only.
        In d3plot a nodal vector field is output for absolute and/or relative penetration, respectively,
        each giving the maximum penetration (magnitude and direction) for all nodes in any sliding interface.
        See also NPEN on *DATABASE_EXTENT_INTFOR.
        EQ.0:	Do not output.
        GE.1 : Output absolute penetration.
        GE.2 : Output relative penetration.
        """ # nopep8
        return self._cards[3].get_value("penout")

    @penout.setter
    def penout(self, value: int) -> None:
        self._cards[3].set_value("penout", value)

    @property
    def shlsig(self) -> int:
        """Get or set the extrapolation/or not of stresses from integration points to the "corners" of thin shells.
        """ # nopep8
        return self._cards[3].get_value("shlsig")

    @shlsig.setter
    def shlsig(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""shlsig must be one of {0,1}""")
        self._cards[3].set_value("shlsig", value)

    @property
    def hisnout(self) -> int:
        """Get or set the Flag to invoke output of extra history variable names. Usually, the extra history variables of material models are given as just numbers. The corresponding meaning of these variables can be determined, for example, using this website: www.dynasupport.com/howtos/material/history-variables.  As an alternative, this new option allows the output of those names to some files, listed for each part separately. The number of supported material models is continuously increasing.
        EQ.0:	No output(default)
        EQ.1 : Information written to d3hsp
        EQ.2 : Information written to d3hsp and XML file hisnames.xml
        """ # nopep8
        return self._cards[3].get_value("hisnout")

    @hisnout.setter
    def hisnout(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""hisnout must be one of {0,1,2}""")
        self._cards[3].set_value("hisnout", value)

    @property
    def engout(self) -> int:
        """Get or set the Flag to output contact sliding energy densities to d3plot for Mortar contact.
        If set to 1, a nodal scalar field is output giving the minimum sliding energy density for each node in any sliding interface.
        See also NENG on *DATABASE_EXTENT_INTFOR
        """ # nopep8
        return self._cards[3].get_value("engout")

    @engout.setter
    def engout(self, value: int) -> None:
        self._cards[3].set_value("engout", value)

    @property
    def insf(self) -> int:
        """Get or set the Flag to invoke output of *SET_NODE data:
        EQ.0:	no output (default)
        EQ.1:	information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("insf")

    @insf.setter
    def insf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""insf must be one of {0,1}""")
        self._cards[4].set_value("insf", value)

    @property
    def isolsf(self) -> int:
        """Get or set the Flag to invoke output of *SET_SOLID data:
        EQ.0:	no output (default).
        EQ.1:	information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("isolsf")

    @isolsf.setter
    def isolsf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isolsf must be one of {0,1}""")
        self._cards[4].set_value("isolsf", value)

    @property
    def ibsf(self) -> int:
        """Get or set the Flag to invoke output of *SET_BEAM data:
        EQ.0:	no output (default)
        EQ.1:	information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("ibsf")

    @ibsf.setter
    def ibsf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibsf must be one of {0,1}""")
        self._cards[4].set_value("ibsf", value)

    @property
    def issf(self) -> int:
        """Get or set the Flag to invoke output of *SET_SHELL data:
        EQ.0:	no output (default)
        EQ.1:	information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("issf")

    @issf.setter
    def issf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""issf must be one of {0,1}""")
        self._cards[4].set_value("issf", value)

    @property
    def mlkbag(self) -> int:
        """Get or set the Flag to invoke output of accumulated airbag mass leakage:
        EQ.0: airbag mass leakage rate is output(default)
        EQ.1 : accumulated airbag mass leakage is output..
        """ # nopep8
        return self._cards[4].get_value("mlkbag")

    @mlkbag.setter
    def mlkbag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mlkbag must be one of {0,1}""")
        self._cards[4].set_value("mlkbag", value)

