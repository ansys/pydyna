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

"""Module providing the ControlOutput class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLOUTPUT_CARD0 = (
    FieldSchema("npopt", int, 0, 10, 0),
    FieldSchema("neecho", int, 10, 10, 0),
    FieldSchema("nrefup", int, 20, 10, 0),
    FieldSchema("iaccop", int, 30, 10, 0),
    FieldSchema("opifs", float, 40, 10, 0.0),
    FieldSchema("ipnint", int, 50, 10, 0),
    FieldSchema("ikedit", int, 60, 10, 100),
    FieldSchema("iflush", int, 70, 10, 5000),
)

_CONTROLOUTPUT_CARD1 = (
    FieldSchema("iprtf", int, 0, 10, 0),
    FieldSchema("ierode", int, 10, 10, 0),
    FieldSchema("tet10s8", int, 20, 10, 2),
    FieldSchema("msgmax", int, 30, 10, 50),
    FieldSchema("ipcurv", int, 40, 10, 0),
    FieldSchema("gmdt", float, 50, 10, 0.0),
    FieldSchema("ip1dblt", int, 60, 10, 0),
    FieldSchema("eocs", int, 70, 10, 0),
)

_CONTROLOUTPUT_CARD2 = (
    FieldSchema("tolev", int, 0, 10, 2),
    FieldSchema("newleg", int, 10, 10, 0),
    FieldSchema("frfreq", int, 20, 10, 1),
    FieldSchema("minfo", int, 30, 10, 0),
    FieldSchema("solsig", int, 40, 10, 0),
    FieldSchema("msgflg", int, 50, 10, 0),
    FieldSchema("cdetol", float, 60, 10, 10.0),
    FieldSchema("igeom", int, 70, 10, 1),
)

_CONTROLOUTPUT_CARD3 = (
    FieldSchema("phschng", int, 0, 10, 0),
    FieldSchema("demden", int, 10, 10, 0),
    FieldSchema("icrfile", int, 20, 10, 0),
    FieldSchema("spc2bnd", int, 30, 10, None),
    FieldSchema("penout", int, 40, 10, 0),
    FieldSchema("shlsig", int, 50, 10, 0),
    FieldSchema("hisnout", int, 60, 10, 0),
    FieldSchema("engout", int, 70, 10, 0),
)

_CONTROLOUTPUT_CARD4 = (
    FieldSchema("insf", int, 0, 10, 0),
    FieldSchema("isolsf", int, 10, 10, 0),
    FieldSchema("ibsf", int, 20, 10, 0),
    FieldSchema("issf", int, 30, 10, 0),
    FieldSchema("mlkbag", int, 40, 10, 0),
    FieldSchema("kineng", int, 50, 10, 0),
    FieldSchema("isfcnt", int, 60, 10, 1),
)

_CONTROLOUTPUT_CARD5 = (
    FieldSchema("ielogkey", int, 0, 10, 0),
    FieldSchema("ielogini", int, 10, 10, 0),
    FieldSchema("ielogsol", int, 20, 10, 0),
)

class ControlOutput(KeywordBase):
    """DYNA CONTROL_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "OUTPUT"

    def __init__(self, **kwargs):
        """Initialize the ControlOutput class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLOUTPUT_CARD5,
                **kwargs,
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
        """Set the npopt property."""
        if value not in [0, 1, None]:
            raise Exception("""npopt must be `None` or one of {0,1}.""")
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
        """Set the neecho property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""neecho must be `None` or one of {0,1,2,3}.""")
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
        """Set the nrefup property."""
        if value not in [0, 1, None]:
            raise Exception("""nrefup must be `None` or one of {0,1}.""")
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
        """Set the iaccop property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iaccop must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iaccop", value)

    @property
    def opifs(self) -> float:
        """Get or set the Output interval for interface file (Dt).
        """ # nopep8
        return self._cards[0].get_value("opifs")

    @opifs.setter
    def opifs(self, value: float) -> None:
        """Set the opifs property."""
        self._cards[0].set_value("opifs", value)

    @property
    def ipnint(self) -> int:
        """Get or set the Print initial time step sizes for all elements on the first cycle:
        EQ.0: 100 elements with the smallest time step sizes are printed.
        EQ.1: the governing time step sizes for each element are printed.
        GT.1: IPNINT elements with the smallest time step sizes are printed
        """ # nopep8
        return self._cards[0].get_value("ipnint")

    @ipnint.setter
    def ipnint(self, value: int) -> None:
        """Set the ipnint property."""
        self._cards[0].set_value("ipnint", value)

    @property
    def ikedit(self) -> int:
        """Get or set the Problem status report interval steps to the D3HSP (printed output)
        """ # nopep8
        return self._cards[0].get_value("ikedit")

    @ikedit.setter
    def ikedit(self, value: int) -> None:
        """Set the ikedit property."""
        self._cards[0].set_value("ikedit", value)

    @property
    def iflush(self) -> int:
        """Get or set the Number of time steps interval for flushing I/O buffers (default =5000).
        """ # nopep8
        return self._cards[0].get_value("iflush")

    @iflush.setter
    def iflush(self, value: int) -> None:
        """Set the iflush property."""
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
        """Set the iprtf property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iprtf must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("iprtf", value)

    @property
    def ierode(self) -> int:
        """Get or set the Output the eroded internal and kinetic energies into the matsum file.
        Also, (1) under the heading of part ID 0 in matsum, output the kinetic energy from nonstructural mass, lumped mass elements, and lumped inertia elements; and
        (2) under the heading of part ID -1in matsum, output the kinetic energy associated with distributed mass from *ELEMENT_MASS_PART. See Remark 3.
        EQ.0: Do not output extra data.
        EQ.1: Output the eroded internal and kinetic energies and other additional kinetic energies.
        """ # nopep8
        return self._cards[1].get_value("ierode")

    @ierode.setter
    def ierode(self, value: int) -> None:
        """Set the ierode property."""
        if value not in [0, 1, None]:
            raise Exception("""ierode must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ierode", value)

    @property
    def tet10s8(self) -> int:
        """Get or set the Output ten connectivity nodes for the 10-node solid tetrahedral (solid formulations 16/17) and the eight connectivity nodes for the 8-node
        shell (shell formulation 23) into the d3plot, d3part, d3eigv, and d3mode databases. The current default is set to 2 since this
        change in the databases may make the data unreadable for many popular post-processors and older versions of LS-PrePost.
        EQ.1: Write the full node connectivity into the databases
        EQ.2: Write only the corner nodes of the elements into the databases

        """ # nopep8
        return self._cards[1].get_value("tet10s8")

    @tet10s8.setter
    def tet10s8(self, value: int) -> None:
        """Set the tet10s8 property."""
        if value not in [2, 1, None]:
            raise Exception("""tet10s8 must be `None` or one of {2,1}.""")
        self._cards[1].set_value("tet10s8", value)

    @property
    def msgmax(self) -> int:
        """Get or set the Maximum number of each error/warning message
        """ # nopep8
        return self._cards[1].get_value("msgmax")

    @msgmax.setter
    def msgmax(self, value: int) -> None:
        """Set the msgmax property."""
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
        """Set the ipcurv property."""
        if value not in [0, 1, None]:
            raise Exception("""ipcurv must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ipcurv", value)

    @property
    def gmdt(self) -> float:
        """Get or set the Output interval for recorded motions from *INTERFACE_SSI_AUX
        """ # nopep8
        return self._cards[1].get_value("gmdt")

    @gmdt.setter
    def gmdt(self, value: float) -> None:
        """Set the gmdt property."""
        self._cards[1].set_value("gmdt", value)

    @property
    def ip1dblt(self) -> int:
        """Get or set the Output information of 1D (bar-type) seatbelt created for 2D (shell-type) seatbelt to sbtout.
        EQ.0: the analysis results of internally created 1D seatbelts are extracted and processed to yield the 2D belt information. The 2D belt information is stored in sbtout,
        EQ.1: the analysis results of internally created 1D retractors and sliprings are stored in sbtout. Belt load can be yielded by *DATABASE_CROSS_SECTION.This might lead to different results from that of IP1DBLT=0 in MPP, if the model it not robust.
        EQ.2: Same as IP1DBLT = 1, but the model is decomposed in the same way as IP1DBLT = 0 in MPP, which guarantee result consistency.
        """ # nopep8
        return self._cards[1].get_value("ip1dblt")

    @ip1dblt.setter
    def ip1dblt(self, value: int) -> None:
        """Set the ip1dblt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ip1dblt must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("ip1dblt", value)

    @property
    def eocs(self) -> int:
        """Get or set the elout Coordinate System: controls the coordinate system to be used when writing out shell data to the elout file.  EOCS has no affect on eloutdet.EOCS has no effect on elout if OPTION2 in *DATABASE_ELOUT is greater than zero:
        EQ.0: default (local element coordinate system, or if an orthotropic material model and CMPFLG=1, then material coordinate system)
        EQ.1: local element coordinate system
        EQ.2: global coordinate system
        """ # nopep8
        return self._cards[1].get_value("eocs")

    @eocs.setter
    def eocs(self, value: int) -> None:
        """Set the eocs property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""eocs must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("eocs", value)

    @property
    def tolev(self) -> int:
        """Get or set the Timing Output Levels: controls the # of levels output in the timing summary at termination. The default is 2.
        """ # nopep8
        return self._cards[2].get_value("tolev")

    @tolev.setter
    def tolev(self, value: int) -> None:
        """Set the tolev property."""
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
        """Set the newleg property."""
        if value not in [0, 1, None]:
            raise Exception("""newleg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("newleg", value)

    @property
    def frfreq(self) -> int:
        """Get or set the Output frequency for failed elemetn report, in cycles. The default is to report the summary every cycle on which an element fails. If > 1, the summary is reported every FRFREQ cycles whether an element fails that cycle or not, provided some element has failed since the last summary report. Individual element failure is still reported as it occurs.
        """ # nopep8
        return self._cards[2].get_value("frfreq")

    @frfreq.setter
    def frfreq(self, value: int) -> None:
        """Set the frfreq property."""
        self._cards[2].set_value("frfreq", value)

    @property
    def minfo(self) -> int:
        """Get or set the Output penetration and memory information for Mortar contact after each implicit step. Reporting this information is also available for explicit analysis when MINFO is a negative number.
        LT.0: Report for each contact interface at the time frequency | MINFO | .This is available for explicitand implicit analysis.
        EQ.0: No information reported.
        EQ.1: Report for each contact interface at each implicit time step.No information is output for explicit analysis.
        """ # nopep8
        return self._cards[2].get_value("minfo")

    @minfo.setter
    def minfo(self, value: int) -> None:
        """Set the minfo property."""
        self._cards[2].set_value("minfo", value)

    @property
    def solsig(self) -> int:
        """Get or set the Flag to extrapolate stresses and other history variables for multi-integration point solids from integration points to nodes.   These extrapolated nodal values replace the integration point values normally stored in d3plot.  When a nonzero SOLSIG is invoked, NINTSLD in *DATABASE_EXTENT_BINARY should be set to 8 as any other value of NINTSLD will result in only one value being reported for each element.  Supported solid formulations are: -1, -2, 2, 3, 4, 18, 16, 17, and 23.
        EQ.0: No extrapolation.
        EQ.1: Extrapolate the stress for linear materials only.
        EQ.2: Extrapolate the stress if plastic strain is zero.
        EQ.3: Extrapolate the stress always.
        EQ.4: Extrapolate all history variables.
        """ # nopep8
        return self._cards[2].get_value("solsig")

    @solsig.setter
    def solsig(self, value: int) -> None:
        """Set the solsig property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""solsig must be `None` or one of {0,1,2,3,4}.""")
        self._cards[2].set_value("solsig", value)

    @property
    def msgflg(self) -> int:
        """Get or set the Flag for writing detailed error/warning message to d3msg.  MSGFLG has no effect on output of standard length error/warning messages; such messages are written to messag or mes****.  NOTE: Most errors/warnings offer only standard length messages.  Only a few also offer optional, detailed messages.
        EQ.0: Do not write detailed messages to d3msg.
        EQ.1: Write detailed messages to d3msg at the conclusion of the run.Each detailed message is written only once even in cases where the associated error or warning occurs multiple times.A detailed message written to d3msg should only be used to help interpret the standard - length message better.The information in d3msg could contain fictitious IDsID's and names.
        """ # nopep8
        return self._cards[2].get_value("msgflg")

    @msgflg.setter
    def msgflg(self, value: int) -> None:
        """Set the msgflg property."""
        if value not in [0, 1, None]:
            raise Exception("""msgflg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("msgflg", value)

    @property
    def cdetol(self) -> float:
        """Get or set the Tolerance for output of *DEFINE_CURVE discretization warnings.  After each curve is discretized, the resulting curve is evaluated at each of the original definition points, and the values compared.  A warning will be issued for any curve where this comparison results in an error of more than CDETOL/100*M, where the curve specific value M is computed as the median of the absolute values of the non-zero curve values.
        """ # nopep8
        return self._cards[2].get_value("cdetol")

    @cdetol.setter
    def cdetol(self, value: float) -> None:
        """Set the cdetol property."""
        self._cards[2].set_value("cdetol", value)

    @property
    def igeom(self) -> int:
        """Get or set the Flag to control whether nodal coordinates or displacements for the nodes in the mesh are output to d3plot, d3part, and d3drlf:
        EQ.1: Nodal coordinates(default)
        EQ.2: Displacements.IGEOM = 2 is useful when the precision of the binary files is insufficient for accurately calculating the displacements from the coordinates in post - processing.This problem arises when the displacements are very small relative to the coordinate values.Note that IGEOM = 2 is supported for post - processing in LS - PrePost 4.10 and later.
        """ # nopep8
        return self._cards[2].get_value("igeom")

    @igeom.setter
    def igeom(self, value: int) -> None:
        """Set the igeom property."""
        if value not in [1, 2, None]:
            raise Exception("""igeom must be `None` or one of {1,2}.""")
        self._cards[2].set_value("igeom", value)

    @property
    def phschng(self) -> int:
        """Get or set the Message to messag file when materials 216, 217, and 218 change phase..
        EQ.0: (default) no message.
        EQ.1: The time and element ID are written..
        """ # nopep8
        return self._cards[3].get_value("phschng")

    @phschng.setter
    def phschng(self, value: int) -> None:
        """Set the phschng property."""
        if value not in [0, 1, None]:
            raise Exception("""phschng must be `None` or one of {0,1}.""")
        self._cards[3].set_value("phschng", value)

    @property
    def demden(self) -> int:
        """Get or set the Output DEM density data to d3plot database..
        EQ.0: (default) no output.
        EQ.1: output data.
        EQ.2: Output data with modification of the boundary density calculation to avoid the low-density distribution near the DES domain boundary.
        """ # nopep8
        return self._cards[3].get_value("demden")

    @demden.setter
    def demden(self, value: int) -> None:
        """Set the demden property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""demden must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("demden", value)

    @property
    def icrfile(self) -> int:
        """Get or set the Flag to output node sets and element sets used in computing secforc data; see *DATABASE_CROSS_SECTION_OPTION and *DATABASE_SECFORC.  These sets are written in keyword format (*SET_...) and thus can be displayed using LS-PrePost.  The assigned set IDs are the same as the ID of the cross-section.
        EQ.0: Do not write sets (default).
        EQ.1: Write a separate file for each cross-section called cross_section_# where # is the cross-section ID.
        EQ.2: Write sets for all cross-sections to a file called cross_sections
        """ # nopep8
        return self._cards[3].get_value("icrfile")

    @icrfile.setter
    def icrfile(self, value: int) -> None:
        """Set the icrfile property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""icrfile must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("icrfile", value)

    @property
    def spc2bnd(self) -> typing.Optional[int]:
        """Get or set the converts all constraints on MAT_RIGID (see CMO, CON1, CON2)
        to corresponding BOUNDARY_PRESCRIBED_MOTION_RIGID with a zero curve,
        which allows the reaction force associated with this constraint to be monitored in bndout
        EQ.0: Not active
        EQ.1: Active
        """ # nopep8
        return self._cards[3].get_value("spc2bnd")

    @spc2bnd.setter
    def spc2bnd(self, value: int) -> None:
        """Set the spc2bnd property."""
        self._cards[3].set_value("spc2bnd", value)

    @property
    def penout(self) -> int:
        """Get or set the Flag to output contact penetration to sleout (binout format only) and d3plot for Mortar contact. In sleout the maximum absolute and/or relative penetration per interface is output, in magnutide only. In d3plot a nodal vector field is output for absolute and/or relative penetration, respectively, each giving the maximum penetration (magnitude and direction) for all nodes in any sliding interface. See also NPEN on *DATABASE_EXTENT_INTFOR.
        EQ.0: Do not output.
        EQ.1: Output absolute penetration.
        EQ.2: Output relative penetration.
        """ # nopep8
        return self._cards[3].get_value("penout")

    @penout.setter
    def penout(self, value: int) -> None:
        """Set the penout property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""penout must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("penout", value)

    @property
    def shlsig(self) -> int:
        """Get or set the Flag to extrapolate stresses for shells with 8 integration points to nodes.   These extrapolated nodal values replace the integration point values normally stored in d3plot.  When a nonzero SHLSIG is invoked, MAXINT in *DATABASE_EXTENT_BINARY should be set to -2 to indicate 4 in-plane integration points and 2 in the thickness direction.  Supported shell formulations are: 16, 20, and 21.
        EQ.0: No extrapolation.
        EQ.1: Extrapolate the stress for linear materials only.
        EQ.2: Extrapolate the stress if plastic strain is zero.
        EQ.3: Extrapolate the stress always.
        EQ.4: Extrapolate all history variables.
        """ # nopep8
        return self._cards[3].get_value("shlsig")

    @shlsig.setter
    def shlsig(self, value: int) -> None:
        """Set the shlsig property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""shlsig must be `None` or one of {0,1,2,3,4}.""")
        self._cards[3].set_value("shlsig", value)

    @property
    def hisnout(self) -> int:
        """Get or set the Flag to invoke output of extra history variable names. Usually, the extra history variables of material models are given as just numbers. The corresponding meaning of these variables can be determined, for example, using this website: https://lsdyna.ansys.com/history-variables-for-certain-material-models/.
        As an alternative, this option allows the output of the history variable names, listed for each part separately, to d3hsp. In addition, XML files that can be read by a postprocessor can be output. Currently, two XML files are available:
        *hisnames.xml (read by LS - PrePost and GS Animator), which lists the history variable names in each part(as with d3hsp)
        *d3labels.xml (read by LS - PrePost), which is similar to hisnames.xml but contains additional information regarding history variable names for each element and integration point when necessary (such as in the case of composites).
        The number of supported material models is continuously increasing.
        EQ.0: No output (default)
        EQ.1: Information written to d3hsp
        EQ.2: Information written to d3hsp and XML file hisnames.xml
        EQ.3: Information written to d3hsp and XML files hisnames.xml and d3labels.xml
        """ # nopep8
        return self._cards[3].get_value("hisnout")

    @hisnout.setter
    def hisnout(self, value: int) -> None:
        """Set the hisnout property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""hisnout must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("hisnout", value)

    @property
    def engout(self) -> int:
        """Get or set the Flag to output contact sliding energy densities to d3plot for Mortar contact. If set to 1, a nodal scalar field is output giving the minimum sliding energy density for each node in any sliding interface. See also NENG on *DATABASE_EXTENT_INTFOR.
        """ # nopep8
        return self._cards[3].get_value("engout")

    @engout.setter
    def engout(self, value: int) -> None:
        """Set the engout property."""
        self._cards[3].set_value("engout", value)

    @property
    def insf(self) -> int:
        """Get or set the Flag to invoke output of *SET_NODE data:
        EQ.0: no output (default)
        EQ.1: information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("insf")

    @insf.setter
    def insf(self, value: int) -> None:
        """Set the insf property."""
        if value not in [0, 1, None]:
            raise Exception("""insf must be `None` or one of {0,1}.""")
        self._cards[4].set_value("insf", value)

    @property
    def isolsf(self) -> int:
        """Get or set the Flag to invoke output of *SET_SOLID data:
        EQ.0: no output (default).
        EQ.1: information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("isolsf")

    @isolsf.setter
    def isolsf(self, value: int) -> None:
        """Set the isolsf property."""
        if value not in [0, 1, None]:
            raise Exception("""isolsf must be `None` or one of {0,1}.""")
        self._cards[4].set_value("isolsf", value)

    @property
    def ibsf(self) -> int:
        """Get or set the Flag to invoke output of *SET_BEAM data:
        EQ.0: no output (default)
        EQ.1: information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("ibsf")

    @ibsf.setter
    def ibsf(self, value: int) -> None:
        """Set the ibsf property."""
        if value not in [0, 1, None]:
            raise Exception("""ibsf must be `None` or one of {0,1}.""")
        self._cards[4].set_value("ibsf", value)

    @property
    def issf(self) -> int:
        """Get or set the Flag to invoke output of *SET_SHELL data:
        EQ.0: no output (default)
        EQ.1: information written to file. See Remark 1.
        """ # nopep8
        return self._cards[4].get_value("issf")

    @issf.setter
    def issf(self, value: int) -> None:
        """Set the issf property."""
        if value not in [0, 1, None]:
            raise Exception("""issf must be `None` or one of {0,1}.""")
        self._cards[4].set_value("issf", value)

    @property
    def mlkbag(self) -> int:
        """Get or set the Flag to invoke output of accumulated airbag mass leakage:
        EQ.0: airbag mass leakage rate is output(default)
        EQ.1: accumulated airbag mass leakage is output..
        """ # nopep8
        return self._cards[4].get_value("mlkbag")

    @mlkbag.setter
    def mlkbag(self, value: int) -> None:
        """Set the mlkbag property."""
        if value not in [0, 1, None]:
            raise Exception("""mlkbag must be `None` or one of {0,1}.""")
        self._cards[4].set_value("mlkbag", value)

    @property
    def kineng(self) -> int:
        """Get or set the Flag to output kinetic energy density as a nodal field:
        EQ.0: Do not output(default).
        EQ.1: Output, will appear as kinetic energy density as a nodal field in the d3plotand d3eigv databases
        """ # nopep8
        return self._cards[4].get_value("kineng")

    @kineng.setter
    def kineng(self, value: int) -> None:
        """Set the kineng property."""
        if value not in [0, 1, None]:
            raise Exception("""kineng must be `None` or one of {0,1}.""")
        self._cards[4].set_value("kineng", value)

    @property
    def isfcnt(self) -> int:
        """Get or set the Continuity level in applying Interface Linking Data::
        LE.1: Continuity in displacements. Velocities and accelerations excluded.
        EQ.2: Continuity in displacements and velocities.Accelerations excluded.
        EQ.3: Continuity in displacements, velocities,and accelerations.This option may result in a smoother response overall.
        """ # nopep8
        return self._cards[4].get_value("isfcnt")

    @isfcnt.setter
    def isfcnt(self, value: int) -> None:
        """Set the isfcnt property."""
        self._cards[4].set_value("isfcnt", value)

    @property
    def ielogkey(self) -> int:
        """Get or set the Flag to invoke diagnostic output to a consolidated error file for MPP called error.log during the keyword input phase (see Remark 2):
        EQ.0: No output during this phase(default)
        EQ.1: Output errors only
        EQ.2: Output errors and warnings
        EQ.3: Output errors, warnings,and informational output
        """ # nopep8
        return self._cards[5].get_value("ielogkey")

    @ielogkey.setter
    def ielogkey(self, value: int) -> None:
        """Set the ielogkey property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ielogkey must be `None` or one of {0,1,2,3}.""")
        self._cards[5].set_value("ielogkey", value)

    @property
    def ielogini(self) -> int:
        """Get or set the Flag to invoke diagnostic output to a consolidated error file for MPP called error.log during the initialization phase (see Remark 2):
        EQ.0: No output during this phase(default)
        EQ.1: Output errors only
        EQ.2: Output errors and warnings
        EQ.3: Output errors, warnings,and informational output
        """ # nopep8
        return self._cards[5].get_value("ielogini")

    @ielogini.setter
    def ielogini(self, value: int) -> None:
        """Set the ielogini property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ielogini must be `None` or one of {0,1,2,3}.""")
        self._cards[5].set_value("ielogini", value)

    @property
    def ielogsol(self) -> int:
        """Get or set the Flag to invoke diagnostic output to a consolidated error file for MPP called error.log during the solution phase (see Remark 2):
        EQ.0: No output during this phase(default)
        EQ.1: Output errors only
        EQ.2: Output errors and warnings
        EQ.3: Output errors, warnings,and informational output
        """ # nopep8
        return self._cards[5].get_value("ielogsol")

    @ielogsol.setter
    def ielogsol(self, value: int) -> None:
        """Set the ielogsol property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ielogsol must be `None` or one of {0,1,2,3}.""")
        self._cards[5].set_value("ielogsol", value)

