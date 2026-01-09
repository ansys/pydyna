# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""
Keywords Backend
================

Module providing a local keywords-based backend for the pre module.
This backend uses the Deck class to build keyword files locally without
requiring a gRPC server connection.
"""

from dataclasses import dataclass, field
import logging
import os
from typing import Any, Dict, List, Optional

from ansys.dyna.core.lib.deck import Deck

logger = logging.getLogger(__name__)


@dataclass
class IdGenerator:
    """Simple ID generator for keywords that need unique IDs."""

    _counters: Dict[str, int] = field(default_factory=dict)

    def next_id(self, category: str = "default") -> int:
        """Get the next available ID for a category.

        Parameters
        ----------
        category : str
            The category of ID (e.g., "curve", "part", "material").

        Returns
        -------
        int
            The next available ID.
        """
        if category not in self._counters:
            self._counters[category] = 0
        self._counters[category] += 1
        return self._counters[category]

    def reset(self, category: Optional[str] = None) -> None:
        """Reset ID counters.

        Parameters
        ----------
        category : str, optional
            The category to reset. If None, resets all categories.
        """
        if category is None:
            self._counters.clear()
        elif category in self._counters:
            del self._counters[category]


class KeywordsBackend:
    """Keywords-based backend for the pre module.

    This class provides a local implementation of the pre module functionality
    using the keywords Deck class instead of gRPC calls.
    """

    def __init__(self):
        """Initialize the keywords backend."""
        logger.debug("Initializing KeywordsBackend")
        self._deck = Deck()
        self._id_generator = IdGenerator()
        self._working_dir = os.getcwd()
        self._main_filename = "output.k"
        self._loaded_files: List[str] = []

        # Termination settings
        self._termination_time = 0.0

        # Database settings
        self._db_binary_settings: Dict[str, Any] = {}
        self._db_ascii_settings: List[Dict[str, Any]] = []

    @property
    def deck(self) -> Deck:
        """Get the underlying Deck instance."""
        return self._deck

    def reset(self) -> None:
        """Reset the backend to initial state."""
        logger.debug("Resetting KeywordsBackend")
        self._deck = Deck()
        self._id_generator.reset()
        self._loaded_files.clear()
        self._termination_time = 0.0
        self._db_binary_settings.clear()
        self._db_ascii_settings.clear()

    def set_working_dir(self, path: str) -> None:
        """Set the working directory.

        Parameters
        ----------
        path : str
            The working directory path.
        """
        self._working_dir = path
        logger.debug(f"Working directory set to: {path}")

    def get_working_dir(self) -> str:
        """Get the current working directory.

        Returns
        -------
        str
            The working directory path.
        """
        return self._working_dir

    def load_file(self, filepath: str) -> bool:
        """Load a keyword file into the deck.

        Parameters
        ----------
        filepath : str
            Path to the keyword file.

        Returns
        -------
        bool
            True if successful, False otherwise.
        """
        try:
            logger.info(f"Loading file: {filepath}")
            self._deck.import_file(filepath)
            self._loaded_files.append(filepath)
            return True
        except Exception as e:
            logger.error(f"Failed to load file {filepath}: {e}")
            return False

    def save_file(self, filename: Optional[str] = None) -> str:
        """Save the deck to a keyword file.

        Parameters
        ----------
        filename : str, optional
            The output filename. If None, uses the main filename.

        Returns
        -------
        str
            The path where the file was saved.
        """
        if filename is None or filename == "":
            filename = self._main_filename

        # Ensure we have a valid filename
        if not filename:
            filename = "output.k"

        output_path = os.path.join(self._working_dir, filename)
        logger.info(f"Saving deck to: {output_path}")

        # Ensure the working directory exists
        os.makedirs(self._working_dir, exist_ok=True)

        self._deck.export_file(output_path)
        return self._working_dir

    def set_main_filename(self, filename: str) -> None:
        """Set the main output filename.

        Parameters
        ----------
        filename : str
            The main filename.
        """
        self._main_filename = filename
        logger.debug(f"Main filename set to: {filename}")

    def get_main_filename(self) -> str:
        """Get the main output filename.

        Returns
        -------
        str
            The main filename.
        """
        return self._main_filename

    def next_id(self, category: str = "default") -> int:
        """Get the next available ID for a category.

        Parameters
        ----------
        category : str
            The category of ID.

        Returns
        -------
        int
            The next available ID.
        """
        return self._id_generator.next_id(category)

    # =========================================================================
    # Keyword Creation Methods
    # =========================================================================

    def create_termination(self, endtim: float) -> bool:
        """Create a CONTROL_TERMINATION keyword.

        Parameters
        ----------
        endtim : float
            Termination time.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        self._termination_time = endtim

        kw = keywords.ControlTermination()
        kw.endtim = endtim

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_TERMINATION with endtim={endtim}")
        return True

    def create_database_binary(
        self,
        filetype: str = "D3PLOT",
        dt: float = 0,
        maxint: int = 3,
        ieverp: int = 0,
        dcomp: int = 1,
        nintsld: int = 1,
    ) -> bool:
        """Create a DATABASE_BINARY keyword.

        Parameters
        ----------
        filetype : str
            Type of file (e.g., "D3PLOT").
        dt : float
            Time interval between outputs.
        maxint : int
            Number of integration points.
        ieverp : int
            Output state flag.
        dcomp : int
            Data compression flag.
        nintsld : int
            Number of solid integration points.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        # Store settings
        self._db_binary_settings = {
            "filetype": filetype,
            "dt": dt,
            "maxint": maxint,
            "ieverp": ieverp,
            "dcomp": dcomp,
            "nintsld": nintsld,
        }

        # Create DATABASE_BINARY_D3PLOT
        kw = keywords.DatabaseBinaryD3Plot()
        kw.dt = dt

        self._deck.append(kw)
        logger.debug(f"Created DATABASE_BINARY_{filetype} with dt={dt}")

        # Create DATABASE_EXTENT_BINARY
        extent_kw = keywords.DatabaseExtentBinary()
        extent_kw.maxint = maxint
        extent_kw.ieverp = ieverp
        extent_kw.dcomp = dcomp
        extent_kw.nintsld = nintsld
        # Match pre server defaults
        extent_kw.ialemat = 0
        extent_kw.sclp = float("nan")  # Leave unset like pre server

        self._deck.append(extent_kw)
        logger.debug("Created DATABASE_EXTENT_BINARY")
        return True

    def create_database_ascii(
        self,
        db_type: str,
        dt: float = 0.0,
        binary: int = 1,
        lcur: int = 0,
        ioopt: int = 1,
    ) -> bool:
        """Create a DATABASE_ASCII keyword.

        Parameters
        ----------
        db_type : str
            Type of database (e.g., "GLSTAT", "MATSUM").
        dt : float
            Time interval between outputs.
        binary : int
            Binary output flag.
        lcur : int
            Curve ID for time interval.
        ioopt : int
            Output frequency flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        self._db_ascii_settings.append({"type": db_type, "dt": dt, "binary": binary, "lcur": lcur, "ioopt": ioopt})

        # Map database types to keyword classes
        db_type_map = {
            "GLSTAT": keywords.DatabaseGlstat,
            "MATSUM": keywords.DatabaseMatsum,
            "NODFOR": keywords.DatabaseNodfor,
            "RCFORC": keywords.DatabaseRcforc,
            "SLEOUT": keywords.DatabaseSleout,
            "BNDOUT": keywords.DatabaseBndout,
            "ELOUT": keywords.DatabaseElout,
            "NODOUT": keywords.DatabaseNodout,
            "RBDOUT": keywords.DatabaseRbdout,
            "SECFORC": keywords.DatabaseSecforc,
            "RWFORC": keywords.DatabaseRwforc,
            "ABSTAT": keywords.DatabaseAbstat,
        }

        if db_type.upper() in db_type_map:
            kw_class = db_type_map[db_type.upper()]
            kw = kw_class()
            kw.dt = dt
            kw.binary = binary
            kw.lcur = lcur
            kw.ioopt = ioopt

            self._deck.append(kw)
            logger.debug(f"Created DATABASE_{db_type.upper()} with dt={dt}")
            return True
        else:
            logger.warning(f"Unknown database type: {db_type}")
            return False

    def create_define_curve(
        self,
        sfo: float = 1.0,
        abscissa: List[float] = None,
        ordinate: List[float] = None,
        title: str = "",
    ) -> int:
        """Create a DEFINE_CURVE keyword.

        Parameters
        ----------
        sfo : float
            Scale factor for ordinate values.
        abscissa : List[float]
            X values (time/independent variable).
        ordinate : List[float]
            Y values (dependent variable).
        title : str
            Curve title.

        Returns
        -------
        int
            The curve ID.
        """
        from ansys.dyna.core.keywords import keywords

        if abscissa is None:
            abscissa = []
        if ordinate is None:
            ordinate = []

        curve_id = self.next_id("curve")

        kw = keywords.DefineCurve()
        kw.lcid = curve_id
        kw.sfo = sfo
        kw.title = title

        # Add curve points
        for a, o in zip(abscissa, ordinate):
            kw.curves.append([a, o])

        self._deck.append(kw)
        logger.debug(f"Created DEFINE_CURVE with id={curve_id}")
        return curve_id

    def create_control_accuracy(
        self,
        osu: int = 0,
        inn: int = 0,
        pidosu: int = 0,
        iacc: int = 0,
        exacc: float = 0.0,
    ) -> bool:
        """Create a CONTROL_ACCURACY keyword.

        Parameters
        ----------
        osu : int
            Objective stress update flag.
        inn : int
            Invariant node numbering flag.
        pidosu : int
            Part set ID for objective stress.
        iacc : int
            Implicit accuracy flag.
        exacc : float
            Explicit accuracy flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlAccuracy()
        kw.osu = osu
        kw.inn = inn
        kw.pidosu = pidosu
        kw.iacc = iacc
        kw.exacc = exacc

        self._deck.append(kw)
        logger.debug("Created CONTROL_ACCURACY")
        return True

    def create_control_energy(
        self,
        hgen: int = 1,
        rwen: int = 2,
        slnten: int = 1,
        rylen: int = 1,
        irgen: int = 2,
    ) -> bool:
        """Create a CONTROL_ENERGY keyword.

        Parameters
        ----------
        hgen : int
            Hourglass energy flag.
        rwen : int
            Rigidwall energy flag.
        slnten : int
            Sliding interface energy flag.
        rylen : int
            Rayleigh energy flag.
        irgen : int
            Initial geometry energy flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlEnergy()
        kw.hgen = hgen
        kw.rwen = rwen
        kw.slnten = slnten
        kw.rylen = rylen
        kw.irgen = irgen

        self._deck.append(kw)
        logger.debug("Created CONTROL_ENERGY")
        return True

    def create_control_hourglass(self, ihq: int = 1, qh: float = 0.1) -> bool:
        """Create a CONTROL_HOURGLASS keyword.

        Parameters
        ----------
        ihq : int
            Hourglass control type.
        qh : float
            Hourglass coefficient.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlHourglass()
        kw.ihq = ihq
        kw.qh = qh

        self._deck.append(kw)
        logger.debug("Created CONTROL_HOURGLASS")
        return True

    def create_control_bulk_viscosity(self, q1: float = 1.5, q2: float = 0.06, bulk_type: int = 1) -> bool:
        """Create a CONTROL_BULK_VISCOSITY keyword.

        Parameters
        ----------
        q1 : float
            Quadratic viscosity coefficient.
        q2 : float
            Linear viscosity coefficient.
        bulk_type : int
            Bulk viscosity type.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlBulkViscosity()
        kw.q1 = q1
        kw.q2 = q2
        kw.type = bulk_type

        self._deck.append(kw)
        logger.debug("Created CONTROL_BULK_VISCOSITY")
        return True

    def create_control_timestep(
        self,
        tssfac: float = 0.9,
        isdo: int = 0,
        dt2ms: float = 0.0,
        lctm: int = 0,
    ) -> bool:
        """Create a CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        tssfac : float
            Scale factor for computed time step.
        isdo : int
            Basis of time size calculation.
        dt2ms : float
            Time step size for mass scaled solutions.
        lctm : int
            Curve ID limiting max time step.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlTimestep()
        kw.tssfac = tssfac
        kw.isdo = isdo
        kw.dt2ms = dt2ms
        kw.lctm = lctm

        self._deck.append(kw)
        logger.debug("Created CONTROL_TIMESTEP")
        return True

    def create_control_solution(self, soln: int = 0) -> bool:
        """Create a CONTROL_SOLUTION keyword.

        Parameters
        ----------
        soln : int
            Solution type flag:
            - 0: Structural only (default)
            - 1: Thermal only
            - 2: Coupled structural-thermal

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlSolution()
        kw.soln = soln

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_SOLUTION with soln={soln}")
        return True

    def create_control_thermal_solver(self, atype: int = 0) -> bool:
        """Create a CONTROL_THERMAL_SOLVER keyword.

        Parameters
        ----------
        atype : int
            Analysis type:
            - 0: Steady state (default)
            - 1: Transient

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlThermalSolver()
        kw.atype = atype

        self._deck.append(kw)
        logger.debug(f"Created CONTROL_THERMAL_SOLVER with atype={atype}")
        return True

    def create_control_thermal_timestep(
        self,
        ts: int = 0,
        tip: float = 1.0,
        its: float = 0.0,
        tmin: float = 0.0,
        tmax: float = 0.0,
        dtemp: float = 0.0,
        tscp: float = 0.0,
        lcts: int = 0,
    ) -> bool:
        """Create a CONTROL_THERMAL_TIMESTEP keyword.

        Parameters
        ----------
        ts : int
            Time step control.
        tip : float
            Thermal integration parameter.
        its : float
            Initial thermal time step.
        tmin : float
            Minimum thermal time step.
        tmax : float
            Maximum thermal time step.
        dtemp : float
            Maximum temperature change per step.
        tscp : float
            Scale factor for thermal time step.
        lcts : int
            Curve ID for time step control.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlThermalTimestep()
        kw.ts = ts
        kw.tip = tip
        kw.its = its
        kw.tmin = tmin
        kw.tmax = tmax
        kw.dtemp = dtemp
        kw.tscp = tscp
        kw.lcts = lcts

        self._deck.append(kw)
        logger.debug("Created CONTROL_THERMAL_TIMESTEP")
        return True

    def create_control_shell(
        self,
        wrpang: float = 20.0,
        esort: int = 0,
        irnxx: int = -1,
        istupd: int = 0,
        theory: int = 2,
        bwc: int = 2,
        miter: int = 1,
        proj: int = 0,
        irquad: int = 0,
    ) -> bool:
        """Create a CONTROL_SHELL keyword.

        Parameters
        ----------
        wrpang : float
            Shell element warpage angle in degrees.
        esort : int
            Sorting of triangular shell elements.
        irnxx : int
            Shell normal update option.
        istupd : int
            Shell thickness change option.
        theory : int
            Default shell formulation.
        bwc : int
            Warping stiffness for Belytschko-Tsay shells.
        miter : int
            Plane stress plasticity option.
        proj : int
            Projection method for warping stiffness.
        irquad : int
            In-plane integration rule for eight-node quadratic shell.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlShell()
        kw.wrpang = wrpang
        kw.esort = esort
        kw.irnxx = irnxx
        kw.istupd = istupd
        # theory=0 means use default, which the keyword class doesn't accept
        # so we only set it if non-zero
        if theory != 0:
            kw.theory = theory
        kw.bwc = bwc
        kw.miter = miter
        kw.proj = proj
        # irquad=0 means use default, which the keyword class doesn't accept
        if irquad != 0:
            kw.irquad = irquad

        self._deck.append(kw)
        logger.debug("Created CONTROL_SHELL")
        return True

    def create_control_contact(
        self,
        rwpnal: float = 0.0,
        shlthk: int = 0,
        orien: int = 1,
        ssthk: int = 0,
        ignore: int = 0,
        igactc: int = 0,
    ) -> bool:
        """Create a CONTROL_CONTACT keyword.

        Parameters
        ----------
        rwpnal : float
            Scale factor for rigid wall penalties.
        shlthk : int
            Shell thickness offset flag.
        orien : int
            Automatic reorientation flag.
        ssthk : int
            Default contact thickness flag.
        ignore : int
            Ignore initial penetrations flag.
        igactc : int
            Isogeometric shells for contact flag.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.ControlContact()
        kw.rwpnal = rwpnal
        kw.shlthk = shlthk
        kw.orien = orien
        kw.ssthk = ssthk
        kw.ignore = ignore
        kw.igactc = igactc

        self._deck.append(kw)
        logger.debug("Created CONTROL_CONTACT")
        return True

    def create_initial_temperature(
        self,
        option: str = "SET",
        nsid: int = 0,
        temp: float = 0.0,
    ) -> bool:
        """Create an INITIAL_TEMPERATURE keyword.

        Parameters
        ----------
        option : str
            Option: "SET" for nodal set or "NODE" for single node.
        nsid : int
            Node set ID or node ID.
        temp : float
            Initial temperature.

        Returns
        -------
        bool
            True if successful.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        if option.upper() == "NODE":
            # Look for existing InitialTemperatureNode keyword and append to it
            for kw in self._deck.all_keywords:
                if isinstance(kw, keywords.InitialTemperatureNode):
                    # Add a new row to the existing keyword's DataFrame
                    new_row = pd.DataFrame({"nid": [nsid], "temp": [temp], "loc": [0]})
                    kw.nodes = pd.concat([kw.nodes, new_row], ignore_index=True)
                    logger.debug(f"Added node {nsid} to existing INITIAL_TEMPERATURE_NODE")
                    return True

            # Create new keyword if not found
            kw = keywords.InitialTemperatureNode()
            kw.nodes = pd.DataFrame({"nid": [nsid], "temp": [temp], "loc": [0]})
        else:
            kw = keywords.InitialTemperatureSet()
            kw.nsid = nsid
            kw.temp = temp

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_TEMPERATURE_{option} with nsid={nsid}, temp={temp}")
        return True

    def create_mat_elastic_plastic_thermal(
        self,
        ro: float = 0.0,
        ti: tuple = (),
        ei: tuple = (),
        pri: tuple = (),
        alphai: tuple = (),
        sigyi: tuple = (),
    ) -> int:
        """Create a MAT_ELASTIC_PLASTIC_THERMAL keyword.

        Parameters
        ----------
        ro : float
            Mass density.
        ti : tuple
            Temperature values.
        ei : tuple
            Young's modulus values.
        pri : tuple
            Poisson's ratio values.
        alphai : tuple
            Thermal expansion coefficient values.
        sigyi : tuple
            Yield stress values.

        Returns
        -------
        int
            Material ID.
        """
        from ansys.dyna.core.keywords import keywords

        mid = self.next_id("material")
        kw = keywords.MatElasticPlasticThermal()
        kw.mid = mid
        kw.ro = ro

        # Set temperature-dependent properties
        for i, (t, e, pr, alpha, sigy) in enumerate(zip(ti, ei, pri, alphai, sigyi)):
            setattr(kw, f"t{i + 1}", t)
            setattr(kw, f"e{i + 1}", e)
            setattr(kw, f"pr{i + 1}", pr)
            setattr(kw, f"alpha{i + 1}", alpha)
            setattr(kw, f"sigy{i + 1}", sigy)
            setattr(kw, f"etan{i + 1}", 0.0)  # Default etan to 0

        self._deck.append(kw)
        logger.debug(f"Created MAT_ELASTIC_PLASTIC_THERMAL with mid={mid}")
        return mid

    def create_mat_thermal_isotropic(
        self,
        tmid: int = 0,
        ro: float = 0.0,
        tgrlc: float = 0.0,
        tgmult: float = 0.0,
        hc: float = 0.0,
        tc: float = 0.0,
    ) -> bool:
        """Create a MAT_THERMAL_ISOTROPIC keyword.

        Parameters
        ----------
        tmid : int
            Thermal material ID.
        ro : float
            Mass density.
        tgrlc : float
            Thermal generation rate load curve.
        tgmult : float
            Thermal generation rate multiplier.
        hc : float
            Specific heat.
        tc : float
            Thermal conductivity.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatThermalIsotropic()
        kw.tmid = tmid
        kw.ro = ro
        kw.tgrlc = tgrlc
        kw.tgmult = tgmult
        kw.hc = hc
        kw.tc = tc

        self._deck.append(kw)
        logger.debug(f"Created MAT_THERMAL_ISOTROPIC with tmid={tmid}")
        return True

    def create_set_node_list_with_solver(
        self,
        sid: int,
        nodes: List[int],
        solver: str = "MECH",
    ) -> bool:
        """Create a SET_NODE_LIST keyword with solver option.

        Parameters
        ----------
        sid : int
            Set ID.
        nodes : List[int]
            List of node IDs.
        solver : str
            Solver type (MECH, THER, etc.).

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetNodeList()
        kw.sid = sid
        kw.solver = solver
        kw.its = None  # Match reference file behavior

        # Add nodes to the set using the SeriesCard's append method
        for nid in nodes:
            kw.nodes.append(nid)

        self._deck.append(kw)
        logger.debug(f"Created SET_NODE_LIST with sid={sid}, solver={solver}")
        return True

    def create_part(
        self,
        pid: int,
        secid: int = 0,
        mid: int = 0,
        title: str = "",
    ) -> bool:
        """Create a PART keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        secid : int
            Section ID.
        mid : int
            Material ID.
        title : str
            Part title.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.Part()
        kw.pid = pid
        kw.secid = secid
        kw.mid = mid
        kw.heading = title

        self._deck.append(kw)
        logger.debug(f"Created PART with pid={pid}")
        return True

    def create_section_shell(
        self,
        secid: int,
        elform: int = 2,
        shrf: float = 1.0,
        nip: int = 2,
        t1: float = 0.0,
        t2: float = 0.0,
        t3: float = 0.0,
        t4: float = 0.0,
    ) -> bool:
        """Create a SECTION_SHELL keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor.
        nip : int
            Number of integration points.
        t1, t2, t3, t4 : float
            Shell thickness at nodes.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SectionShell()
        kw.secid = secid
        kw.elform = elform
        kw.shrf = shrf
        kw.nip = nip
        kw.t1 = t1
        kw.t2 = t2
        kw.t3 = t3
        kw.t4 = t4

        self._deck.append(kw)
        logger.debug(f"Created SECTION_SHELL with secid={secid}")
        return True

    def create_section_solid(
        self,
        secid: int,
        elform: int = 1,
    ) -> bool:
        """Create a SECTION_SOLID keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        elform : int
            Element formulation.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SectionSolid()
        kw.secid = secid
        kw.elform = elform

        self._deck.append(kw)
        logger.debug(f"Created SECTION_SOLID with secid={secid}")
        return True

    def set_part_property(
        self,
        pid: int,
        secid: int = 0,
        mid: int = 0,
        eosid: int = 0,
        hgid: int = 0,
        grav: int = 0,
        adpopt: int = 0,
        tmid: int = 0,
    ) -> bool:
        """Set properties for an existing PART or create a new one.

        Parameters
        ----------
        pid : int
            Part ID.
        secid : int
            Section ID.
        mid : int
            Material ID.
        eosid : int
            Equation of state ID.
        hgid : int
            Hourglass ID.
        grav : int
            Gravity load flag.
        adpopt : int
            Adaptive meshing flag.
        tmid : int
            Thermal material ID.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        # Look for an existing Part keyword and update the row with matching pid
        for kw in self._deck.all_keywords:
            if isinstance(kw, keywords.Part):
                # Part keyword has a DataFrame in kw.parts
                if not kw.parts.empty:
                    # Find row with matching pid
                    mask = kw.parts["pid"] == pid
                    if mask.any():
                        # Update the existing row
                        kw.parts.loc[mask, "secid"] = secid
                        kw.parts.loc[mask, "mid"] = mid
                        kw.parts.loc[mask, "eosid"] = eosid
                        kw.parts.loc[mask, "hgid"] = hgid
                        kw.parts.loc[mask, "grav"] = grav
                        kw.parts.loc[mask, "adpopt"] = adpopt
                        kw.parts.loc[mask, "tmid"] = tmid
                        logger.debug(f"Updated PART properties for pid={pid}")
                        return True

        # If not found in existing keywords, create a new PART keyword
        kw = keywords.Part()
        # Add a row to the parts dataframe
        import pandas as pd

        new_row = pd.DataFrame(
            {
                "heading": [""],
                "pid": [pid],
                "secid": [secid],
                "mid": [mid],
                "eosid": [eosid],
                "hgid": [hgid],
                "grav": [grav],
                "adpopt": [adpopt],
                "tmid": [tmid],
            }
        )
        kw.parts = pd.concat([kw.parts, new_row], ignore_index=True)

        self._deck.append(kw)
        logger.debug(f"Created PART with pid={pid}")
        return True

    def create_mat_rigid(
        self,
        mid: int,
        ro: float,
        e: float,
        pr: float,
    ) -> bool:
        """Create a MAT_RIGID keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatRigid()
        kw.mid = mid
        kw.ro = ro
        kw.e = e
        kw.pr = pr

        self._deck.append(kw)
        logger.debug(f"Created MAT_RIGID with mid={mid}")
        return True

    def create_mat_piecewise_linear_plasticity(
        self,
        mid: int,
        ro: float,
        e: float,
        pr: float = 0.3,
        sigy: float = 0.0,
        etan: float = 0.0,
    ) -> bool:
        """Create a MAT_PIECEWISE_LINEAR_PLASTICITY keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.
        sigy : float
            Yield stress.
        etan : float
            Tangent modulus.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatPiecewiseLinearPlasticity()
        kw.mid = mid
        kw.ro = ro
        kw.e = e
        kw.pr = pr
        kw.sigy = sigy
        kw.etan = etan

        self._deck.append(kw)
        logger.debug(f"Created MAT_PIECEWISE_LINEAR_PLASTICITY with mid={mid}")
        return True

    def create_contact(
        self,
        contact_type: str,
        ssid: int = 0,
        msid: int = 0,
        sstyp: int = 0,
        mstyp: int = 0,
        fs: float = 0.0,
        fd: float = 0.0,
    ) -> int:
        """Create a CONTACT keyword.

        Parameters
        ----------
        contact_type : str
            Contact type (e.g., "AUTOMATIC_SINGLE_SURFACE").
        ssid : int
            Slave set ID.
        msid : int
            Master set ID.
        sstyp : int
            Slave set type.
        mstyp : int
            Master set type.
        fs : float
            Static friction coefficient.
        fd : float
            Dynamic friction coefficient.

        Returns
        -------
        int
            The contact ID.
        """
        from ansys.dyna.core.keywords import keywords

        contact_id = self.next_id("contact")

        # Map contact types to keyword classes
        contact_type_map = {
            "AUTOMATIC_SINGLE_SURFACE": keywords.ContactAutomaticSingleSurface,
            "AUTOMATIC_SURFACE_TO_SURFACE": keywords.ContactAutomaticSurfaceToSurface,
            "NODES_TO_SURFACE": keywords.ContactNodesToSurface,
        }

        if contact_type.upper() in contact_type_map:
            kw_class = contact_type_map[contact_type.upper()]
            kw = kw_class()
            kw.ssid = ssid
            kw.msid = msid
            kw.sstyp = sstyp
            kw.mstyp = mstyp
            kw.fs = fs
            kw.fd = fd

            self._deck.append(kw)
            logger.debug(f"Created CONTACT_{contact_type} with id={contact_id}")
            return contact_id
        else:
            logger.warning(f"Unknown contact type: {contact_type}")
            return 0

    def create_set_node_list(self, sid: int, nodes: List[int]) -> bool:
        """Create a SET_NODE_LIST keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        nodes : List[int]
            List of node IDs.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetNodeList()
        kw.sid = sid

        # Add nodes to the set using SeriesCard's append method
        for nid in nodes:
            kw.nodes.append(nid)

        self._deck.append(kw)
        logger.debug(f"Created SET_NODE_LIST with sid={sid}")
        return True

    def create_set_part_list(self, sid: int, parts: List[int]) -> bool:
        """Create a SET_PART_LIST keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        parts : List[int]
            List of part IDs.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetPartList()
        kw.sid = sid

        # Add parts to the set
        for pid in parts:
            kw.pid.append(pid)

        self._deck.append(kw)
        logger.debug(f"Created SET_PART_LIST with sid={sid}")
        return True

    def create_boundary_spc_node(
        self,
        nid: int,
        dofx: int = 0,
        dofy: int = 0,
        dofz: int = 0,
        dofrx: int = 0,
        dofry: int = 0,
        dofrz: int = 0,
    ) -> bool:
        """Create a BOUNDARY_SPC_NODE keyword.

        Parameters
        ----------
        nid : int
            Node ID.
        dofx, dofy, dofz : int
            Translational DOF constraints (0=free, 1=constrained).
        dofrx, dofry, dofrz : int
            Rotational DOF constraints (0=free, 1=constrained).

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.BoundarySpcNode()
        kw.nid = nid
        kw.dofx = dofx
        kw.dofy = dofy
        kw.dofz = dofz
        kw.dofrx = dofrx
        kw.dofry = dofry
        kw.dofrz = dofrz

        self._deck.append(kw)
        logger.debug(f"Created BOUNDARY_SPC_NODE for nid={nid}")
        return True

    def create_initial_velocity_node(
        self,
        nid: int,
        vx: float = 0.0,
        vy: float = 0.0,
        vz: float = 0.0,
    ) -> bool:
        """Create an INITIAL_VELOCITY_NODE keyword.

        Parameters
        ----------
        nid : int
            Node ID.
        vx, vy, vz : float
            Velocity components.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.InitialVelocityNode()
        kw.nid = nid
        kw.vx = vx
        kw.vy = vy
        kw.vz = vz

        self._deck.append(kw)
        logger.debug(f"Created INITIAL_VELOCITY_NODE for nid={nid}")
        return True

    # =========================================================================
    # EM (Electromagnetic) Methods
    # =========================================================================

    def create_em_control(
        self,
        emsol: int = 0,
        numls: int = 100,
        macrodt: float = 0.0,
        dimtype: int = 0,
        nperio: int = 2,
        ncylfem: int = 5000,
        ncylbem: int = 5000,
    ) -> bool:
        """Create an EM_CONTROL keyword.

        Parameters
        ----------
        emsol : int
            EM solver type.
        numls : int
            Number of local EM steps.
        macrodt : float
            Macro time step.
        dimtype : int
            Problem dimension type.
        nperio : int
            Number of periods.
        ncylfem : int
            FEM recalculation cycles.
        ncylbem : int
            BEM recalculation cycles.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmControl()
        kw.emsol = emsol
        kw.numls = numls
        kw.macrodt = macrodt
        kw.dimtype = dimtype
        kw.nperio = nperio
        kw.ncylfem = ncylfem
        kw.ncylbem = ncylbem

        self._deck.append(kw)
        logger.debug(f"Created EM_CONTROL with emsol={emsol}")
        return True

    def create_em_timestep(
        self,
        tstype: int = 1,
        dtconst: float = 0.0,
        factor: float = 1.0,
        rlcsf: int = 25,
    ) -> bool:
        """Create an EM_CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        tstype : int
            Time step type.
        dtconst : float
            Constant time step value.
        factor : float
            Time step factor.
        rlcsf : int
            RLC sub-cycling factor.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmControlTimestep()
        kw.tstype = tstype
        kw.dtcons = dtconst
        kw.factor = factor
        kw.rlcsf = rlcsf

        self._deck.append(kw)
        logger.debug(f"Created EM_CONTROL_TIMESTEP with tstype={tstype}, dtcons={dtconst}")
        return True

    def create_em_output(
        self,
        mats: int = 0,
        matf: int = 0,
        sols: int = 0,
        solf: int = 0,
    ) -> bool:
        """Create an EM_OUTPUT keyword.

        Parameters
        ----------
        mats : int
            Matrix output level to screen.
        matf : int
            Matrix output level to file.
        sols : int
            Solver output level to screen.
        solf : int
            Solver output level to file.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmOutput()
        kw.mats = mats
        kw.matf = matf
        kw.sols = sols
        kw.solf = solf

        self._deck.append(kw)
        logger.debug(f"Created EM_OUTPUT with mats={mats}, matf={matf}, sols={sols}, solf={solf}")
        return True

    def create_em_mat_001(
        self,
        mid: int,
        mtype: int = 0,
        sigma: float = 0.0,
    ) -> bool:
        """Create an EM_MAT_001 keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Material type.
        sigma : float
            Electrical conductivity.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmMat001()
        kw.mid = mid
        kw.mtype = mtype
        kw.sigma = sigma

        self._deck.append(kw)
        logger.debug(f"Created EM_MAT_001 with mid={mid}, mtype={mtype}")
        return True

    def create_em_isopotential(
        self,
        settype: int = 1,
        setid: int = 0,
        rdltype: int = 0,
    ) -> int:
        """Create an EM_ISOPOTENTIAL keyword.

        Parameters
        ----------
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.
        rdltype : int
            Randles layer type.

        Returns
        -------
        int
            Isopotential ID.
        """
        from ansys.dyna.core.keywords import keywords

        isoid = self.next_id("isopotential")

        kw = keywords.EmIsopotential()
        kw.isoid = isoid
        kw.settype = settype
        kw.setid = setid
        kw.rdltype = rdltype

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL with isoid={isoid}, settype={settype}, setid={setid}")
        return isoid

    def create_em_isopotential_connect(
        self,
        contype: int = 1,
        isoid1: int = 0,
        isoid2: int = 0,
        val: float = 0.0,
        lcid: int = 0,
        l: float = 0.0,
        c: float = 0.0,
        v0: float = 0.0,
    ) -> int:
        """Create an EM_ISOPOTENTIAL_CONNECT keyword.

        Parameters
        ----------
        contype : int
            Connection type.
        isoid1 : int
            First isopotential ID.
        isoid2 : int
            Second isopotential ID.
        val : float
            Value (resistance, voltage, or current).
        lcid : int
            Load curve ID.
        l : float
            Inductance (for RLC circuit).
        c : float
            Capacitance (for RLC circuit).
        v0 : float
            Initial voltage (for RLC circuit).

        Returns
        -------
        int
            Connection ID.
        """
        from ansys.dyna.core.keywords import keywords

        conid = self.next_id("isopotential_connect")

        kw = keywords.EmIsopotentialConnect()
        kw.conid = conid
        kw.contype = contype
        kw.isoid1 = isoid1
        kw.isoid2 = isoid2
        kw.val = val
        kw.lcid_rdlid = lcid
        # For RLC circuits (contype=6), set the second card values
        if contype == 6:
            kw.l = l
            kw.c = c
            kw.v0 = v0

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL_CONNECT with conid={conid}, contype={contype}")
        return conid

    def create_em_isopotential_rogo(
        self,
        isoid: int,
        settype: int = 1,
        setid: int = 0,
    ) -> bool:
        """Create an EM_ISOPOTENTIAL_ROGO keyword.

        Parameters
        ----------
        isoid : int
            Isopotential ID for Rogowski coil.
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.EmIsopotentialRogo()
        kw.isoid = isoid
        kw.settype = settype
        kw.setid = setid

        self._deck.append(kw)
        logger.debug(f"Created EM_ISOPOTENTIAL_ROGO with isoid={isoid}, setid={setid}")
        return True

    def create_em_circuit_rogo(
        self,
        settype: int = 1,
        setid: int = 0,
        curtyp: int = 1,
    ) -> int:
        """Create an EM_CIRCUIT_ROGO keyword (for backward compatibility).

        Parameters
        ----------
        settype : int
            Set type (1=segment, 2=node).
        setid : int
            Set ID.
        curtyp : int
            Current type.

        Returns
        -------
        int
            Rogowski coil ID.
        """
        from ansys.dyna.core.keywords import keywords

        rogid = self.next_id("rogo")

        kw = keywords.EmCircuitRogo()
        kw.rogid = rogid
        kw.settype = settype
        kw.setid = setid
        kw.curtyp = curtyp

        self._deck.append(kw)
        logger.debug(f"Created EM_CIRCUIT_ROGO with rogid={rogid}, setid={setid}")
        return rogid

    def create_set_segment(
        self,
        sid: int,
        segments: List[tuple],
        solver: str = "MECH",
    ) -> bool:
        """Create a SET_SEGMENT keyword.

        Parameters
        ----------
        sid : int
            Set ID.
        segments : List[tuple]
            List of segment tuples (n1, n2, n3, n4).
        solver : str
            Solver type.

        Returns
        -------
        bool
            True if successful.
        """
        import pandas as pd

        from ansys.dyna.core.keywords import keywords

        kw = keywords.SetSegment()
        kw.sid = sid
        kw.solver = solver

        # Build rows for the dataframe
        rows = []
        for seg in segments:
            if len(seg) >= 4:
                rows.append({"n1": seg[0], "n2": seg[1], "n3": seg[2], "n4": seg[3]})
            elif len(seg) == 3:
                # Triangle - n4 = n3
                rows.append({"n1": seg[0], "n2": seg[1], "n3": seg[2], "n4": seg[2]})

        if rows:
            new_df = pd.DataFrame(rows)
            kw.segments = pd.concat([kw.segments, new_df], ignore_index=True)

        self._deck.append(kw)
        logger.debug(f"Created SET_SEGMENT with sid={sid}, {len(segments)} segments")
        return True


class KeywordsStub:
    """Stub class that mimics the gRPC stub interface for keywords backend.

    This class provides the same method signatures as the gRPC stub but
    implements them using the KeywordsBackend.
    """

    def __init__(self, backend: KeywordsBackend):
        """Initialize the stub with a backend.

        Parameters
        ----------
        backend : KeywordsBackend
            The keywords backend instance.
        """
        self._backend = backend

    # The stub methods will be called by existing pre module code
    # and delegate to the keywords backend

    def CreateTermination(self, request):
        """Create termination keyword."""
        self._backend.create_termination(request.endtim)
        return type("Response", (), {"success": True})()

    def CreateDBBinary(self, request):
        """Create database binary keyword."""
        self._backend.create_database_binary(
            filetype=request.filetype,
            dt=request.dt,
            maxint=request.maxint,
            ieverp=request.ieverp,
            dcomp=request.dcomp,
            nintsld=request.nintsld,
        )
        return type("Response", (), {"success": True})()

    def CreateDBAscii(self, request):
        """Create database ASCII keyword."""
        self._backend.create_database_ascii(
            db_type=request.type,
            dt=request.dt,
            binary=request.binary,
            lcur=request.lcur,
            ioopt=request.ioopt,
        )
        return type("Response", (), {"success": True})()

    def CreateDefineCurve(self, request):
        """Create define curve keyword."""
        curve_id = self._backend.create_define_curve(
            sfo=request.sfo,
            abscissa=list(request.abscissa),
            ordinate=list(request.ordinate),
            title=request.title,
        )
        return type("Response", (), {"id": curve_id})()

    def CreateControlAccuracy(self, request):
        """Create control accuracy keyword."""
        self._backend.create_control_accuracy(
            osu=request.osu,
            inn=request.inn,
            pidosu=request.pidosu,
            iacc=request.iacc,
            exacc=request.exacc,
        )
        return type("Response", (), {"success": True})()

    def CreateControlEnergy(self, request):
        """Create control energy keyword."""
        self._backend.create_control_energy(
            hgen=request.hgen,
            rwen=request.rwen,
            slnten=request.slnten,
            rylen=request.rylen,
            irgen=request.irgen,
        )
        return type("Response", (), {"success": True})()

    def CreateControlHourgalss(self, request):
        """Create control hourglass keyword."""
        self._backend.create_control_hourglass(
            ihq=request.ihq,
            qh=request.qh,
        )
        return type("Response", (), {"success": True})()

    def CreateControlBulkViscosity(self, request):
        """Create control bulk viscosity keyword."""
        self._backend.create_control_bulk_viscosity(
            q1=request.q1,
            q2=request.q2,
            bulk_type=request.type,
        )
        return type("Response", (), {"success": True})()

    def CreateControlShell(self, request):
        """Create control shell keyword."""
        self._backend.create_control_shell(
            wrpang=request.wrpang,
            esort=request.esort,
            irnxx=request.irnxx,
            istupd=request.istupd,
            theory=request.theory,
            bwc=request.bwc,
            miter=request.miter,
            proj=request.proj,
            irquad=request.irquad,
        )
        return type("Response", (), {"success": True})()

    def CreateControlContact(self, request):
        """Create control contact keyword."""
        self._backend.create_control_contact(
            rwpnal=request.rwpnal,
            shlthk=request.shlthk,
            orien=request.orien,
            ssthk=request.ssthk,
            ignore=request.ignore,
            igactc=request.igactc,
        )
        return type("Response", (), {"success": True})()

    def CreateTimestep(self, request):
        """Create control timestep keyword."""
        self._backend.create_control_timestep(
            tssfac=request.tssfac,
            isdo=request.isdo,
            dt2ms=request.dt2ms,
            lctm=request.lctm,
        )
        return type("Response", (), {"success": True})()

    def SaveFile(self, request):
        """Save the keyword file."""
        output_path = self._backend.save_file(request.name)
        return type("Response", (), {"outpath": output_path})()

    def LoadFile(self, request):
        """Load a keyword file."""
        return type("Response", (), {"success": True})()

    def kwSetFileName(self, request):
        """Set the filename."""
        if request.num == 0:
            self._backend.set_main_filename(request.name)
        return type("Response", (), {"success": True})()

    def Upload(self, chunks):
        """Upload file chunks (no-op for local backend)."""
        # For local backend, files are loaded directly
        return type("Response", (), {"success": True})()

    def Download(self, request):
        """Download file (yields chunks)."""
        # For local backend, read the file and yield chunks
        filepath = request.url
        if os.path.exists(filepath):
            with open(filepath, "rb") as f:
                while True:
                    chunk = f.read(1024 * 1024)
                    if not chunk:
                        break
                    yield type("Chunk", (), {"buffer": chunk})()

    def CreateControlSolution(self, request):
        """Create control solution keyword."""
        self._backend.create_control_solution(soln=request.soln)
        return type("Response", (), {"success": True})()

    def CreateControlThermalSolver(self, request):
        """Create control thermal solver keyword."""
        self._backend.create_control_thermal_solver(atype=request.atype)
        return type("Response", (), {"success": True})()

    def CreateControlThermalTimestep(self, request):
        """Create control thermal timestep keyword."""
        self._backend.create_control_thermal_timestep(its=request.its)
        return type("Response", (), {"success": True})()

    def CreateControlThermalNonlinear(self, request):
        """Create control thermal nonlinear keyword (stub)."""
        # TODO: Implement CONTROL_THERMAL_NONLINEAR when needed
        return type("Response", (), {"success": True})()

    def CreateInitTemperature(self, request):
        """Create initial temperature keyword."""
        self._backend.create_initial_temperature(
            option=request.option,
            nsid=request.nsid,
            temp=request.temp,
        )
        return type("Response", (), {"success": True})()

    def CreateMatElasticPlasticThermal(self, request):
        """Create MAT_ELASTIC_PLASTIC_THERMAL keyword."""
        mid = self._backend.create_mat_elastic_plastic_thermal(
            ro=request.ro,
            ti=request.ti,
            ei=request.ei,
            pri=request.pri,
            alphai=request.alphai,
            sigyi=request.sigyi,
        )
        return type("Response", (), {"mid": mid})()

    def CreateMatThermalIsotropic(self, request):
        """Create MAT_THERMAL_ISOTROPIC keyword."""
        self._backend.create_mat_thermal_isotropic(
            tmid=request.mid,
            ro=request.tro,
            tgrlc=request.tgrlc,
            tgmult=request.tgmult,
            hc=request.hc,
            tc=request.tc,
        )
        return type("Response", (), {"success": True})()

    def CreateSectionSolid(self, request):
        """Create SECTION_SOLID keyword."""
        secid = self._backend.next_id("section")
        self._backend.create_section_solid(secid=secid, elform=request.elform)
        return type("Response", (), {"id": secid})()

    def SetPartProperty(self, request):
        """Set part properties."""
        # Find the part keyword and update it, or create one if needed
        self._backend.set_part_property(
            pid=request.pid,
            secid=request.secid,
            mid=request.mid,
            eosid=getattr(request, "eosid", 0),
            hgid=getattr(request, "hgid", 0),
            grav=getattr(request, "grav", 0),
            adpopt=getattr(request, "adpopt", 0),
            tmid=getattr(request, "tmid", 0),
        )
        return type("Response", (), {"success": True})()

    def CreateNodeSet(self, request):
        """Create a node set keyword."""
        # Generate a set ID if not provided
        sid = self._backend.next_id("nodeset")
        nodes = list(request.entities) if request.entities else []

        if request.option == "LIST":
            solver = "MECH"  # Default solver
            self._backend.create_set_node_list_with_solver(sid=sid, nodes=nodes, solver=solver)
        else:
            self._backend.create_set_node_list(sid=sid, nodes=nodes)

        return type("Response", (), {"id": sid})()

    def CreateHourglass(self, request):
        """Create HOURGLASS keyword."""
        hgid = self._backend.next_id("hourglass")
        # For individual part hourglass, we'll create a HOURGLASS keyword
        from ansys.dyna.core.keywords import keywords

        kw = keywords.Hourglass()
        kw.hgid = hgid
        kw.ihq = request.ihq
        kw.qm = request.qm
        kw.q1 = getattr(request, "q1", 0)
        kw.q2 = getattr(request, "q2", 0)
        kw.qb = getattr(request, "qb", 0)
        kw.qw = getattr(request, "qw", 0)

        self._backend._deck.append(kw)
        return type("Response", (), {"id": hgid})()

    # =========================================================================
    # EM (Electromagnetic) Stub Methods
    # =========================================================================

    def CreateEMControl(self, request):
        """Create EM_CONTROL keyword."""
        self._backend.create_em_control(
            emsol=request.emsol,
            numls=request.numls,
            macrodt=request.macrodt,
            dimtype=getattr(request, "dimtype", 0),
            nperio=getattr(request, "nperio", 2),
            ncylfem=request.ncylfem,
            ncylbem=request.ncylbem,
        )
        return type("Response", (), {"success": True})()

    def CreateEMTimestep(self, request):
        """Create EM_CONTROL_TIMESTEP keyword."""
        self._backend.create_em_timestep(
            tstype=request.tstype,
            dtconst=request.dtconst,
        )
        return type("Response", (), {"success": True})()

    def CreateEMOutput(self, request):
        """Create EM_OUTPUT keyword."""
        self._backend.create_em_output(
            mats=request.mats,
            matf=request.matf,
            sols=request.sols,
            solf=request.solf,
        )
        return type("Response", (), {"success": True})()

    def CreateEMMat001(self, request):
        """Create EM_MAT_001 keyword."""
        self._backend.create_em_mat_001(
            mid=request.mid,
            mtype=request.mtype,
            sigma=request.sigma,
        )
        return type("Response", (), {"success": True})()

    def CreateEMIsopotential(self, request):
        """Create EM_ISOPOTENTIAL keyword."""
        isoid = self._backend.create_em_isopotential(
            settype=request.settype,
            setid=request.setid,
            rdltype=getattr(request, "rdltype", 0),
        )
        return type("Response", (), {"id": isoid})()

    def CreateEMIsopotentialConnect(self, request):
        """Create EM_ISOPOTENTIAL_CONNECT keyword."""
        conid = self._backend.create_em_isopotential_connect(
            contype=request.contype,
            isoid1=request.isoid1,
            isoid2=request.isoid2,
            val=request.val,
            lcid=getattr(request, "lcid", 0),
            l=getattr(request, "l", 0),
            c=getattr(request, "c", 0),
            v0=getattr(request, "v0", 0),
        )
        return type("Response", (), {"id": conid})()

    def CreateEMCircuitRogo(self, request):
        """Create EM_CIRCUIT_ROGO keyword (maps to EM_ISOPOTENTIAL_ROGO)."""
        # The old API uses CreateEMCircuitRogo but we use EM_ISOPOTENTIAL_ROGO
        # First, create an isopotential and then the rogo connection
        isoid = self._backend.create_em_isopotential(
            settype=request.settype,
            setid=request.setid,
            rdltype=0,
        )
        self._backend.create_em_isopotential_rogo(
            isoid=isoid,
            settype=request.settype,
            setid=request.setid,
        )
        return type("Response", (), {"id": isoid})()

    def CreateSegmentSet(self, request):
        """Create SET_SEGMENT keyword."""
        sid = self._backend.next_id("segmentset")
        # Request has n1, n2, n3, n4 arrays - combine them into segments
        n1 = list(request.n1) if request.n1 else []
        n2 = list(request.n2) if request.n2 else []
        n3 = list(request.n3) if request.n3 else []
        n4 = list(request.n4) if request.n4 else []
        segments = list(zip(n1, n2, n3, n4))
        self._backend.create_set_segment(sid=sid, segments=segments, solver="MECH")
        return type("Response", (), {"id": sid})()

    def CreateMatRigid(self, request):
        """Create MAT_RIGID keyword."""
        from ansys.dyna.core.keywords import keywords

        mid = self._backend.next_id("material")

        kw = keywords.MatRigid()
        kw.mid = mid
        kw.ro = request.ro
        kw.e = request.e
        kw.pr = request.pr
        kw.cmo = getattr(request, "cmo", 0)
        kw.con1 = getattr(request, "con1", 0)
        kw.con2 = getattr(request, "con2", 0)

        self._backend._deck.append(kw)
        return type("Response", (), {"mid": mid})()

    def CreateEMMat(self, request):
        """Create EM_MAT_001 keyword for material EM properties."""
        self._backend.create_em_mat_001(
            mid=request.mid,
            mtype=request.mtype,
            sigma=request.sigma,
        )
        return type("Response", (), {"success": True})()

    def CreateEMIsopotentialRogo(self, request):
        """Create EM_ISOPOTENTIAL_ROGO keyword."""
        isoid = self._backend.next_id("isopotential")
        self._backend.create_em_isopotential_rogo(
            isoid=isoid,
            settype=request.settype,
            setid=request.setid,
        )
        return type("Response", (), {"id": isoid})()
