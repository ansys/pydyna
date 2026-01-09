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
