"""
EM API
==========

Module to create electromagnetism input deck
"""

import logging

from .dynabase import *  # noqa : F403


class Isopotential_ConnType(Enum):
    SHORT_CIRCUIT = 1
    RESISTANCE = 2
    VOLTAGE_SOURCE = 3
    CURRENT_SOURCE = 4
    RLC_CIRCUIT = 6


class DynaEM(DynaBase):
    """Contain methods to create keyword related to EM."""

    def __init__(self):
        DynaBase.__init__(self)
        self.analysis = EMAnalysis()

    def create_em_control(self, emsol=0, numls=100, macrodt=0, ncylfem=5000, ncylbem=5000):
        """Enable the EM solver and set its options.

        Parameters
        ----------
        emsol : int
           Electromagnetism solver selector:
           EQ.-1: Turns the EM solver off after reading the EM keywords.
           EQ.1: Eddy current solver.
           EQ.2: Induced heating solver.
           EQ.3: Resistive heating solver.
           EQ.11: Electrophysiology monodomain.
           EQ.12: Electrophysiology bidomain.
           EQ.13: Electrophysiology monodomain coupled with bidomain.
        numls : int
            Number of local EM steps in a whole period for emsol = 2.
        macrodt : float
            Macro time step when EMSOL = 2.
        ncylfem : int
            Number of electromagnetism cycles between the recalculation of FEM matrices.
        ncylbem : int
            Number of electromagnetism cycles between the recalculation of BEM matrices.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMControl(
            EMControlRequest(
                emsol=emsol,
                numls=numls,
                macrodt=macrodt,
                ncylfem=ncylfem,
                ncylbem=ncylbem,
            )
        )
        logging.info("EM Control Created...")
        return ret

    def create_em_timestep(self, tstype, dtconst):
        """Control the EM time step and its evolution.

        Parameters
        ----------
        tstype : int
           Time step type:
           EQ.1: constant time step given in DTCONST
           EQ.2: time step as a function of time given by a load curve specified in LCID
           EQ.3: automatic time step computation, depending on the solver type.
           This time step is then multiplied by FACTOR
        dtconst : float
            Constant value for the time step for TSTYPE = 1.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMTimestep(EMTimestepRequest(tstype=tstype, dtconst=dtconst))
        logging.info("EM Timestep Created...")
        return ret

    def create_em_contact(self, contid=0, dtype=0, psidm=0, psids=0, eps1=0.3, eps2=0.3, eps3=0.3, d0=0):
        """Optional card used for defining and specifying options on electromagnetic contacts between two sets of parts.

        Parameters
        ----------
        contid : int
            Electromagnetic contact ID
        dtype : int
            Detection type:
            EQ.0: Contact type 0 (Default).
            EQ.1: Contact type 1.
        psidm : int
            Master part set ID.
        psids : int
            Slave part set ID.
        eps1,eps2,eps3 : float
            Contact Coefficients for contact detection conditions.
        d0 : float
            Contact condition 3 when COTYPE = 1.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMContact(
            EMContactRequest(
                contid=contid,
                dtype=dtype,
                psidm=psidm,
                psids=psids,
                eps1=eps1,
                eps2=eps2,
                eps3=eps3,
                d0=d0,
            )
        )
        logging.info("EM Contact Created...")
        return ret

    def set_rogowsky_coil_to_output_current(self, segmentset=SegmentSet([[]]), settype=1, curtyp=1):
        """Define Rogowsky coils to measure a global current vs time through a segment set or a node set.

        Parameters
        ----------
        rogid : int
            Rogowsky coil ID.
        setid : int
            Segment or node set ID.
        settype : int
            Type of set:
            EQ.1: Segment set
            EQ.2: Node set
        curtyp : int
            Type of current measured:
            EQ.1: Volume current
            EQ.2: Surface current (not available yet}
            EQ.3: Magnetic field flow (B field times Area)

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        segmentset.create(self.stub)
        setid = segmentset.id
        ret = self.stub.CreateEMCircuitRogo(EMCircuitRogoRequest(setid=setid, settype=settype, curtyp=curtyp))
        logging.info("EM Circuit Rogo Created...")
        return ret.id

    def create_em_mat001(self, mid, mtype, sigma):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Defines the electromagnetism type of the material:
            EQ.0: Air or vacuum.
            EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
            EQ.2: Conductor carrying a source.
            EQ.3: Fluid conductor.
            EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved.
        sigma : float
            Initial electrical conductivity of the material.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMMat001(EMMat001Request(mid=mid, mtype=mtype, sigma=sigma))
        logging.info("EM Material 001 Created...")
        return ret

    def create_em_mat002(self, mid, mtype, sigma, eosid, murel):
        """Define an electromagnetic material type and properties
        whose permeability is different than the free space permeability.

        Parameters
        ----------
        mid : int
            Material identification.
        mtype : int
            Electromagnetism type of the material:
            EQ.0: Air or vacuum.
            EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
            EQ.2: Conductor carrying a source.
            EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved.
        sigma : float
            Initial electrical conductivity of the material.
        eosid : int
            ID of the EOS to be used for the electrical conductivity.
        murel : float
            Relative permeability which is the ratio of the permeability of a specific
            medium to the permeability of free space

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMMat002(EMMat002Request(mid=mid, mtype=mtype, sigma=sigma, eosid=eosid, murel=murel))
        logging.info("EM Material 002 Created...")
        return ret

    def create_em_solver_fembem_monolithic(self, mtype=0, stype=0, abstol=1e-6, reltol=1e-4, maxit=500):
        """Turn on the monolithic FEM-BEM solver.

        Parameters
        ----------
        mtype : int
            Monolithic solver type:
            EQ.0: Direct symmetric solver.
        stype : int
            Solver type:
            EQ.0: MINRES iterative solver
            EQ.1: GMRES iterative solver
        abstol : float
            Absolute tolerance.
        reltol : float
            Relative tolerance.
        maxit : int
            Maximum number of iterations.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMSolverFemBemMonolithic(
            EMSolverFemBemMonolithicRequest(mtype=mtype, stype=stype, abstol=abstol, reltol=reltol, maxit=maxit)
        )
        logging.info("EM Solver FEMBEM Monolithic Created...")
        return ret

    def create_em_output(self, mats=0, matf=0, sols=0, solf=0):
        """Define the level of EM related output on the screen and in the messag file.

        Parameters
        ----------
        mats : int
            Level of matrix assembly output to the screen:
            EQ.0: no output
            EQ.1: basic assembly steps
            EQ.2: basic assembly steps + percentage completed + final statistics
            EQ.3: basic assembly steps + percentage completed + statistics at each percentage of completion
        matf : int
            Level of matrix assembly output to the messag file:
            EQ.0: no output
            EQ.1: basic assembly steps
            EQ.2: basic assembly steps + percentage completed + final statistics
            EQ.3: basic assembly steps + percentage completed + statistics at each percentage of completion
        sols : int
            Level of solver output on the screen:
            EQ.0: no output
            EQ.1: global information at each FEM iteration
            EQ.2: detailed information at each FEM iteration
        solf : int
            Level of solver output to the messag file:
            EQ.0: no output
            EQ.1: global information at each FEM iteration
            EQ.2: detailed information at each FEM iteration

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMOutput(EMOutputRequest(mats=mats, matf=matf, sols=sols, solf=solf))
        logging.info("EM Output Created...")
        return ret

    def connect_isopotential(
        self, contype=Isopotential_ConnType.SHORT_CIRCUIT, isopotential1=None, isopotential2=None, value=0
    ):
        """Define a connection between two isopotentials or between an isopotential and the ground.

        Parameters
        ----------
        contype : Isopotential_ConnType
            See Isopotential_ConnType.
        isopotential1 : Isopotential
            First isopotential to be connected
        isopotential2 : Isopotential
            Second isopotential to be connected
        value : float
            Value of the resistance, voltage or current depending on contype
        Returns
        -------
        int
            Connection ID.
        """
        contype = contype.value
        if isopotential1 is not None:
            isoid1 = isopotential1.create()
        else:
            isoid1 = 0
        if isopotential2 is not None:
            isoid2 = isopotential2.create()
        else:
            isoid2 = 0
        ret = self.stub.CreateEMIsopotentialConnect(
            EMIsopotentialConnectRequest(contype=contype, isoid1=isoid1, isoid2=isoid2, val=value)
        )
        logging.info("Isopotential connection Created...")
        return ret.id

    def create_em_database_globalenergy(self, outlv=0):
        """Enable the output of global EM.

        Parameters
        ----------
        outlv : int
            Determines if the output file should be dumped.
            EQ.0: No output file is generated.
            EQ.1: The output file is generated.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMDatabaseGlobalEnergy(EMDatabaseGlobalEnergyRequest(outlv=outlv))
        logging.info("EM Database Global Energy Created...")
        return ret

    def create_Permanent_magnet(self, id, partid, mtype, north, sourth, hc):
        """Define a permanent magnet.

        Parameters
        ----------
        id : int
            ID of the magnet.
        partid : int
            Part ID.
        mtype : int
            Magnet definition type:
            EQ.0: Magnet defined by two node sets for North and South Poles.
            EQ.1: Magnet defined by two segments sets for North and South Poles.
            EQ.3: Magnet defined by a global vector orientation.
            EQ.4: Magnet defined by a global vector orientation given by two node IDs.
        north : int
            Set ID of the magnet north face for MTYPE = 0 and MTYPE = 1.
        sourth : int
            Set ID of the magnet south face for MTYPE = 0 and MTYPE = 1.
        hc : int
            Coercive force.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMPermanentMagnet(
            EMPermanentMagnetRequest(id=id, partid=partid, mtype=mtype, north=north, sourth=sourth, hc=hc)
        )
        logging.info("EM Permanent Magnet Created...")
        return ret

    def create_em_eos_permeability(self, eosid, eostype, lcid):
        """Define the parameters for the behavior of a material's permeability.

        Parameters
        ----------
        eosid : int
            ID of the EM_EOS.
        eostype : int
            Define the type of EOS:
            EQ.1: Permeability defined by a B function of H curve
            EQ.2: Permeability defined by a H function of B curve
        lcid : int
            Load curve ID.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMEOSPermeability(EMEOSPermeabilityRequest(eosid=eosid, eostype=eostype, lcid=lcid))
        logging.info("EM EOS Permeability Created...")
        return ret

    def save_file(self):
        """Save keyword files."""
        self.analysis.create()
        DynaBase.save_file(self)


class EMType(Enum):
    EDDY_CURRENT = 1
    INDUCTIVE_HEATING = 2
    RESISTIVE_HEATING = 3
    ELECTROPHYSIOLOGY = 11


class BEMSOLVER(Enum):
    DIRECT_SOLVER = 1
    PCG = 2


class FEMSOLVER(Enum):
    DIRECT_SOLVER = 1
    PCG = 2


class EMAnalysis:
    """Enable the EM solver and set its options.

    Parameters
    ----------
    type : int
       Electromagnetism solver selector:
       EQ.1: Eddy current solver.
       EQ.2: Induced heating solver.
       EQ.3: Resistive heating solver.
       EQ.11: Electrophysiology monodomain.
    """

    p_matrix_tol = 1e-6
    q_matrix_tol = 1e-6
    w_matrix_tol = 1e-6

    def __init__(self, type=EMType.EDDY_CURRENT):
        self.defined = False
        self.stub = DynaBase.get_stub()
        self.type = type.value
        self.defined_bem = False
        self.defined_fem = False

    def set_timestep(self, timestep):
        """Control the EM time step and its evolution."""
        self.defined = True
        self.timestep = timestep

    def set_em_solver(self, type=EMType.EDDY_CURRENT):
        """Select Electromagnetism solver."""
        self.type = type.value

    def set_solver_bem(self, solver=BEMSOLVER.PCG, relative_tol=1e-6, max_iteration=1000):
        """Define the type of linear solver and pre-conditioner as well as tolerance for the EM_BEM solve."""
        self.defined_bem = True
        self.bem_relative_tol = relative_tol
        self.max_iteration = max_iteration
        self.bemsolver = solver.value

    def set_solver_fem(self, solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-6, max_iteration=1000):
        """Define some parameters for the EM FEM solver."""
        self.defined_fem = True
        self.femsolver = solver.value
        self.fem_relative_tol = relative_tol
        self.max_iteration = max_iteration

    def set_bem_matrix_tol(self, p_matrix_tol=1e-6, q_matrix_tol=1e-6, w_matrix_tol=1e-6):
        """Define the type of BEM matrices as well as the way they are assembled."""
        EMAnalysis.p_matrix_tol = p_matrix_tol
        EMAnalysis.q_matrix_tol = q_matrix_tol
        EMAnalysis.w_matrix_tol = w_matrix_tol

    def create(self):
        """Create EMAnalysis."""
        if self.defined == False:
            return
        self.stub.CreateEMControl(EMControlRequest(emsol=self.type, numls=100, macrodt=0, ncylfem=5000, ncylbem=5000))
        self.stub.CreateEMTimestep(EMTimestepRequest(tstype=1, dtconst=self.timestep))
        logging.info("EM Timestep Created...")
        if self.defined_bem:
            self.stub.CreateEMSolverBem(
                EMSolverBemRequest(
                    reltol=self.bem_relative_tol,
                    maxite=self.max_iteration,
                    stype=self.bemsolver,
                    precon=1,
                    uselast=1,
                    ncylbem=3,
                )
            )
            logging.info("EM Solver BEM Created...")
        if self.defined_fem:
            self.stub.CreateEMSolverFem(
                EMSolverFemRequest(
                    reltol=self.fem_relative_tol,
                    maxite=self.max_iteration,
                    stype=self.femsolver,
                    precon=1,
                    uselast=1,
                    ncylbem=3,
                )
            )
            logging.info("EM Solver FEM Created...")
        if self.defined_bem:
            self.stub.CreateEMSolverBemMat(EMSolverBemMatRequest(matid=1, reltol=EMAnalysis.p_matrix_tol))
            self.stub.CreateEMSolverBemMat(EMSolverBemMatRequest(matid=2, reltol=EMAnalysis.q_matrix_tol))
            self.stub.CreateEMSolverBemMat(EMSolverBemMatRequest(matid=3, reltol=EMAnalysis.w_matrix_tol))
            logging.info("EM Solver BEMMAT Created...")


class CircuitType(Enum):
    IMPOSED_CURRENT_VS_TIME = 1
    IMPOSED_VOLTAGE_VS_TIME = 2


class Circuit:
    """Define an electrical circuit.

    Parameters
    ----------
    circtyp : int
        Circuit type:
        EQ.1: Imposed current vs time defined by a load curve.
        EQ.2: Imposed voltage vs time defined by a load curve.
    loadcurve : Curve
        Load curve for circtyp = 1, 2, 21 or 22
    """

    def __init__(self, loadcurve, circuit_type=CircuitType.IMPOSED_CURRENT_VS_TIME):
        self.stub = DynaBase.get_stub()
        self.circuit_type = circuit_type.value
        loadcurve.create(self.stub)
        self.lcid = loadcurve.id

    def set_current(self, current, current_inlet, current_outlet):
        """Define segment set for current.

        Parameters
        ----------
        current : SegmentSet
            Segment set for the current.
        current_inlet : SegmentSet
            Segment set for input voltage or input current
            when CIRCTYP.EQ.2/3/12/22 and CIRCTYP.EQ 1/11/21 respectively.
        current_outlet : SegmentSet
            Segment set for output voltage or output current when
            CIRCTYP = 2/3/12/22 and CIRCTYP = 1/11/21 respectively.
        """
        self.current_id = current.create(self.stub)
        self.inlet_id = current_inlet.create(self.stub)
        self.outlet_id = current_outlet.create(self.stub)

    def create(self):
        """Create circuit."""
        ret = self.stub.CreateEMCircuit(
            EMCircuitRequest(
                circtyp=self.circuit_type,
                lcid=self.lcid,
                sidcurr=self.current_id,
                sidvin=self.inlet_id,
                sidvout=self.outlet_id,
            )
        )
        self.id = ret.id
        logging.info(f"EM Circuit {self.id} Created...")


class EMContactType(Enum):
    NODE_TO_NODE_BASED_ON_CONSTRAINTS = -1
    NODE_TO_NODE_PENALTY_BASED_CONTACT = 0
    DISCRETE_MORTAR_PENALTY_CONTACT = 1


class EMContact:
    """Detect contact between conductors.If no contact parts defined,
    contact detection between all active parts associated
    with a conducting material."""

    def __init__(self, contact_type=EMContactType.NODE_TO_NODE_PENALTY_BASED_CONTACT):
        self.stub = DynaBase.get_stub()
        self.contacttype = contact_type.value

    def create(self):
        """Create EM contact."""
        self.stub.CreateEMControlContact(EMControlContactRequest(emct=1, cconly=0, ctype=self.contacttype, dtype=0))
        logging.info("EM Contact Created...")


class Isopotential:
    """Defining an isopotential, i.e. constrain nodes so that they have the same scalar potential value.

    Parameters
    ----------
    set : Set
        Segment Set or Node Set.
    """

    def __init__(self, set=None):
        self.stub = DynaBase.get_stub()
        self.set = set
        self.id = 0

    def create(self):
        """Create Isopotential."""
        id, settype = 0, 1
        if self.set is not None:
            id = self.set.create(self.stub)
            type = self.set.type
            if type == "NODESET":
                settype = 2
            elif type == "SEGMENTSET":
                settype = 1
        ret = self.stub.CreateEMIsopotential(EMIsopotentialRequest(settype=settype, setid=id))
        self.id = ret.id
        logging.info(f"EM Isopotential {self.id} Created...")
        return self.id
