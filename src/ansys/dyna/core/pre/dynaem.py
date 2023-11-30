"""
EM API
======

Module for creating an electromagnetism (EM) input deck.
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
    """Contains methods for creating keywords related to EM."""

    def __init__(self):
        DynaBase.__init__(self)
        self.analysis = EMAnalysis()

    def create_em_control(self, emsol=0, numls=100, macrodt=0, ncylfem=5000, ncylbem=5000):
        """Enable the EM solver and set its options.

        Parameters
        ----------
        emsol : int, optional
           EM solver. The default is ``0``. Options are:

           - EQ.-1: Turn off the EM solver after reading the EM keywords.
           - EQ.1: Eddy current solver.
           - EQ.2: Induced heating solver.
           - EQ.3: Resistive heating solver.
           - EQ.11: Electrophysiology monodomain.
           - EQ.12: Electrophysiology bidomain.
           - EQ.13: Electrophysiology monodomain coupled with bidomain.

        numls : int, optional
            Number of local EM steps in a whole period for when ``emsol = 2``.
            The default is ``100``.
        macrodt : int, optional
            Macro time step for when ``emsol = 2``. The default is ``0``.
        ncylfem : int, optional
            Number of electromagnetism cycles between the recalculation of FEM matrices.
            The default is ``5000``.
        ncylbem : int, optional
            Number of electromagnetism cycles between the recalculation of BEM matrices.
            The default is ``5000``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
           Time step type. Options are:

           - EQ.1: Constant time step given in DTCONST
           - EQ.2: Time step as a function of time given by a load curve specified in LCID
           - EQ.3: Automatic time step computation, depending on the solver type

           This time step is then multiplied by FACTOR.
        dtconst : float
            Constant value for the time step for when ``tstype = 1``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMTimestep(EMTimestepRequest(tstype=tstype, dtconst=dtconst))
        logging.info("EM Timestep Created...")
        return ret

    def create_em_contact(self, contid=0, dtype=0, psidm=0, psids=0, eps1=0.3, eps2=0.3, eps3=0.3, d0=0):
        """Create an optional card for defining options on electromagnetic contacts between two sets of parts.

        Parameters
        ----------
        contid : int, optional
            Electromagnetic contact ID. The default is ``0``.
        dtype : int, optional
            Detection type.  The default is ``0``. Options are:

            - EQ.0: Contact type 0
            - EQ.1: Contact type 1

        psidm : int, optional
            Master part set ID. The default is ``0``.
        psids : int
            Slave part set ID.  The default is ``0``.
        eps1 : float, optional
            First contact coefficient for contact detection conditions.
            The default is ``0.3``.
        eps2 : float, optional
            Second contact coefficient for contact detection conditions.
            The default is ``0.3``.
        eps3 : float, optional
            Third contact coefficient for contact detection conditions.
            The default is ``0.3``.
        d0 : float, optional
            Contact condition 3 when ``dtype = 1``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
        """Define Rogowsky coils to measure a global current versus time through a segment set or a node set.

        Parameters
        ----------
        rogid : int
            Rogowsky coil ID.
        setid : int
            Segment or node set ID.
        settype : int, optional
            Type of set. The default is ``1``. Options are:

            - EQ.1: Segment set
            - EQ.2: Node set

        curtyp : int
            Type of current measured. The default is ``1``. Options are:

            - EQ.1: Volume current
            - EQ.2: Surface current (not available yet}
            - EQ.3: Magnetic field flow (B field times area)

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        segmentset.create(self.stub)
        setid = segmentset.id
        ret = self.stub.CreateEMCircuitRogo(EMCircuitRogoRequest(setid=setid, settype=settype, curtyp=curtyp))
        logging.info("EM Circuit Rogo Created...")
        return ret.id

    def create_em_mat001(self, mid, mtype, sigma):
        """Create an electromagnetic material type and set properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Electromagnetism type of the material. Options are:

            - EQ.0: Air or vacuum
            - EQ.1: Insulator material (These materials have the same electromagnetism behavior as EQ.0.)
            - EQ.2: Conductor carrying a source
            - EQ.3: Fluid conductor
            - EQ.4: Conductor not connected to any current or voltage source, where the eddy current problem is solved

        sigma : float
            Initial electrical conductivity of the material.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMMat001(EMMat001Request(mid=mid, mtype=mtype, sigma=sigma))
        logging.info("EM Material 001 Created...")
        return ret

    def create_em_mat002(self, mid, mtype, sigma, eosid, murel):
        """Create an electromagnetic material type and set properties
        whose permeability is different than the free space permeability.

        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Electromagnetism type of the material. Options are:

            - EQ.0: Air or vacuum
            - EQ.1: Insulator material (These materials have the same electromagnetism behavior as EQ.0.)
            - EQ.2: Conductor carrying a source
            - EQ.4: Conductor not connected to any current or voltage source, where the wddy current problem is solved

        sigma : float
            Initial electrical conductivity of the material.
        eosid : int
            ID of the EOS to use for the electrical conductivity.
        murel : float
            Relative permeability, which is the ratio of the permeability of a specific
            medium to the permeability of the free space.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMMat002(EMMat002Request(mid=mid, mtype=mtype, sigma=sigma, eosid=eosid, murel=murel))
        logging.info("EM Material 002 Created...")
        return ret

    def create_em_solver_fembem_monolithic(self, mtype=0, stype=0, abstol=1e-6, reltol=1e-4, maxit=500):
        """Turn on the monolithic FEM-BEM solver.

        Parameters
        ----------
        mtype : int, optional
            Monolithic solver type. The default is ``0``. The only option is EQ.0: Direct symmetric solver.
        stype : int
            Solver type. The default is ``0``. Options are:

            - EQ.0: MINRES iterative solver
            - EQ.1: GMRES iterative solver

        abstol : float, optional
            Absolute tolerance. The default is ``1e-6``.
        reltol : float, optional
            Relative tolerance. The default is ``1e-4``.
        maxit : int, optional.
            Maximum number of iterations. The default is 500.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMSolverFemBemMonolithic(
            EMSolverFemBemMonolithicRequest(mtype=mtype, stype=stype, abstol=abstol, reltol=reltol, maxit=maxit)
        )
        logging.info("EM Solver FEMBEM Monolithic Created...")
        return ret

    def create_em_output(self, mats=0, matf=0, sols=0, solf=0):
        """Define the level of EM-related output on the screen and in the message file.

        Parameters
        ----------
        mats : int, optional
            Level of matrix assembly output to show on the screen. The default is ``0``.
            Options are:

            - EQ.0: No output
            - EQ.1: Basic assembly steps
            - EQ.2: Basic assembly steps + percentage completed + final statistics
            - EQ.3: Basic assembly steps + percentage completed + statistics at each percentage of completion

        matf : int, optional
            Level of matrix assembly output to write to the message file. The default
            is ``0``. Options are:

            - EQ.0: No output
            - EQ.1: Basic assembly steps
            - EQ.2: Basic assembly steps + percentage completed + final statistics
            - EQ.3: Vasic assembly steps + percentage completed + statistics at each percentage of completion

        sols : int
            Level of solver output to show on the screen. The default is ``0``. Options are:

            - EQ.0: No output
            - EQ.1: Global information at each FEM iteration
            - EQ.2: Detailed information at each FEM iteration

        solf : int, optional
            Level of solver output to write to the message file. The default is ``0``.
            Options are:

            - EQ.0: No output
            - EQ.1: Global information at each FEM iteration
            0 EQ.2: Detailed information at each FEM iteration

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMOutput(EMOutputRequest(mats=mats, matf=matf, sols=sols, solf=solf))
        logging.info("EM Output Created...")
        return ret

    def connect_isopotential(
        self,
        contype=Isopotential_ConnType.SHORT_CIRCUIT,
        isopotential1=None,
        isopotential2=None,
        value=0,
        func=None,
        curve=None,
        inductance=0,
        capacity=0,
        initial_voltage=0,
    ):
        """Define a connection between two isopotentials or between an isopotential and the ground.

        Parameters
        ----------
        contype : Isopotential_ConnType
            Isopotential connection type. The default is ``SHORT_CIRCUIT``.
        isopotential1 : Isopotential
            First isopotential to connect.
        isopotential2 : Isopotential
            Second isopotential to connect.
        value : float, optional
            Value of the resistance, voltage, or current depending on the isopotential
            connection type. The default is ``0``.
        func  :
        curve :
        inductance :
        capacity :
        initial voltage :

        Returns
        -------
        int
            Connection ID.
        """
        contype = contype.value
        if contype == 6:
            l, c, v0 = inductance, capacity, initial_voltage
        else:
            l, c, v0 = 0, 0, 0
        if func is not None:
            lcid = -func.create(self.stub)
        elif curve is not None:
            lcid = curve.create(self.stub)
        else:
            lcid = 0
        if isopotential1 is not None:
            isoid1 = isopotential1.create()
        else:
            isoid1 = 0
        if isopotential2 is not None:
            isoid2 = isopotential2.create()
        else:
            isoid2 = 0
        ret = self.stub.CreateEMIsopotentialConnect(
            EMIsopotentialConnectRequest(
                contype=contype, isoid1=isoid1, isoid2=isoid2, val=value, lcid=lcid, l=l, c=c, v0=v0
            )
        )
        logging.info("Isopotential connection Created...")
        return ret.id

    def create_em_database_globalenergy(self, outlv=0):
        """Enable the output of global EM.

        Parameters
        ----------
        outlv : int, optional
            Flag for whether to generate the output file. The default is ``0``.
            Options are:

            - EQ.0: No output file is generated.
            - EQ.1: The output file is generated.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateEMDatabaseGlobalEnergy(EMDatabaseGlobalEnergyRequest(outlv=outlv))
        logging.info("EM Database Global Energy Created...")
        return ret

    def create_Permanent_magnet(self, id, partid, mtype, north, sourth, hc):
        """Create a permanent magnet.

        Parameters
        ----------
        id : int
            ID of the magnet.
        partid : int
            Part ID.
        mtype : int, optional
            Magnet definition type. Options are:

            - EQ.0: Magnet defined by two node set for the north and south poles
            - EQ.1: Magnet defined by two segments set for the north and south poles
            - EQ.3: Magnet defined by a global vector orientation
            - EQ.4: Magnet defined by a global vector orientation given by two node IDs

        north : int
            ID of the magnet's north face for ``mtype = 0`` and ``mtype = 1``.
        sourth : int
            ID of the magnet's south face for ``mtype = 0`` and ``mtype = 1``.
        hc : int
            Coercive force.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
            EM EOS ID.
        eostype : int
            Define the type of EOS. Options are:

            - EQ.1: Permeability defined by a B function of the H curve
            - EQ.2: Permeability defined by an H function of the B curve

        lcid : int
            Load curve ID.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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


class EMDimension(Enum):
    SOLVER_3D = 0
    PLANAR_2D = 1
    AXISYMMETRIC_2D = 3


class BEMSOLVER(Enum):
    DIRECT_SOLVER = 1
    PCG = 2


class FEMSOLVER(Enum):
    DIRECT_SOLVER = 1
    PCG = 2


class EMAnalysis:
    """Enables the EM solver and sets its options.

    Parameters
    ----------
    type : int
       Electromagnetism solver. The default is ``EDDY_CURRENT``. Options are:

       - EQ.1: Eddy current solver
       - EQ.2: Induced heating solver
       - EQ.3: Resistive heating solver
       - EQ.11: Electrophysiology monodomain
    """

    p_matrix_tol = 1e-6
    q_matrix_tol = 1e-6
    w_matrix_tol = 1e-6

    def __init__(self, type=EMType.EDDY_CURRENT):
        self.defined = False
        self.stub = DynaBase.get_stub()
        self.type = type.value
        self.dimtype = 0
        self.defined_bem = False
        self.defined_fem = False

    def set_timestep(self, timestep):
        """Set the EM time step and its evolution."""
        self.defined = True
        self.timestep = timestep

    def set_em_solver(self, type=EMType.EDDY_CURRENT, dimtype=EMDimension.SOLVER_3D):
        """Set the EM solver."""
        self.type = type.value
        self.dimtype = dimtype.value

    def set_solver_bem(self, solver=BEMSOLVER.PCG, relative_tol=1e-6, max_iteration=1000):
        """Set the type of linear solver, pre-conditioner, and tolerance for the EM BEM solver."""
        self.defined_bem = True
        self.bem_relative_tol = relative_tol
        self.max_iteration = max_iteration
        self.bemsolver = solver.value

    def set_solver_fem(self, solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-6, max_iteration=1000):
        """Set some parameters for the EM FEM solver."""
        self.defined_fem = True
        self.femsolver = solver.value
        self.fem_relative_tol = relative_tol
        self.max_iteration = max_iteration

    def set_bem_matrix_tol(self, p_matrix_tol=1e-6, q_matrix_tol=1e-6, w_matrix_tol=1e-6):
        """Set the type of BEM matrices and the way that they are assembled."""
        EMAnalysis.p_matrix_tol = p_matrix_tol
        EMAnalysis.q_matrix_tol = q_matrix_tol
        EMAnalysis.w_matrix_tol = w_matrix_tol

    def create(self):
        """Create an EM analysis."""
        if self.defined == False:
            return
        self.stub.CreateEMControl(
            EMControlRequest(emsol=self.type, numls=100, macrodt=0, dimtype=self.dimtype, ncylfem=5000, ncylbem=5000)
        )
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
    """Defines an electrical circuit.

    Parameters
    ----------
    circtyp : int
        Circuit type. Options are:

        - EQ.1: Imposed current vs time defined by a load curve
        - EQ.2: Imposed voltage vs time defined by a load curve

    loadcurve : Curve
        Load curve for when the ``circtyp`` parameter is set to ``1``,
        ``2``, ``21`` or ``22``.
    """

    def __init__(self, loadcurve, circuit_type=CircuitType.IMPOSED_CURRENT_VS_TIME):
        self.stub = DynaBase.get_stub()
        self.circuit_type = circuit_type.value
        loadcurve.create(self.stub)
        self.lcid = loadcurve.id

    def set_current(self, current, current_inlet, current_outlet):
        """Define the segment set for the current.

        Parameters
        ----------
        current : SegmentSet
            Segment set for the current.
        current_inlet : SegmentSet
            Segment set for input voltage or input current
            for CIRCTYP.EQ.2/3/12/22 and CIRCTYP.EQ 1/11/21 respectively.
        current_outlet : SegmentSet
            Segment set for the output voltage or output current for
            CIRCTYP = 2/3/12/22 and CIRCTYP = 1/11/21 respectively.
        """
        self.current_id = current.create(self.stub)
        self.inlet_id = current_inlet.create(self.stub)
        self.outlet_id = current_outlet.create(self.stub)

    def create(self):
        """Create a circuit."""
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
    """Detects contact between conductors.

    If no contact parts are defined, this method detects contact between
    all active parts associated with a conducting material.
    """

    def __init__(self, contact_type=EMContactType.NODE_TO_NODE_PENALTY_BASED_CONTACT):
        self.stub = DynaBase.get_stub()
        self.contacttype = contact_type.value

    def create(self):
        """Create an EM contact."""
        self.stub.CreateEMControlContact(EMControlContactRequest(emct=1, cconly=0, ctype=self.contacttype, dtype=0))
        logging.info("EM Contact Created...")


class EMRandlesLayer(Enum):
    DEFAULT = 0
    CURRENT_COLLECTOR_POSITIVE = 1
    POSITIVE_ELECTRODE = 2
    SEPARATOR = 3
    NEGATIVE_ELECTRODE = 4
    CURRENT_COLLECTOR_NEGATIVE = 5


class Isopotential:
    """Defines an isopotential.

    This method constrain nodes so that they have the same scalar potential value.

    Parameters
    ----------
    set : Set
        Segment set or node set.
    """

    isopotlist = []

    def __init__(self, set=None, layer=EMRandlesLayer.DEFAULT):
        self.stub = DynaBase.get_stub()
        self.set = set
        self.id = 0
        self.rdltype = layer.value

    def create(self):
        """Create an isopotential."""
        if self.set.type == "NODESETBOX":
            isoinfo = [self.set.type, self.set.boxes]
        else:
            isoinfo = [self.set.type, self.set.nodes]
        if isoinfo in Isopotential.isopotlist:
            pass
        id, settype = 0, 1
        if self.set is not None:
            id = self.set.create(self.stub)
            type = self.set.type
            if type == "NODESET" or type == "NODESETBOX":
                settype = 2
            elif type == "SEGMENTSET":
                settype = 1
        ret = self.stub.CreateEMIsopotential(EMIsopotentialRequest(settype=settype, setid=id, rdltype=self.rdltype))
        self.id = ret.id
        logging.info(f"EM Isopotential {self.id} Created...")
        return self.id


class RogoCoil:
    """Measures the total current flowing through a given section of the conductor.

    Parameters
    ----------
    set : Set
        Segment set.
    """

    def __init__(self, set=None):
        self.stub = DynaBase.get_stub()
        self.set = set
        self.id = 0

    def create(self):
        """Create a Rogowsky coil."""
        id, settype = 0, 1
        if self.set is not None:
            id = self.set.create(self.stub)
            type = self.set.type
            if type != "SEGMENTSET":
                return self.id
        ret = self.stub.CreateEMIsopotentialRogo(EMIsopotentialRogoRequest(settype=1, setid=id))
        self.id = ret.id
        logging.info(f"EM Isopotential Rogo {self.id} Created...")
        return self.id


class RandlesCellType(Enum):
    USER_DEFINED_EQUIVALENT_CIRCUIT_MODEL = -1
    RANDLES_CELL_0_ORDER = 0
    RANDLES_CELL_1_ORDER = 1
    RANDLES_CELL_2_ORDER = 2
    RANDLES_CELL_3_ORDER = 3


class RandlesCell:
    """Define parameters for a Randles Cell."""

    def __init__(self, set=None):
        self.stub = DynaBase.get_stub()
        self.define_batmac = False
        self.define_randles_short = False
        self.define_extra_heat_source = False

    def set_batmac_model(
        self,
        cell_type=RandlesCellType.RANDLES_CELL_1_ORDER,
        cell_parts=None,
        area=2,
        cell_capacity=0,
        soc_conversion_factor=0,
        charge_init_state=0,
        equilibrium_voltage=None,
        circuit_parameter=None,
        constant_temperature=0,
        temperature_from_thermal_solver=False,
        add_heating_to_thermal_solver=False,
    ):
        """define the distributed Randles circuit parameters for a Randles cell when using the batmac model."""
        self.define_batmac = True
        self.rdltype = cell_type.value
        self.rdlarea = area
        self.psid = cell_parts
        self.q = cell_capacity
        self.cq = soc_conversion_factor
        self.socinit = charge_init_state
        self.soctou = equilibrium_voltage
        self.prm = circuit_parameter
        self.temp = constant_temperature
        self.frther = temperature_from_thermal_solver
        self.r0toth = add_heating_to_thermal_solver

    def set_randles_short(self, resistances_func=None):
        """Define conditions to turn on a Randles short (replace one or several Randles circuits by resistances),
        and to define the value of the short resistance.

        Parameters
        ----------
        resistances_func : Function
            Define the local resistance function of local parameters for the local Randles circuit.

        """
        self.define_randles_short = True
        self.randles_short_function = resistances_func

    def set_extra_heat_source(self, heat_source_func=None):
        """Add an extra heat source term to the Randles circuit nodes in order to account for thermal runaway
        situations.

        Parameters
        ----------
        heat_source_func : Function
            Define the local heat source function of local parameters for the local Randles circuit.

        """
        self.define_extra_heat_source = True
        self.heat_source_func = heat_source_func

    def create(self):
        """Set parameter for Randles Cell."""
        if self.define_batmac:
            sid = 0
            soutouid = 0
            if self.psid is not None:
                sid = self.psid.create(self.stub)
            if self.soctou is not None:
                soutouid = self.soctou.create(self.stub)
            modified_prm = []
            for par in self.prm:
                if type(par) == float or type(par) == int:
                    modified_prm.append(par)
                elif type(par) == Table2D:
                    tid = par.create(self.stub)
                    modified_prm.append(-tid)
            while len(modified_prm) < 6:
                modified_prm.append(0)
            ret = self.stub.CreateEMRandlesBatmac(
                EMRandlesBatmacRequest(
                    rdltype=self.rdltype,
                    rdlarea=self.rdlarea,
                    psid=sid,
                    q=self.q,
                    cq=self.cq,
                    socinit=self.socinit,
                    soctou=soutouid,
                    chargedirparam=modified_prm,
                    temp=self.temp,
                    frther=self.frther,
                    r0toth=self.r0toth,
                )
            )
            self.id = ret.rdlid
            logging.info(f"EM Randles Batmac {self.id} Created...")
        if self.define_randles_short:
            fid = 0
            if self.randles_short_function is not None:
                fid = self.randles_short_function.create(self.stub)
            ret = self.stub.CreateEMRandlesShort(EMRandlesShortRequest(function=fid))
            logging.info(f"EM Randles Short Created...")
        if self.define_extra_heat_source:
            fid = 0
            if self.heat_source_func is not None:
                fid = self.heat_source_func.create(self.stub)
            ret = self.stub.CreateEMRandlesExothermicReaction(EMRandlesExothermicReactionRequest(function=fid))
            logging.info(f"EM Randles Exothermic Reaction Created...")
