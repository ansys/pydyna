"""
ICFD API
========

Module for creating an ICFD (incompressible computational fluid dynamics) DYNA input deck.
"""

import logging

from .dynabase import *  # noqa : F403


class DynaICFD(DynaBase):
    """Contains methods for create a keyword related to an ICFD analysis."""

    def __init__(self):
        DynaBase.__init__(self)
        self.create_section_icfd(1)
        self.timestep = 0
        self.termination = 1e28

    def set_termination(self, termination_time):
        """Set the total time of the simulation for the fluid problem.

        Parameters
        ----------
        termination_time : float
            Total time of the simulation for the fluid problem.
        """
        self.termination = termination_time

    def create_control_general(self, atype=0, mtype=0, dvcl=0, rdvcl=0):
        """Specify the type of CFD analysis.

        Parameters
        ----------
        atype : int, optional
            Analysis type. The default is ``0``.
        mtype : int, optional
            Solving method type. The default is ``0``.
        dvcl : int, optional
            Flag for divergence cleaning. The default is ``0``.
        rdvcl : int, optional
            Flag for remeshing divergence cleaning. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateControlGeneral(
            ICFDControlGeneralRequest(atype=atype, mtype=mtype, dvcl=dvcl, rdvcl=rdvcl)
        )
        logging.info("ICFD control general Created...")
        return ret

    def create_control_output(self, msgl):
        """Modify default values for screen and file outputs related to the fluid solver only.

        Parameters
        ----------
        msgl : int
            Message level.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateControlOutput(ICFDControlOutputRequest(msgl=msgl))
        logging.info("ICFD control output Created...")
        return ret

    def create_control_turbulence(self, tmod):
        """Modify the default values for the turbulence model.

        Parameters
        ----------
        tmod : int
            Turbulence model to use.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateControlTurbulence(ICFDControlTurbulenceRequest(tmod=tmod))
        logging.info("ICFD control turbulence Created...")
        return ret

    def create_control_dem_coupling(self, ctype=0, bt=0, dt=1e28, sf=1):
        """Activate coupling between the ICFD and DEM solvers.

        Parameters
        ----------
        ctype : int, optional
            Coupling direction to the solver. The default is ``0``.
        bt : float, optional
            Birth time for the DEM coupling. The default is ``0``.
        dt : float, optional
            Death time for the DEM coupling. The default is ``1e28``.
        sf : float, optional
            Scale factor to apply to the force transmitted by the fluid to the structure.
            The default is ``1``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateControlDEMCoupling(ICFDControlDEMCouplingRequest(ctype=ctype, bt=bt, dt=dt, sf=sf))
        logging.info("ICFD control dem coupling Created...")
        return ret

    def create_section_icfd(self, sid):
        """Define a section for the ICFD solver.

        Parameters
        ----------
        sid : int
            Section ID.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateSection(ICFDSectionRequest(sid=sid))
        logging.info("ICFD Section Created...")
        return ret

    def create_part_icfd(self, pid, secid, mid):
        """Define parts for the ICFD solver.

        Parameters
        ----------
        pid : int
            Part ID for fluid surfaces.
        secid : int
            Section ID defined with the ``\*ICFD_SECTION`` card.
        mid : int
            Material ID defined with the ``\*ICFD_MAT`` card.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreatePart(ICFDPartRequest(pid=pid, secid=secid, mid=mid))
        logging.info("ICFD part Created...")
        return ret

    def create_solver_tol_mmov(self, atol=1e-8, rtol=1e-8):
        """Change the default tolerance values for the mesh movement algorithm.

        Parameters
        ----------
        atol : float, optional
            Absolute convergence criteria. The default is ``1e-8``.
        rtol : float, optional
            Relative convergence criteria. The default is ``1e-8``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ICFDCreateSolverTolMMOV(ICFDSolverTolMMOVRequest(atol=atol, rtol=rtol))
        logging.info("tolerance values for the mesh movement algorithm changed...")
        return ret

    def set_initial(self, velocity=Velocity(0, 0, 0), temperature=0, pressure=0):
        """Assign the initial condition to all nodes at once.

        Parameters
        ----------
        velocity : Velocity, optional
            Initial velocity. The default is ``(0, 0, 0)``,
        temperature : float, optional
            Initial temperature. The default is ``0``.
        pressure : float, optional
            Initial pressure. The default is ``0``.

        """
        ret = self.stub.ICFDCreateInit(
            ICFDInitRequest(pid=0, vx=velocity.x, vy=velocity.y, vz=velocity.z, t=temperature, p=pressure)
        )
        logging.info("ICFD_INIT Created...")
        return ret

    def set_imposed_move(self, vx=None, vy=None, vz=None):
        """Impose a velocity on the whole volume mesh.

        Parameters
        ----------
        vx :
        vy :

        """
        lcvx, lcvy, lcvz = 0, 0, 0
        if vx != None:
            vx.create(self.stub)
            lcvx = vx.id
        if vy != None:
            vy.create(self.stub)
            lcvy = vy.id
        if vz != None:
            vz.create(self.stub)
            lcvz = vz.id
        ret = self.stub.ICFDCreateControlImposedMove(
            ICFDControlImposedMoveRequest(pid=0, lcvx=lcvx, lcvy=lcvy, lcvz=lcvz)
        )
        return ret

    def save_file(self):
        """Save keyword files."""
        self.create_section_icfd(1)
        DynaBase.save_file(self)


class ICFD_SurfRemeshMethod(Enum):
    LAPLACIAN_SMOOTHING = 1
    CURVATURE_PRESERVING = 2


class ICFD_AnalysisType(Enum):
    TURNOFF_ICFD_SOLVER = -1
    TRANSIENT_ANALYSIS = 0
    STEADY_STATE_ANALYSIS = 1


class ICFD_MessageLevel(Enum):
    TIMESTEP_INFORMATION = 0
    FULL_OUTPUT_INFORMATION = 4


class ICFD_CouplingForm(Enum):
    FORCE_BASED_ON_VELOCITY_DRAG_VALUE = 0
    FORCE_USING_FLUID_PRESSURE_GRADIENT = 1


class ICFD_CouplingDirection(Enum):
    TWO_WAY_COUPLING = 0
    ONE_WAY_COUPLING_MECHANICS_TRANS_DISPLACEMENT_TO_FLUID = 1
    ONE_WAY_COUPLING_FLUID_TRANS_STRESS_TO_SOLID = 2
    TWO_WAY_WEAK_COUPLING = 3


class ICFDAnalysis:
    """Activates an ICFD analysis and defines associated control parameters."""

    def __init__(self):
        self.defined_timestep = False
        self.defined_volumemesh = False
        self.defined_surfmesh = False
        self.defined_type = False
        self.defined_output = False
        self.defined_fsi = False
        self.defined_steady_state = False
        self.defined_coupling_dem = False
        self.defined_mesh_adapt = False
        self.stub = DynaBase.get_stub()

    def set_type(self, analysis_type=ICFD_AnalysisType.TRANSIENT_ANALYSIS):
        """Set the type of the CFD analysis.

        Parameters
        ----------
        analysis_type : ICFD_AnalysisType
            Analysis type. The default is ``TRANSIENT_ANALYSIS``.
        """
        self.defined_type = True
        self.atype = analysis_type.value

    def set_output(self, messagelevel=ICFD_MessageLevel.TIMESTEP_INFORMATION, iteration_interval=0):
        """Modify default values for screen and file outputs related to the fluid solver only.

        Parameters
        ----------
        messagelevel : ICFD_MessageLevel, optional
            Message level. The default is ``TIMESTEP_INFORMATION``.
        iteration_interval : int, optional
            Iteration interval to print the output at. The default is ``0``.
        """
        self.defined_output = True
        self.msgl = messagelevel.value
        self.itout = iteration_interval

    def set_fsi(self, couplingdir=ICFD_CouplingDirection.TWO_WAY_COUPLING):
        """Modify default values for the fluid-structure interaction coupling algorithm.

        Parameters
        ----------
        couplingdir : ICFD_CouplingDirection, optional
            Coupling direction to the solver. The default is ``TWO_WAY_COUPLING``.
        """
        self.defined_fsi = True
        self.owc = couplingdir.value

    def set_steady_state(
        self,
        max_iteration=1e6,
        momentum_tol_limit=1e-3,
        pressure_tol_limit=1e-3,
        temperature_tol_limit=1e-3,
        velocity_relax_param=0.3,
        pressure_relax_param=0.7,
    ):
        """Set convergence options for the steady state solver.

        Parameters
        ----------
        max_iteration : int, optional
            Maximum number of iterations to reach convergence. The default is
            ``1000000.0``.
        momentum_tol_limit : float, optional
            Tolerance limits for the momentum equations. The default is ``0.001``.
        pressure_tol_limit : float, optional
            Tolerance limits for the pressure equations. The default is ``0.001``.
        temperature_tol_limit : float, optional
            Tolerance limits for the temperature equations. The default is ``0.001``.
        velocity_relax_param : float, optional
            Relaxation parameters for the velocity.  The default is ``0.3``.
        pressure_relax_param : float, optional
            Relaxation parameters for the pressure.  The default is ``0.7``.
        """
        self.defined_steady_state = True
        self.its = max_iteration
        self.tol1 = momentum_tol_limit
        self.tol2 = pressure_tol_limit
        self.tol3 = temperature_tol_limit
        self.rel1 = velocity_relax_param
        self.rel2 = pressure_relax_param

    def set_timestep(self, timestep=0):
        """Set the time step for the fluid problem.

        Parameters
        ----------
        dt : float, optional
            Time step for the fluid problem. The default is ``0``.
        """
        self.defined_timestep = True
        self.timestep = timestep

    def set_volume_mesh(self, mesh_growth_scale_factor=1.41):
        """Modify the default value for automatic volume mesh generation.

        Parameters
        ----------
        mesh_growth_scale_factor : float, optional
            Maximum mesh size that the volume mesher is allowed to use when generating
            the volume mesh. The default is ``1.41``.
        """
        self.defined_volumemesh = True
        self.mgsf = mesh_growth_scale_factor

    def set_mesh_adaptivity(self, min_mesh_size=0, max_mesh_size=0, max_perceptual_error=0, num_iteration=0):
        """Activate the adaptive mesh refinement feature.

        Parameters
        ----------
        min_mesh_size : float, optional
            Minimum mesh size for the mesh generator. The default is ``0``.
        max_mesh_size : float, optional
            Maximum mesh size. The default is ``0``.
        max_perceptual_error : float, optional
            Maximum perceptual error allowed in the whole domain. The default
            is ``0``.
        num_iteration : int, optional
            Number of iterations before a forced remeshing. The default is
            ``0``.
        """
        self.defined_mesh_adapt = True
        self.minh = min_mesh_size
        self.maxh = max_mesh_size
        self.err = max_perceptual_error
        self.nit = num_iteration

    def set_surface_mesh(self, remesh_method=ICFD_SurfRemeshMethod.LAPLACIAN_SMOOTHING):
        """Enable automatic surface remeshing.

        Parameters
        ----------
        remesh_method : ICFD_SurfRemeshMethod, optional
            Whether to perform a surface remeshing. The default is ``LAPLACIAN_SMOOTHING``.
        """
        self.defined_surfmesh = True
        self.rsrf = remesh_method.value

    def set_coupling_dem(
        self,
        coupling_type=0,
        birth_time=0,
        death_time=1e28,
        scale_factor=1,
        formulation=ICFD_CouplingForm.FORCE_BASED_ON_VELOCITY_DRAG_VALUE,
    ):
        """Activate coupling between the ICFD and DEM solvers.

        Parameters
        ----------
        coupling_type : int, optional
            Coupling direction to the solver. The default is ``0``.
        birth_time : float, optional
            Birth time for the DEM coupling. The default is ``0``.
        death_time : float, optional
            Death time for the DEM coupling.  The default is ``1e+28``.
        scale_factor : float, optional
            Scale factor applied to the force transmitted by the fluid to
            the structure. The default is ``1``.
        formulation : int, optional
            Type of formulation to use in the coupling. The default is
            ``FORCE_BASED_ON_VELOCITY_DRAG_VALUE``.
        """
        self.defined_coupling_dem = True
        self.ctype = coupling_type
        self.bt = birth_time
        self.dt = death_time
        self.sf = scale_factor
        self.form = formulation.value

    def create(self):
        """Create ICFD analysis."""
        if self.defined_timestep:
            self.stub.ICFDCreateControlTime(ICFDControlTimeRequest(tim=DynaSolution.termination_time, dt=self.timestep))
        if self.defined_volumemesh:
            self.stub.ICFDCreateControlMesh(ICFDControlMeshRequest(mgsf=self.mgsf))
        if self.defined_surfmesh:
            self.stub.ICFDCreateControlSurfMesh(ICFDControlSurfMeshRequest(rsrf=self.rsrf))
        if self.defined_type:
            self.stub.ICFDCreateControlGeneral(ICFDControlGeneralRequest(atype=self.atype, mtype=0, dvcl=0, rdvcl=0))
        if self.defined_output:
            self.stub.ICFDCreateControlOutput(ICFDControlOutputRequest(msgl=self.msgl, itout=self.itout))
        if self.defined_fsi:
            self.stub.ICFDCreateControlFSI(ICFDControlFSIRequest(owc=self.owc))
        if self.defined_steady_state:
            self.stub.ICFDCreateControlSteady(
                ICFDControlSteadyRequest(
                    its=self.its, tol1=self.tol1, tol2=self.tol2, tol3=self.tol3, rel1=self.rel1, rel2=self.rel2
                )
            )
        if self.defined_coupling_dem:
            self.stub.ICFDCreateControlDEMCoupling(
                ICFDControlDEMCouplingRequest(ctype=self.ctype, bt=self.bt, dt=self.dt, sf=self.sf, form=self.form)
            )
        if self.defined_mesh_adapt:
            self.stub.ICFDCreateControlAdapt(
                ICFDControlAdaptRequest(minh=self.minh, maxh=self.maxh, err=self.err, nit=self.nit)
            )


class Compressible(Enum):
    VACUUM = 0
    FULLY_INCOMPRESSIBLE_FLUID = 1


class MatICFD:
    """Defines physical properties for the fluid material.

    Parameters
    ----------
        flag : int
            Flag for chooseing between fully incompressible, slightly compressible, or barotropic flows.
            The default is ``FULLY_INCOMPRESSIBLE_FLUID``. Options are:

            - EQ.0: Vacuum (free surface problems only)
            - EQ.1: Fully incompressible fluid

        flow_density : float, optional
            Flow density. The default is ``0``.
        dynamic_viscosity : float, optional
            Dynamic viscosity. The default is ``0``.
        heat_capacity :
        thermal_conductivity :
        thremal_expansion_coefficent :
    """

    def __init__(
        self,
        flag=Compressible.FULLY_INCOMPRESSIBLE_FLUID,
        flow_density=0,
        dynamic_viscosity=0,
        heat_capacity=0,
        thermal_conductivity=0,
        thermal_expansion_coefficient=0,
    ):
        self.stub = DynaBase.get_stub()
        self.flag = flag.value
        self.flow_density = flow_density
        self.dynamic_viscosity = dynamic_viscosity
        self.hc = heat_capacity
        self.tc = thermal_conductivity
        self.beta = thermal_expansion_coefficient

    def create(self, stub):
        """Create an ICFD material."""
        ret = self.stub.ICFDCreateMat(
            ICFDMatRequest(
                flg=self.flag, ro=self.flow_density, vis=self.dynamic_viscosity, hc=self.hc, tc=self.tc, beta=self.beta
            )
        )
        self.material_id = ret.id
        logging.info(f"ICFD material {self.material_id} Created...")


class ICFDDOF(Enum):
    X = 1
    Y = 2
    Z = 3


class Vel(Enum):
    LINEAR_VELOCITY = 1
    ANGULAR_VELOCITY = 2


class ICFDPart:
    """Defines a part for the ICFD solver."""

    def __init__(self, id):
        self.stub = DynaBase.get_stub()
        self.type = "ICFD"
        self.id = id
        self.secid = 1
        self.mid = 0

    def set_material(self, mat):
        """Set a material."""
        mat.create(self.stub)
        self.mid = mat.material_id

    def set_prescribed_velocity(self, motion, dof=ICFDDOF.X, velocity_flag=Vel.LINEAR_VELOCITY):
        """Impose the fluid velocity on the boundary.

        Parameters
        ----------
        dof : int, optional
            Applicable degrees of freedom. The default is ``ICFDDOF.X``.
            Options are:

            - EQ.1: x-degree of freedom
            - EQ.2: y-degree of freedom
            - EQ.3: z-degree of freedom
            - EQ.4: Normal direction degree of freedom

        velocity_flag : int, optional
            Velocity flag. The default is ``LINEAR_VELOCITY``. Options are:

            - EQ.1: Linear velocity
            - EQ.2: Angular velocity
            - EQ.3: Parabolic velocity profile
            - EQ.4: Activates synthetic turbulent field on part

        """
        motion.create(self.stub)
        lcid = motion.id
        ret = self.stub.ICFDCreateBdyPrescribedVel(
            ICFDBdyPrescribedVelRequest(pid=self.id, dof=dof.value, vad=velocity_flag.value, lcid=lcid)
        )
        logging.info("ICFD boundary prescribed velocity Created...")

    def set_prescribed_pressure(self, pressure):
        """Impose a fluid pressure on the boundary.

        Parameters
        ----------
        pressure : Curve
            Load curve to describe the pressure value versus time.
        """
        pressure.create(self.stub)
        lcid = pressure.id
        ret = self.stub.ICFDCreateBdyPrescribedPre(ICFDBdyPrescribedPreRequest(pid=self.id, lcid=lcid))
        logging.info("ICFD boundary prescribed pressure Created...")
        return ret

    def set_prescribed_temperature(self, temperature):
        """Impose a fluid temperature on the boundary.

        Parameters
        ----------
        temperature : Curve
            Load curve to describe the temperature value versus time.
        """
        temperature.create(self.stub)
        lcid = temperature.id
        ret = self.stub.ICFDCreateBdyPrescribedTemp(ICFDBdyPrescribedTempRequest(pid=self.id, lcid=lcid))
        logging.info("ICFD boundary prescribed temperature Created...")
        return ret

    def set_free_slip(self):
        """Specify the fluid boundary with a free-slip boundary condition."""
        ret = self.stub.ICFDCreateBdyFreeSlip(ICFDBdyFreeSlipRequest(pid=self.id))
        logging.info("ICFD boundary freeslip Created...")
        return ret

    def set_non_slip(self):
        """Specify the fluid boundary with a non-slip boundary condition."""
        ret = self.stub.ICFDCreateBdyNonSlip(ICFDBdyNonSlipRequest(pid=self.id))
        logging.info("ICFD boundary nonslip Created...")
        return ret

    def set_fsi(self):
        """Define the fluid surface to consider in contact with the solid surfaces
        for fluid-structure interaction (FSI) analysis."""
        ret = self.stub.ICFDCreateBdyFSI(ICFDBdyFSIRequest(pid=self.id))
        logging.info("ICFD boundary FSI Created...")
        return ret

    def compute_drag_force(self):
        """Enable the computation of drag forces over given surface parts of the model."""
        ret = self.stub.ICFDCreateDBDrag(ICFDDBDragRequest(pid=self.id))
        logging.info("ICFD database drag Created...")
        return ret

    def compute_flux(self):
        """Enable the computation of the flow rate and average pressure over given parts of the model."""
        ret = self.stub.ICFDCreateDBFlux(ICFDDBFluxRequest(pid=self.id))
        logging.info("ICFD database flux Created...")
        return ret

    def compute_temperature(self):
        """Enable the computation of the average temperature and the heat flux over given parts of the model."""
        ret = self.stub.ICFDCreateDBTemp(ICFDDBTempRequest(pid=self.id))
        logging.info("ICFD database temperature Created...")
        return ret

    def set_boundary_layer(self, number=3):
        """Define a boundary layer mesh as a refinement on the volume mesh.

        Parameters
        ----------
        number : int, optional
            Number of elements normal to the surface (in the boundary layer).
            The default is ``3.``
        """
        ret = self.stub.MESHCreateBl(MeshBlRequest(pid=self.id, nelth=number - 1))
        logging.info("MESH boundary-layer Created...")
        return ret

    def set_boundary_layer_symmetry_condition(self):
        """Specify the part that is to have symmetry conditions for the boundary layer."""
        ret = self.stub.MESHCreateBlSym(MeshBlSymRequest(pid=self.id))
        return ret

    def set_imposed_move(self, vx=None, vy=None, vz=None):
        """Impose a velocity on a specific ICFD part.

        Parameters
        ----------
        vx :
        vy :
        vz :

        """
        lcvx, lcvy, lcvz = 0, 0, 0
        if vx != None:
            vx.create(self.stub)
            lcvx = vx.id
        if vy != None:
            vy.create(self.stub)
            lcvy = vy.id
        if vz != None:
            vz.create(self.stub)
            lcvz = vz.id
        ret = self.stub.ICFDCreateControlImposedMove(
            ICFDControlImposedMoveRequest(pid=self.id, lcvx=lcvx, lcvy=lcvy, lcvz=lcvz)
        )
        return ret

    def set_property(self):
        """Set properties for an ICFD part."""
        secid = 1
        self.stub.SetICFDPartProperty(ICFDPartPropertyRequest(pid=self.id, secid=secid, mid=self.mid))


class ICFDVolumePart:
    """Assigns material properties to the nodes enclosed by surface ICFD parts.

    Parameters
    ----------
    surfaces : list
        List of part IDs for the surface elements that define the volume mesh.
    """

    def __init__(self, surfaces):
        self.stub = DynaBase.get_stub()
        self.type = "ICFDVOLUME"
        self.id = id
        self.secid = 1
        self.mid = 0
        self.surfaces = surfaces
        self.defined_imposed_move = False

    def set_material(self, mat):
        """Set a material."""
        self.mid = mat.material_id

    def set_imposed_move(self, vx=None, vy=None, vz=None):
        """Impose a velocity on a specific ICFD part.

        Parameters
        ----------
        vx :
        vy :
        vz :

        """
        self.defined_imposed_move = True
        self.vx = vx
        self.vy = vy
        self.vz = vz

    def create(self):
        """Create an ICFD volume part."""
        ret = self.stub.ICFDCreatePartVol(ICFDPartVolRequest(secid=1, mid=self.mid, spids=self.surfaces))
        self.id = ret.id
        if self.defined_imposed_move:
            lcvx, lcvy, lcvz = 0, 0, 0
            if self.vx != None:
                self.vx.create(self.stub)
                lcvx = self.vx.id
            if self.vy != None:
                self.vy.create(self.stub)
                lcvy = self.vy.id
            if self.vz != None:
                self.vz.create(self.stub)
                lcvz = self.vz.id
            ret = self.stub.ICFDCreateControlImposedMove(
                ICFDControlImposedMoveRequest(pid=self.id, lcvx=lcvx, lcvy=lcvy, lcvz=lcvz)
            )
        logging.info(f"ICFD part volume {self.id} Created...")
        return ret


class MeshedVolume:
    """Defines the volume space to mesh.

    Parameters
    ----------
    surfaces : list
        List of part IDs for the surface elements to use to define the volume.
    """

    def __init__(self, surfaces):
        self.surfaces = surfaces
        self.stub = DynaBase.get_stub()
        self.meshsizeshape = []
        self.embeded_surf = []
        self.meshsize_surf = []
        self.fluid_interfaces = []

    def embed_shell(self, embeded):
        """Define surfaces that the mesher is to embed inside the volume mesh.

        Parameters
        ----------
        embeded : list
            List of part IDs for the surface elements.
        """
        self.embeded_surf = embeded

    def meshsize_box(self, size, min_point, max_point):
        """Define a local mesh size in specific zones corresponding to given geometrical shapes.

        Parameters
        ----------
        size : float
            Mesh size to apply in the zone of the shape defined by ``SNAME``.
        parameter : list
            List of the parameters that define the shape.
        """
        parameter = [min_point.x, min_point.y, min_point.z, max_point.x, max_point.y, max_point.z]
        self.meshsizeshape.append(["BOX", size, parameter])

    def set_meshsize(self, surfaces):
        """Define the surfaces that the mesher is to use to specify a local mesh size inside the volume.

        Parameters
        ----------
        surfaces : list
            List of part IDs for the surface elements.
        """
        self.meshsize_surf = surfaces

    def set_fluid_interfaces(self, surfaces):
        """Define the surfaces that the mesher is to use to specify fluid interfaces in multi-fluid simulations.

        Parameters
        ----------
        surfaces : list
            List of part IDs for the surface elements.
        """
        self.fluid_interfaces = surfaces

    def create(self):
        """Create mesh volume."""
        ret = self.stub.MESHCreateVolume(MeshVolumeRequest(pids=self.surfaces))
        self.id = ret.id
        logging.info(f"MESH volume {self.id} Created...")
        if len(self.embeded_surf) > 0:
            self.stub.MESHCreateEmbedShell(MeshEmbedShellRequest(volid=self.id, pids=self.embeded_surf))
            logging.info("Embed surfaces Created...")
        if len(self.meshsize_surf) > 0:
            self.stub.MESHCreateSize(MeshSizeRequest(volid=self.id, pids=self.meshsize_surf))
            logging.info("Mesh size surfaces Created...")
        if len(self.fluid_interfaces) > 0:
            self.stub.MESHCreateInterf(MeshInterfRequest(volid=self.id, pids=self.fluid_interfaces))
            logging.info("Mesh fluid interfaces Created...")
        for i in range(len(self.meshsizeshape)):
            self.stub.MESHCreateSizeShape(
                MeshSizeShapeRequest(
                    sname=self.meshsizeshape[i][0],
                    force=1,
                    method=0,
                    msize=self.meshsizeshape[i][1],
                    parameter=self.meshsizeshape[i][2],
                )
            )
        logging.info("MESH size shape Created...")
