"""
Airbag API
==========

Module for creating a Structural ALE DYNA input deck.
"""

from enum import Enum
import logging

from .dynabase import *  # noqa : F403


class AdvectionMethod(Enum):
    DONOR_CELL_WITH_HALF_INDEX_SHIFT = 1
    VAN_LEER_WITH_HIS = 2
    DONOR_CELL_WITH_HIS = 3
    FINITE_VOLUME_METHOD = 6


class FillDirection(Enum):
    INSIDE_THE_GEOMETRY = 0
    OUTSIDE_THE_GEOMETRY = 1


class ControlPoint:
    """Provide spacing information to generate a 3D structured ALE mesh.

    Parameters
    ----------
    number : int
        Control point node number.
    position : float
        Control point position.
    ratio : float
        Ratio for progressive mesh spacing.
    """

    def __init__(self, number, position, ratio):
        self.number = number
        self.position = position
        self.ratio = ratio


class StructuredMesh:
    """Generate a structured 2D or 3D mesh and invoke the Structured ALE (S-ALE) solver."""

    def __init__(self, control_points_x, control_points_y, control_points_z):
        self.stub = DynaBase.get_stub()
        self.control_points_x = control_points_x
        self.control_points_y = control_points_y
        self.control_points_z = control_points_z
        self.refine_factor_x = 1
        self.refine_factor_y = 1
        self.refine_factor_z = 1
        self.fillings = []

    def fill(
        self,
        material,
        geometry_type="NULL",
        nsample=4,
        define_geometry_parameters=[0, 0, 0, 0, 0],
        inout=FillDirection.INSIDE_THE_GEOMETRY,
        vid=0,
        reference_pressure=0,
    ):
        """Perform volume filling operations on a structured ALE mesh.

        Parameters
        ----------
        material_name : string
            material name.
        nsample : int
            Number of sampling points.
        geometry_type : string
            Geometry types. They are: PARTSET, PART, SEGSET, PLANE, CYLINDER, BOXCOR, BOXCPT and SPHERE.
        define_geometry_parameters : list
            These values have different definitions for different options.
        in_out : int
            To fill inside or outside of the geometry.
        vid : int
            This flag is used to assign initial velocity to material filling the domain.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.fillings.append(
            [
                material,
                geometry_type,
                nsample,
                define_geometry_parameters,
                inout,
                vid,
                reference_pressure,
            ]
        )

    def refine(self, refine_factor_x=1, refine_factor_y=1, refine_factor_z=1):
        """Refine existing structured ALE (S-ALE) meshes.

        Parameters
        ----------
        refine_factor_x/y/y : int
            Refinement factor for each local direction.
        """
        self.refine_factor_x = refine_factor_x
        self.refine_factor_y = refine_factor_y
        self.refine_factor_z = refine_factor_z

    def initial_detonation(self, detonation_point):
        """Define points to initiate the location of high explosive detonations.

        Parameters
        ----------
        detonation_point : Point
            x,y,z-coordinate of detonation point.
        """
        self.detonation_point = detonation_point

    def create(self):
        """Create mesh."""
        nx = []
        xx = []
        ratiox = []
        for i in range(len(self.control_points_x)):
            nx.append(self.control_points_x[i].number)
            xx.append(self.control_points_x[i].position)
            ratiox.append(self.control_points_x[i].ratio)
        ny = []
        xy = []
        ratioy = []
        for i in range(len(self.control_points_y)):
            ny.append(self.control_points_y[i].number)
            xy.append(self.control_points_y[i].position)
            ratioy.append(self.control_points_y[i].ratio)
        nz = []
        xz = []
        ratioz = []
        for i in range(len(self.control_points_z)):
            nz.append(self.control_points_z[i].number)
            xz.append(self.control_points_z[i].position)
            ratioz.append(self.control_points_z[i].ratio)

        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=nx, x=xx, ratio=ratiox)
        )
        cpidx = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=ny, x=xy, ratio=ratioy)
        )
        cpidy = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=nz, x=xz, ratio=ratioz)
        )
        cpidz = ret.cpid
        ret = self.stub.ALECreateStructuredMesh(
            ALECreateStructuredMeshRequest(nbid=2000001, ebid=2000001, cpidx=cpidx, cpidy=cpidy, cpidz=cpidz)
        )
        meshid = ret.meshid
        partid = ret.partid
        logging.info(f"ALE Structured mesh {meshid} Created...")

        for obj in self.fillings:
            material = obj[0]
            geometry_type = obj[1]
            nsample = obj[2]
            define_geometry_parameters = obj[3]
            inout = obj[4]
            vid = obj[5]
            reference_pressure = obj[6]
            material.create(self.stub)
            self.stub.ALECreateStructuredMultiMaterialGroup(
                ALECreateStructuredMultiMatGroupRequest(
                    nmmgnm=material.name,
                    mid=material.material_id,
                    eosid=material.eos_id,
                    pref=reference_pressure,
                )
            )
            logging.info(f"Material {material.name} Created...")
            if geometry_type.upper() != "NULL":
                self.stub.ALECreateStructuredMeshVolumeFilling(
                    ALECreateStructuredMeshVolumeFillingRequest(
                        mshid=meshid,
                        ammgto=material.name,
                        nsample=nsample,
                        geom=geometry_type.upper(),
                        vid=vid,
                        inout=inout.value,
                        e=define_geometry_parameters,
                    )
                )
                logging.info(f"Material {material.name} filled in Mesh {meshid}...")
        self.stub.ALECreateStructuredMeshRefine(
            ALECreateStructuredMeshRefineRequest(
                mshid=meshid,
                ifx=self.refine_factor_x,
                ify=self.refine_factor_y,
                ifz=self.refine_factor_z,
            )
        )
        logging.info(f"Mesh {meshid} Refined...")
        dpoint = [self.detonation_point.x, self.detonation_point.y, self.detonation_point.z]
        self.stub.CreateInitDetonation(InitDetonationRequest(pid=partid, coord=dpoint, lt=0))
        logging.info("Location of high explosive detonation Defined...")


class DynaSALE(DynaBase):
    """Setup SALE simulation process."""

    def __init__(self):
        DynaBase.__init__(self)
        self.stub.CreateDBSALE(DBSALERequest(switch=1))

    def set_termination(self, endtime):
        """Setting termination time to stop the job.

        Parameters
        ----------
        termination_time : float
            Termination time.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.stub.CreateTermination(TerminationRequest(endtim=endtime))

    def set_output_interval(self, database_plot_interval):
        """Request binary output.

        Parameters
        ----------
        database_plot_interval : float
            Defines the time interval between output states.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.stub.CreateDBBinary(DBBinaryRequest(filetype="D3PLOT", dt=database_plot_interval))

    def set_analysis_type(
        self,
        num_of_cycle=1,
        method=AdvectionMethod.DONOR_CELL_WITH_HALF_INDEX_SHIFT,
        background_pressure=0,
    ):
        """Setup analysis type.

        Parameters
        ----------
        num_of_cycle : float
            Total time of simulation for the fluid problem.
        method : float
            Time step for the fluid problem.
        background_pressure : int
            Reference pressure to compute the internal forces

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ALECreateControl(
            ControlALERequest(
                dct=0,
                nadv=num_of_cycle,
                meth=method.value,
                afac=0,
                end=1e20,
                aafac=1,
                vfact=1e-6,
                pref=background_pressure,
            )
        )
        logging.info("Setup Analysis...")
        return ret

    def set_output_database(self, matsum=0, glstat=0):
        """Obtain output files containing results information.

        Parameters
        ----------
        matsum : float
            Time interval between outputs of part energies.
        glstat : float
            Time interval between outputs of global statistics and energies.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if matsum > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="MATSUM", dt=matsum, binary=1, lcur=0, ioopt=0))
        if glstat > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="GLSTAT", dt=glstat, binary=1, lcur=0, ioopt=0))
        ret = 1
        logging.info("Output Setting...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.set_energy(
            hourglass_energy=EnergyFlag.COMPUTED,
            sliding_interface_energy=EnergyFlag.COMPUTED,
        )
        DynaBase.save_file(self)
