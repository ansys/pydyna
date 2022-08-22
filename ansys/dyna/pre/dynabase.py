"""Module to create dyna input deck"""

from bisect import bisect_right
import os
#from subprocess import DETACHED_PROCESS
import grpc
import sys
import logging
from enum import Enum

"""
import kwprocess_pb2
import kwprocess_pb2_grpc
"""

from .kwprocess_pb2 import *
from .kwprocess_pb2_grpc import *

CHUNK_SIZE = 1024 * 1024


def get_file_chunks(filename):
    with open(filename, "rb") as f:
        while True:
            piece = f.read(CHUNK_SIZE)
            if len(piece) == 0:
                return
            yield Chunk(buffer=piece)


def upload(stub_, filename):
    chunks_generator = get_file_chunks(filename)
    response = stub_.Upload(chunks_generator)


def download(stub_, remote_name, local_name):
    response = stub_.Download(DownloadRequest(url=remote_name))
    with open(local_name, "wb") as f:
        for chunk in response:
            f.write(chunk.buffer)


def init_log(log_file):
    if not logging.getLogger().handlers:
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s : %(levelname)s  %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
            filename=log_file,
            filemode="w",
        )
        console = logging.StreamHandler()
        console.setLevel(logging.INFO)
        formatter = logging.Formatter(
            "%(asctime)s :  %(message)s", datefmt="%Y-%m-%d %H:%M:%S"
        )
        console.setFormatter(formatter)
        logging.getLogger().addHandler(console)

class Motion(Enum):
    VELOCITY = 0
    ACCELERATION = 1
    DISPLACEMENT = 2

class DOF(Enum):
    X_TRANSLATIONAL = 1
    Y_TRANSLATIONAL = 2
    Z_TRANSLATIONAL = 3

class Switch(Enum):
        OFF = 0
        ON = 1

class InvariantNode(Enum):
    OFF = 1
    ON_FOR_SHELL_TSHELL = 2
    ON_FOR_SOLID = 3
    ON_FOR_SHELL_TSHELL_SOLID = 4

class EnergyFlag(Enum):
    NOT_COMPUTED = 1
    COMPUTED = 2

class HourglassControl(Enum):
    STANDARD_VISCOSITY_FORM = 1
    FLANAGAN_BELYTSCHKO_INTEGRATION_SOLID = 2

class BulkViscosity(Enum):
    STANDARD_BULK_VISCOSITY = 1
    RICHARDS_WILKINS_BULK_VISCOSITY=2
    COMPUTE_INTERNAL_ENERGY_DISSIPATED = -2

class CaseType(Enum):
    STRUCTURE = 1
    ICFD = 2
    SALE = 3
    EM = 4
    IGA = 5

class DynaBase:
    """Contains methods to create general LS-DYNA keyword"""

    def __init__(self, hostname="localhost"):
        init_log("client.log")
        channel = grpc.insecure_channel(hostname + ":50051")
        try:
            grpc.channel_ready_future(channel).result(timeout=5)
        except grpc.FutureTimeoutError:
            logging.critical("Can not connect to kwServer")
            sys.exit()
        logging.info("Connected to kwServer...")
        self.stub = kwC2SStub(channel)
        self.mainname = ""
        DynaBase.stub=self.stub
        self.casetype = CaseType.STRUCTURE

    def get_stub():
        return DynaBase.stub

    def open_files(self, filenames):
        """Open IGA model files
        Parameters
        ----------
        filenames : list
            filenames[0] is the main file,the others are subfile.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        splitfiles = os.path.split(filenames[0])
        path = splitfiles[0]
        for filename in filenames:
            fn = os.path.basename(filename)
            self.stub.kwSetFileName(kwFileName(name=fn, num=filenames.index(filename)))
            upload(self.stub, path + os.sep + fn)
            logging.info(
                path + os.sep + "input" + os.sep + fn + " uploaded to server..."
            )

        self.mainname = os.path.basename(filenames[0])
        return self.stub.LoadFile(LoadFileRequest())

    def set_timestep(self, tssfac=0.9, isdo=0, dt2ms=0.0):
        """Create *CONTROL_TIMESTEP keyword
        Parameters
        ----------
        tssfac : float
            Scale factor for computed time step.
        isdo : int
            Basis of time size calculation for 4-node shell elements.
        dt2ms : float
            Time step size for mass scaled solutions.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTimestep(
            TimestepRequest(tssfac=tssfac, isdo=isdo, dt2ms=dt2ms)
        )
        logging.info("Timestep Created...")
        return ret

    def set_termination(self, termination_time):
        """
        Setting termination time to stop the job.
        Refer to: *CONTROL_TERMINATION keyword
        Parameters
        ----------
        termination_time : float
            Termination time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTermination(TerminationRequest(endtim=termination_time))
        logging.info("Setting termination time ...")
        return ret

    def set_accuracy(self, objective_stress_updates=Switch.OFF, 
        invariant_node_number=InvariantNode.OFF, 
        partsetid_for_objective_stress_updates=0, 
        implicit_accuracy_flag=Switch.OFF, 
        explicit_accuracy_flag=Switch.OFF):
        """Define control parameters that can improve the accuracy of the calculation.
        Refer to: *CONTROL_ACCURACY
        Parameters
        ----------
        objective_stress_updates : int
            Global flag for 2nd order objective stress updates.
        invariant_node_number : int
            Invariant node numbering for shell and solid elements.
        partsetid_for_objective_stress_updates : int
            Part set ID for objective stress updates.
        implicit_accuracy_flag : int
            Implicit accuracy flag.
        explicit_accuracy_flag : float
             Explicit accuracy parameter.EQ.0.0: Off,GT.0.0: On
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlAccuracy(
            ControlAccuracyRequest(
                osu=objective_stress_updates.value,
                inn=invariant_node_number.value,
                pidosu=partsetid_for_objective_stress_updates, 
                iacc=implicit_accuracy_flag.value, 
                exacc=explicit_accuracy_flag.value
            )
        )
        logging.info("Control Accuracy Created...")
        return ret

    def set_energy(self, hourglass_energy=EnergyFlag.NOT_COMPUTED,
        rigidwall_energy=EnergyFlag.COMPUTED, 
        sliding_interface_energy=EnergyFlag.NOT_COMPUTED, 
        rayleigh_energy=EnergyFlag.NOT_COMPUTED, 
        initial_reference_geometry_energy=EnergyFlag.COMPUTED):
        """Provide controls for energy dissipation options.
        Refer to: *CONTROL_ENERGY
        Parameters
        ----------
        hourglass_energy : enum
            Hourglass energy calculation option.
        rigidwall_energy : int
            Rigidwall energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        sliding_interface_energy : int
            Sliding interface energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        rayleigh_energy : int
            Rayleigh energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        initial_reference_geometry_energy : int
             Initial reference geometry energy option.EQ.1: Initial reference geometry energy is not computed,EQ.2: Initial reference geometry energy is computed
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlEnergy(
            ControlEnergyRequest(
                hgen=hourglass_energy.value, 
                rwen=rigidwall_energy.value, 
                slnten=sliding_interface_energy.value, 
                rylen=rayleigh_energy.value, 
                irgen=initial_reference_geometry_energy.value
            )
        )
        logging.info("Control Energy Created...")
        return ret

    def set_hourglass(self, controltype=HourglassControl.STANDARD_VISCOSITY_FORM, coefficient=0.1):
        """Redefine the default values of hourglass control type and coefficient.
        Refer to: *CONTROL_HOURGLASS
        Parameters
        ----------
        controltype : enum
            Default hourglass control type.
        coefficient : float
            Default hourglass coefficient.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlHourgalss(
            ControlHourglassRequest(
                ihq=controltype.value, qh=coefficient
            )
        )
        logging.info("Control Hourglass Created...")
        return ret

    def set_bulk_viscosity(self, quadratic_viscosity_coeff=1.5, linear_viscosity_coeff=0.06,bulk_viscosity_type=BulkViscosity.STANDARD_BULK_VISCOSITY):
        """Reset the default values of the bulk viscosity coefficients globally.
        Refer to: *CONTROL_BULK_VISCOSITY
        Parameters
        ----------
        quadratic_viscosity_coeff : float
            Default quadratic viscosity coefficient.
        linear_viscosity_coeff : float
            Default linear viscosity coefficient.
        bulk_viscosity_type : enum
            Default bulk viscosity type.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlBulkViscosity(
            ControlBulkViscosityRequest(
                q1=quadratic_viscosity_coeff, q2=linear_viscosity_coeff,type=bulk_viscosity_type.value
            )
        )
        logging.info("Control Bulk Viscosity Created...")
        return ret

    def create_control_shell(
        self,
        wrpang=20,
        esort=0,
        irnxx=-1,
        istupd=0,
        theory=2,
        bwc=2,
        miter=1,
        proj=0,
        irquad=0,
        ):
        """Provide controls for computing shell response.
        Refer to: *CONTROL_SHELL
        Parameters
        ----------
        wrpang : float
            Shell element warpage angle in degrees.
        esort : int
            Sorting of triangular shell elements to automatically switch degenerate quadrilateral shell formulations to more suitable triangular shell formulations.
        irnxx : int
            Shell normal update option.
        istupd : int
            Shell thickness change option for deformable shells.
        theory : int
            Default shell formulation.
        bwc : int
            Warping stiffness for Belytschko-Tsay shells.
        miter : int
            Plane stress plasticity option.
        proj : int
            Projection method for the warping stiffness in the Belytschko-Tsay shell and the Belytschko-Wong-Chiang elements
        irquad : int
             In plane integration rule for the 8-node quadratic shell element.EQ.2: 2*2 Gauss quadrature,EQ.3: 3*3 Gauss quadrature.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlShell(
            ControlShellRequest(
                wrpang=wrpang,
                esort=esort,
                irnxx=irnxx,
                istupd=istupd,
                theory=theory,
                bwc=bwc,
                miter=miter,
                proj=proj,
                irquad=irquad,
            )
        )
        logging.info("Control Shell Created...")
        return ret

    def create_control_solid(
        self,
        esort=0,
        fmatrx=0,
        niptets=4,
        swlocl=1,
        psfail=0,
        t10jtol=0.0,
        icoh=0,
        tet13k=0,
    ):
        """Provides controls for solid element response.
        Refer to: *CONTROL_SOLID
        Parameters
        ----------
        esort : int
            Automatic sorting of tetrahedral and pentahedral elements to avoid use of degenerate formulations for these shapes.EQ.0: No sorting,EQ.1: Sort.
        fmatrx : int
            Default method used in the calculation of the deformation gradient matrix.
        niptets : int
            Number of integration points used in the quadratic tetrahedron elements.
        swlocl : int
            Output option for stresses in solid elements used as spot welds with material *MAT_SPOTWELD.
        psfail : int
            Solid element erosion from negative volume is limited only to solid elements in the part set indicated by PSFAIL.
        t10jtol : float
            Tolerance for Jacobian in 4-point 10-noded quadratic tetrahedra.
        icoh : int
            Breaking LS-DYNA convention ICOH is interpreted digit-wise.
        tet13k : int
            Set to 1 to invoke a consistent tangent stiffness matrix for the pressure averaged tetrahedron.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlSolid(
            ControlSolidRequest(
                esort=esort,
                fmatrx=fmatrx,
                niptets=niptets,
                swlocl=swlocl,
                psfail=psfail,
                t10jtol=t10jtol,
                icoh=icoh,
                tet13k=tet13k,
            )
        )
        logging.info("Control Solid Created...")
        return ret

    def create_control_output(self, npopt=0, neecho=0):
        """Set miscellaneous output parameters.
        Refer to : *CONTROL_OUTPUT
        Parameters
        ----------
        npopt : int
            Print suppression during input phase flag for the d3hsp file:
            EQ.0: No suppression.
            EQ.1: Nodal coordinates, element connectivities, rigid wall definitions, nodal SPCs, initial velocities, initial strains, adaptive constraints, and SPR2/SPR3 constraints are not printed.
        neecho : int
            Print suppression during input phase flag for echo file:
            EQ.0: All data printed.
            EQ.1: Nodal printing is suppressed.
            EQ.2: Element printing is suppressed.
            EQ.3: Both nodal and element printing is suppressed.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlOutput(
            ControlOutputRequest(npopt=npopt, neecho=neecho)
        )
        logging.info("Control Output Created...")
        return ret

    def create_control_contact(
        self, rwpnal, shlthk=0, orien=1, ssthk=0, ignore=0, igactc=0
    ):
        """Change defaults for computation with contact surfaces
        Refer to : *CONTROL_CONTACT
        Parameters
        ----------
        shlthk : int
            Flag for consideration of shell thickness offsets in non-automatic surface-to-surface and non-automatic nodes-to-surface type contacts.
        ssthk : int
            Flag for determining default contact thickness for shells in single surface contact types.
        orien : int
            Optional automatic reorientation of contact interface segments during initialization.
        rwpnal : float
            Scale factor for rigid wall penalties, which treat nodal points interacting with rigid walls.
        ignore : int
            Ignore initial penetrations in the *CONTACT_AUTOMATIC options.
        igactc : int
            Options to use isogeometric shells for contact detection when contact involves isogeometric shells.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlContact(
            ControlContactRequest(
                rwpnal=rwpnal,
                shlthk=shlthk,
                orien=orien,
                ssthk=ssthk,
                ignore=ignore,
                igactc=igactc,
            )
        )
        logging.info("Control Contact Created...")
        return ret

    def create_database_binary(
        self, filetype="D3PLOT", dt=0, maxint=3, ieverp=0, dcomp=1, nintsld=1
    ):
        """Request binary output.
        Refer to : *DATABASE_BINARY_*
                   *DATABASE_EXTENT_BINARY
        Parameters
        ----------
        dt : float
            Defines the time interval between output states.
        maxint : int
            Number of shell and thick shell through-thickness integration points for which output is written to d3plot.
        ieverp : int
            Every output state for the d3plot database is written to a separate file.
            EQ.0: More than one state can be on each plot file.
            EQ.1: One state only on each plot file.
        dcomp : int
            Data compression to eliminate rigid body data.
        nintsld : int
            Number of solid element integration points written to the LS-DYNA database.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDBBinary(
            DBBinaryRequest(
                filetype=filetype,
                dt=dt,
                maxint=maxint,
                ieverp=ieverp,
                dcomp=dcomp,
                nintsld=nintsld,
            )
        )
        logging.info("DB Binary Created...")
        return ret

    def create_database_ascii(self, type, dt=0.0, binary=1, lcur=0, ioopt=0):
        """Create *DATABASE keyword
        Parameters
        ----------
        type : string
            Specifies the type of database.(BNDOUT,GLSTAT,MATSUM,NODFOR,RCFORC,SLEOUT)
        dt : float
            Time interval between outputs
        binary : int
            Flag for binary output.
        lcur : int
            Optional curve ID specifying time interval between outputs.
        ioopt : int
            Flag to govern behavior of the output frequency load curve defined by LCUR.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDBAscii(
            DBAsciiRequest(type=type, dt=dt, binary=binary, lcur=lcur, ioopt=ioopt)
        )
        logging.info("DB Ascii Created...")
        return ret

    def create_rigidwall_geom(
        self, geomtype, motion, display, parameter, lcid, vx, vy, vz
    ):
        """Define a rigid wall with an analytically described form.
        Refer to : *RIGIDWALL_GEOMETRIC
        Parameters
        ----------
        geomtype : int
            The available shape variants are FLAT, PRISM, CYLINDER and SPHERE.
        motion : int
            If prescribed motion is desired an additional option is available.
        display : int
            To view the rigid wall, this option is available.
        parameter : list
            x,y,z-coordinate of tail of normal vector n,x,y,z-coordinate of head of normal vector n,radius of cylinder and length of cylinder.
        lcid : int
            if motion defined,this is Rigidwall motion curve ID.
        vx vy vz : float
            if motion defined, these are x,y,z-direction cosine of velocity/displacement vector.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateRigidWallGeom(
            RigidWallGeomRequest(
                geomtype=geomtype,
                motion=motion,
                display=display,
                parameter=parameter,
                lcid=lcid,
                vx=vx,
                vy=vy,
                vz=vz,
            )
        )
        logging.info("Cylinder Rigidwall Geometric Created...")
        return ret

    def create_rigidwall_planar(self, nsid, tail, head, nsidex=0, boxid=0, fric=0):
        """Define planar rigid walls with either finite or infinite size.
        Parameters
        ----------
        nsid : int
            Nodal set ID containing tracked nodes.
        tail : list [xt,yt,zt]
            xt,yt,zt : x,y,z-coordinate of tail of normal vector n.
        head : list [xh,yh,zh]
            xh,yh,zh : x,y,z-coordinate of head of normal vector n.
        nsidex : int
            Nodal set ID containing nodes that are exempted as tracked nodes.
        boxid : int
            All nodes in box are included as tracked nodes for interacting with the rigid wall.
        fric : float
            Coulomb friction coefficient.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        normal = [tail[0], tail[1], tail[2], head[0], head[1], head[2]]
        ret = self.stub.CreateRigidWallPlanar(
            RigidWallPlanarRequest(
                nsid=nsid, nsidex=nsidex, boxid=boxid, fric=fric, normal=normal
            )
        )
        logging.info("Rigidwall Planar Created...")
        return ret

    def create_init_vel(self, nsid, velocity):
        """Define initial nodal point velocities using nodal set ID.
        Refer to:*INITIAL_VELOCITY
        Parameters
        ----------
        nsid : int
            Nodal set ID.
        velocity : list [vx,vy,vx,vxr,vyr,vzr]
            vx,vy,vz: Initial translational velocity in x,y,z-direction.
            vxr,vyr,vzr: Initial rotational velocity about the x,y,z-axis.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateInitVel(InitVelRequest(nsid=nsid, velocity=velocity))
        logging.info("Initial velocity Created...")
        return ret

    def create_init_vel_rigidbody(
        self, pid, vx=0, vy=0, vz=0, vxr=0, vyr=0, vzr=0, lcid=0
    ):
        """Define the initial translational and rotational velocities at the center of gravity for a rigid body or a nodal rigid body
        Refer to:*INITIAL_VELOCITY_RIGID_BODY
        Parameters
        ----------
        pid : int
            Part ID of the rigid body or the nodal rigid body.
        vx/vy/vz : float
            Initial translational velocity at the center of gravity in global x/y/z-direction.
        vxr/vyr/vzr : float
            Initial rotational velocity at the center of gravity about the global x/y/z-axis.
        lcid : int
            Local coordinate system ID.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateInitVelRigidBody(
            InitVelRigidBodyRequest(
                pid=pid, vx=vx, vy=vy, vz=vz, vxr=vxr, vyr=vyr, vzr=vzr, lcid=lcid
            )
        )
        logging.info("Initial velocity rigid body Created...")
        return ret

    def create_init_vel_bodies(
        self,
        id,
        styp=2,
        omega=0,
        vx=0,
        vy=0,
        vz=0,
        xc=0,
        yc=0,
        zc=0,
        nx=0,
        ny=0,
        nz=0,
        phase=0,
        stime=0,
    ):
        """Define initial velocities for rotating and/or translating bodies.
        Refer to:*INITIAL_VELOCITY_GENERATION
                 *INITIAL_VELOCITY_GENERATION_START_TIME
        Parameters
        ----------
        id : int
            Part ID, part set ID, or node set ID.
        styp : int
            Set type.EQ.1: Part set ID,EQ.2: Part ID,EQ.3: Node set ID
        omega : float
            Angular velocity about the rotational axis.
        vx/vy/vz : float
            Initial translational velocity in x/y/z-direction.
        xc/yc/zc : float
            Global x/y/z-coordinate on rotational axis.
        nx/ny/nz : float
            x/y/z-direction cosine.
        phase : int
            Flag determining basis for initialization of velocity.
        stime : float
            Define a time to initialize velocities after time zero.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateInitVelGeneration(
            InitVelGenerationRequest(
                id=id,
                styp=styp,
                omega=omega,
                vx=vx,
                vy=vy,
                vz=vz,
                xc=xc,
                yc=yc,
                zc=zc,
                nx=nx,
                ny=ny,
                nz=nz,
                phase=phase,
            )
        )
        if stime > 0:
            self.stub.CreateInitVelGenerationStartTime(
                InitVelGenerationStartTimeRequest(stime=stime)
            )
        logging.info("Initial velocity for bodies Created...")
        return ret

    def create_definecurve(self, lcid, sfo, abscissa, ordinate):
        """Create *DEFINE_CURVE keyword
        Parameters
        ----------
        lcid : int
            Load curve identification.
        sfo : float
            Scale factor for ordinate value.
        abscissa : list
            Abscissa values.
        ordinate : list
            Ordinate (function) values.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDefineCurve(
            DefineCurveRequest(lcid=lcid, sfo=sfo, abscissa=abscissa, ordinate=ordinate)
        )
        logging.info("DefineCurve Created...")
        return ret

    def create_definevector(self, title, vid, tail, head):
        """Create *DEFINE_VECTOR keyword
        Parameters
        ----------
        vid : int
            Vector ID.
        tail : list [x,y,z]
            x,y,z-coordinate of tail of vector.
        head : list [x,y,z]
            x,y,z-coordinate of head of vector.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDefineVector(
            DefineVectorRequest(
                title=title,
                vid=vid,
                xt=tail[0],
                yt=tail[1],
                zt=tail[2],
                xh=head[0],
                yh=head[1],
                zh=head[2],
            )
        )
        logging.info("DefineVector Created...")
        return ret

    def create_defineorientation(self, vid, iop, vector, node1, node2):
        """Define orientation vectors for discrete springs and dampers.
        Refer to : *DEFINE_SD_ORIENTATION
        Parameters
        ----------
        vid : int
            Orientation vector ID.
        iop : int
            Option:
            EQ.0: deflections/rotations are measured and forces/moments applied along the following orientation vector.
            EQ.1: deflections/rotations are measured and forces/moments applied along the axis between the two spring/damper nodes projected onto the plane normal to the following orientation vector.
            EQ.2: deflections/rotations are measured and forces/moments applied along a vector defined by the following two nodes.
            EQ.3: deflections/rotations are measured and forces/moments applied along the axis between the two spring/damper nodes projected onto the plane normal to the a vector defined by the following two nodes.
        vector : list [x,y,z]
            x,y,z : x,y,z-value of orientation vector.
        node1 : int
            Node 1 ID.
        node2 : int
            Node 2 ID.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDefineOrientation(
            DefineOrientationRequest(
                vid=vid, iop=iop, vector=vector, node1=node1, node2=node2
            )
        )
        logging.info("DefineOrientation Created...")
        return ret

    def set_partproperty(
        self, pid, secid=0, mid=0, eosid=0, hgid=0, grav=0, adpopt=0, tmid=0
    ):
        """Reset property for *PART keyword
        Parameters
        ----------
        pid : int
            Part identification. A unique number must be specified..
        secid : int
            Section identification defined in a *SECTION keyword.
        mid : int
            Material identification defined in the *MAT section.
        eosid : int
            Equation of state identification defined in the *EOS section.
        hgid : int
            Hourglass/bulk viscosity identification defined in the *HOURGLASS Section.
        grav : int
            Flag to turn on gravity initialization according to *LOAD_DENSITY_DEPTH.
        adpopt : int
            Indicate if this part is adapted or not.
        tmid : int
            Thermal material property identification defined in the *MAT_THERMAL Section.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=pid,
                secid=secid,
                mid=mid,
                eosid=eosid,
                hgid=hgid,
                grav=grav,
                adpopt=adpopt,
                tmid=tmid,
            )
        )
        return ret

    def create_partset(self, sid, pids):
        """Define a set of parts with optional attributes.
        Refer to:*SET_PART
        Parameters
        ----------
        sid : int
            Set ID. All part sets should have a unique set ID.
        pids : list
            A list of part ID.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreatePartSet(PartSetRequest(sid=sid, pids=pids))
        logging.info("Part Set Created...")
        return ret

    def create_shellset(self, option, title, sid, eids):
        """Define a set of shell elements with optional identical or unique attributes.
        Refer to: *SET_SHELL
        Parameters
        ----------
        option : string
            Available options:<BLANK>,LIST,GENERAL
        title : string
            Define title for shell set.
        sid : int
            Set ID.
        eids : list
            Shell element IDs.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateShellSet(
            ShellSetRequest(option=option, title=title, sid=sid, eids=eids)
        )
        logging.info("Shell Set Created...")
        return ret

    def create_solidset(self, title, sid, ki):
        """Define a set of solid elements.
        Refer to: *SET_SOLID
        Parameters
        ----------
        title : string
            Define title for solid set.
        sid : int
            Set ID.
        ki : list
            Solid element IDs.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSolidSet(SolidSetRequest(title=title, sid=sid, ki=ki))
        logging.info("Solid Set Created...")
        return ret

    def create_nodeset(self, option, sid, entities, genoption=""):
        """Define a nodal set with some identical or unique attributes.
        Refer to: *SET_NODE
        Parameters
        ----------
        option : string
            Available options:<BLANK>,LIST,GENERAL
        sid : int
            Set identification.
        genoption : string
            Option for GENERAL:ALL,NODE,PART
        entities : list
            Specified entity.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateNodeSet(
            NodeSetRequest(
                option=option, sid=sid, genoption=genoption, entities=entities
            )
        )
        logging.info("Node Set Created...")
        return ret

    def create_segmentset(self, sid, segments, solver="MECH"):
        """Define a nodal set with some identical or unique attributes.
        Refer to: *SET_NODE
        Parameters
        ----------
        sid : int
            Set ID.
        segments : list [[point1,point2,point3,point4],[point5,point6,point7,point8]...]
            Define segments.
        solver : string
            Name of solver using this set:MECH,CESE
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        n1 = []
        n2 = []
        n3 = []
        n4 = []
        for i in range(len(segments)):
            n1.append(segments[i][0])
            n2.append(segments[i][1])
            n3.append(segments[i][2])
            n4.append(segments[i][3])
        ret = self.stub.CreateSegmentSet(
            SegmentSetRequest(sid=sid, solver=solver, n1=n1, n2=n2, n3=n3, n4=n4)
        )
        logging.info("Segment Set Created...")
        return ret

    def create_section_shell(self, secid, elform, thick, shrf=1.0, nip=2, propt=0):
        """Define section properties for shell elements.
        Refer to:*SECTION_SHELL
        Parameters
        ----------
        secid : int
            Section ID.
        elform : int
            Element formulation options.
        thick : list [t1,t2,t3,t4]
            Shell thickness at node t1,t2,t3,t4
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionShell(
            SectionShellRequest(
                secid=secid,
                elform=elform,
                shrf=shrf,
                nip=nip,
                propt=propt,
                t1=thick[0],
                t2=thick[1],
                t3=thick[2],
                t4=thick[3],
            )
        )
        logging.info("Section Shell Created...")
        return ret

    def create_section_solid(self, title, secid, elform):
        """Create *SECTION_SOLID keyword
        Parameters
        ----------
        title : string
            Define title for section solid.
        secid : int
            Section ID. SECID is referenced on the *PART card. A unique number must be specified.
        elform : int
            Element formulation options.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionSolid(
            SectionSolidRequest(title=title, secid=secid, elform=elform)
        )
        logging.info("Section Solid Created...")
        return ret

    def create_section_discrete(
        self, secid, dro=0, kd=0, v0=0, cl=0, fd=0, cdl=0, tdl=0
    ):
        """Defined spring and damper elements for translation and rotation.
        Refer to : *SECTION_DISCRETE
        Parameters
        ----------
        secid : int
            Section ID.
        dro : int
            Displacement/Rotation Option:
            EQ.0: the material describes a translational spring/damper,
            EQ.1: the material describes a torsional spring/damper.
        kd : float
            Dynamic magnification factor.
        v0 : float
            Test velocity.
        cl : float
            Clearance.
        fd : float
            Failure deflection.
        cdl : float
            Deflection limit in compression.
        cd1 : float
            Deflection limit in tension.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionDiscrete(
            SectionDiscreteRequest(
                secid=secid, dro=dro, kd=kd, v0=v0, cl=cl, fd=fd, cdl=cdl, tdl=tdl
            )
        )
        logging.info("Section Discrete Created...")
        return ret

    def create_hourglass(self, ghid, ihq, qm=0.1, q1=1.5, q2=0.06, qb=1e-9, qw=1e-9):
        """Create *HOURGLASS keyword
        Parameters
        ----------
        ghid : int
            Hourglass ID. A unique number or label must be specified.
        ihq : int
            Hourglass control type.
        qm : float
            Hourglass coefficient.
        q1 : float
            Quadratic bulk viscosity coefficient.
        q2 : float
            Linear bulk viscosity coefficient.
        qb : float
            Hourglass coefficient for shell bending.
        qw : float
            Hourglass coefficient for shell warping.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateHourglass(
            HourglassRequest(ghid=ghid, ihq=ihq, qm=qm, q1=q1, q2=q2, qb=qb, qw=qw)
        )
        logging.info("Hourglass 1 Created...")
        return ret

    def create_contact(
        self,
        cid,
        title,
        option1,
        option3=True,
        offset="",
        ssid=0,
        msid=0,
        sstyp=3,
        mstyp=3,
        sapr=0,
        sbpr=0,
        sfsa=1,
        sfsb=1,
        fs=0,
        fd=0,
        vdc=0,
        penchk=0,
        birthtime=0,
        sst=1,
        mst=1,
        optionres=1,
        nfls=1e32,
        sfls=1e32,
        param=0,
        ct2cn=0,
        soft=0,
        sofscl=0.1,
        lcidab=0,
        maxpar=1.025,
        sbopt=2,
        depth=2,
        bsort=10,
        frcfrq=1,
        igap=1,
    ):
        """Define a contact interface in a 3D model.
        Parameters
        ----------
        option1 : string
            Specifies contact type.
            "TIED_SHELL_EDGE_TO_SURFACE"
            "AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK"
            "AUTOMATIC_SINGLE_SURFACE_SMOOTH"
            "AUTOMATIC_SINGLE_SURFACE"
            "NODES_TO_SURFACE"
        option3 : bool
            Flag indicating ID cards follow.
        offset : string
            Offset options.
            NULL
            OFFSET
            BEAM_OFFSET
            CONSTRAINED_OFFSET
        ssid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface.
        msid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact.
        sstyp : int
            The ID type of SURFA.
        mstyp : int
            ID type of SURFB.
        option : int
            Soft constraint option.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateContact(
            ContactRequest(
                cid=cid,
                title=title,
                option1=option1,
                option3=option3,
                offset=offset,
                ssid=ssid,
                msid=msid,
                sstyp=sstyp,
                mstyp=mstyp,
                sapr=sapr,
                sbpr=sbpr,
                sfsa=sfsa,
                sfsb=sfsb,
                fs=fs,
                fd=fd,
                vdc=vdc,
                penchk=penchk,
                birthtime=birthtime,
                sst=sst,
                mst=mst,
                optionres=optionres,
                nfls=nfls,
                sfls=sfls,
                param=param,
                ct2cn=ct2cn,
                soft=soft,
                sofscl=sofscl,
                lcidab=lcidab,
                maxpar=maxpar,
                sbopt=sbopt,
                depth=depth,
                bsort=bsort,
                frcfrq=frcfrq,
                igap=igap,
            )
        )
        logging.info("Contact  Created...")
        return ret

    def create_boundary_prescribed_motion(
        self,
        id,
        heading,
        option,
        typeid,
        dof,
        vad=0,
        lcid=0,
        sf=1.0,
        vid=0,
        birth=0,
        death=1e28,
    ):
        """Define an imposed nodal motion (velocity, acceleration, or displacement) on a node or a set of nodes.
        Refer to:*BOUNDARY_PRESCRIBED_MOTION
        Parameters
        ----------
        id : int
            PRESCRIBED MOTION set ID to which this node, node set, segment set, or rigid body belongs.
        heading : string
            An optional descriptor for the given ID that will be written into the d3hsp file and the bndout file.
        option : string
            Available options:(NODE,SET,RIGID)
        typeid : int
            Node ID, nodal set ID,segment set ID , part ID for a rigid body.
        dof : int
            Applicable degrees-of-freedom.
        vad : int
            Velocity/Acceleration/Displacement flag:EQ.0: Velocity,EQ.1: Acceleration,EQ.2: Displacement,EQ.3: Velocity as a function of displacement,EQ.4: Relative displacement
        lcid : int
            Curve ID or function ID to describe motion value as a function of time.
        sf :float
            Load curve scale factor.
        vid : int
            Vector ID for DOF values of 4 or 8.
        birth : float
            Time that the imposed motion/constraint is activated.
        death : float
            Time imposed motion/constraint is removed.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateBdyPrescribedMotion(
            BdyPrescribedMotionRequest(
                id=id,
                heading=heading,
                option=option,
                typeid=typeid,
                dof=dof,
                vad=vad,
                lcid=lcid,
                sf=sf,
                vid=vid,
                birth=birth,
                death=death,
            )
        )
        logging.info("Boundary prescribed motion Created...")
        return ret

    def create_boundary_spc(
        self,
        option1,
        birthdeath=False,
        nid=0,
        cid=0,
        dofx=0,
        dofy=0,
        dofz=0,
        dofrx=0,
        dofry=0,
        dofrz=0,
        birth=0,
        death=1e20,
    ):
        """Define nodal single point constraints.
        Refer to:*BOUNDARY_SPC
        Parameters
        ----------
        id : int
            Optional SPC set ID to which this node or node set belongs.
        heading : string
            An optional SPC descriptor that will be written into the d3hsp file and the spcforc file.
        option : string
            Available options:(NODE,SET)
        birthdeath : bool
            Allows optional birth and death times to be assigned the single node or node set.
        nid : int
            Node ID or nodal set ID.
        cid : int
            Coordinate system ID.
        dofx/dofy/dofz : int
            Insert 1 for translational constraint in local x/y/z-direction.
        dofrx/dofry/dofrz : int
            Insert 1 for rotational constraint about local x/y/z-axis.
        birth/death : float
            Activation/Deactivation time for SPC constraint.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateBdySpc(
            BdySpcRequest(
                option1=option1,
                birthdeath=birthdeath,
                nid=nid,
                cid=cid,
                dofx=dofx,
                dofy=dofy,
                dofz=dofz,
                dofrx=dofrx,
                dofry=dofry,
                dofrz=dofrz,
                birth=birth,
                death=death,
            )
        )
        logging.info("Boundary spc Created...")
        return ret

    def create_constrained_extra_nodes(self, option="NODE", pid=0, nid=0, iflag=0):
        """Define extra nodes for rigid body.
        Refer to:*CONSTRAINED_EXTRA_NODES
        Parameters
        ----------
        option : string
            Available options include:NODE,SET
        pid : int
            Part ID of rigid body to which the nodes will be added.
        nid : int
            Node or node set ID
        iflag : int
            This flag is meaningful if and only if the inertia properties of the Part ID are defined in PART_INERTIA.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateConstrainedExtraNodes(
            ConstrainedExtraNodesRequest(option=option, pid=pid, nid=nid, iflag=iflag)
        )
        logging.info("Constrained extra nodes Created...")
        return ret

    def create_constrained_joint(self, type, nodes, rps=1.0, damp=1.0):
        """Define a joint between two rigid bodies.
        Refer to:*CONSTRAINED_JOINT
        Parameters
        ----------
        type : string
            The available joint variants are:
            "SPHERICAL"
        nodes : list
            Define nodes for joint.
        rps : int
            Relative penalty stiffness.
        damp : int
            Damping scale factor on default damping value.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateConstrainedJoint(
            ConstrainedJointRequest(type=type, nodes=nodes, rps=rps, damp=damp)
        )
        logging.info("Constrained joint Created...")
        return ret

    def create_load_body(self, option="X", lcid=0):
        """Define body force loads due to a prescribed base acceleration or angular velocity using global axes directions.
        Refer to:*LOAD_BODY
        Parameters
        ----------
        option : string
            Available options include:X,Y,Z,RX,RY,RZ
        lcid : int
            Load curve ID specifying loading.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateLoadBody(LoadBodyRequest(option=option, lcid=lcid))
        logging.info("Load body Created...")
        return ret

    def create_mat_rigid(self, mid, ro, e, pr, cmo=0, con1=0, con2=0):
        """Parts made from this material are considered to belong to a rigid body.
        Refer to:*MAT_RIGID
        Parameters
        ----------
        mid : int
            Material identification.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.
        com : int
            Center of mass constraint option.EQ.1: constraints applied in global directions,EQ.0: no constraints,EQ.-1: constraints applied in local directions.
        con1 : int
            Global translational constraint.
        con2 : int
            Global rotational constraint.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatRigid(
            MatRigidRequest(mid=mid, ro=ro, e=e, pr=pr, cmo=cmo, con1=con1, con2=con2)
        )
        logging.info("Material Rigid Created...")
        return ret

    def create_mat_elastic(self, mid, ro, e, pr):
        """Parts made from this material are considered to belong to a rigid body.
        Refer to:*MAT_RIGID
        Parameters
        ----------
        mid : int
            Material identification.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatElastic(MatElasticRequest(mid=mid, ro=ro, e=e, pr=pr))
        logging.info("Material Elastic Created...")
        return ret

    def create_mat_fabric(self, mid, ro, ea, eb, prba, prab, gab):
        """This material is especially developed for airbag materials.
        Refer to:*MAT_FABRIC
        Parameters
        ----------
        mid : int
            Material identification.
        ro : float
            Mass density.
        ea : float
            Young's modulus-longitudinal direction.
        eb : float
            Young's modulus-transverse direction.
        prba : float
            Minor Poisson's ratio ba direction.
        prab : float
            Major Poisson's ratio ab direction.
        gab : float
            shear modulus in the ab direction.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatFabric(
            MatFabricRequest(
                mid=mid, ro=ro, ea=ea, eb=eb, prba=prba, prab=prab, gab=gab
            )
        )
        logging.info("Material Fabric Created...")
        return ret

    def create_mat_spring_nonlinear_elastic(self, mid, lcid):
        """This material provides a nonlinear elastic translational and rotational spring with arbitrary force as a function of displacement and moment as a function of rotation.
        Refer to:*MAT_SPRING_NONLINEAR_ELASTIC
        Parameters
        ----------
        mid : int
            Material identification.
        lcid : int
            Load curve ID (see *DEFINE_CURVE) describing force as a function of displacement or moment as a function of rotation relationship.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatSpringNonlinearElastic(
            MatSpringNonlinearElasticRequest(mid=mid, lcid=lcid)
        )
        logging.info("Material Spring Nonlinear Elastic Created...")
        return ret

    def create_mat_damper_viscous(self, mid, dc):
        """This material provides a linear translational or rotational damper located between two nodes.
        Refer to : *MAT_DAMPER_VISCOUS
        Parameters
        ----------
        mid : int
            Material identification.
        dc : float
            Damping constant.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatDamperViscous(MatDamperViscousRequest(mid=mid, dc=dc))
        logging.info("Material Damper Viscous Created...")
        return ret

    def create_mat_damper_nonlinear_viscous(self, mid, lcdr):
        """This material provides a viscous translational damper with an arbitrary force as a function of velocity dependency or a rotational damper with an arbitrary moment as a function of rotational velocity dependency.
        Refer to : *MAT_DAMPER_NONLINEAR_VISCOUS
        Parameters
        ----------
        mid : int
            Material identification.
        lcdr : int
            Load curve ID defining force as a function of rate-of-displacement relationship or a moment as a function of rate-of-rotation relationship.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatDamperNonlinearViscous(
            MatDamperNonlinearViscousRequest(mid=mid, lcdr=lcdr)
        )
        logging.info("Material Damper Nonlinear Viscous Created...")
        return ret

    def create_damping_global(self, lcid=0, valdmp=0.0):
        """Define mass weighted nodal damping that applies globally to the nodes of deformable bodies and to the mass center of the rigid bodies.
        Refer to:*DAMPING_GLOBAL
        Parameters
        ----------
        lcid : int
            Load curve ID which specifies the system damping constant vs. time:
        valdmp : float
            System damping constant.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDampingGlobal(
            DampingGlobalRequest(lcid=lcid, valdmp=valdmp)
        )
        logging.info("Damping global Created...")
        return ret

    def set_part_damping_stiffness(self, pids, coef=0.0):
        """Assign stiffness damping coefficient by part ID or part set ID.
        Refer to:*DAMPING_PART_STIFFNESS
        Parameters
        ----------
        pids : list
            Part IDs.
        coef : float
            Rayleigh damping coefficient.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        if len(pids) > 1:
            sid = self.create_partset(0, pids)
            ret = self.stub.CreateDampingPartStiffness(
                DampingPartStiffnessRequest(isset=True, id=sid, coef=coef)
            )
        else:
            id = pids[0]
            ret = self.stub.CreateDampingPartStiffness(
                DampingPartStiffnessRequest(isset=False, id=id, coef=coef)
            )

        logging.info("Damping global Created...")
        return ret

    def get_solid_elements(self):
        """Get solid elements.

        Returns
        -------
        list
            list[0],solid element connectivity,list[0] = [[n1,n2,n3,n4,n5,n6,n7,n8],[...],...]
            list[1],node coordinates,list[1] = [[x1,y1,z1],[x2,y2,z2],...]
        """
        cons = self.stub.GetSolidElements(GetSolidElementsRequest())
        nodes = self.stub.GetNodes(GetNodesRequest())
        num = 8
        lscons = cons.nodeids
        elist = [lscons[i : i + num] for i in range(0, len(lscons), num)]
        numconn = 3
        lsnodes = nodes.coords
        nlist = [lsnodes[i : i + numconn] for i in range(0, len(lsnodes), numconn)]
        elements = [elist, nlist]
        return elements

    def create_general_keyword(self, opcode, keyworddata):
        """Create general keyword.
        Parameters
        ----------
        opcode : string
            keyword card name.
        keyworddata : string
            keyword data.

        Returns
        -------
        bool
            "True" when successful, "False" when failed

        example
            create a *INITIAL_VELOCITY keyword
            
            $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            *INITIAL_VELOCITY
            &     nsid    nsidex     boxid
                     0

            &       vx        vy        vz       vxr       vyr       vzr
             1.480E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
            $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

            opcode = "INITIAL_VELOCITY"
            keyworddata = "0\n1.480E+01,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00"
            create_general_keyword(opcode = opcode,keyworddata=keyworddata)
        """
        ret = self.stub.CreateGeneralKWD(
            GeneralKWDRequest(opcode=opcode, keyworddata=keyworddata)
        )
        msg = opcode + " Created..."
        logging.info("msg")
        return ret
    
    def set_output_database(self, matsum=0,glstat=0,elout=0,nodout=0,rbdout=0,rcforc=0,secforc=0):
        """obtain output files containing results information.
        Parameters
        ----------
        matsum : float
            Time interval between outputs of part energies.
        glstat : float      
            Time interval between outputs of global statistics and energies.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        if matsum>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="MATSUM", dt=matsum, binary=1, lcur=0,ioopt=0)
            )
        if glstat>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="GLSTAT", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        if elout>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="ELOUT", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        if nodout>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="NODOUT", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        if rbdout>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="RBDOUT", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        if rcforc>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="RCFORC", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        if secforc>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="SECFORC", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        ret = 1
        logging.info("Output Setting...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.set_accuracy(objective_stress_updates=Switch.ON,invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,implicit_accuracy_flag=Switch.ON)
        self.set_bulk_viscosity(bulk_viscosity_type=BulkViscosity.COMPUTE_INTERNAL_ENERGY_DISSIPATED)
        self.set_energy(hourglass_energy=EnergyFlag.COMPUTED,sliding_interface_energy=EnergyFlag.COMPUTED)
        self.set_hourglass(controltype=HourglassControl.FLANAGAN_BELYTSCHKO_INTEGRATION_SOLID,coefficient=0)
        self.create_control_shell(wrpang=0,esort=1,irnxx=0,istupd=4,theory=0,bwc=1,miter=1,proj=1,irquad=0)
        if self.casetype == CaseType.IGA:
            igactc=1
        else:
            igactc=0
        self.create_control_contact(rwpnal=1.0,ignore=1,igactc=igactc)
        for obj in Contact.contactlist:
            obj.create()
        for obj in BeamPart.partlist:
            obj.set_property()
        for obj in ShellPart.partlist:
            obj.set_property()
        for obj in SolidPart.partlist:
            obj.set_property()
        for obj in IGAPart.partlist:
            obj.set_property()
        for obj in RigidwallCylinder.rwlist:
            obj.create()
        Constraint.create(self.stub)


        ret = self.stub.SaveFile(SaveFileRequest(name=self.mainname))
        msg = self.mainname + " is outputed..."
        logging.info(msg)
        return ret
#-------------------------------------------------------------------------------------------------
class Box:
    """Define a box-shaped volume."""
    def __init__(self,xmin=0,xmax=0,ymin=0,ymax=0,zmin=0,zmax=0):
        self.xmin=xmin
        self.xmax=xmax
        self.ymin=ymin
        self.ymax=ymax
        self.zmin=zmin
        self.xmax=zmax

    def create(self,stub):
        ret = stub.CreateDefineBox(
        DefineBoxRequest(xmin=self.xmin, xmax=self.xmax,ymin=self.ymin,ymax=self.ymax,zmin=self.zmin,zmax=self.zmax)
        )
        self.id = ret.boxid
        logging.info(f"Box {self.boxid} defined...")

class Curve:
    """Define a curve [for example, load (ordinate value)] as a function of time"""
    def __init__(self,sfo=1,x=[],y=[]):
        self.sfo=sfo
        self.abscissa = x
        self.ordinate = y
    
    def create(self,stub):
        ret = stub.CreateDefineCurve(
        DefineCurveRequest(sfo=self.sfo, abscissa=self.abscissa,ordinate=self.ordinate)
        )
        self.id = ret.id
        logging.info(f"Curve {self.id} defined...")

class NodeSet:
    """Define a nodal set with some identical or unique attributes."""
    def __init__(self,nodes=[]):
        self.nodes = nodes
        self.id = 0

    def create(self,stub):
        if len(self.nodes)<=0:
            return 0
        ret = stub.CreateNodeSet(
            NodeSetRequest(
                option="LIST", sid=0, genoption="NODE", entities=self.nodes
            )
        )
        self.id = ret.id
        if len(self.nodes)>1:
            self.type = "NODESET"
        else:
            self.type = "NODE"
        return self.id
        
    def num(self):
        return len(self.nodes)
    
    def id(self,pos):
        return self.nodes[pos]

class PartSet:
    """Define a set of parts with optional attributes."""
    def __init__(self,parts=[]):
        self.parts=parts
        self.id = 0

    def create(self,stub):
        if len(self.parts)<=0:
            return 0
        ret = stub.CreatePartSet(PartSetRequest(sid=0, pids=self.parts))
        self.id = ret.id
        if len(self.parts)>1:
            self.type = "PARTSET"
        else:
            self.type = "PART"
        return self.id

    def num(self):
        return self.parts.len()

    def pos(self,pos):
        return self.parts[pos]

class BoundaryCondition:
    """ provides a way of defining imposed motions on boundary nodes"""
    def __init__(self):
        self.stub = DynaBase.get_stub()

    def create_spc(self,nodeset,
        tx=True,
        ty=True,
        tz=True,
        rx=True,
        ry=True,
        rz=True,
        cid=0,
        birth=0,
        death=1e20
        ):
        """Define nodal single point constraints.
        
        Parameters
        ----------
        nodeset : NodeSet.
            nodal set.
        contraint_x/y/z_direction : int      
            translational constraint in local x/y/z-direction.
        contraint_x/y/zaxis_rotate : int
            rotational constraint about local x/y/z-axis.
        
        """
        if birth==0 and death == 1e20:
            birthdeath = False
        else:
            birthdeath=True
        if nodeset.num()==1:
            nid = nodeset.pos(pos=0)
            option1='NODE'
        else:
            nid = nodeset.create(self.stub)
            option1='SET'
        ret = self.stub.CreateBdySpc(
            BdySpcRequest(
                option1=option1,
                birthdeath=birthdeath,
                nid=nid,
                cid=cid,
                dofx=tx,
                dofy=ty,
                dofz=tz,
                dofrx=rx,
                dofry=ry,
                dofrz=rz,
                birth=birth,
                death=death,
            )
        )
        logging.info("Boundary spc Created...")
        return ret

    def create_imposed_motion(self,partset,curve,motion=Motion.DISPLACEMENT,dof=DOF.X_TRANSLATIONAL,scalefactor=1):
        """Define an imposed nodal motion (velocity, acceleration, or displacement) on a node or a set of nodes.
        
        Parameters
        ----------
        partset : PartSet.
            part set.
        curve : Curve      
            Curve ID or function ID to describe motion value as a function of time.
        motion : enum
            Velocity/Acceleration/Displacement flag.
        dof : enum
            Applicable degrees-of-freedom.
        scalefactor : float
            Load curve scale factor.

        """
        partset.create(self.stub)
        curve.create(self.stub)
        for id in partset.parts:
            ret = self.stub.CreateBdyPrescribedMotion(
                BdyPrescribedMotionRequest(
                    id=0,
                    heading="",
                    option="RIGID",
                    typeid=id,
                    dof=dof.value,
                    vad=motion.value,
                    lcid=curve.id,
                    sf=scalefactor,
                    vid=0,
                    birth=0,
                    death=0,
                )
            )
        logging.info("Boundary prescribed motion Created...")
        return ret

class BeamFormulation(Enum):
    SPOTWELD = 9

class ShellFormulation(Enum):
    FULLY_INTEGRATED = -16
    BELYTSCHKO_TSAY = 2

class IGAFormulation(Enum):
    REISSNER_MINDLIN_FIBERS_AT_CONTROL_POINTS = 0
    KIRCHHOFF_LOVE_FIBERS_AT_CONTROL_POINTS = 1
    KIRCHHOFF_LOVE_FIBERS_AT_INTEGRATION_POINTS = 2
    REISSNER_MINDLIN_FIBERS_AT_INTEGRATION_POINTS = 3

class SolidFormulation(Enum):
    CONSTANT_STRESS_SOLID_ELEMENT  = 1
    EIGHT_POINT_HEXAHEDRON = 2
    FULLY_INTEGRATED_QUADRATIC_EIGHT_NODE_ELEMENT = 3

class HourglassType(Enum):
    STANDARD_LSDYNA_VISCOUS = 1
    FLANAGAN_BELYTSCHKO_VISOCOUS = 2
    FLANAGAN_BELYTSCHKO_VISOCOUS_WITH_EXTRA_VOLUME_INTEGRATION = 3
    FLANAGAN_BELYTSCHKO_STIFFNESS = 4
    FLANAGAN_BELYTSCHKO_STIFFNESS_WITH_EXTRA_VOLUME_INTEGRATION = 5
    BELYTSCHKO_BINDEMAN = 6

class BeamSection:
    def __init__(self,element_formulation,
        shear_factor=1,
        cross_section=0,
        thickness_n1=0,
        thickness_n2=0):
        stub = DynaBase.get_stub()
        ret = stub.CreateSectionBeam(
            SectionBeamRequest(
                elform=element_formulation,shrf=shear_factor,
                cst=cross_section,ts1=thickness_n1,ts2=thickness_n2
            )
        )
        self.id = ret.id

class ShellSection:
    def __init__(self,element_formulation,
        shear_factor=1,
        integration_points=5,
        printout=0,
        thickness1=0,thickness2=0,thickness3=0,thickness4=0):
        stub = DynaBase.get_stub()
        ret = stub.CreateSectionShell(
            SectionShellRequest(
                elform=element_formulation,shrf=shear_factor,
                nip=integration_points,
                propt=printout,t1=thickness1,t2=thickness2,t3=thickness3,t4=thickness4,
            )
        )
        self.id = ret.id

class IGASection:
    def __init__(self,element_formulation,
        shear_factor=1,thickness=1):
        stub = DynaBase.get_stub()
        ret = stub.CreateSectionIGAShell(
            SectionIGAShellRequest(
                elform=element_formulation,shrf=shear_factor,thickness=thickness
            )
        )
        self.id = ret.id

class Part:
    def __init__(self,id):
        self.stub = DynaBase.get_stub()
        self.id=id
        self.secid=0
        self.mid=0
        self.eosid=0
        self.hgid=0
        self.grav=0
        self.adpopt=0
        self.tmid=0
        self.formulation = 0

    def set_material(self,mat):
        """Set material"""
        mat.create(self.stub)
        self.mid = mat.material_id

    def set_element_formulation(self,formulation):
        """Element formulation options."""
        self.formulation = formulation.value

class BeamPart(Part):
    """ Define parts, that is, combine material information, section properties, hourglass type, thermal properties, and a flag for part adaptivity."""
    partlist = []
    def __init__(self,pid):
        Part.__init__(self,pid)
        self.stub = DynaBase.get_stub()
        BeamPart.partlist.append(self)
        self.crosstype = 1
    
    def set_cross_type(self,cross):
        self.crosstype = cross

    def set_diameter(self,diameter):
        self.diameter=diameter

    def set_property(self):
        sec=BeamSection(element_formulation=self.formulation,
        cross_section=self.crosstype,
        thickness_n1=self.diameter,
        thickness_n2=self.diameter
        )
        self.secid = sec.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id,
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )

class ShellPart(Part):
    """ Define parts, that is, combine material information, section properties, hourglass type, thermal properties, and a flag for part adaptivity."""
    partlist = []
    def __init__(self,pid):
        Part.__init__(self,pid)
        self.stub = DynaBase.get_stub()
        ShellPart.partlist.append(self)
        self.shear_factor = 1
        self.intpoints = 5
        self.print = 0
        self.thickness = 1

    def set_shear_factor(self,factor):
        """Shear correction factor which scales the transverse shear stress."""
        self.shear_factor = factor
    
    def set_integration_points(self,points=5):
        """Number of through thickness integration points."""
        self.intpoints = points

    def set_printout(self,print):
        """Printout option."""
        self.print = print

    def set_thickness(self,thickness):
        """Shell thickness."""
        self.thickness = thickness

    def set_property(self):
        sec=ShellSection(element_formulation=self.formulation,
        shear_factor=self.shear_factor,
        integration_points=self.intpoints,
        printout=self.print,
        thickness1 = self.thickness,
        thickness2 = self.thickness,
        thickness3 = self.thickness,
        thickness4 = self.thickness,
        )
        self.secid = sec.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id, 
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )

class IGAPart(Part):
    """ Define parts, that is, combine material information, section properties, hourglass type, thermal properties, and a flag for part adaptivity."""
    partlist = []
    def __init__(self,pid):
        Part.__init__(self,pid)
        self.stub = DynaBase.get_stub()
        IGAPart.partlist.append(self)
        self.shear_factor = 1
        self.thickness = 1

    def set_shear_factor(self,factor):
        """Shear correction factor which scales the transverse shear stress."""
        self.shear_factor = factor

    def set_thickness(self,thickness):
        """Shell thickness."""
        self.thickness = thickness

    def set_property(self):
        sec=IGASection(element_formulation=self.formulation,
        shear_factor=self.shear_factor,
        thickness = self.thickness,
        )
        self.secid = sec.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id, 
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )

class SolidPart(Part):
    """ Define parts, that is, combine material information, section properties, hourglass type, thermal properties, and a flag for part adaptivity."""
    partlist = []
    def __init__(self,pid):
        Part.__init__(self,pid)
        self.stub = DynaBase.get_stub()
        SolidPart.partlist.append(self)
        self.hourglasstype = -1

    def set_hourglass(self,type = HourglassType.STANDARD_LSDYNA_VISCOUS):
        self.hourglasstype = type.value

    def set_property(self):
        ret = self.stub.CreateSectionSolid(
            SectionSolidRequest(
                elform=self.formulation
            )
        )
        self.id = ret.id
        if self.hourglasstype>0:
            ret = self.stub.CreateHourglass(
            HourglassRequest(ihq=self.hourglasstype, qm=1, q1=0, q2=0, qb=0, qw=0)
            )
            self.hgid = ret.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id, 
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )

class AnalysisType(Enum):
    EXPLICIT = 0
    IMPLICIT = 1
    EXPLICIT_FOLLOWED_BY_IMPLICIT=2

class TimestepCtrol(Enum):
    CONSTANT_TIMESTEP_SIZE = 0
    AUTOMATICALLY_ADJUST_TIMESTEP_SIZE = 1

class Integration(Enum):
    NEWMARK_TIME_INTEGRATION = 1
    MODAL_SUPERPOSITION_FOLLOWING_EIGENVALUE = 2

class ImplicitAnalysis():
    """Activate implicit analysis and define associated control parameters."""
    def __init__(self,analysis_type=AnalysisType.IMPLICIT,initial_timestep_size=0):
        self.imflag = analysis_type.value
        self.dt0 = initial_timestep_size
        self.stub = DynaBase.get_stub()
        ret = self.stub.CreateControlImplicitGeneral(
            ControlImplicitGeneralRequest(
                imflag=self.imflag,dt0=self.dt0
            )
        )
    
    def set_timestep(self, control_flag=TimestepCtrol.CONSTANT_TIMESTEP_SIZE,Optimum_equilibrium_iteration_count=11):
        """Define parameters for automatic time step control during implicit analysis

        Parameters
        ----------
        control_flag : int
            Automatic time step control flag.
        Optimum_equilibrium_iteration_count : int      
            Optimum equilibrium iteration count per time step.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.iauto=control_flag.value
        self.iteopt = Optimum_equilibrium_iteration_count
        ret = self.stub.CreateControlImplicitAuto(
            ControlImplicitAutoRequest(
                iauto=self.iauto,iteopt=self.iteopt
            )
        )
        return ret

    def set_dynamic(self, integration_method=Integration.NEWMARK_TIME_INTEGRATION,gamma=0.5,beta=0.25):
        """Activate implicit dynamic analysis and define time integration constants

        Parameters
        ----------
        integration_method : enum
            Implicit analysis type.
        gamma : float      
            Newmark time integration constant.
        beta : float      
            Newmark time integration constant.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.imass = integration_method.value
        self.gamma = gamma
        self.beta = beta
        ret = self.stub.CreateControlImplicitDynamic(
            ControlImplicitDynamicRequest(
                imass=self.imass,gamma=self.gamma,beta=self.beta
            )
        )
        return ret

    def set_eigenvalue(self,number_eigenvalues=0,shift_scale=0):
        """Activates implicit eigenvalue analysis and defines associated input parameters

        Parameters
        ----------
        number_eigenvalues : int
            Number of eigenvalues to extract.
        shift_scale : float      
            Shift scale.
       
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.neig = number_eigenvalues
        self.shfscl= shift_scale
        ret = self.stub.CreateControlImplicitEigenvalue(
            ControlImplicitEigenvalueRequest(
                neig=self.neig,shfscl=self.shfscl
            )
        )
        return ret

    def set_solution(self,solution_method=12,
        iteration_limit=11,
        stiffness_reformation_limit=55,
        absolute_convergence_tolerance=1e-10):
        """specify whether a linear or nonlinear solution is desired.

        Parameters
        ----------
        solution_method : int
            Solution method for implicit analysis.
        iteration_limit : int      
            Iteration limit between automatic stiffness reformations.
        stiffness_reformation_limit : int
            Stiffness reformation limit per time step.
        absolute_convergence_tolerance : float
            Absolute convergence tolerance.
       
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.nsolver=solution_method
        self.ilimit=iteration_limit
        self.maxref=stiffness_reformation_limit
        self.abstol=absolute_convergence_tolerance
        ret = self.stub.CreateControlImplicitSolution(
            ControlImplicitSolutionRequest(
                nsolver=self.nsolver,ilimit=self.ilimit,maxref=self.maxref,abstol=self.abstol
            )
        )
        return ret


class ContactCategory(Enum):
    SURFACE_TO_SURFACE_CONTACT = 2
    SINGLE_SURFACE_CONTACT = 3
    SHELL_EDGE_TO_SURFACE_CONTACT = 4

class ContactType(Enum):
    AUTOMATIC = 1
    GENERAL=2
    RIGID = 3
    TIED=4
    TIED_WITH_FAILURE=5
    ERODING = 6
    EDGE=7

class ContactAlgorithm(Enum):
    PENALTY_BASED = 1
    CONSTRAINT_BASED = 2

class OffsetType(Enum):
    NULL = ""
    OFFSET = "OFFSET"
    BEAM_OFFSET= "BEAM_OFFSET"
    CONSTRAINED_OFFSET= "CONSTRAINED_OFFSET"

class ContactSurface:
    """Define contact interface"""
    def __init__(self,set):
        self.stub = DynaBase.get_stub()
        self.id = set.create(self.stub)
        self.thickness = 0
        if set.type.upper() == "PART":
            self.type = 3
            self.id = set.pos(0)
        elif set.type.upper() == "PARTSET":
            self.type = 2
        elif set.type.upper() == "NODESET" or set.type.upper() == "NODE":
            self.type = 4
        else:
            self.type = 0

    def set_contact_region(self,box):
        """Include in contact definition only those SURFA nodes/segments within box
        
        Parameters
        ----------
        box : Box
            Define a box-shaped volume.
        """
        self.id = box.id
        return self.id

    def set_contact_thickness(self,thickness):
        """ contact thickness for SURFA surface
        
         Parameters
        ----------
        thickness : float
            Contact thickness.
        """
        self.thickness = thickness

class Contact:
    """Provides a way of treating interaction between disjoint parts"""
    contactlist = []
    def __init__(self,type=ContactType.AUTOMATIC,category=ContactCategory.SINGLE_SURFACE_CONTACT,offset=OffsetType.NULL):
        self.stub = DynaBase.get_stub()
        self.rigidwall_penalties_scale_factor= 1
        self.max_penetration_check_multiplier = 4
        self.initial_penetrations = 0
        self.rigidwall_gap_stiffness =0
        self.category = category
        self.type = type
        self.mortar = False
        self.ignore = 0
        self.offset = offset.value
        self.static_friction_coeff=0
        self.dynamic_friction_coeff=0
        Contact.contactlist.append(self)
        
    def set_mortar(self):
        """The mortar contact,is a segment to segment penalty based contact."""
        self.mortar=True

    def set_algorithm(self,algorithm=ContactAlgorithm.PENALTY_BASED):
        self.algorithm = algorithm.value

    def set_friction_coefficient(self,static=0,dynamic=0):
        """Define the coefficient of friction.

        Parameters
        ----------
        static : float
            Static coefficient of friction.
        dynamic : float      
            Dynamic coefficient of friction.
        """
        self.static_friction_coeff = static
        self.dynamic_friction_coeff = dynamic
    
    def set_initial_penetration(self):
        """Ignore initial penetrations"""
        self.ignore=1

    def set_slave_surface(self,surface):
        """specifying the slave contact interface"""
        self.slavesurface = surface

    def set_master_surface(self,surface):
        """specifying the master contact interface"""
        self.mastersurface = surface

    def create(self):
        opcode = ""
        if self.type==ContactType.AUTOMATIC:
            opcode += "AUTOMATIC"
        elif self.type == ContactType.TIED:
            opcode += "TIED"
        else:
            opcode+=""
        if self.category != ContactCategory.SINGLE_SURFACE_CONTACT:
            msid=self.mastersurface.id
            mstyp=self.mastersurface.type
            mst=self.mastersurface.thickness
        if self.category == ContactCategory.SURFACE_TO_SURFACE_CONTACT:
            opcode += "_SURFACE_TO_SURFACE"
        elif self.category == ContactCategory.SINGLE_SURFACE_CONTACT:
            opcode += "_SINGLE_SURFACE"
            msid = 0
            mstyp = 0
            mst = 0
        elif self.category == ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT:
            opcode += "_SHELL_EDGE_TO_SURFACE"
        else:
            opcode +=""

        if self.offset == OffsetType.OFFSET:
            opcode += "_OFFSET"
        elif self.offset == OffsetType.BEAM_OFFSET:
            opcode += "_BEAM_OFFSET"
        elif self.offset == OffsetType.CONSTRAINED_OFFSET:
            opcode += "_CONSTRAINED_OFFSET"
        else:
            opcode += ""
        
        if self.mortar == True:
            option2 = "MORTAR"
        else:
            option2 = ""
        ret = self.stub.CreateContact(
            ContactRequest(
            cid=0,
            title="",
            option1=opcode,
            option2=option2,
            option3=False,
            offset=self.offset,
            ssid=self.slavesurface.id,
            msid=msid,
            sstyp=self.slavesurface.type,
            mstyp=mstyp,
            sapr=0,
            sbpr=0,
            fs=self.static_friction_coeff,
            fd=self.dynamic_friction_coeff,
            vdc=0,
            penchk=0,
            birthtime=0,
            sfsa=1,
            sfsb=1,
            sst=self.slavesurface.thickness,
            mst=mst,
            optionres=0,
            nfls=0,
            sfls=0,
            param=0,
            ct2cn=1,
            soft=0,
            sofscl=0.1,
            lcidab=0,
            maxpar=1.025,
            sbopt=2,
            depth=2,
            bsort=100,
            frcfrq=1,
            ignore = self.ignore,
            igap=1  
            )
        )
        logging.info("Contact  Created...")
        return ret

class Constraint:
    """provides a way of constraining degrees of freedom to move together in some way."""
    cnrbsetidlist = []
    def __init__(self):
        self.stub = stub = DynaBase.get_stub()

    def create_spotweld(self,nodeid1,nodeid2):
        """Define massless spot welds between non-contiguous nodal pairs.
        Parameters
        ----------
        nodeid1 : int
            Node ID.
        nodeid2 : int      
            Node ID.
       
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateConstrainedSpotWeld(
            ConstrainedSpotWeldRequest(
                node1=nodeid1,node2=nodeid2
            )
        )
        logging.info("Spotweld Created...")
        return ret

    def create_cnrb(self,nodeset):
        """Define a nodal rigid body which is a rigid body that consists of defined nodes.
        
        Parameters
        ----------
        nodeset : NodeSet
            This nodal set defines the rigid body.
       
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        nodeset.create(self.stub)
        nsid = nodeset.id
        Constraint.cnrbsetidlist.append(nsid)
        logging.info("CNRB Created...")
    
    @staticmethod
    def create(stub):
        for i in range(len(Constraint.cnrbsetidlist)):
            stub.CreateConstrainedNodalRigidBody(
                ConstrainedNodalRigidBodyRequest(pid=i,
                    nsid=Constraint.cnrbsetidlist[i]
                )
            )

class Point:
    def __init__(self,x=0,y=0,z=0):
        self.x = x
        self.y = y
        self.z = z

class Direction:
    def __init__(self,x=0,y=0,z=0):
        self.x = x
        self.y = y
        self.z = z

class RigidwallCylinder:
    rwlist = []
    def __init__(self,tail=Point(0,0,0),head=Point(0,0,0),radius=1,length=10):
        self.stub = DynaBase.get_stub()
        self.tail = tail
        self.head = head
        self.radius = radius
        self.length = length
        self.motion = 0
        self.lcid = 0
        self.dir = Direction(1,0,0)
        RigidwallCylinder.rwlist.append(self)

    def set_motion(self,curve,motion=Motion.VELOCITY,dir=Direction(1,0,0)):
        curve.create(self.stub)
        self.lcid=curve.id
        self.motion = motion.value
        if self.motion==Motion.DISPLACEMENT:
            self.motion = 1
        self.dir = dir

    def create(self):
        parameter = [self.tail.x,self.tail.y,self.tail.z,self.head.x,self.head.y,self.head.z,self.radius,self.length]
        self.stub.CreateRigidWallGeom(
            RigidWallGeomRequest(
                geomtype=3,
                motion=self.motion,
                display=1,
                parameter=parameter,
                lcid=self.lcid,
                vx=self.dir.x,
                vy=self.dir.y,
                vz=self.dir.z,
            )
        )
        logging.info("Cylinder Rigidwall Created...")


