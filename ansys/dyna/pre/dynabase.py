"""Module to create dyna input deck"""

import os
import grpc
import sys
import logging

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


class DynaBase:
    """Contains methods to create general LS-DYNA keyword"""

    def __init__(self, hostname = 'localhost'):
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

    def create_timestep(self, tssfac=0.0, isdo=0, dt2ms=0.0):
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

    def create_termination(self, endtim):
        """Create *CONTROL_TERMINATION keyword
        Parameters
        ----------
        endtim : float
            Termination time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTermination(TerminationRequest(endtim=endtim))
        logging.info("Termination Created...")
        return ret

    def control_accuracy(self, osu=0, inn=1, pidosu=0, iacc=0, exacc=0.0):
        """Define control parameters that can improve the accuracy of the calculation.
        Refer to: *CONTROL_ACCURACY
        Parameters
        ----------
        osu : int
            Global flag for 2nd order objective stress updates.
        inn : int
            Invariant node numbering for shell and solid elements.
        pidosu : int
            Part set ID for objective stress updates.
        iacc : int
            Implicit accuracy flag.
        exacc : float
             Explicit accuracy parameter.EQ.0.0: Off,GT.0.0: On
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlAccuracy(
            ControlAccuracyRequest(
                osu=osu, inn=inn, pidosu=pidosu, iacc=iacc, exacc=exacc
            )
        )
        logging.info("Control Accuracy Created...")
        return ret

    def control_energy(self, hgen=1, rwen=2, slnten=1, rylen=1, irgen=2):
        """Provide controls for energy dissipation options.
        Refer to: *CONTROL_ENERGY
        Parameters
        ----------
        hgen : int
            Hourglass energy calculation option.
        rwen : int
            Rigidwall energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        slnten : int
            Sliding interface energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        rylen : int
            Rayleigh energy dissipation option.EQ.1: Energy dissipation is not computed,EQ.2: Energy dissipation is computed
        irgen : int
             Initial reference geometry energy option.EQ.1: Initial reference geometry energy is not computed,EQ.2: Initial reference geometry energy is computed
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlEnergy(
            ControlEnergyRequest(
                hgen=hgen, rwen=rwen, slnten=slnten, rylen=rylen, irgen=irgen
            )
        )
        logging.info("Control Energy Created...")
        return ret

    def control_shell(
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

    def control_solid(
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

    def create_control_output(self, npopt=0,neecho=0):
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
            ControlOutputRequest(
                npopt=npopt, neecho=neecho
            )
        )
        logging.info("Control Output Created...")
        return ret

    def create_control_contact(self, rwpnal, shlthk=0,orien=1, ssthk=0, ignore=0, igactc=0):
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
                rwpnal=rwpnal, shlthk=shlthk,orien=orien, ssthk=ssthk, ignore=ignore, igactc=igactc
            )
        )
        logging.info("Control Contact Created...")
        return ret

    def create_database_binary(self,filetype="D3PLOT", dt=0, maxint=3, ieverp=0,dcomp=1, nintsld=1):
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
            DBBinaryRequest(filetype=filetype,dt=dt, maxint=maxint,ieverp=ieverp, dcomp=dcomp, nintsld=nintsld)
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
                vz=vz
            )
        )
        logging.info("Cylinder Rigidwall Geometric Created...")
        return ret
    
    def create_rigidwall_planar(
        self, nsid, tail, head, nsidex=0, boxid=0, fric=0
    ):
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
        normal = [tail[0],tail[1],tail[2],head[0],head[1],head[2]]
        ret = self.stub.CreateRigidWallPlanar(
            RigidWallPlanarRequest(
                nsid=nsid,
                nsidex=nsidex,
                boxid=boxid,
                fric = fric,
                normal = normal
            )
        )
        logging.info("Rigidwall Planar Created...")
        return ret

    def create_init_vel(
        self, nsid, velocity
    ):
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
        ret = self.stub.CreateInitVel(
            InitVelRequest(
                nsid=nsid, velocity=velocity
            )
        )
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

    def create_defineorientation(self, vid, iop, vector, node1,node2):
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
                vid=vid,
                iop=iop,
                vector=vector,
                node1=node1,
                node2=node2
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

    def create_nodeset(self, option, sid, entities,genoption=""):
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

    def create_section_discrete(self, secid, dro=0,kd=0,v0=0,cl=0,fd=0,cdl=0,tdl=0):
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
            SectionDiscreteRequest( secid=secid, dro=dro,kd=kd,v0=v0,cl=cl,fd=fd,cdl=cdl,tdl=tdl)
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
        offset = "",
        ssid=0,
        msid=0,
        sstyp=3,
        mstyp=3,
        sapr=0,
        sbpr=0,
        sfsa = 1,
        sfsb = 1,
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
                offset = offset,
                ssid=ssid,
                msid=msid,
                sstyp=sstyp,
                mstyp=mstyp,
                sapr=sapr,
                sbpr = sbpr,
                sfsa = sfsa,
                sfsb = sfsb,
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

    def create_constrained_joint(self, type, nodes, rps=1.0,damp=1.0):
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
            ConstrainedJointRequest(type=type,nodes=nodes,rps=rps,damp=damp)
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

    def create_mat_fabric(self, mid, ro, ea, eb,prba,prab,gab):
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
        ret = self.stub.CreateMatFabric(MatFabricRequest(mid=mid, ro=ro, ea=ea, eb=eb,prba=prba,prab=prab,gab=gab))
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
        ret = self.stub.CreateMatSpringNonlinearElastic(MatSpringNonlinearElasticRequest(mid=mid, lcid=lcid))
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
        ret = self.stub.CreateMatDamperNonlinearViscous(MatDamperNonlinearViscousRequest(mid=mid, lcdr=lcdr))
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

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.SaveFile(SaveFileRequest(name=self.mainname))
        msg = self.mainname + " is outputed..."
        logging.info(msg)
        return ret
