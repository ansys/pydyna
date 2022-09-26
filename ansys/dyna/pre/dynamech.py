"""
Mechanical API
==============

Module to setup Explicit or Implicit analysis
"""

from .dynabase import *  # noqa : F403

class DynaMech(DynaBase):
    """Define an Mechanical analysis."""

    def __init__(self):
        self.stub = DynaSolution.get_stub()
        self.mainname = ""
        DynaBase.stub = self.stub
        self.casetype = CaseType.STRUCTURE

    def create_control_output(self, npopt=0, neecho=0):
        """Set miscellaneous output parameters.

        Parameters
        ----------
        npopt : int
            Print suppression during input phase flag for the d3hsp file:
            EQ.0: No suppression.
            EQ.1: Nodal coordinates, element connectivities, rigid wall definitions,
            nodal SPCs, initial velocities, initial strains, adaptive constraints, and
            SPR2/SPR3 constraints are not printed.
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
        ret = self.stub.CreateControlOutput(ControlOutputRequest(npopt=npopt, neecho=neecho))
        logging.info("Control Output Created...")
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
            RigidWallPlanarRequest(nsid=nsid, nsidex=nsidex, boxid=boxid, fric=fric, normal=normal)
        )
        logging.info("Rigidwall Planar Created...")
        return ret

    def set_init_velocity(self, translational=Velocity(0,0,0),rotational=RotVelocity(0,0,0)):
        """Define initial nodal point velocities using nodal set ID.

        Parameters
        ----------
        nsid : int
            Nodal set ID.
        translational : Velocity
            Initial translational velocity in x,y,z-direction.
        rotational : RotVelocity
            Initial rotational velocity about the x,y,z-axis.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        velocity = [translational.x,translational.y,translational.z,rotational.x,rotational.y,rotational.z]
        ret = self.stub.CreateInitVel(InitVelRequest(nsid=0, velocity=velocity))
        logging.info("Initial velocity Created...")
        return ret

    def create_init_vel_rigidbody(self, pid, vx=0, vy=0, vz=0, vxr=0, vyr=0, vzr=0, lcid=0):
        """Define the initial translational and rotational velocities at the center
        of gravity for a rigid body or a nodal rigid body.

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
            InitVelRigidBodyRequest(pid=pid, vx=vx, vy=vy, vz=vz, vxr=vxr, vyr=vyr, vzr=vzr, lcid=lcid)
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
            self.stub.CreateInitVelGenerationStartTime(InitVelGenerationStartTimeRequest(stime=stime))
        logging.info("Initial velocity for bodies Created...")
        return ret

    def create_defineorientation(self, vid, iop, vector, node1, node2):
        """Define orientation vectors for discrete springs and dampers.

        Parameters
        ----------
        vid : int
            Orientation vector ID.
        iop : int
            Option:

            * EQ.0: deflections/rotations are measured and forces/moments applied
              along the following orientation vector.
            * EQ.1: deflections/rotations are measured and forces/moments applied
              along the axis between the two spring/damper nodes projected onto the plane normal
              to the following orientation vector.
            * EQ.2: deflections/rotations are measured and forces/moments applied
              along a vector defined by the following two nodes.
            * EQ.3: deflections/rotations are measured and forces/moments applied
              along the axis between the two spring/damper nodes projected onto the
              plane normal to the a vector defined by the following two nodes.
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
            DefineOrientationRequest(vid=vid, iop=iop, vector=vector, node1=node1, node2=node2)
        )
        logging.info("DefineOrientation Created...")
        return ret

    def create_shellset(self, option, title, sid, eids):
        """Define a set of shell elements with optional identical or unique attributes.

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
        ret = self.stub.CreateShellSet(ShellSetRequest(option=option, title=title, sid=sid, eids=eids))
        logging.info("Shell Set Created...")
        return ret

    def create_solidset(self, title, sid, ki):
        """Define a set of solid elements.

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

    def create_section_solid(self, title, secid, elform):
        """Define section properties for solid continuum and fluid elements.

        Parameters
        ----------
        title : string
            Define title for section solid.
        secid : int
            Section ID. SECID is referenced on the \*PART card.
            A unique number must be specified.
        elform : int
            Element formulation options.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionSolid(SectionSolidRequest(title=title, secid=secid, elform=elform))
        logging.info("Section Solid Created...")
        return ret

    def create_section_discrete(self, secid, dro=0, kd=0, v0=0, cl=0, fd=0, cdl=0, tdl=0):
        """Defined spring and damper elements for translation and rotation.

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
            SectionDiscreteRequest(secid=secid, dro=dro, kd=kd, v0=v0, cl=cl, fd=fd, cdl=cdl, tdl=tdl)
        )
        logging.info("Section Discrete Created...")
        return ret

    def create_hourglass(self, ghid, ihq, qm=0.1, q1=1.5, q2=0.06, qb=1e-9, qw=1e-9):
        """Define hourglass and bulk viscosity properties.

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
        ret = self.stub.CreateHourglass(HourglassRequest(ghid=ghid, ihq=ihq, qm=qm, q1=q1, q2=q2, qb=qb, qw=qw))
        logging.info("Hourglass 1 Created...")
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
        """Define an imposed nodal motion (velocity, acceleration, or displacement) on a
        node or a set of nodes.

        Parameters
        ----------
        id : int
            PRESCRIBED MOTION set ID to which this node, node set,
            segment set, or rigid body belongs.
        heading : string
            An optional descriptor for the given ID that will be written
            into the d3hsp file and the bndout file.
        option : string
            Available options:(NODE,SET,RIGID)
        typeid : int
            Node ID, nodal set ID,segment set ID , part ID for a rigid body.
        dof : int
            Applicable degrees-of-freedom.
        vad : int
            Velocity/Acceleration/Displacement flag:EQ.0:Velocity,
            EQ.1: Acceleration,EQ.2: Displacement,
            EQ.3: Velocity as a function of displacement,
            EQ.4: Relative displacement
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

    def create_constrained_extra_nodes(self, option="NODE", pid=0, nid=0, iflag=0):
        """Define extra nodes for rigid body.

        Parameters
        ----------
        option : string
            Available options include:NODE,SET
        pid : int
            Part ID of rigid body to which the nodes will be added.
        nid : int
            Node or node set ID
        iflag : int
            This flag is meaningful if and only if the inertia properties of
            the Part ID are defined in PART_INERTIA.

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
        ret = self.stub.CreateConstrainedJoint(ConstrainedJointRequest(type=type, nodes=nodes, rps=rps, damp=damp))
        logging.info("Constrained joint Created...")
        return ret

    def create_load_body(self, option="X", lcid=0):
        """Define body force loads due to a prescribed base acceleration or
        angular velocity using global axes directions.

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

    
        """Provide a viscous translational damper with an arbitrary force
         a function of velocity dependency or a rotational damper with an
         arbitrary moment as a function of rotational velocity dependency.

        Parameters
        ----------
        mid : int
            Material identification.
        lcdr : int
            Load curve ID defining force as a function of rate-of-displacement
            relationship or a moment as a function of rate-of-rotation relationship.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateMatDamperNonlinearViscous(MatDamperNonlinearViscousRequest(mid=mid, lcdr=lcdr))
        logging.info("Material Damper Nonlinear Viscous Created...")
        return ret

    def set_part_damping_stiffness(self, pids, coef=0.0):
        """Assign stiffness damping coefficient by part ID or part set ID.

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
            ret = self.stub.CreateDampingPartStiffness(DampingPartStiffnessRequest(isset=True, id=sid, coef=coef))
        else:
            id = pids[0]
            ret = self.stub.CreateDampingPartStiffness(DampingPartStiffnessRequest(isset=False, id=id, coef=coef))

        logging.info("Damping global Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.set_accuracy(
            objective_stress_updates=Switch.ON,
            invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,
            implicit_accuracy_flag=Switch.ON,
        )
        self.set_bulk_viscosity(bulk_viscosity_type=BulkViscosity.COMPUTE_INTERNAL_ENERGY_DISSIPATED)
        self.set_energy(
            hourglass_energy=EnergyFlag.COMPUTED,
            sliding_interface_energy=EnergyFlag.COMPUTED,
        )
        self.set_hourglass(
            controltype=HourglassControl.FLANAGAN_BELYTSCHKO_INTEGRATION_SOLID,
            coefficient=0,
        )
        self.create_control_shell(
            wrpang=0,
            esort=1,
            irnxx=0,
            istupd=4,
            theory=0,
            bwc=1,
            miter=1,
            proj=1,
            irquad=0,
        )
        self.create_control_contact(rwpnal=1.0, ignore=1, igactc=0)
        for obj in Contact.contactlist:
            obj.create()
        for obj in Gravity.gravitylist:
            obj.create()
        for obj in BeamPart.partlist:
            obj.set_property()
        for obj in ShellPart.partlist:
            obj.set_property()
        for obj in SolidPart.partlist:
            obj.set_property()
        for obj in IGAPart.partlist:
            obj.set_property()
        for obj in DiscretePart.partlist:
            obj.set_property()
        for obj in RigidwallCylinder.rwlist:
            obj.create()
        Constraint.create(self.stub)

class Airbag:
    """Define an airbag or control volume.

    Parameters
    ----------
    modeltype : string
        specifies one of the following thermodynamic models for modeling airbags using a control volume (CV) approach.
        "SIMPLE_AIRBAG_MODEL"

    Set : SegmentSet/PartSet
        Set : EQ.0:segment EQ.1: part set ID.
    heat_capacity_at_constant_volume : float
        Heat capacity at constant volume.
    heat_capacity_at_constant_pressure : float
        Heat capacity at constant pressure.
    input_gas_temperature : float
        Temperature of input gas.
    input_mass_flow_rate : int
        Load curve ID specifying input mass flow rate.
    shape_factor_for_exit_hole : float
        Shape factor for exit hole.
    ambient_pressure : float
        Ambient pressure.
    ambient_density : float
        Ambient density.

    Returns
    -------
    bool
        "True" when successful, "False" when failed.
    """

    def __init__(
        self,
        set,
        heat_capacity_at_constant_volume=0,
        heat_capacity_at_constant_pressure=0,
        input_gas_temperature=0,
        input_mass_flow_rate=Curve(x=[], y=[]),
        shape_factor_for_exit_hole=0,
        ambient_pressure=0,
        ambient_density=0,
    ):
        self.stub = DynaSolution.get_stub()
        self.cv = heat_capacity_at_constant_volume
        self.cp = heat_capacity_at_constant_pressure
        self.t = input_gas_temperature
        self.lcid = input_mass_flow_rate.create(self.stub)
        self.mu = shape_factor_for_exit_hole
        self.pe = ambient_pressure
        self.ro = ambient_density
        self.sid = set.create(self.stub)
        if set.type == "SEGMENTSET":
            self.sidtyp = 0
        else:
            self.sidtyp = 1
        ret = self.stub.CreateAirbagModel(
            AirbagModelRequest(
                modeltype="SIMPLE_AIRBAG_MODEL",
                sid=self.sid,
                sidtyp=self.sidtyp,
                cv=self.cv,
                cp=self.cp,
                t=self.t,
                lcid=self.lcid,
                mu=self.mu,
                area=0,
                pe=self.pe,
                ro=self.ro,
            )
        )
        logging.info("Airbag Model Created...")