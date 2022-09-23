"""
Base
==========

Module to create dyna input deck
"""

# from subprocess import DETACHED_PROCESS
import grpc

"""
import kwprocess_pb2
import kwprocess_pb2_grpc
"""

from .kwprocess_pb2 import *  # noqa : F403
from .kwprocess_pb2_grpc import *  # noqa : F403

import logging
from enum import Enum

class Motion(Enum):
    VELOCITY = 0
    ACCELERATION = 1
    DISPLACEMENT = 2


class DOF(Enum):
    X_TRANSLATIONAL = 1
    Y_TRANSLATIONAL = 2
    Z_TRANSLATIONAL = 3
    X_ROTATIONAL = 5
    Y_ROTATIONAL = 6
    Z_ROTATIONAL = 7


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
    RICHARDS_WILKINS_BULK_VISCOSITY = 2
    COMPUTE_INTERNAL_ENERGY_DISSIPATED = -2


class CaseType(Enum):
    STRUCTURE = 1
    ICFD = 2
    SALE = 3
    EM = 4
    IGA = 5

from .dynasolution import DynaSolution # noqa : F403

class Box:
    """Define a box-shaped volume."""

    def __init__(self, xmin=0, xmax=0, ymin=0, ymax=0, zmin=0, zmax=0):
        self.xmin = xmin
        self.xmax = xmax
        self.ymin = ymin
        self.ymax = ymax
        self.zmin = zmin
        self.xmax = zmax

    def create(self, stub):
        """Create box."""
        ret = stub.CreateDefineBox(
            DefineBoxRequest(
                xmin=self.xmin,
                xmax=self.xmax,
                ymin=self.ymin,
                ymax=self.ymax,
                zmin=self.zmin,
                zmax=self.zmax,
            )
        )
        self.id = ret.boxid
        logging.info(f"Box {self.boxid} defined...")


class Curve:
    """Define a curve [for example, load (ordinate value)] as a function of time."""

    def __init__(self, sfo=1, x=[], y=[]):
        self.sfo = sfo
        self.abscissa = x
        self.ordinate = y

    def create(self, stub):
        """Create curve."""
        ret = stub.CreateDefineCurve(
            DefineCurveRequest(
                sfo=self.sfo, abscissa=self.abscissa, ordinate=self.ordinate
            )
        )
        self.id = ret.id
        logging.info(f"Curve {self.id} defined...")
        return self.id


class Point:
    """Define point."""
    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z


class Direction:
    """Define direction."""
    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z

class Velocity:
    """Define translational velocity."""
    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z

class RotVelocity:
    """Define rotational velocity."""
    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z

class DynaBase:
    """Contains methods to create general LS-DYNA keyword."""
    def __init__(self):
        self.stub = DynaSolution.get_stub()
        self.mainname = ""
        DynaBase.stub = self.stub

    def set_timestep(self, tssfac=0.9, isdo=0, timestep_size_for_mass_scaled=0.0):
        """Set structural time step size control using different options.

        Parameters
        ----------
        tssfac : float
            Scale factor for computed time step.
        isdo : int
            Basis of time size calculation for 4-node shell elements.
        timestep_size_for_mass_scaled : float
            Time step size for mass scaled solutions.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTimestep(TimestepRequest(tssfac=tssfac, isdo=isdo, dt2ms=timestep_size_for_mass_scaled))
        logging.info("Timestep Created...")
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

        Examples
        --------
        Create a \*INITIAL_VELOCITY keyword.

        \$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        \*INITIAL_VELOCITY


        &       vx        vy        vz       vxr       vyr       vzr
        1.480E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
        \$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

        opcode = "INITIAL_VELOCITY"
        keyworddata = "0\\n1.480E+01,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00"
        create_general_keyword(opcode = opcode,keyworddata=keyworddata)
        """
        ret = self.stub.CreateGeneralKWD(GeneralKWDRequest(opcode=opcode, keyworddata=keyworddata))
        msg = opcode + " Created..."
        logging.info("msg")
        return ret

# -------------------------------------------------------------------------------------------------

class BaseSet:
    """Define the base class for all set classes."""
    def __init__(self):
        self.type = "PARTSET"
        self.id = 0


class NodeSet:
    """Define a nodal set with some identical or unique attributes."""

    def __init__(self, nodes=[]):
        self.nodes = nodes

    def create(self, stub):
        """Create node set."""
        if len(self.nodes) <= 0:
            return 0
        ret = stub.CreateNodeSet(NodeSetRequest(option="LIST", sid=0, genoption="NODE", entities=self.nodes))
        self.id = ret.id
        if len(self.nodes) > 1:
            self.type = "NODESET"
        else:
            self.type = "NODE"
        return self.id

    def num(self):
        """Get the number of nodes in this set."""
        return len(self.nodes)

    def id(self, pos):
        """Get the node ID by position."""
        return self.nodes[pos]


class SetType(Enum):
    SHELL = "SET_SHELL"
    SOLID = "SET_SOLID"
    BEAM = "SET_BEAM"
    TSHELL = "SET_TSHELL"
    DISCRETE = "SET_DISCRETE"


class NodesetGeneral(BaseSet):
    """Include nodal points of element sets defined by SET_XXXX_LIST, where XXXX could be SHELL, SOLID, BEAM, TSHELL and DISCRETE."""
    def __init__(self,settype=SetType.SHELL,setids=[]):
        self.settype = settype.value
        self.setids = setids

    def create(self, stub):
        """Create node set."""
        if len(self.setids) <= 0:
            return 0
        ret = stub.CreateNodeSet(
            NodeSetRequest(
                option="GENERAL", sid=0, genoption=self.settype, entities=self.setids
            )
        )
        self.id = ret.id
        self.type = "NODESET"
        return self.id


class PartSet(BaseSet):
    """Define a set of parts with optional attributes."""

    def __init__(self, parts=[]):
        self.parts = parts

    def create(self, stub):
        """Create part set."""
        if len(self.parts) <= 0:
            return 0
        ret = stub.CreatePartSet(PartSetRequest(sid=0, pids=self.parts))
        self.id = ret.id
        if len(self.parts) > 1:
            self.type = "PARTSET"
        else:
            self.type = "PART"
        return self.id

    def num(self):
        """Get the number of parts in this set."""
        return len(self.parts)

    def pos(self, pos):
        """Get the part ID by position."""
        return self.parts[pos]


class SegmentSet(BaseSet):
    """Define a set of segments with optional identical or unique attributes.

    Parameters
    ----------
    segments : list [[point1,point2,point3,point4],[point5,point6,point7,point8]...]
       Define segments.
    """

    def __init__(self, segments=[]):
        self.segments = segments
        self.type = "SEGMENTSET"

    def create(self, stub):
        """Create segment set."""
        if len(self.segments) <= 0:
            return 0
        n1 = []
        n2 = []
        n3 = []
        n4 = []
        for i in range(len(self.segments)):
            n1.append(self.segments[i][0])
            n2.append(self.segments[i][1])
            n3.append(self.segments[i][2])
            n4.append(self.segments[i][3])
        ret = stub.CreateSegmentSet(SegmentSetRequest(n1=n1, n2=n2, n3=n3, n4=n4))
        self.id = ret.id
        logging.info("Segment Set Created...")
        return self.id


class BoundaryCondition:
    """Provides a way of defining imposed motions on boundary nodes."""

    def __init__(self):
        self.stub = DynaSolution.get_stub()

    def create_spc(
        self,
        nodeset,
        tx=True,
        ty=True,
        tz=True,
        rx=True,
        ry=True,
        rz=True,
        cid=0,
        birth=0,
        death=1e20,
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
        if birth == 0 and death == 1e20:
            birthdeath = False
        else:
            birthdeath = True
        if nodeset.num() == 1:
            nid = nodeset.pos(pos=0)
            option1 = "NODE"
        else:
            nid = nodeset.create(self.stub)
            option1 = "SET"
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

    def create_imposed_motion(
        self,
        partset,
        curve,
        motion=Motion.DISPLACEMENT,
        dof=DOF.X_TRANSLATIONAL,
        scalefactor=1,
    ):
        """Define an imposed nodal motion (velocity, acceleration, or displacement)
        on a node or a set of nodes.

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
    FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE = 9


class IGAFormulation(Enum):
    REISSNER_MINDLIN_FIBERS_AT_CONTROL_POINTS = 0
    KIRCHHOFF_LOVE_FIBERS_AT_CONTROL_POINTS = 1
    KIRCHHOFF_LOVE_FIBERS_AT_INTEGRATION_POINTS = 2
    REISSNER_MINDLIN_FIBERS_AT_INTEGRATION_POINTS = 3


class SolidFormulation(Enum):
    EIGHT_POINT_ENHANCED_STRAIN_SOLID_ELEMENT = -18
    CONSTANT_STRESS_SOLID_ELEMENT = 1
    EIGHT_POINT_HEXAHEDRON = 2
    FULLY_INTEGRATED_QUADRATIC_EIGHT_NODE_ELEMENT = 3


class HourglassType(Enum):
    STANDARD_LSDYNA_VISCOUS = 1
    FLANAGAN_BELYTSCHKO_VISOCOUS = 2
    FLANAGAN_BELYTSCHKO_VISOCOUS_WITH_EXTRA_VOLUME_INTEGRATION = 3
    FLANAGAN_BELYTSCHKO_STIFFNESS = 4
    FLANAGAN_BELYTSCHKO_STIFFNESS_WITH_EXTRA_VOLUME_INTEGRATION = 5
    BELYTSCHKO_BINDEMAN = 6
    ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS = 8


class BeamSection:
    """Define cross sectional properties for beam, truss, discrete beam, and cable elements."""

    def __init__(
        self,
        element_formulation,
        shear_factor=1,
        cross_section=0,
        thickness_n1=0,
        thickness_n2=0,
    ):
        stub = DynaSolution.get_stub()
        ret = stub.CreateSectionBeam(
            SectionBeamRequest(
                elform=element_formulation,
                shrf=shear_factor,
                cst=cross_section,
                ts1=thickness_n1,
                ts2=thickness_n2,
            )
        )
        self.id = ret.id


class ShellSection:
    """Define section properties for shell elements."""

    def __init__(
        self,
        element_formulation,
        shear_factor=1,
        integration_points=5,
        printout=0,
        thickness1=0,
        thickness2=0,
        thickness3=0,
        thickness4=0,
    ):
        stub = DynaSolution.get_stub()
        ret = stub.CreateSectionShell(
            SectionShellRequest(
                elform=element_formulation,
                shrf=shear_factor,
                nip=integration_points,
                propt=printout,
                t1=thickness1,
                t2=thickness2,
                t3=thickness3,
                t4=thickness4,
            )
        )
        self.id = ret.id


class IGASection:
    """Define section properties for isogeometric shell elements."""

    def __init__(self, element_formulation, shear_factor=1, thickness=1):
        stub = DynaSolution.get_stub()
        ret = stub.CreateSectionIGAShell(
            SectionIGAShellRequest(elform=element_formulation, shrf=shear_factor, thickness=thickness)
        )
        self.id = ret.id


class Part:
    """Define part object."""

    def __init__(self, id):
        self.stub = DynaSolution.get_stub()
        self.id = id
        self.secid = 0
        self.mid = 0
        self.eosid = 0
        self.hgid = 0
        self.grav = 0
        self.adpopt = 0
        self.tmid = 0
        self.formulation = 0
        self.stiffness_damping = 0
        self.rigidbody_initial_velocity = False
        self.translation = Velocity(0,0,0)
        self.rotation = RotVelocity(0,0,0)
        self.extra_nodes_defined = False

    def set_material(self, mat):
        """Set material."""
        mat.create(self.stub)
        self.mid = mat.material_id

    def set_element_formulation(self, formulation):
        """Set Element formulation."""
        self.formulation = formulation.value

    def set_stiffness_damping_coefficient(self, coefficient):
        """Assign stiffness damping coefficient."""
        self.stiffness_damping = coefficient

    def set_extra_nodes(self,nodeset):
        """Define extra nodes for rigid body.

        Parameters
        ----------
        nodeset : NodeSet
            Extra nodes list.
        """
        self.extra_nodes_defined = True
        self.extra_nodes = nodeset

    def set_rigidbody_initial_velocity(self,translation=Velocity(0,0,0),rotation=RotVelocity(0,0,0)):
        """Define the initial translational and rotational velocities at the center of gravity for a rigid body or a nodal rigid body."""
        self.rigidbody_initial_velocity = True
        self.translation = translation
        self.rotation = rotation
    
    def set_property(self):
        """Set Properties for part object."""
        if self.stiffness_damping > 0:
            self.stub.CreateDampingPartStiffness(
                DampingPartStiffnessRequest(isset=False, id=self.id, coef=self.stiffness_damping)
            )
            logging.info(f"Assign stiffness damping coefficient to part {self.id}.")
        if self.extra_nodes_defined:
            nid = self.extra_nodes.create(self.stub)
            option=self.extra_nodes.type
            self.stub.CreateConstrainedExtraNodes(ConstrainedExtraNodesRequest(option="SET", pid=self.id, nid=nid, iflag=0))
            logging.info(f"Constrained extra nodes defined for part {self.id}.")
        if self.rigidbody_initial_velocity:
            ret = self.stub.CreateInitVelRigidBody(
            InitVelRigidBodyRequest(
                pid=self.id, vx=self.translation.x, vy=self.translation.y, vz=self.translation.z, vxr=self.rotation.x, vyr=self.rotation.y, vzr=self.rotation.z, lcid=0
                )
            )
            logging.info(f"Initial velocity for rigidbody {self.id}.")

class BeamPart(Part):
    """Define parts, that is, combine material information, section properties,
    hourglass type, thermal properties, and a flag for part adaptivity."""

    partlist = []

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaSolution.get_stub()
        BeamPart.partlist.append(self)
        self.crosstype = 1

    def set_cross_type(self, cross):
        """Set cross section type."""
        self.crosstype = cross

    def set_diameter(self, diameter):
        """Set outer diameter for cross section."""
        self.diameter = diameter

    def set_property(self):
        """Set Properties for beam part object."""
        sec = BeamSection(
            element_formulation=self.formulation,
            cross_section=self.crosstype,
            thickness_n1=self.diameter,
            thickness_n2=self.diameter,
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
    """Define parts, that is, combine material information, section properties,
    hourglass type, thermal properties, and a flag for part adaptivity."""

    partlist = []

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaSolution.get_stub()
        ShellPart.partlist.append(self)
        self.shear_factor = 1
        self.intpoints = 5
        self.print = 0
        self.thickness = 1
        self.hourglasstype = -1
        self.defined_des_surface = False
        self.despid = 0
        self.desxid = 0
        self.des_nquad = 1
        self.des_nsid = 0
        self.des_rsf = 1

    def set_hourglass(self, type=HourglassType.STANDARD_LSDYNA_VISCOUS):
        """Define hourglass/bulk viscosity identification."""
        self.hourglasstype = type.value

    def set_shear_factor(self, factor):
        """Shear correction factor which scales the transverse shear stress."""
        self.shear_factor = factor

    def set_integration_points(self, points=5):
        """Number of through thickness integration points."""
        self.intpoints = points

    def set_printout(self, print):
        """Printout option."""
        self.print = print

    def set_thickness(self, thickness):
        """Shell thickness."""
        self.thickness = thickness

    def set_des_surface(self, despid=0, desxid=0, nquad=1, nsid=0, rsf=-1):
        """Generate and place discrete element sphere (DES) elements on the surface of shell elements.

        Parameters
        ----------
        despid : int
            Part ID for generated DES elements.
        desxid : int
            Section ID for generated DES elements.
        nquad : int
            Number of equally spaced DES elements created on a shell element in each local shell direction.
        nsid : int
            If defined, this card creates a node set with ID NSID for the nodes generated by this card.
        rsf : float
            Scale factor for determining the DES radius.
        """
        self.defined_des_surface = True
        self.despid = despid
        self.desxid = desxid
        self.des_nquad = nquad
        self.des_nsid = nsid
        self.des_rsf = rsf
        

    def set_property(self):
        """Set properties for shell part."""
        if self.defined_des_surface:
            ret = self.stub.CreateDefineDEMeshSurface(
                DefineDEMeshSurfaceRequest(
                    sid=self.id,
                    type=1,
                    despid=self.despid,
                    desxid=self.desxid,
                    nquad=self.des_nquad,
                    nsid=self.des_nsid,
                    rsf=self.des_rsf,
                )
            )
            logging.info("Define discrete element mesh surface Created...")
        Part.set_property(self)
        sec = ShellSection(
            element_formulation=self.formulation,
            shear_factor=self.shear_factor,
            integration_points=self.intpoints,
            printout=self.print,
            thickness1=self.thickness,
            thickness2=self.thickness,
            thickness3=self.thickness,
            thickness4=self.thickness,
        )
        self.secid = sec.id
        if self.hourglasstype > 0:
            ret = self.stub.CreateHourglass(HourglassRequest(ihq=self.hourglasstype, qm=1, q1=0, q2=0, qb=0, qw=0))
            self.hgid = ret.id
        else:
            self.hgid = 0
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
    """Define parts, that is, combine material information, section properties,
    hourglass type, thermal properties, and a flag for part adaptivity."""

    partlist = []

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaSolution.get_stub()
        IGAPart.partlist.append(self)
        self.shear_factor = 1
        self.thickness = 1

    def set_shear_factor(self, factor):
        """Shear correction factor which scales the transverse shear stress."""
        self.shear_factor = factor

    def set_thickness(self, thickness):
        """Shell thickness."""
        self.thickness = thickness

    def set_property(self):
        """Set properties for IGA part."""
        sec = IGASection(
            element_formulation=self.formulation,
            shear_factor=self.shear_factor,
            thickness=self.thickness,
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
    """Define parts, that is, combine material information, section properties,
    hourglass type, thermal properties, and a flag for part adaptivity."""

    partlist = []

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaSolution.get_stub()
        SolidPart.partlist.append(self)
        self.hourglasstype = -1

    def set_hourglass(self, type=HourglassType.STANDARD_LSDYNA_VISCOUS):
        """Set hourglass/bulk viscosity identification."""
        self.hourglasstype = type.value

    def set_property(self):
        """Set properties for solid part."""
        ret = self.stub.CreateSectionSolid(SectionSolidRequest(elform=self.formulation))
        self.secid = ret.id
        if self.hourglasstype > 0:
            ret = self.stub.CreateHourglass(HourglassRequest(ihq=self.hourglasstype, qm=1, q1=0, q2=0, qb=0, qw=0))
            self.hgid = ret.id
        else:
            self.hgid = 0
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

class DRO(Enum):
    DESCRIBES_TRANSLATIONAL_SPRING = 0
    DESCRIBES_TORSIONAL_SPRING = 1

class DiscretePart(Part):
    """Define parts, that is, combine material information, section properties, hourglass type, thermal properties, and a flag for part adaptivity."""
    partlist = []
    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaSolution.get_stub()
        DiscretePart.partlist.append(self)
        self.displacement_option = 0

    def set_displacement_option(self, displacement_option=DRO.DESCRIBES_TRANSLATIONAL_SPRING):
        """Set displacement/Rotation Option."""
        self.displacement_option = displacement_option.value

    def set_property(self):
        """Set properties for discrete part."""
        Part.set_property(self)
        ret = self.stub.CreateSectionDiscrete(SectionDiscreteRequest(dro=self.displacement_option, kd=0, v0=0, cl=0, fd=0, cdl=0, tdl=0))
        self.secid = ret.id
        self.hgid = 0
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
    EXPLICIT_FOLLOWED_BY_IMPLICIT = 2


class TimestepCtrol(Enum):
    CONSTANT_TIMESTEP_SIZE = 0
    AUTOMATICALLY_ADJUST_TIMESTEP_SIZE = 1


class Integration(Enum):
    NEWMARK_TIME_INTEGRATION = 1
    MODAL_SUPERPOSITION_FOLLOWING_EIGENVALUE = 2


class ImplicitAnalysis:
    """Activate implicit analysis and define associated control parameters."""

    def __init__(self, analysis_type=AnalysisType.IMPLICIT, initial_timestep_size=0):
        self.imflag = analysis_type.value
        self.dt0 = initial_timestep_size
        self.stub = DynaSolution.get_stub()
        ret = self.stub.CreateControlImplicitGeneral(ControlImplicitGeneralRequest(imflag=self.imflag, dt0=self.dt0))

    def set_timestep(
        self,
        control_flag=TimestepCtrol.CONSTANT_TIMESTEP_SIZE,
        Optimum_equilibrium_iteration_count=11,
    ):
        """Define parameters for automatic time step control during implicit analysis.

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
        self.iauto = control_flag.value
        self.iteopt = Optimum_equilibrium_iteration_count
        ret = self.stub.CreateControlImplicitAuto(ControlImplicitAutoRequest(iauto=self.iauto, iteopt=self.iteopt))
        return ret

    def set_dynamic(
        self,
        integration_method=Integration.NEWMARK_TIME_INTEGRATION,
        gamma=0.5,
        beta=0.25,
    ):
        """Activate implicit dynamic analysis and define time integration constants.

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
            ControlImplicitDynamicRequest(imass=self.imass, gamma=self.gamma, beta=self.beta)
        )
        return ret

    def set_eigenvalue(self, number_eigenvalues=0, shift_scale=0):
        """Activate implicit eigenvalue analysis and defines associated input parameters.

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
        self.shfscl = shift_scale
        ret = self.stub.CreateControlImplicitEigenvalue(
            ControlImplicitEigenvalueRequest(neig=self.neig, shfscl=self.shfscl)
        )
        return ret

    def set_solution(
        self,
        solution_method=12,
        iteration_limit=11,
        stiffness_reformation_limit=55,
        absolute_convergence_tolerance=1e-10,
    ):
        """Specify whether a linear or nonlinear solution is desired.

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
        self.nsolver = solution_method
        self.ilimit = iteration_limit
        self.maxref = stiffness_reformation_limit
        self.abstol = absolute_convergence_tolerance
        ret = self.stub.CreateControlImplicitSolution(
            ControlImplicitSolutionRequest(
                nsolver=self.nsolver,
                ilimit=self.ilimit,
                maxref=self.maxref,
                abstol=self.abstol,
            )
        )
        return ret


class ContactCategory(Enum):
    SURFACE_TO_SURFACE_CONTACT = 2
    SINGLE_SURFACE_CONTACT = 3
    SHELL_EDGE_TO_SURFACE_CONTACT = 4
    NODES_TO_SURFACE = 5


class ContactType(Enum):
    NULL = 0
    AUTOMATIC = 1
    GENERAL = 2
    RIGID = 3
    TIED = 4
    TIED_WITH_FAILURE = 5
    ERODING = 6
    EDGE = 7


# class ContactAlgorithm(Enum):
# PENALTY_BASED = 1
# CONSTRAINT_BASED = 2


class OffsetType(Enum):
    NULL = ""
    OFFSET = "OFFSET"
    BEAM_OFFSET = "BEAM_OFFSET"
    CONSTRAINED_OFFSET = "CONSTRAINED_OFFSET"


class ContactFormulation(Enum):
    STANDARD_PENALTY = 0
    SOFT_CONSTRAINT_PENALTY = 1
    SEGMENT_BASED_CONTACT_PENALTY = 2


class SBOPT(Enum):
    ASSUME_PLANER_SEGMENTS = 2
    WRAPED_SEGMENT_CHECKING = 3
    SLDING_OPTION = 4


class ContactSurface:
    """Define contact interface."""

    def __init__(self, set):
        self.stub = DynaSolution.get_stub()
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
        self.penalty_stiffness = 1.0

    def set_contact_region(self, box):
        """Include in contact definition only those SURFA nodes/segments within box.

        Parameters
        ----------
        box : Box
            Define a box-shaped volume.
        """
        self.id = box.id
        return self.id

    def set_contact_thickness(self, thickness):
        """Set contact thickness for SURFA surface.

        Parameters
        ----------
        thickness : float
            Contact thickness.
        """
        self.thickness = thickness

    def set_penalty_stiffness_scale_factor(self, scalefactor=1.0):
        """Set scale factor on default surface penalty stiffness."""
        self.penalty_stiffness = scalefactor


class Contact:
    """Provide a way of treating interaction between disjoint parts."""

    contactlist = []

    def __init__(
        self,
        type=ContactType.NULL,
        category=ContactCategory.SINGLE_SURFACE_CONTACT,
        offset=OffsetType.NULL,
    ):
        self.stub = DynaSolution.get_stub()
        self.rigidwall_penalties_scale_factor = 1
        self.max_penetration_check_multiplier = 4
        self.initial_penetrations = 0
        self.rigidwall_gap_stiffness = 0
        self.category = category
        self.type = type
        self.mortar = False
        self.ignore = 0
        self.offset = offset.value
        self.static_friction_coeff = 0
        self.dynamic_friction_coeff = 0
        self.birth_time = 0
        self.death_time = 1e20
        self.option_tiebreak = False
        self.optionres = 0
        self.contact_formulation = 0
        self.segment_based_contact_option = 2
        Contact.contactlist.append(self)

    def set_mortar(self):
        """Set mortar contact,it is a segment to segment penalty based contact."""
        self.mortar = True

    # def set_algorithm(self, algorithm=ContactAlgorithm.PENALTY_BASED):
    #    self.algorithm = algorithm.value

    def set_tiebreak(self):
        """Define the contact allow for failure, TIEBREAK is a special case of
        this in which after failure the contact usually becomes a normal one-way,
        two-way, or single surface version."""
        self.option_tiebreak = True
        self.optionres = 2

    def set_friction_coefficient(self, static=0, dynamic=0):
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

    def set_active_time(self, birth_time=0, death_time=1e20):
        """Set birth and death time to active and deactivate the contact."""
        self.birth_time = birth_time
        self.death_time = death_time

    def set_initial_penetration(self):
        """Ignore initial penetrations."""
        self.ignore = 1

    def set_slave_surface(self, surface):
        """Specifying the slave contact interface."""
        self.slavesurface = surface

    def set_master_surface(self, surface):
        """Specifying the master contact interface."""
        self.mastersurface = surface

    def set_penalty_algorithm(
        self,
        formulation=ContactFormulation.STANDARD_PENALTY,
        segment_based_contact_option=SBOPT.ASSUME_PLANER_SEGMENTS,
    ):
        """Set contact formulation."""
        self.contact_formulation = formulation.value
        self.segment_based_contact_option = segment_based_contact_option.value

    def create(self):
        """Create contact."""
        opcode = ""
        if self.type == ContactType.AUTOMATIC:
            opcode += "AUTOMATIC_"
        elif self.type == ContactType.TIED:
            opcode += "TIED_"
        else:
            opcode += ""
        if self.category != ContactCategory.SINGLE_SURFACE_CONTACT:
            msid = self.mastersurface.id
            mstyp = self.mastersurface.type
            mst = self.mastersurface.thickness
            penalty_stiffness = self.mastersurface.penalty_stiffness
        if self.category == ContactCategory.SURFACE_TO_SURFACE_CONTACT:
            opcode += "SURFACE_TO_SURFACE"
        elif self.category == ContactCategory.SINGLE_SURFACE_CONTACT:
            opcode += "SINGLE_SURFACE"
            msid = 0
            mstyp = 0
            mst = 0
            penalty_stiffness = 0
        elif self.category == ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT:
            opcode += "SHELL_EDGE_TO_SURFACE"
        elif self.category == ContactCategory.NODES_TO_SURFACE:
            opcode += "NODES_TO_SURFACE"
        else:
            opcode += ""

        if self.offset == OffsetType.OFFSET:
            opcode += "_OFFSET"
        elif self.offset == OffsetType.BEAM_OFFSET:
            opcode += "_BEAM_OFFSET"
        elif self.offset == OffsetType.CONSTRAINED_OFFSET:
            opcode += "_CONSTRAINED_OFFSET"
        else:
            opcode += ""

        if self.option_tiebreak:
            opcode += "_TIEBREAK"

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
                birthtime=self.birth_time,
                sfsa=self.slavesurface.penalty_stiffness,
                sfsb=penalty_stiffness,
                sst=self.slavesurface.thickness,
                mst=mst,
                optionres=0,
                nfls=0,
                sfls=0,
                param=0,
                ct2cn=1,
                soft=self.contact_formulation,
                sofscl=0.1,
                lcidab=0,
                maxpar=1.025,
                sbopt=self.segment_based_contact_option,
                depth=2,
                bsort=100,
                frcfrq=1,
                ignore=self.ignore,
                igap=1,
            )
        )
        logging.info("Contact  Created...")
        return ret


class Constraint:
    """Provides a way of constraining degrees of freedom to move together in some way."""

    cnrbsetidlist = []
    jointsphericallist = []
    def __init__(self):
        self.stub = stub = DynaSolution.get_stub()

    def create_spotweld(self, nodeid1, nodeid2):
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
        ret = self.stub.CreateConstrainedSpotWeld(ConstrainedSpotWeldRequest(node1=nodeid1, node2=nodeid2))
        logging.info("Spotweld Created...")
        return ret

    def create_cnrb(self, nodeset):
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

    def create_joint_spherical(self, nodes, relative_penalty_stiffness=1.0, damping_scale_factor=1.0):
        """Define a joint between two rigid bodies.
        
        Parameters
        ----------
        nodes : list
            Define nodes for joint.
        relative_penalty_stiffness : int
            Relative penalty stiffness.
        damping_scale_factor : int
            Damping scale factor on default damping value.
        """
        Constraint.jointsphericallist.append([nodes,relative_penalty_stiffness,damping_scale_factor])
        logging.info("joint spherical Created...")

    @staticmethod
    def create(stub):
        """Create constraint."""
        for i in range(len(Constraint.cnrbsetidlist)):
            stub.CreateConstrainedNodalRigidBody(
                ConstrainedNodalRigidBodyRequest(pid=i, nsid=Constraint.cnrbsetidlist[i])
            )
        for i in range(len(Constraint.jointsphericallist)):
            stub.CreateConstrainedJoint(
                ConstrainedJointRequest(
                    type="SPHERICAL", nodes=Constraint.jointsphericallist[i][0],rps=Constraint.jointsphericallist[i][1], damp=Constraint.jointsphericallist[i][2]
                )
            )

class BoundaryCondition:
    """Provide a way of defining imposed motions on boundary nodes."""

    def __init__(self):
        self.stub = DynaSolution.get_stub()

    def create_spc(
        self,
        nodeset,
        tx=True,
        ty=True,
        tz=True,
        rx=True,
        ry=True,
        rz=True,
        cid=0,
        birth=0,
        death=1e20,):
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
        if birth == 0 and death == 1e20:
            birthdeath = False
        else:
            birthdeath = True
        if nodeset.num() == 1:
            nid = nodeset.pos(pos=0)
            option1 = "NODE"
        else:
            nid = nodeset.create(self.stub)
            option1 = "SET"
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

    def create_imposed_motion(
        self,
        set,
        curve,
        motion=Motion.DISPLACEMENT,
        dof=DOF.X_TRANSLATIONAL,
        scalefactor=1,
        birthtime = 0,
    ):
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
        set.create(self.stub)
        curve.create(self.stub)
        if set.type == "PARTSET":
            for id in set.parts:
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
                        birth=birthtime,
                        death=0,
                    )
                )
        elif set.type == "NODESET":
            ret = self.stub.CreateBdyPrescribedMotion(
                BdyPrescribedMotionRequest(
                    id=0,
                    heading="",
                    option="SET",
                    typeid=set.id,
                    dof=dof.value,
                    vad=motion.value,
                    lcid=curve.id,
                    sf=scalefactor,
                    vid=0,
                    birth=birthtime,
                    death=0,
                )
            )
        else:
            pass
        logging.info("Boundary prescribed motion Created...")

class InitialCondition:
    """Provide a way of initializing velocities and detonation points."""

    def __init__(self):
        self.stub = DynaSolution.get_stub()
        self.velocityset = BaseSet()
        self.angular_velocity = 0
        self.velocity = Velocity(0,0,0)
        self.dirction = Direction(0,0,0)
        self.stime = 0
    
    def create_velocity(
        self,
        velocityset,
        angular_velocity=0,
        velocity = Velocity(0,0,0),
        direction = Direction(0,0,0),
        stime=0):
        """Define initial velocities for rotating and/or translating bodies."""
        self.velocityset = velocityset
        self.angular_velocity = angular_velocity
        self.velocity = velocity
        self.direction = direction
        self.stime = stime
        if self.velocityset.num()>0:
            id = self.velocityset.create(self.stub)
            if self.velocityset.type.upper() == "PARTSET":
                type = 1
            elif self.velocityset.type.upper() == "PART":
                type = 2
            else:
                type = 3
            phase = 0
            if self.stime !=0 :
                phase = 1
                self.stub.CreateInitVelGenerationStartTime(InitVelGenerationStartTimeRequest(stime=self.stime))
            self.stub.CreateInitVelGeneration(
                InitVelGenerationRequest(
                    id=id,
                    styp=type,
                    omega=self.angular_velocity,
                    vx=self.velocity.x,
                    vy=self.velocity.y,
                    vz=self.velocity.z,
                    xc=0,
                    yc=0,
                    zc=0,
                    nx=self.direction.x,
                    ny=self.direction.y,
                    nz=self.direction.z,
                    phase=phase,
                )
            )
            logging.info(f"Define initial velocities for {type} {id}.")
    
    def create(self):
        """Create initial condition."""
        if self.velocityset.num()>0:
            id = self.velocityset.create(self.stub)
            if self.velocityset.type.upper() == "PARTSET":
                type = 1
            elif self.velocityset.type.upper() == "PART":
                type = 2
            else:
                type = 3
            phase = 0
            if self.stime !=0 :
                phase = 1
                self.stub.CreateInitVelGenerationStartTime(InitVelGenerationStartTimeRequest(stime=self.stime))
            self.stub.CreateInitVelGeneration(
                InitVelGenerationRequest(
                    id=id,
                    styp=type,
                    omega=self.angular_velocity,
                    vx=self.velocity.x,
                    vy=self.velocity.y,
                    vz=self.velocity.z,
                    xc=0,
                    yc=0,
                    zc=0,
                    nx=self.direction.x,
                    ny=self.direction.y,
                    nz=self.direction.z,
                    phase=phase,
                )
            )
            logging.info(f"Define initial velocities for {type} {id}.")

class RigidwallCylinder:
    """Define a rigid wall with a cylinder form.

    Parameters
    ----------
      tail : Point
        The coordinate of tail of normal vector.
      head : Point
        The coordinate of head of normal vector.
      radius : float
        Radius of cylinder.
      length : float
        Length of cylinder.
    """

    rwlist = []

    def __init__(self, tail=Point(0, 0, 0), head=Point(0, 0, 0), radius=1, length=10):
        self.stub = DynaSolution.get_stub()
        self.tail = tail
        self.head = head
        self.radius = radius
        self.length = length
        self.motion = -1
        self.lcid = 0
        self.dir = Direction(1, 0, 0)
        RigidwallCylinder.rwlist.append(self)

    def set_motion(self, curve, motion=Motion.VELOCITY, dir=Direction(1, 0, 0)):
        """Set prescribed motion."""
        curve.create(self.stub)
        self.lcid = curve.id
        self.motion = motion.value
        if self.motion == Motion.DISPLACEMENT:
            self.motion = 1
        self.dir = dir

    def create(self):
        """Create rigidwall cylinder."""
        parameter = [
            self.tail.x,
            self.tail.y,
            self.tail.z,
            self.head.x,
            self.head.y,
            self.head.z,
            self.radius,
            self.length,
        ]
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


class RigidwallPlanar:
    """Define planar rigid walls with either finite or infinite size.

    Parameters
    ----------
      tail : Point
        The coordinate of tail of normal vector.
      head : Point
        The coordinate of head of normal vector.
      radius : float
        Radius of cylinder.
      length : float
        Length of cylinder.
    """

    def __init__(self, tail=Point(0, 0, 0), head=Point(0, 0, 0), coulomb_friction_coefficient=0.5):
        self.stub = DynaSolution.get_stub()
        self.tail = tail
        self.head = head
        self.fric = coulomb_friction_coefficient
        normal = [
            self.tail.x,
            self.tail.y,
            self.tail.z,
            self.head.x,
            self.head.y,
            self.head.z,
        ]
        self.stub.CreateRigidWallPlanar(
            RigidWallPlanarRequest(nsid=0, nsidex=0, boxid=0, fric=self.fric, normal=normal)
        )
        logging.info("Rigidwall Planar Created...")

class GravityOption(Enum):
    DIR_X = "X"
    DIR_Y = "Y"
    DIR_Z = "Z"

class Gravity:
    """Define body force loads due to a prescribed base acceleration or angular velocity using global axes directions."""
    gravitylist=[]
    def __init__(self,dir=GravityOption.DIR_Z,load = Curve(x=[0,0],y=[0,0])):
        self.stub = DynaSolution.get_stub()
        self.dir = dir.value
        self.load = load
        Gravity.gravitylist.append(self)

    def create(self):
        """Define body force."""
        id = self.load.create(self.stub)
        ret = self.stub.CreateLoadBody(LoadBodyRequest(option=self.dir, lcid=id))
        logging.info("Load body Created...")
