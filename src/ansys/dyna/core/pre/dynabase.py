"""
Base
====

Module for creating a DYNA input deck.
"""

from enum import Enum
import logging

# from subprocess import DETACHED_PROCESS
from typing import List

from ansys.api.dyna.v0.kwprocess_pb2 import *  # noqa : F403
from ansys.api.dyna.v0.kwprocess_pb2_grpc import *  # noqa : F403

# from .kwprocess_pb2 import *
# from .kwprocess_pb2_grpc import *


class Motion(Enum):
    VELOCITY = 0
    ACCELERATION = 1
    DISPLACEMENT = 2


class RWMotion(Enum):
    VELOCITY = 0
    DISPLACEMENT = 1


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


# set_output() argument
class OutputEcho(Enum):
    ALL_DATA_PRINTED = 0
    SUPPRESSED_NODAL_PRINTING = 1
    SUPPRESSED_ELEMENT_PRINTING = 2
    SUPPRESSED_NODAL_AND_ELEMENT_PRINTING = 3


from .dynamaterial import MatAdditional
from .dynasolution import DynaSolution  # noqa : F403


class Box:
    """Defines a box-shaped volume."""

    def __init__(self, xmin=0, xmax=0, ymin=0, ymax=0, zmin=0, zmax=0):
        self.xmin = xmin
        self.xmax = xmax
        self.ymin = ymin
        self.ymax = ymax
        self.zmin = zmin
        self.zmax = zmax

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
        logging.info(f"Box {self.id} defined...")
        return self.id


class Curve:
    """
    Defines a curve as a function of time.

    For example, ``load (ordinate value)``.

    """

    def __init__(self, sfo=1, x=[], y=[], func=None, title=""):
        self.sfo = sfo
        self.abscissa = x
        self.ordinate = y
        self.func = func
        self.title = title

    def create(self, stub=None):
        """Create a curve."""
        if stub is None:
            stub = DynaBase.get_stub()
        if self.func != None:
            ret = stub.CreateDefineCurveFunction(DefineCurveFunctionRequest(function=self.func, title=self.title))
        else:
            ret = stub.CreateDefineCurve(
                DefineCurveRequest(sfo=self.sfo, abscissa=self.abscissa, ordinate=self.ordinate, title=self.title)
            )
        self.id = ret.id
        logging.info(f"Curve {self.id} defined...")
        return self.id


class Function:
    """Defines a function that can be referenced by a limited number of keyword options."""

    def __init__(self, Function=None):
        self.function = Function
        self.tabulated = False

    def set_tabulated(self, heading="", function="", x=[], y=[]):
        self.tabulated = True
        self.heading = heading
        self.function_name = function
        self.x = x
        self.y = y

    def create(self, stub):
        """Create function."""
        if self.tabulated:
            ret = stub.CreateDefineFunctionTabulated(
                DefineFunctionTabulatedRequest(
                    heading=self.heading, function=self.function_name, abscissa=self.x, ordinate=self.y
                )
            )
        ret = stub.CreateDefineFunction(DefineFunctionRequest(function=self.function))
        self.id = ret.id
        logging.info(f"Function {self.id} defined...")

        return self.id


class Table2D:
    """Define a table,a curve ID is specified for each value defined in the table."""

    def __init__(self, title=""):
        self.title = title
        self.valuecurvelist = []

    def append(self, value=0, curve=None):
        self.valuecurvelist.append((value, curve))

    def create(self, stub=None):
        """Create Table2D."""
        if stub is None:
            stub = DynaBase.get_stub()
        vls = []
        cvs = []
        for obj in self.valuecurvelist:
            vls.append(obj[0])
            cid = obj[1].create(stub)
            cvs.append(cid)
        ret = stub.CreateDefineTable2D(DefineTable2DRequest(title=self.title, values=vls, cids=cvs))
        self.id = ret.id
        logging.info(f"Table2D {self.id} defined...")
        return self.id


class Point:
    """Defines a point."""

    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z


class Direction:
    """Defines a direction."""

    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z


class Transform:
    """Defines a transformation."""

    def __init__(self, option=None, param1=0, param2=0, param3=0, param4=0, param5=0, param6=0, param7=0):
        param = [option, param1, param2, param3, param4, param5, param6, param7]
        self.paramlist = []
        self.paramlist.append(param)

    def add_transform(self, option=None, param1=0, param2=0, param3=0, param4=0, param5=0, param6=0, param7=0):
        """Defines a transformation matrix."""
        param = [option, param1, param2, param3, param4, param5, param6, param7]
        self.paramlist.append(param)

    def create(self, stub):
        """Create a transformation."""
        options = []
        params = []
        for obj in self.paramlist:
            options.append(obj[0])
            for i in range(1, 8):
                params.append(obj[i])
        ret = stub.CreateDefineTransformation(DefineTransformationRequest(option=options, param=params))
        self.id = ret.id
        logging.info(f"Transformation {self.id} defined...")
        return self.id


class Velocity:
    """Defines a translational velocity."""

    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z


class RotVelocity:
    """Defines a rotational velocity."""

    def __init__(self, x=0, y=0, z=0):
        self.x = x
        self.y = y
        self.z = z


class BaseObj:
    """Define the base object."""

    def __init__(self):
        self.type = ""
        self.subtype = ""

    def get_data(self) -> List:
        """Get the data of the object."""
        return None


class ParameterType(Enum):
    """Contains the parameter types."""

    R = 1
    I = 2
    C = 3


class DynaBase:
    """Contains methods for creating a general LS-DYNA keyword."""

    def __init__(self):
        self.stub = DynaSolution.get_stub()
        self.mainname = ""
        DynaBase.stub = self.stub
        self.implicitanalysis = ImplicitAnalysis(initial_timestep_size=0.1)
        self.parts = Parts()
        self.boundaryconditions = BoundaryCondition()
        self.initialconditions = InitialCondition()
        self.constraints = Constraint()
        self.contacts = ContactGroup()
        self.entities = []
        self.have_accuracy = False
        self.have_energy = False
        self.have_hourglass = False
        self.have_bulk_viscosity = False
        self.have_control_shell = False
        # add for drawing entity
        self._parent: DynaSolution = None
        self.init_velocity: List = None
        self.bdy_spc: List = None

    def get_stub():
        """Get the stub of the ``DynaBase`` object."""
        return DynaBase.stub

    def set_parent(self, parent=None):
        self._parent = parent
        model = self._parent.model
        self.boundaryconditions.assign_model(model)
        self.initialconditions.assign_model(model)

    def set_timestep(self, tssfac=0.9, isdo=0, timestep_size_for_mass_scaled=0.0, max_timestep=None):
        """Set the structural time step size control using different options.

        Parameters
        ----------
        tssfac : float, optional
            Scale factor for computed time step. The default is ``0.9``.
        isdo : int, optional
            Basis of the time size calculation for four-node shell elements.
            The default is ``0``.
        timestep_size_for_mass_scaled : float, optional
            Time step size for mass scaled solutions. The default is ``0.0``.
        max_timestep : Curve, optional
            Load curve that limits the maximum time step size. The default
            is ``None``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if max_timestep == None:
            cid = 0
        else:
            cid = max_timestep.create(self.stub)
        ret = self.stub.CreateTimestep(
            TimestepRequest(tssfac=tssfac, isdo=isdo, dt2ms=timestep_size_for_mass_scaled, lctm=cid)
        )
        logging.info("Timestep Created...")
        return ret

    def set_accuracy(
        self,
        objective_stress_updates=Switch.OFF,
        invariant_node_number=InvariantNode.OFF,
        partsetid_for_objective_stress_updates=0,
        implicit_accuracy_flag=Switch.OFF,
        explicit_accuracy_flag=Switch.OFF,
    ):
        """Define control parameters that can improve the accuracy of the calculation.

        Parameters
        ----------
        objective_stress_updates : int
            Global flag for 2nd order objective stress updates.
        invariant_node_number : int
            Invariant node numbering for shell and solid elements.
        partsetid_for_objective_stress_updates : int, optional
            Part set ID for objective stress updates. The default is ``0``.
        implicit_accuracy_flag : int
            Implicit accuracy flag.
        explicit_accuracy_flag : float
            Explicit accuracy parameter.

            - EQ.0.0: Off
            - GT.0.0: On

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_accuracy:
            ret = self.stub.CreateControlAccuracy(
                ControlAccuracyRequest(
                    osu=objective_stress_updates.value,
                    inn=invariant_node_number.value,
                    pidosu=partsetid_for_objective_stress_updates,
                    iacc=implicit_accuracy_flag.value,
                    exacc=explicit_accuracy_flag.value,
                )
            )
            self.have_accuracy = True
            logging.info("Control Accuracy Created...")
        return ret

    def set_energy(
        self,
        hourglass_energy=EnergyFlag.NOT_COMPUTED,
        rigidwall_energy=EnergyFlag.COMPUTED,
        sliding_interface_energy=EnergyFlag.NOT_COMPUTED,
        rayleigh_energy=EnergyFlag.NOT_COMPUTED,
        initial_reference_geometry_energy=EnergyFlag.COMPUTED,
    ):
        """Provide controls for energy dissipation options.

        Parameters
        ----------
        hourglass_energy : enum
            Hourglass energy calculation option.
        rigidwall_energy : int
            Rigidwall energy dissipation option.

            - EQ.1: Energy dissipation is not computed.
            - EQ.2: Energy dissipation is computed.

        sliding_interface_energy : int
            Sliding interface energy dissipation option.

            - EQ.1: Energy dissipation is not computed.
            - EQ.2: Energy dissipation is computed.

        rayleigh_energy : int
            Rayleigh energy dissipation option.

            - EQ.1: Energy dissipation is not computed.
            - EQ.2: Energy dissipation is computed.

        initial_reference_geometry_energy : int
            Initial reference geometry energy option.

            - EQ.1: Initial reference geometry energy is not computed.
            - EQ.2: Initial reference geometry energy is computed.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_energy:
            ret = self.stub.CreateControlEnergy(
                ControlEnergyRequest(
                    hgen=hourglass_energy.value,
                    rwen=rigidwall_energy.value,
                    slnten=sliding_interface_energy.value,
                    rylen=rayleigh_energy.value,
                    irgen=initial_reference_geometry_energy.value,
                )
            )
            self.have_energy = True
            logging.info("Control Energy Created...")
        return ret

    def set_output(
        self,
        print_suppression_d3hsp=False,
        print_suppression_echo=OutputEcho.ALL_DATA_PRINTED,
    ):
        """Set miscellaneous output parameters.

        Parameters
        ----------
        print_suppression_d3hsp : bool, optional
            Whether to suppress printing during the input phase flag for the D3HSP file.
            The default is ``True``, which means that none of these are printed: nodal
            coordinates, element connectivities, rigid wall definitions, nodal SPCs,
            initial velocities, initial strains, adaptive constraints, and SPR2/SPR3
            constraints. If ``False``, no suppression occurs.
        print_suppression_echo : OutputEcho
            Print suppression setting during the input phase flag for the echo file.
            Options are:

            - ALL_DATA_PRINTED: All data is printed.
            - SUPPRESSED_NODAL_PRINTING: Nodal printing is suppressed.
            - SUPPRESSED_ELEMENT_PRINTING: Element printing is suppressed.
            - SUPPRESSED_NODAL_AND_ELEMENT_PRINTING : Both nodal and element printing is suppressed.

        """
        if print_suppression_d3hsp:
            npopt = 1
        else:
            npopt = 0
        ret = self.stub.CreateControlOutput(
            ControlOutputRequest(
                npopt=npopt,
                neecho=print_suppression_echo.value,
            )
        )
        logging.info("Control Output Created...")
        return ret

    def set_hourglass(self, controltype=HourglassControl.STANDARD_VISCOSITY_FORM, coefficient=0.1):
        """Redefine the default values for the hourglass control type and coefficient.

        Parameters
        ----------
        controltype : enum
            Default hourglass control type.
        coefficient : float, optional
            Default hourglass coefficient. The default is ``0.``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_hourglass:
            ret = self.stub.CreateControlHourgalss(ControlHourglassRequest(ihq=controltype.value, qh=coefficient))
            self.have_hourglass = True
            logging.info("Control Hourglass Created...")
        return ret

    def set_bulk_viscosity(
        self,
        quadratic_viscosity_coeff=1.5,
        linear_viscosity_coeff=0.06,
        bulk_viscosity_type=BulkViscosity.STANDARD_BULK_VISCOSITY,
    ):
        """Reset the default values of the bulk viscosity coefficients globally.

        Parameters
        ----------
        quadratic_viscosity_coeff : float, optional
            Default quadratic viscosity coefficient. The default is ``1.5``.
        linear_viscosity_coeff : float, optional
            Default linear viscosity coefficient. The default is ``0.06``.
        bulk_viscosity_type : enum
            Default bulk viscosity type.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_bulk_viscosity:
            ret = self.stub.CreateControlBulkViscosity(
                ControlBulkViscosityRequest(
                    q1=quadratic_viscosity_coeff,
                    q2=linear_viscosity_coeff,
                    type=bulk_viscosity_type.value,
                )
            )
            self.have_bulk_viscosity = True
            logging.info("Control Bulk Viscosity Created...")
        return ret

    def set_init_temperature(self, temp=0):
        """Define initial nodal point temperatures on all nodes.

        Parameters
        ----------
        temp : float, optional
            Temperature at node. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateInitTemperature(InitTemperatureRequest(option="SET", nsid=0, temp=temp))
        logging.info("Initial Temperature Created...")
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

        Parameters
        ----------
        wrpang : int, optional
            Shell element warpage angle in degrees. The default is ``20``.
        esort : int
            Sorting of triangular shell elements to automatically switch
            degenerate quadrilateral shell formulations to more suitable
            triangular shell formulations. The default is ``0``.
        irnxx : int, optional
            Shell normal update option. The default is ``1``.
        istupd : int, optional
            Shell thickness change option for deformable shells. The
            default is ``0``.
        theory : int, optional
            Default shell formulation. The default is ``2``.
        bwc : int, optional
            Warping stiffness for Belytschko-Tsay shells. The
            default is ``2``.
        miter : int, optional
            Plane stress plasticity option. The default is ``1``.
        proj : int, optional
            Projection method for the warping stiffness in the Belytschko-Tsay
            shell and the Belytschko-Wong-Chiang elements. The default is
            ``0``.
        irquad : int, optional
            In-plane integration rule for the eight-node quadratic shell element.
            The default is ``0``.

            - EQ.2: 2*2 Gauss quadrature
            - EQ.3: 3*3 Gauss quadrature


        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_control_shell:
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
            self.have_control_shell = True
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
        """Provide controls for a solid element response.

        Parameters
        ----------
        esort : int, optional
            Automatic sorting of tetrahedral and pentahedral elements to avoid
            use of degenerate formulations for these shapes. The default is ``0``.

            - EQ.0: No sorting
            - EQ.1: Sort

        fmatrx : int, optional
            Method to use in the calculation of the deformation gradient matrix.
            The default is ``1``.
        niptets : int, optional
            Number of integration points used in the quadratic tetrahedron elements.
            The default is ``4``.
        swlocl : int, optional
            Output option for stresses in solid elements used as spot welds with
            material ``\*MAT_SPOTWELD``. The default is ``1``.
        psfail : int, optional
            Solid element erosion from negative volume is limited only to solid elements in
            the part set indicated by PSFAIL. The default is ``0``.
        t10jtol : float, optional
            Tolerance for Jacobian in four-point, 10-noded quadratic tetrahedra. The
            default is ``0.0``.
        icoh : int, optional
            Breaking LS-DYNA convention ICOH is interpreted digit-wise. The default
            is ``0``.
        tet13k : int, optional
            Flag for whether to invoke a consistent tangent stiffness matrix
            for the pressure averaged tetrahedron. The default is ``0``, in which
            case this matrix is not invoked. If this parameter is set to ``1``,
            this matrix is invoked.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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

    def create_control_contact(self, rwpnal, shlthk=0, orien=1, ssthk=0, ignore=0, igactc=0):
        """Change defaults for computation with contact surfaces.

        Parameters
        ----------
        rwpnal : float
            Scale factor for rigid wall penalties, which treat nodal points interacting
            with rigid walls.
        shlthk : int, optional
            Flag for whether to consider shell thickness offsets in non-automatic
            surface-to-surface and non-automatic nodes-to-surface type contacts.
            The default is ``0``, in which case these offsets are not considered.
            If this parameter is set to ``1``, these offsets are considered.
        orien : int, optional
            Flag for whether to automatically reorient contact interface segments
            during initialization. The default is ``1``, in which case reorientation
            automatically occurs. If this parameter is set to ``0``, reorientation
            does not occur.
        ssthk : int, optional
            Flag for whether to determine default contact thickness for shells in single
            surface contact types. The default is ``0``, in which case default contact
            thickness is not determined. If this parameter is set to ``1``, default
            contact thickness is determined.
        ignore : int, optional
            Flag for whether to ignore initial penetrations in the ``\*CONTACT_AUTOMATIC``
            options. The default is ``0``, in which case initial penetrations are ignored.
            If this parameter is set to ``1``, initial penetrations are not ignored.
        igactc : int, optional
            Flag for whether to use isogeometric shells for contact detection when the
            contact involves isogeometric shells. The default is ``0``, which means
            isogeometric shells are not used. If this parameter is set to ``1``, isogeometric
            shells are used.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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

    def create_damping_global(self, lcid=0, valdmp=0.0):
        """Define mass-weighted nodal damping.

        Mass-weighted nodal damping applies globally to the
        nodes of deformable bodies and to the mass center of
        rigid bodies.

        Parameters
        ----------
        lcid : int, optional
            Load curve ID, which specifies the system damping constant
            versus the time. The default is ``0``.
        valdmp : float, optional
            System damping constant. The default is ``0.0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateDampingGlobal(DampingGlobalRequest(lcid=lcid, valdmp=valdmp))
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
            Keyword card name.
        keyworddata : string
            Keyword data.

        Examples
        --------
        Create a ``\*INITIAL_VELOCITY`` keyword.

        \$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        \*INITIAL_VELOCITY


        &       vx        vy        vz       vxr       vyr       vzr
        1.480E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
        \$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

        opcode = "INITIAL_VELOCITY"
        keyworddata = "0\n1.480E+01,0.000E+00,0.000E+00,0.000E+00,0.000E+00,0.000E+00"
        create_general_keyword(opcode = opcode,keyworddata=keyworddata)
        """
        ret = self.stub.CreateGeneralKWD(GeneralKWDRequest(opcode=opcode, keyworddata=keyworddata))
        msg = opcode + " Created..."
        logging.info("msg")
        return ret

    def add(self, obj):
        """Add entities to an object."""

        if obj.type == "rigidwall_cylinder" or obj.type == "rigidwall_sphere" or obj.type == "rigidwall_planar":
            data = obj.get_data()
            if data != None:
                model = self._parent.model
                model.add_rigidwall(data)
        self.entities.append(obj)

    def set_transform(self, filename=None, idnoff=0, ideoff=0, idpoff=0, idmoff=0, idsoff=0, idfoff=0, transform=None):
        """Include independent input files containing model data, allow for node, element, and set
        IDs to be offset and for coordinates and constitutive parameters to be transformed and scaled.

        Parameters
        ----------
        filename : string
            Name of file to include in the keyword file.
        idnoff : int
            Offset to node ID.
        ideoff : int
            Offset to element ID.
        idpoff : int
            Offset to part ID.
        idmoff : int
            Offset to material ID.
        idsoff : int
            Offset to set ID.
        idfoff : int
            Offset to function ID, table ID, and curve ID.
        transform : Transform
            Definition for the transformation.
        """
        tranid = transform.create(self.stub)
        ret = self.stub.CreateIncludeTransform(
            IncludeTransformRequest(
                filename=filename,
                idnoff=idnoff,
                ideoff=ideoff,
                idpoff=idpoff,
                idmoff=idmoff,
                idsoff=idsoff,
                idfoff=idfoff,
                tranid=tranid,
            )
        )
        logging.info("Include transform Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if self.contacts.num() > 0:
            self.create_control_contact(rwpnal=1.0, ignore=1, igactc=0)
        self.implicitanalysis.create()
        self.parts.set_property()
        self.initialconditions.create()
        self.constraints.create()
        self.boundaryconditions.create()
        self.contacts.create()
        for obj in self.entities:
            obj.create()


# -------------------------------------------------------------------------------------------------


class BaseSet:
    """Defines the base class for all set classes."""

    def __init__(self):
        self.type = "PARTSET"
        self.id = 0


class NodeSet:
    """Defines a nodal set with some identical or unique attributes."""

    def __init__(self, nodes=[]):
        self.nodes = nodes
        self.type = "NODESET"

    def create(self, stub):
        """Create a node set."""
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
        """Get the number of nodes in the node set."""
        return len(self.nodes)

    def id(self, pos):
        """Get the node ID by position."""
        return self.nodes[pos]

    def get_nid(self):
        """Get the node ID."""
        if self.type == "NODE":
            return self.nodes[0]
        else:
            return 0


class SetType(Enum):
    """Contains the enums for setting types."""

    SHELL = "SET_SHELL"
    SOLID = "SET_SOLID"
    BEAM = "SET_BEAM"
    TSHELL = "SET_TSHELL"
    DISCRETE = "SET_DISCRETE"


class NodesetGeneral(BaseSet):
    """Includes nodal points of element sets.

    Element sets are defined by ``SET_XXXX_LIST``,
    where ``XXXX`` can be ``SHELL``, ``SOLID``, ``BEAM``,
    ``TSHELL`` or ``DISCRETE``.
    """

    def __init__(self, settype=SetType.SHELL, setids=[]):
        self.settype = settype.value
        self.setids = setids

    def create(self, stub):
        """Create a node set."""
        if len(self.setids) <= 0:
            return 0
        ret = stub.CreateNodeSet(NodeSetRequest(option="GENERAL", sid=0, genoption=self.settype, entities=self.setids))
        self.id = ret.id
        self.type = "NODESET"
        return self.id


class NodeSetBox(BaseSet):
    """include the nodes inside boxes.

    Parameters
        ----------
        boxes : list
            A list of BOX.
    """

    def __init__(self, boxes=[]):
        self.boxes = boxes
        self.type = "NODESETBOX"

    def create(self, stub):
        """Create a node set."""
        if len(self.boxes) <= 0:
            return 0
        boxids = []
        for box in self.boxes:
            boxid = box.create(stub)
            boxids.append(boxid)
        ret = stub.CreateNodeSet(NodeSetRequest(option="GENERAL", sid=0, genoption="BOX", entities=boxids))
        self.id = ret.id
        self.type = "NODESETBOX"
        return self.id


class PartSet(BaseSet):
    """Defines a set of parts with optional attributes."""

    def __init__(self, parts=[]):
        self.parts = parts

    def create(self, stub):
        """Create a part set."""
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
        """Get the number of parts in the part set."""
        return len(self.parts)

    def pos(self, pos):
        """Get a part ID by position."""
        return self.parts[pos]

    def get_pid(self):
        """Get the part ID."""
        if self.type == "PART":
            return self.parts[0]
        else:
            return 0


class SegmentSet(BaseSet):
    """Defines a set of segments with optional identical or unique attributes.

    Parameters
    ----------
    segments : list [[point1,point2,point3,point4],[point5,point6,point7,point8]...]
       Define segments.
    """

    def __init__(self, segments=[]):
        self.segments = segments
        self.type = "SEGMENTSET"

    def create(self, stub):
        """Create a segment set."""
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


class BeamFormulation(Enum):
    SPOTWELD = 9


class ShellFormulation(Enum):
    FULLY_INTEGRATED = -16
    BELYTSCHKO_TSAY = 2
    SR_HUGHES_LIU = 6
    FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE = 9
    PLANE_STRESS = 12


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
    ONE_POINT_COROTATIONAL = 0
    IMPLICIT_9_POINT_ENHANCED_STRAIN = 18


class HourglassType(Enum):
    STANDARD_LSDYNA_VISCOUS = 1
    FLANAGAN_BELYTSCHKO_VISOCOUS = 2
    FLANAGAN_BELYTSCHKO_VISOCOUS_WITH_EXTRA_VOLUME_INTEGRATION = 3
    FLANAGAN_BELYTSCHKO_STIFFNESS = 4
    FLANAGAN_BELYTSCHKO_STIFFNESS_WITH_EXTRA_VOLUME_INTEGRATION = 5
    BELYTSCHKO_BINDEMAN = 6
    ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS = 8


class BeamSection:
    """Defines cross-sectional properties for beams, trusses, discrete beams, and cable elements."""

    def __init__(
        self,
        element_formulation,
        shear_factor=1,
        cross_section=0,
        thickness_n1=0,
        thickness_n2=0,
    ):
        stub = DynaBase.get_stub()
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
    """Defines section properties for shell elements."""

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
        stub = DynaBase.get_stub()
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
    """Defines section properties for isogeometric shell elements."""

    def __init__(self, element_formulation, shear_factor=1, thickness=1):
        stub = DynaBase.get_stub()
        ret = stub.CreateSectionIGAShell(
            SectionIGAShellRequest(elform=element_formulation, shrf=shear_factor, thickness=thickness)
        )
        self.id = ret.id


class Part:
    """Defines the part object."""

    def __init__(self, id):
        self.stub = DynaBase.get_stub()
        self.type = ""
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
        self.translation = Velocity(0, 0, 0)
        self.rotation = RotVelocity(0, 0, 0)
        self.extra_nodes_defined = False

    def set_material(self, mat, mat_thermal=None):
        """Set the material."""
        mat.create(self.stub)
        self.mid = mat.material_id
        if mat_thermal is not None:
            mat_thermal.create(self.stub)
            self.tmid = mat_thermal.material_id
        else:
            self.tmid = 0
        if isinstance(mat, MatAdditional):
            if mat.thermal_isotropic:
                self.tmid = self.mid

    def set_element_formulation(self, formulation):
        """Set the element formulation."""
        self.formulation = formulation.value

    def set_stiffness_damping_coefficient(self, coefficient):
        """Set the stiffness damping coefficient."""
        self.stiffness_damping = coefficient

    def set_extra_nodes(self, nodeset):
        """Set extra nodes for the rigid body.

        Parameters
        ----------
        nodeset : NodeSet
            Extra nodes list.
        """
        self.extra_nodes_defined = True
        self.extra_nodes = nodeset

    def set_rigidbody_initial_velocity(self, translation=Velocity(0, 0, 0), rotation=RotVelocity(0, 0, 0)):
        """Set initial translational and rotational velocities for the rigid body.

        Initial translational and rotational velocities are set
        at the center of gravity for a rigid body or a nodal rigid body.
        """
        self.rigidbody_initial_velocity = True
        self.translation = translation
        self.rotation = rotation

    def set_property(self):
        """Set properties for the part."""
        if self.stiffness_damping > 0:
            self.stub.CreateDampingPartStiffness(
                DampingPartStiffnessRequest(isset=False, id=self.id, coef=self.stiffness_damping)
            )
            logging.info(f"Assign stiffness damping coefficient to part {self.id}.")
        if self.extra_nodes_defined:
            nid = self.extra_nodes.create(self.stub)
            option = self.extra_nodes.type
            self.stub.CreateConstrainedExtraNodes(
                ConstrainedExtraNodesRequest(option="SET", pid=self.id, nid=nid, iflag=0)
            )
            logging.info(f"Constrained extra nodes defined for part {self.id}.")
        if self.rigidbody_initial_velocity:
            ret = self.stub.CreateInitVelRigidBody(
                InitVelRigidBodyRequest(
                    pid=self.id,
                    vx=self.translation.x,
                    vy=self.translation.y,
                    vz=self.translation.z,
                    vxr=self.rotation.x,
                    vyr=self.rotation.y,
                    vzr=self.rotation.z,
                    lcid=0,
                )
            )
            logging.info(f"Initial velocity for rigidbody {self.id}.")


class BeamPart(Part):
    """
    Defines a beam part.

    A beam part definition consists of the combined material information,
    section properties, hourglass type, thermal properties, and a flag
    for part adaptivity.
    """

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "BEAM"
        self.crosstype = 1

    def set_cross_type(self, cross):
        """Set the type for the cross section."""
        self.crosstype = cross

    def set_diameter(self, diameter):
        """Set the outer diameter for the cross section."""
        self.diameter = diameter

    def set_property(self):
        """Set properties for the beam part."""
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
    """Defines a shell part.

    A shell part definition consists of the combined material information,
    section properties, hourglass type, thermal properties, and a flag
    for part adaptivity.
    """

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "SHELL"
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

    def set_hourglass(self, type=HourglassType.STANDARD_LSDYNA_VISCOUS, coefficient=0.1):
        """Set the hourglass type, which identifies the bulk viscosity.

        Parameters
        ----------
        type : enum
            Default hourglass control type.
        coefficient : float, optional
            Default hourglass coefficient. The default is ``0.``.
        """
        self.hourglasstype = type.value
        self.coefficient = coefficient

    def set_shear_factor(self, factor):
        """Set the shear correction factor, which scales the transverse shear stress."""
        self.shear_factor = factor

    def set_integration_points(self, points=5):
        """Set the number of through thickness integration points."""
        self.intpoints = points

    def set_printout(self, print):
        """Set the printout option."""
        self.print = print

    def set_thickness(self, thickness):
        """Set the shell thickness."""
        self.thickness = thickness

    def set_des_surface(self, despid=0, desxid=0, nquad=1, nsid=0, rsf=-1):
        """Generate and place discrete element sphere (DES) elements on the surface of shell elements.

        Parameters
        ----------
        despid : int, optional
            Part ID for the generated DES elements. The default is ``0``.
        desxid : int, optional
            Section ID for the generated DES elements. The default is ``0``.
        nquad : int, optional
            Number of equally spaced DES elements to create on a shell element in each local shell direction.
            The default is ``1``.
        nsid : int, optional
            If defined, this card creates a node set with ID NSID for the nodes generated by this card.
            The default is ``0``.
        rsf : float, optional
            Scale factor for determining the DES radius. The default is ``1``.
        """
        self.defined_des_surface = True
        self.despid = despid
        self.desxid = desxid
        self.des_nquad = nquad
        self.des_nsid = nsid
        self.des_rsf = rsf

    def set_property(self):
        """Set properties for the shell part."""
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
            ret = self.stub.CreateHourglass(
                HourglassRequest(ihq=self.hourglasstype, qm=self.coefficient, q1=0, q2=0, qb=0, qw=0)
            )
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
    """
    Defines an isogeometric shell part.

    The part definition consists of the combined material information,
    section properties, hourglass type, thermal properties, and a flag
    for part adaptivity.
    """

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "IGA"
        self.shear_factor = 1
        self.thickness = 1

    def set_shear_factor(self, factor):
        """Set the shear correction factor, which scales the transverse shear stress."""
        self.shear_factor = factor

    def set_thickness(self, thickness):
        """Set the shell thickness."""
        self.thickness = thickness

    def set_property(self):
        """Set properties for the IGA part."""
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
    """
    Defines a solid part.

    The part definition consists of the combined material information,
    section properties, hourglass type, thermal properties, and a flag
    for part adaptivity.
    """

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "SOLID"
        self.hourglasstype = -1

    def set_hourglass(self, type=HourglassType.STANDARD_LSDYNA_VISCOUS, coefficient=0.1):
        """Set the hourglass type, which identifies the bulk viscosity.

        Parameters
        ----------
        type : enum
            Default hourglass control type.
        coefficient : float, optional
            Default hourglass coefficient. The default is ``0.``.
        """
        self.hourglasstype = type.value
        self.coefficient = coefficient

    def set_property(self):
        """Set the properties for the solid part."""
        ret = self.stub.CreateSectionSolid(SectionSolidRequest(elform=self.formulation))
        self.secid = ret.id
        if self.hourglasstype > 0:
            ret = self.stub.CreateHourglass(
                HourglassRequest(ihq=self.hourglasstype, qm=self.coefficient, q1=0, q2=0, qb=0, qw=0)
            )
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
    """Defines a discrete part.

    The part definition consists of the combined material information,
    section properties, hourglass type, thermal properties, and a flag
    for part adaptivity.
    """

    def __init__(self, pid):
        Part.__init__(self, pid)
        self.type = "DISCRETE"
        self.stub = DynaBase.get_stub()
        self.displacement_option = 0

    def set_displacement_option(self, displacement_option=DRO.DESCRIBES_TRANSLATIONAL_SPRING):
        """Set the displacement, which defines the rotation."""
        self.displacement_option = displacement_option.value

    def set_property(self):
        """Set properties for the discrete part."""
        Part.set_property(self)
        ret = self.stub.CreateSectionDiscrete(
            SectionDiscreteRequest(dro=self.displacement_option, kd=0, v0=0, cl=0, fd=0, cdl=0, tdl=0)
        )
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


class Parts:
    """Stores the part list."""

    def __init__(self):
        self.beamlist = []
        self.shelllist = []
        self.solidlist = []
        self.igalist = []
        self.icfdlist = []
        self.icfdvolumelist = []
        self.discretelist = []
        self.isphstructlist = []
        self.isphfluidlist = []

    def add(self, part):
        """Add a part to the part list."""
        if part.type == "BEAM":
            self.beamlist.append(part)
        elif part.type == "SHELL":
            self.shelllist.append(part)
        elif part.type == "SOLID":
            self.solidlist.append(part)
        elif part.type == "IGA":
            self.igalist.append(part)
        elif part.type == "ICFD":
            self.icfdlist.append(part)
        elif part.type == "ICFDVOLUME":
            self.icfdvolumelist.append(part)
        elif part.type == "DISCRETE":
            self.discretelist.append(part)
        elif part.type == "ISPHSTRUCT":
            self.isphstructlist.append(part)
        elif part.type == "ISPHFLUID":
            self.isphfluidlist.append(part)
        else:
            logging.info("Warning: Invalid part type!")

    def get_num_shellpart(self):
        """Get the number of shell parts."""
        return len(self.shelllist)

    def set_property(self):
        """Set properties for added parts."""
        for obj in self.beamlist:
            obj.set_property()
        for obj in self.shelllist:
            obj.set_property()
        for obj in self.solidlist:
            obj.set_property()
        for obj in self.igalist:
            obj.set_property()
        for obj in self.icfdlist:
            obj.set_property()
        for obj in self.icfdvolumelist:
            obj.create()
        for obj in self.discretelist:
            obj.set_property()
        for obj in self.isphstructlist:
            obj.set_property()
        for obj in self.isphfluidlist:
            obj.set_property()


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
    """Activates implicit analysis and defines associated control parameters."""

    def __init__(self, analysis_type=AnalysisType.IMPLICIT, initial_timestep_size=0):
        self.defined = False
        self.defined_auto = False
        self.defined_dynamic = False
        self.defined_eigenvalue = False
        self.defined_solution = False
        self.defined_mass_matrix = False
        self.imflag = analysis_type.value
        self.dt0 = initial_timestep_size
        self.stub = DynaBase.get_stub()

    def set_initial_timestep_size(self, size=0):
        """Define the initial time step size."""
        self.defined = True
        self.dt0 = size

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
        Optimum_equilibrium_iteration_count : int, optional
            Optimum equilibrium iteration count per time step. The default is ``11``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_auto = True
        self.iauto = control_flag.value
        self.iteopt = Optimum_equilibrium_iteration_count

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
        gamma : float, optional
            Newmark time integration constant. The default is ``0.5``.
        beta : float, optional
            Newmark time integration constant. The default is ``0.25``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_dynamic = True
        self.imass = integration_method.value
        self.gamma = gamma
        self.beta = beta

    def set_eigenvalue(self, number_eigenvalues=0, shift_scale=0):
        """Activate implicit eigenvalue analysis and define associated input parameters.

        Parameters
        ----------
        number_eigenvalues : int, optional
            Number of eigenvalues to extract. The default is ``0``.
        shift_scale : float, optional
            Shift scale. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_eigenvalue = True
        self.neig = number_eigenvalues
        self.shfscl = shift_scale

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
        solution_method : int, optional
            Solution method for implicit analysis. The default is ``12``.
        iteration_limit : int, optional
            Iteration limit between automatic stiffness reformations.
            The default is ``11``.
        stiffness_reformation_limit : int, optional
            Stiffness reformation limit per time step. The default is
            ``55``.
        absolute_convergence_tolerance : float, optional
            Absolute convergence tolerance. The default is ``1e-10``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_solution = True
        self.nsolver = solution_method
        self.ilimit = iteration_limit
        self.maxref = stiffness_reformation_limit
        self.abstol = absolute_convergence_tolerance

    def set_consistent_mass_matrix(self):
        """Use the consistent mass matrix in implicit dynamics and eigenvalue solutions."""
        self.defined_mass_matrix = True

    def create(self):
        """Create an implicit analysis."""
        if self.defined == False:
            return
        if self.defined:
            self.stub.CreateControlImplicitGeneral(ControlImplicitGeneralRequest(imflag=self.imflag, dt0=self.dt0))
        if self.defined_auto:
            self.stub.CreateControlImplicitAuto(ControlImplicitAutoRequest(iauto=self.iauto, iteopt=self.iteopt))
        if self.defined_dynamic:
            self.stub.CreateControlImplicitDynamic(
                ControlImplicitDynamicRequest(imass=self.imass, gamma=self.gamma, beta=self.beta)
            )
        if self.defined_eigenvalue:
            self.stub.CreateControlImplicitEigenvalue(
                ControlImplicitEigenvalueRequest(neig=self.neig, shfscl=self.shfscl)
            )
        if self.defined_solution:
            self.stub.CreateControlImplicitSolution(
                ControlImplicitSolutionRequest(
                    nsolver=self.nsolver, ilimit=self.ilimit, maxref=self.maxref, abstol=self.abstol
                )
            )
        if self.defined_mass_matrix:
            self.stub.CreateControlImplicitConsistentMass(ControlImplicitConsistentMassRequest(iflag=1))


class ThermalAnalysisType(Enum):
    STEADY_STATE = 0
    TRANSIENT = 1


class ThermalAnalysisTimestep(Enum):
    FIXED = 0
    VARIABLE = 1


class ThermalAnalysis(BaseObj):
    """Activates thermal analysis and defines associated control parameters."""

    def __init__(self):
        self.defined_solver = False
        self.defined_timestep = False
        self.defined_nonlinear = False
        self.stub = DynaBase.get_stub()
        self.type = "analysis_thermal"

    def set_timestep(self, timestep_control=ThermalAnalysisTimestep.FIXED, initial_timestep=0):
        """Set time step controls for the thermal solution in a thermal only or coupled structural/thermal analysis.

        Parameters
        ----------
        timestep_control : ThermalAnalysisTimestep
            Time step control.
        initial_timestep : float, optional
            Initial thermal time step. The default is ``0``.
        """
        self.defined_timestep = True
        self.ts = timestep_control.value
        self.its = initial_timestep

    def set_solver(self, analysis_type=ThermalAnalysisType.STEADY_STATE):
        """Set options for the thermal solution in a thermal only or coupled structural-thermal analysis.

        Parameters
        ----------
        analysis_type : ImplicitAnalysis
            Thermal analysis type.
        """
        self.defined_solver = True
        self.atype = analysis_type.value

    def set_nonlinear(self, convergence_tol=1e-4, divergence=0.5):
        """Set parameters for a nonlinear thermal or coupled structural/thermal analysis.

        Parameters
        ----------
        convergence_tol : float
            Convergence tolerance for temperature.
        divergence : float
            Divergence control parameter.
        """
        self.defined_nonlinear = True
        self.tol = convergence_tol
        self.dcp = divergence

    def create(self):
        """Create a thermal analysis."""
        if self.defined_timestep:
            self.stub.CreateControlThermalTimestep(ControlThermalTimestepRequest(its=self.its))
        if self.defined_solver:
            self.stub.CreateControlThermalSolver(ControlThermalSolverRequest(atype=self.atype))
        if self.defined_timestep or self.defined_solver:
            self.stub.CreateControlSolution(ControlSolutionRequest(soln=2))
        if self.defined_nonlinear:
            self.stub.CreateControlThermalNonlinear(ControlThermalNonlinearRequest(tol=self.tol, dcp=self.dcp))


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
    """Defines a contact interface."""

    def __init__(self, set):
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
        self.penalty_stiffness = 1.0

    def set_contact_region(self, box):
        """Include in the contact definition only those SURFA nodes/segments within a box.

        Parameters
        ----------
        box : Box
            Box-shaped volume.
        """
        self.id = box.id
        return self.id

    def set_contact_thickness(self, thickness):
        """Set the contact thickness for the SURFA surface.

        Parameters
        ----------
        thickness : float
            Contact thickness.
        """
        self.thickness = thickness

    def set_penalty_stiffness_scale_factor(self, scalefactor=1.0):
        """Set the scale factor on the default surface penalty stiffness.

        Parameters
        ----------
        scalefactor : int, optional
            Scale factor. The default is ``1.0``.
        """
        self.penalty_stiffness = scalefactor


class Contact:
    """Provide a way of treating interaction between disjoint parts."""

    def __init__(
        self,
        type=ContactType.NULL,
        category=ContactCategory.SINGLE_SURFACE_CONTACT,
        offset=OffsetType.NULL,
    ):
        self.stub = DynaBase.get_stub()
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

    def set_mortar(self):
        """Set the mortar contact.

        A mort contact is a segment-to-segment, penalty-based contact.
        """
        self.mortar = True

    # def set_algorithm(self, algorithm=ContactAlgorithm.PENALTY_BASED):
    #    self.algorithm = algorithm.value

    def set_tiebreak(self):
        """Set the contact allow for failure.

        A tieback is a special case of this. After failure, the contact
        usually becomes a normal one-way, two-way, or single surface version.
        """
        self.option_tiebreak = True
        self.optionres = 2

    def set_friction_coefficient(self, static=0, dynamic=0):
        """Set the coefficient of friction.

        Parameters
        ----------
        static : float, optional
            Static coefficient of friction. The default is ``0``.
        dynamic : float, optional
            Dynamic coefficient of friction.  The default is ``0``.
        """
        self.static_friction_coeff = static
        self.dynamic_friction_coeff = dynamic

    def set_active_time(self, birth_time=0, death_time=1e20):
        """Set the birth and death time to active and deactivate the contact.

        Parameters
        ----------
        birth_time : int, optional
            Time to activate the contact. The default is ``0``.
        death_time : float, optional
            Time to deactivate the contact. The default is ``1e20``.
        """
        self.birth_time = birth_time
        self.death_time = death_time

    def set_initial_penetration(self):
        """Ignore initial penetrations."""
        self.ignore = 1

    def set_slave_surface(self, surface):
        """Set the slave contact interface."""
        self.slavesurface = surface

    def set_master_surface(self, surface):
        """Set the master contact interface."""
        self.mastersurface = surface

    def set_penalty_algorithm(
        self,
        formulation=ContactFormulation.STANDARD_PENALTY,
        segment_based_contact_option=SBOPT.ASSUME_PLANER_SEGMENTS,
    ):
        """Set the contact formulation."""
        self.contact_formulation = formulation.value
        self.segment_based_contact_option = segment_based_contact_option.value

    def create(self):
        """Create a contact."""
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


class ContactGroup:
    """Create a contact group."""

    def __init__(self):
        self.stub = DynaBase.get_stub()
        self.contactlist = []

    def add(self, contact):
        """Add a contact in the group."""
        self.contactlist.append(contact)

    def create(self):
        """Create contacts."""
        for obj in self.contactlist:
            obj.create()

    def num(self):
        """Get the number of contact objects."""
        return len(self.contactlist)


class Constraint:
    """Provides a way of constraining degrees of freedom to move together in some way."""

    def __init__(self):
        self.stub = stub = DynaBase.get_stub()
        self.spotweldlist = []
        self.cnrbsetidlist = []
        self.jointsphericallist = []
        self.mergerigidlist = []

    def create_spotweld(self, nodeid1, nodeid2):
        """Define massless spot welds between non-contiguous nodal pairs.

        Parameters
        ----------
        nodeid1 : int
            ID for the first node.
        nodeid2 : int
            ID for the second node.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        param = [nodeid1, nodeid2]
        self.spotweldlist.append(param)

    def create_cnrb(self, nodeset):
        """Create a nodal rigid body, which is a rigid body that consists of defined nodes.

        Parameters
        ----------
        nodeset : NodeSet
            Node set that defines the rigid body.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        nodeset.create(self.stub)
        nsid = nodeset.id
        self.cnrbsetidlist.append(nsid)

    def create_joint_spherical(self, nodes, relative_penalty_stiffness=1.0, damping_scale_factor=1.0):
        """Create a joint between two rigid bodies.

        Parameters
        ----------
        nodes : list
            List of nodes for creating the joint.
        relative_penalty_stiffness : int, optional
            Relative penalty stiffness. The default is ``1.0``.
        damping_scale_factor : int, optional
            Damping scale factor on the default damping value.
            The default is ``1.0``.
        """
        self.jointsphericallist.append([nodes, relative_penalty_stiffness, damping_scale_factor])

    def merge_two_rigid_bodies(self, lead_rigidbody=0, constrained_rigidbody=0):
        """Merge two rigid bodies.

        One rigid body, called the constrained rigid body,
        is merged into another rigid body, called the lead rigid body.

        Parameters
        ----------
        lead_rigidbody : int, optional
            Part ID for the lead rigid body. The default is ``0``.
        constrained_rigidbody : int
            Part ID for the constrained rigid body. The default is ``0``.
        """
        self.mergerigidlist.append([lead_rigidbody, constrained_rigidbody])

    def create(self):
        """Create a constraint."""
        for obj in self.spotweldlist:
            self.stub.CreateConstrainedSpotWeld(ConstrainedSpotWeldRequest(node1=obj[0], node2=obj[1]))
            logging.info("Spotweld Created...")

        for i in range(len(self.cnrbsetidlist)):
            self.stub.CreateConstrainedNodalRigidBody(
                ConstrainedNodalRigidBodyRequest(pid=i, nsid=self.cnrbsetidlist[i])
            )
            logging.info("CNRB Created...")
        for i in range(len(self.jointsphericallist)):
            self.stub.CreateConstrainedJoint(
                ConstrainedJointRequest(
                    type="SPHERICAL",
                    nodes=self.jointsphericallist[i][0],
                    rps=self.jointsphericallist[i][1],
                    damp=self.jointsphericallist[i][2],
                )
            )
            logging.info("joint spherical Created...")
        for i in range(len(self.mergerigidlist)):
            self.stub.CreateConstrainedRigidBodies(
                ConstrainedRigidBodiesRequest(pidl=self.mergerigidlist[i][0], pidc=self.mergerigidlist[i][1])
            )
            logging.info("constrained rigid bodies Created...")


class BoundaryCondition:
    """Provides a way of defining imposed motions on boundary nodes."""

    def __init__(self):
        self.stub = DynaBase.get_stub()
        self.spclist = []
        self.imposedmotionlist = []
        self.templist = []
        self.convectionlist = []
        self._model = None

    def assign_model(self, model):
        self._model = model

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
            Node set.
        contraint_x/y/z_direction : int
            Translational constraint in local x/y/z-direction.
        contraint_x/y/zaxis_rotate : int
            Rotational constraint about local x/y/z-axis.

        """
        param = [nodeset, tx, ty, tz, rx, ry, rz, cid, birth, death]
        self.spclist.append(param)
        # self.bdy_spc = nodeset.nodes
        self._model.add_bdy_spc(nodeset.nodes)

    def create_imposed_motion(
        self,
        set,
        curve,
        motion=Motion.DISPLACEMENT,
        dof=DOF.X_TRANSLATIONAL,
        scalefactor=1,
        birthtime=0,
    ):
        """Create an imposed nodal motion on a node or set of nodes.

        An imposed nodal motion can be a velocity, acceleration, or displacement.

        Parameters
        ----------
        partset : PartSet.
            Part set.
        curve : Curve
            Curve ID or function ID to describe the motion value as a function of time.
        motion : enum
            Velocity/Acceleration/Displacement flag.
        dof : enum
            Applicable degrees of freedom.
        scalefactor : int, optional
            Load curve scale factor. The default is ``1``.
        birthtime : int, optional

        """
        param = [set, curve, motion, dof, scalefactor, birthtime]
        self.imposedmotionlist.append(param)

    def create_temperature(
        self,
        nodeset,
        curve=None,
        scalefactor=1,
    ):
        """Create temperature boundary conditions for a thermal or coupled thermal/structural analysis.

        Parameters
        ----------
        nodeset : NodeSet.
            Node set.
        curve : Curve, optional
            Temperature, T, specification. The default is ``None``.
        scalefactor : float, optional
            Temperature, T, curve multiplier. The default is ``1``.

        """
        param = [nodeset, curve, scalefactor]
        self.templist.append(param)

    def create_convection(
        self,
        segmentset=None,
        convection_heat_transfer_coefficient=None,
        convection_heat_transfer_coefficient_multiplier=0.0,
        environment_temperature=None,
        environment_temperature_multiplier=0.0,
    ):
        """Apply a convection boundary condition on SEGMENT_SET for a thermal analysis.

        Parameters
        ----------
        segmentset : SegmentSet.
            Segment set.
        convection_heat_transfer_coefficient : Curve
            Convection heat transfer coefficient.
        convection_heat_transfer_coefficient_multiplier : float
            Curve multiplier for convection heat transfer coefficient.
        environment_temperature : Curve
            Environment temperature.
        environment_temperature_multiplier : float
            Curve multiplier for environment temperature.

        """
        param = [
            segmentset,
            convection_heat_transfer_coefficient,
            convection_heat_transfer_coefficient_multiplier,
            environment_temperature,
            environment_temperature_multiplier,
        ]
        self.convectionlist.append(param)

    def create(self):
        """Create a boundary condition."""
        for obj in self.imposedmotionlist:
            set = obj[0]
            curve = obj[1]
            motion = obj[2]
            dof = obj[3]
            scalefactor = obj[4]
            birthtime = obj[5]
            set.create(self.stub)
            curve.create(self.stub)
            if set.type == "PARTSET" or set.type == "PART":
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
                self.stub.CreateBdyPrescribedMotion(
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
        for obj in self.spclist:
            nodeset = obj[0]
            tx = obj[1]
            ty = obj[2]
            tz = obj[3]
            rx = obj[4]
            ry = obj[5]
            rz = obj[6]
            cid = obj[7]
            birth = obj[8]
            death = obj[9]
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
            self.stub.CreateBdySpc(
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
        for obj in self.templist:
            nodeset, curve, scalefactor = obj[0], obj[1], obj[2]
            if nodeset.num() == 1:
                nid = nodeset.pos(pos=0)
                option = "NODE"
            else:
                nid = nodeset.create(self.stub)
                option = "SET"
            if curve is not None:
                cid = curve.create(self.stub)
            else:
                cid = 0
            self.stub.CreateBdyTemp(
                BdyTempRequest(
                    option=option,
                    nid=nid,
                    tlcid=cid,
                    tmult=scalefactor,
                )
            )
            logging.info("Boundary Temperature Created...")
        for obj in self.convectionlist:
            ss, hlc, hmult, tlc, tmult = obj[0], obj[1], obj[2], obj[3], obj[4]
            ssid, hlcid, tlcid = 0, 0, 0
            if ss is not None:
                ssid = ss.create(self.stub)
            if hlc is not None:
                hlcid = hlc.create(self.stub)
            if tlc is not None:
                tlcid = tlc.create(self.stub)
            self.stub.CreateBdyConvection(
                BdyConvectionRequest(ssid=ssid, pserod=0, hlcid=hlcid, hmult=hmult, tlcid=tlcid, tmult=tmult, loc=0)
            )
            logging.info("Boundary Convection Created...")


class InitialCondition:
    """Provides a way of initializing velocities and detonation points."""

    def __init__(self):
        self.stub = DynaBase.get_stub()
        self.velocitylist = []
        self.velocitynodelist = []
        self.temperaturelist = []
        self._model = None

    def assign_model(self, model):
        self._model = model

    def create_velocity(
        self,
        velocityset,
        angular_velocity=0,
        velocity=Velocity(0, 0, 0),
        direction=Direction(0, 0, 0),
        stime=0,
    ):
        """Create initial velocities for rotating and/or translating bodies."""
        self.velocitylist.append([velocityset, angular_velocity, velocity, direction, stime])

    def create_velocity_node(self, nodeid, trans=Velocity(0, 0, 0), rot=RotVelocity(0, 0, 0)):
        """Define initial nodal point velocities for a node."""
        self.velocitynodelist.append([nodeid, trans, rot])
        # self.init_velocity.append([nodeid,trans.x,trans.y,trans.z])
        self._model.add_init_velocity([nodeid, trans.x, trans.y, trans.z])

    def create_temperature(self, nodeset=None, temperature=0):
        """Create an initial nodal point temperature."""
        self.temperaturelist.append((nodeset, temperature))

    def create(self):
        """Create an initial condition."""
        for obj in self.velocitylist:
            velocityset = obj[0]
            angular_velocity = obj[1]
            velocity = obj[2]
            direction = obj[3]
            stime = obj[4]
            id = velocityset.create(self.stub)
            if velocityset.type.upper() == "PARTSET":
                type = 1
            elif velocityset.type.upper() == "PART":
                type = 2
            else:
                type = 3
            phase = 0
            if stime != 0:
                phase = 1
                self.stub.CreateInitVelGenerationStartTime(InitVelGenerationStartTimeRequest(stime=stime))
            self.stub.CreateInitVelGeneration(
                InitVelGenerationRequest(
                    id=id,
                    styp=type,
                    omega=angular_velocity,
                    vx=velocity.x,
                    vy=velocity.y,
                    vz=velocity.z,
                    xc=0,
                    yc=0,
                    zc=0,
                    nx=direction.x,
                    ny=direction.y,
                    nz=direction.z,
                    phase=phase,
                )
            )
            logging.info(f"Define initial velocities for {type} {id}.")
        for obj in self.velocitynodelist:
            nid = obj[0]
            trans = obj[1]
            rot = obj[2]
            velocity = [trans.x, trans.y, trans.z, rot.x, rot.y, rot.z]
            self.stub.CreateInitVel(InitVelRequest(nsid=nid, velocity=velocity))
            logging.info(f"Define initial velocities for node {nid}.")
        for obj in self.temperaturelist:
            nset = obj[0]
            temp = obj[1]
            id = nset.create(self.stub)
            type = nset.type.upper()
            if type == "NODESET":
                option = "SET"
            elif type == "NODE":
                option = "NODE"
                id = nset.get_nid()
            else:
                print("Error:Invalid set type!")
            self.stub.CreateInitTemperature(InitTemperatureRequest(option=option, nsid=id, temp=temp))
            logging.info(f"Define temperature at {type} {id}.")


class RigidwallCylinder(BaseObj):
    """Defines a rigid wall with a cylinder form.

    Parameters
    ----------
      tail : Point, optional
        Coordinates of the tail of the normal vector.
        The default is ``(0, 0, 0)``.
      head : Point, optional
        Coordinates of the head of the normal vector.
        The default is ``(0, 0, 0)``.
      radius : float, optional
        Radius of the cylinder. The default is ``1``.
      length : float, optional
        Length of cylinder. The default is ``10``.
    """

    def __init__(self, tail=Point(0, 0, 0), head=Point(0, 0, 0), radius=1, length=10):
        self.stub = DynaBase.get_stub()
        self.tail = tail
        self.head = head
        self.radius = radius
        self.length = length
        self.motion = -1
        self.lcid = 0
        self.dir = Direction(1, 0, 0)
        self.type = "rigidwall_cylinder"

    def set_motion(self, curve, motion=RWMotion.VELOCITY, dir=Direction(1, 0, 0)):
        """Set the prescribed motion."""
        curve.create(self.stub)
        self.lcid = curve.id
        self.motion = motion.value
        self.dir = dir

    def get_data(self) -> List:
        """Get the rigidwall data."""
        data = [
            self.type,
            self.tail.x,
            self.tail.y,
            self.tail.z,
            self.head.x,
            self.head.y,
            self.head.z,
            self.radius,
            self.length,
        ]
        return data

    def create(self):
        """Create a rigidwall cylinder."""
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


class RigidwallSphere(BaseObj):
    """Defines a rigid wall with a sphere form.

    Parameters
    ----------
      center : Point, optional
        The center of sphere.
        The default is ``(0, 0, 0)``.
      orient : Point, optional
        Vector n determines the orintation of the rigidwall,the center define the tail of normal n,
        the orient define the head of normal n.
        The default is ``(0, 0, 0)``.
      radius : float, optional
        Radius of the sphere. The default is ``1``.
    """

    def __init__(self, center=Point(0, 0, 0), orient=Point(0, 0, 0), radius=1):
        self.stub = DynaBase.get_stub()
        self.center = center
        self.orient = orient
        self.radius = radius
        self.motion = -1
        self.lcid = 0
        self.dir = Direction(1, 0, 0)
        self.type = "rigidwall_sphere"

    def set_motion(self, curve, motion=RWMotion.VELOCITY, dir=Direction(1, 0, 0)):
        """Set the prescribed motion."""
        curve.create(self.stub)
        self.lcid = curve.id
        self.motion = motion.value
        self.dir = dir

    def get_data(self) -> List:
        """Get the rigidwall data."""
        data = [
            self.type,
            self.center.x,
            self.center.y,
            self.center.z,
            self.orient.x,
            self.orient.y,
            self.orient.z,
            self.radius,
        ]
        return data

    def create(self):
        """Create a rigidwall sphere."""
        parameter = [
            self.center.x,
            self.center.y,
            self.center.z,
            self.orient.x,
            self.orient.y,
            self.orient.z,
            self.radius,
        ]
        self.stub.CreateRigidWallGeom(
            RigidWallGeomRequest(
                geomtype=4,
                motion=self.motion,
                display=1,
                parameter=parameter,
                lcid=self.lcid,
                vx=self.dir.x,
                vy=self.dir.y,
                vz=self.dir.z,
            )
        )
        logging.info("Sphere Rigidwall Created...")


class RigidwallPlanar(BaseObj):
    """Defines planar rigid walls with either finite or infinite size.

    Parameters
    ----------
      tail : Point
        Coordinate of the tail of the normal vector.
        The default is ``(0, 0, 0)``.
      head : Point
        Coordinate of the head of the normal vector.
        The default is ``(0, 0, 0)``.
      coulomb_friction_coefficient : float, optional
        Friction coefficieint in coulomb units. The default is ``0.5``.
    """

    def __init__(self, tail=Point(0, 0, 0), head=Point(0, 0, 0), coulomb_friction_coefficient=0.5):
        self.stub = DynaBase.get_stub()
        self.tail = tail
        self.head = head
        self.fric = coulomb_friction_coefficient
        self.type = "rigidwall_planar"

    def get_data(self) -> List:
        """Get the rigidwall data."""
        data = [self.type, self.tail.x, self.tail.y, self.tail.z, self.head.x, self.head.y, self.head.z]
        return data

    def create(self):
        """Create planar rigid walls."""
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


class Gravity(BaseObj):
    """Defines body force loads using global axes directions.

    Body force loads are due to a prescribed base acceleration or
    angular velocity.
    """

    def __init__(self, dir=GravityOption.DIR_Z, load=Curve(x=[0, 0], y=[0, 0])):
        self.stub = DynaBase.get_stub()
        self.dir = dir.value
        self.load = load
        self.type = "gravity"

    def create(self):
        """Define a body force."""
        id = self.load.create(self.stub)
        ret = self.stub.CreateLoadBody(LoadBodyRequest(option=self.dir, lcid=id))
        logging.info("Load body Created...")
