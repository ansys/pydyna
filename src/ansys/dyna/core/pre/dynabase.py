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

    Parameters
    ----------
    sfo : float, optional
        Scale factor for ordinate values. Default is 1.
    x : list, optional
        X values (abscissa/independent variable).
    y : list, optional
        Y values (ordinate/dependent variable).
    func : str, optional
        Mathematical function expression (e.g., "sin(TIME)"). If provided,
        creates a DEFINE_CURVE_FUNCTION instead of DEFINE_CURVE.
    title : str, optional
        Curve title.
    """

    def __init__(self, sfo=1, x=[], y=[], func=None, title=""):
        self.sfo = sfo
        self.abscissa = x
        self.ordinate = y
        self.func = func
        self.title = title

    def create(self, stub=None):
        """Create a curve.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        int
            The curve ID.
        """
        if stub is None:
            stub = DynaBase.get_stub()

        # Check if this is a keywords backend
        if hasattr(stub, "_backend"):
            # Keywords backend - use direct method
            backend = stub._backend
            if self.func is not None:
                curve_id = backend.create_define_curve_function(
                    function=self.func,
                    sfo=self.sfo,
                    title=self.title,
                )
            else:
                curve_id = backend.create_define_curve(
                    sfo=self.sfo,
                    abscissa=list(self.abscissa),
                    ordinate=list(self.ordinate),
                    title=self.title,
                )
            self.id = curve_id
        else:
            # gRPC stub
            if self.func is not None:
                ret = stub.CreateDefineCurveFunction(DefineCurveFunctionRequest(function=self.func, title=self.title))
            else:
                ret = stub.CreateDefineCurve(
                    DefineCurveRequest(sfo=self.sfo, abscissa=self.abscissa, ordinate=self.ordinate, title=self.title)
                )
            self.id = ret.id

        logging.info(f"Curve {self.id} defined...")
        return self.id


class Function:
    """Defines a function that can be referenced by a limited number of keyword options.

    Parameters
    ----------
    Function : str, optional
        The function definition string.
    fid : int, optional
        Explicit function ID. If None, auto-generated.
    """

    def __init__(self, Function=None, fid=None):
        self.function = Function
        self.tabulated = False
        self._explicit_fid = fid

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
        # Check if using keywords backend (supports fid parameter)
        if hasattr(stub, "_backend") and self._explicit_fid is not None:
            # Keywords backend - use a simple object that supports attribute access
            request = type("Request", (), {"function": self.function, "fid": self._explicit_fid})()
        else:
            # gRPC stub - DefineFunctionRequest doesn't support fid field
            request = DefineFunctionRequest(function=self.function)
        ret = stub.CreateDefineFunction(request)
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
    """Defines a transformation.

    Parameters
    ----------
    option : str, optional
        Transformation option (e.g., 'MIRROR', 'SCALE', 'TRANSL', 'ROTATE').
    param1-param7 : float, optional
        Transformation parameters.
    tranid : int, optional
        Explicit transformation ID. If None, auto-generated.
    """

    def __init__(self, option=None, param1=0, param2=0, param3=0, param4=0, param5=0, param6=0, param7=0, tranid=None):
        param = [option, param1, param2, param3, param4, param5, param6, param7]
        self.paramlist = []
        self.paramlist.append(param)
        self._explicit_tranid = tranid

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
        # Check if using keywords backend (supports tranid parameter)
        if hasattr(stub, "_backend") and self._explicit_tranid is not None:
            # Keywords backend - use a simple object that supports attribute access
            request = type("Request", (), {"option": options, "param": params, "tranid": self._explicit_tranid})()
        else:
            # gRPC stub - DefineTransformationRequest doesn't support tranid field
            request = DefineTransformationRequest(option=options, param=params)
        ret = stub.CreateDefineTransformation(request)
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

    # Class-level backend reference (set when using keywords backend)
    _backend = None

    def __init__(self):
        self.stub = DynaSolution.get_stub()
        self.mainname = ""
        DynaBase.stub = self.stub
        # Get backend reference if using keywords backend
        self._backend = DynaSolution._backend
        DynaBase._backend = self._backend
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
        max_timestep : Curve or int, optional
            Load curve that limits the maximum time step size. Can be a Curve
            object (which will be created) or an integer curve ID referencing
            an existing curve. The default is ``None`` (no curve limit).

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if max_timestep is None:
            cid = 0
        elif isinstance(max_timestep, int):
            # Direct curve ID reference
            cid = max_timestep
        else:
            # Curve object - create it
            cid = max_timestep.create(self.stub)
        if self._backend is not None:
            # Use keywords backend directly
            self._backend.create_control_timestep(
                tssfac=tssfac,
                isdo=isdo,
                dt2ms=timestep_size_for_mass_scaled,
                lctm=cid,
            )
            ret = True
        else:
            # Fall back to gRPC stub
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
            if self._backend is not None:
                # Use keywords backend directly
                self._backend.create_control_accuracy(
                    osu=objective_stress_updates.value,
                    inn=invariant_node_number.value,
                    pidosu=partsetid_for_objective_stress_updates,
                    iacc=implicit_accuracy_flag.value,
                    exacc=explicit_accuracy_flag.value,
                )
            else:
                # Fall back to gRPC stub
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
        material_energy=None,
        damping_energy=None,
        discrete_element_energy=None,
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

        material_energy : int, optional
            Material energies output option (keywords backend only).

            - EQ.1: Output material energies.
            - EQ.2: Do not output material energies.

        damping_energy : int, optional
            Damping energy dissipation option (keywords backend only).

            - EQ.1: Energy dissipation is computed.
            - EQ.2: Energy dissipation is not computed.

        discrete_element_energy : int, optional
            Discrete element energy dissipation option (keywords backend only).

            - EQ.1: Energy dissipation is computed.
            - EQ.2: Energy dissipation is not computed.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = True
        if not self.have_energy:
            if self._backend is not None:
                # Use keywords backend directly with extended parameters
                kwargs = {
                    "hgen": hourglass_energy.value,
                    "rwen": rigidwall_energy.value,
                    "slnten": sliding_interface_energy.value,
                    "rylen": rayleigh_energy.value,
                    "irgen": initial_reference_geometry_energy.value,
                }
                if material_energy is not None:
                    kwargs["maten"] = material_energy
                if damping_energy is not None:
                    kwargs["drlen"] = damping_energy
                if discrete_element_energy is not None:
                    kwargs["disen"] = discrete_element_energy
                self._backend.create_control_energy(**kwargs)
            else:
                # Fall back to gRPC stub (does not support extended parameters)
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
        edit_frequency=None,
        flush_frequency=None,
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

        edit_frequency : int, optional
            Edit frequency for the d3hsp file (IKEDIT). The default is None (0).
        flush_frequency : int, optional
            Flush frequency for output files (IFLUSH). The default is None (0).

        """
        if print_suppression_d3hsp:
            npopt = 1
        else:
            npopt = 0

        ikedit = edit_frequency if edit_frequency is not None else 0
        iflush = flush_frequency if flush_frequency is not None else 0

        if self._backend is not None:
            # Use keywords backend directly
            self._backend.create_control_output(
                npopt=npopt,
                neecho=print_suppression_echo.value,
                ikedit=ikedit,
                iflush=iflush,
            )
            ret = True
        else:
            # Fall back to gRPC stub
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
            if self._backend is not None:
                # Use keywords backend directly
                self._backend.create_control_hourglass(
                    ihq=controltype.value,
                    qh=coefficient,
                )
            else:
                # Fall back to gRPC stub
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
            if self._backend is not None:
                # Use keywords backend directly
                self._backend.create_control_bulk_viscosity(
                    q1=quadratic_viscosity_coeff,
                    q2=linear_viscosity_coeff,
                    bulk_type=bulk_viscosity_type.value,
                )
            else:
                # Fall back to gRPC stub
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
        lcid : int or Curve, optional
            Load curve ID, which specifies the system damping constant
            versus the time. Can be an integer ID or a Curve object
            (will extract its .id attribute). The default is ``0``.
        valdmp : float, optional
            System damping constant. The default is ``0.0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        # Support both integer IDs and Curve objects
        curve_id = lcid.id if hasattr(lcid, "id") else lcid

        ret = self.stub.CreateDampingGlobal(DampingGlobalRequest(lcid=curve_id, valdmp=valdmp))
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

        Example::
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

    def set_transform(
        self,
        filename=None,
        idnoff=0,
        ideoff=0,
        idpoff=0,
        idmoff=0,
        idsoff=0,
        idfoff=0,
        iddoff=0,
        fctlen=0.0,
        transform=None,
    ):
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
        iddoff : int
            Offset to define ID.
        fctlen : float
            Length scale factor for coordinates.
        transform : Transform
            Definition for the transformation.
        """
        tranid = transform.create(self.stub)
        # Create request object with all fields (including extended fields not in gRPC)
        request = type(
            "IncludeTransformReq",
            (),
            {
                "filename": filename,
                "idnoff": idnoff,
                "ideoff": ideoff,
                "idpoff": idpoff,
                "idmoff": idmoff,
                "idsoff": idsoff,
                "idfoff": idfoff,
                "iddoff": iddoff,
                "fctlen": fctlen,
                "tranid": tranid,
            },
        )()
        ret = self.stub.CreateIncludeTransform(request)
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
    """Defines a nodal set with some identical or unique attributes.

    Parameters
    ----------
    nodes : list, optional
        List of node IDs in the set.
    solver : str, optional
        Solver type for the node set. Options are "MECH" (mechanical),
        "THER" (thermal), etc. Default is None (no solver specification).
    sid : int, optional
        Explicit set ID. If None, auto-generated.
    """

    def __init__(self, nodes=[], solver=None, sid=None):
        self.nodes = nodes
        self.type = "NODESET"
        self.solver = solver
        self._explicit_sid = sid

    def create(self, stub=None):
        """Create a node set.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        int
            The set ID, or 0 if no nodes.
        """
        if len(self.nodes) <= 0:
            return 0
        if stub is None:
            stub = DynaBase.get_stub()

        # Check if this is a keywords backend
        if hasattr(stub, "_backend"):
            # Keywords backend - use direct method
            backend = stub._backend
            sid = self._explicit_sid if self._explicit_sid is not None else backend.next_id("nodeset")
            if self.solver:
                backend.create_set_node_list_with_solver(sid=sid, nodes=self.nodes, solver=self.solver)
            else:
                backend.create_set_node_list(sid=sid, nodes=self.nodes)
            self.id = sid
        else:
            # gRPC stub - use explicit sid if provided, otherwise 0 for auto-generation
            explicit_sid = self._explicit_sid if self._explicit_sid is not None else 0
            ret = stub.CreateNodeSet(
                NodeSetRequest(option="LIST", sid=explicit_sid, genoption="NODE", entities=self.nodes)
            )
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
    """Defines a set of parts with optional attributes.

    Parameters
    ----------
    parts : list, optional
        List of part IDs in the set.
    solver : str, optional
        Solver type for the part set. Options are "MECH" (mechanical),
        "THER" (thermal), etc. Default is None (no solver specification).
    sid : int, optional
        Explicit set ID. If None, auto-generated.
    """

    def __init__(self, parts=[], solver=None, sid=None):
        self.parts = parts
        self.solver = solver
        self._explicit_sid = sid

    def create(self, stub=None):
        """Create a part set.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        int
            The set ID, or 0 if no parts.
        """
        if len(self.parts) <= 0:
            return 0
        if stub is None:
            stub = DynaBase.get_stub()

        # Check if this is a keywords backend
        if hasattr(stub, "_backend"):
            # Keywords backend - use direct method
            backend = stub._backend
            sid = self._explicit_sid if self._explicit_sid is not None else backend.next_id("partset")
            solver = self.solver if self.solver else "MECH"
            backend.create_set_part_list(sid=sid, parts=self.parts, solver=solver)
            self.id = sid
        else:
            # gRPC stub - use explicit sid if provided, otherwise 0 for auto-generation
            explicit_sid = self._explicit_sid if self._explicit_sid is not None else 0
            ret = stub.CreatePartSet(PartSetRequest(sid=explicit_sid, pids=self.parts))
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
    """Defines section properties for shell elements.

    Parameters
    ----------
    element_formulation : int
        Element formulation. Common options:
        - 2: Belytschko-Tsay (default)
        - 6: S/R Hughes-Liu
        - 16: Fully integrated shell element (very accurate)
    shear_factor : float, optional
        Shear correction factor. Default is 1.0.
    integration_points : int, optional
        Number of through-thickness integration points. Default is 5.
    printout : float, optional
        Printout option. Default is 0.
    thickness1 : float, optional
        Shell thickness at node 1. Default is 0.
    thickness2 : float, optional
        Shell thickness at node 2. Default is 0.
    thickness3 : float, optional
        Shell thickness at node 3. Default is 0.
    thickness4 : float, optional
        Shell thickness at node 4. Default is 0.
    secid : int, optional
        Section ID. If not provided, auto-assigned by the backend.
    """

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
        secid=None,
    ):
        self.element_formulation = element_formulation
        self.shear_factor = shear_factor
        self.integration_points = integration_points
        self.printout = printout
        self.thickness1 = thickness1
        self.thickness2 = thickness2
        self.thickness3 = thickness3
        self.thickness4 = thickness4
        self.secid = secid
        self.id = None

    def create(self, stub=None):
        """Create the shell section.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        int
            The section ID.
        """
        if stub is None:
            stub = DynaBase.get_stub()

        # Check if using keywords backend
        if hasattr(stub, "_backend"):
            backend = stub._backend
            secid = self.secid if self.secid is not None else backend.next_id("section")
            backend.create_section_shell(
                secid=secid,
                elform=self.element_formulation,
                shrf=self.shear_factor,
                nip=self.integration_points,
                propt=self.printout,
                t1=self.thickness1,
                t2=self.thickness2,
                t3=self.thickness3,
                t4=self.thickness4,
            )
            self.id = secid
        else:
            # gRPC stub
            ret = stub.CreateSectionShell(
                SectionShellRequest(
                    elform=self.element_formulation,
                    shrf=self.shear_factor,
                    nip=self.integration_points,
                    propt=self.printout,
                    t1=self.thickness1,
                    t2=self.thickness2,
                    t3=self.thickness3,
                    t4=self.thickness4,
                )
            )
            self.id = ret.id

        logging.info(f"ShellSection {self.id} created...")
        return self.id


class SolidSection:
    """Defines section properties for solid elements.

    Parameters
    ----------
    element_formulation : int
        Element formulation. The default is ``1`` (constant stress solid element).

        Common options:
        - EQ.0: One point corotational for *ELEMENT_SOLID_ORTHO
        - EQ.1: Constant stress solid element (default)
        - EQ.2: Fully integrated S/R solid (8-point Gauss)
        - EQ.-1: Fully integrated S/R solid with nodal rotations (8-point Gauss)
        - EQ.-2: Fully integrated S/R solid with nodal rotations and edge contact
        - EQ.10: 1 point tetrahedron
        - EQ.13: 1 point nodal pressure tetrahedron for bulk metal forming
        - EQ.18: 8 node enhanced strain solid for shell-solid assemblies (NVH)
    secid : int, optional
        Section ID. If not provided, an ID will be auto-generated.
    """

    def __init__(self, element_formulation: int = 1, secid: int = None):
        self.element_formulation = element_formulation
        self.id = secid
        self._explicit_id = secid is not None

    def create(self, stub=None):
        """Create the solid section.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        int
            The section ID.
        """
        if stub is None:
            stub = DynaBase.get_stub()

        # Check if using keywords backend
        if hasattr(stub, "_backend"):
            backend = stub._backend
            # Use explicit ID if provided, otherwise auto-generate
            if self._explicit_id:
                secid = self.id
            else:
                secid = backend.next_id("section")
            backend.create_section_solid(
                secid=secid,
                elform=self.element_formulation,
            )
            self.id = secid
        else:
            # gRPC stub - use explicit ID if provided via request
            if self._explicit_id:
                ret = stub.CreateSectionSolid(
                    SectionSolidRequest(
                        secid=self.id,
                        elform=self.element_formulation,
                    )
                )
            else:
                ret = stub.CreateSectionSolid(
                    SectionSolidRequest(
                        elform=self.element_formulation,
                    )
                )
            self.id = ret.id

        logging.info(f"SolidSection {self.id} created...")
        return self.id


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
        sec.create(self.stub)
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
        self.defined_general = False
        self.imflag = analysis_type.value
        self.dt0 = initial_timestep_size
        # Initialize general parameters with defaults
        self.imform = 2
        self.nsbs = 1
        self.igs = 2
        self.cnstn = 0
        self.form = 0
        self.zero_v = 0
        # Initialize eigenvalue parameters with defaults
        self.neig = 0
        self.shfscl = 0
        self.center = 0.0
        self.eigmth = 2
        # Initialize solution parameters with defaults
        self.nsolver = 12
        self.ilimit = 11
        self.maxref = 55
        self.abstol = 1e-10
        self.dctol = 0.001
        self.ectol = 0.01
        self.rctol = 1.0e10
        self.lstol = 0.9
        self.dnorm = 2
        self.diverg = 1
        self.istif = 1
        self.nlprint = 0
        self.nlnorm = 0.0
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
        birth_time=0.0,
        death_time=1.0e28,
        burial_time=1.0e28,
        rate_effects=0,
        hht_alpha=0.0,
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
        birth_time : float, optional
            Birth time for dynamic analysis (TDYBIR). The default is ``0.0``.
            Only supported with the keywords backend.
        death_time : float, optional
            Death time for dynamic analysis (TDYDTH). The default is ``1.0e28``.
            Only supported with the keywords backend.
        burial_time : float, optional
            Burial time for dynamic analysis (TDYBUR). The default is ``1.0e28``.
            Only supported with the keywords backend.
        rate_effects : int, optional
            Rate effects flag (IRATE). The default is ``0``.
            Only supported with the keywords backend.
        hht_alpha : float, optional
            HHT time integration constant (ALPHA). The default is ``0.0``.
            Only supported with the keywords backend.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_dynamic = True
        self.imass = integration_method.value
        self.gamma = gamma
        self.beta = beta
        # Extended parameters (keywords backend only)
        self.tdybir = birth_time
        self.tdydth = death_time
        self.tdybur = burial_time
        self.irate = rate_effects
        self.alpha = hht_alpha

    def set_eigenvalue(self, number_eigenvalues=0, shift_scale=0, center=0.0, eigenvalue_method=2):
        """Activate implicit eigenvalue analysis and define associated input parameters.

        Parameters
        ----------
        number_eigenvalues : int, optional
            Number of eigenvalues to extract. The default is ``0``.
        shift_scale : float, optional
            Shift scale. The default is ``0``.
        center : float, optional
            Center frequency for eigenvalue extraction. The default is ``0.0``.
        eigenvalue_method : int, optional
            Eigenvalue extraction method. The default is ``2``.

            - EQ.2: Block Shift and Invert Lanczos (default)
            - EQ.3: Lanczos with [M] = [I]
            - EQ.5: Same as 2 but includes a mass orthogonality check
            - EQ.6: Same as 3 but includes a mass orthogonality check
            - EQ.102: Block Shift and Invert Lanczos with BCSLIB-EXT
            - EQ.103: Same as 102 with [M] = [I]
            - EQ.105: Same as 102 but includes mass orthogonality check
            - EQ.106: Same as 103 but includes mass orthogonality check

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined_eigenvalue = True
        self.neig = number_eigenvalues
        self.shfscl = shift_scale
        self.center = center
        self.eigmth = eigenvalue_method

    def set_general(self, imflag=1, dt0=0.0, imform=2, nsbs=1, igs=2, cnstn=0, form=0, zero_v=0):
        """Set general implicit analysis parameters.

        Parameters
        ----------
        imflag : int, optional
            Implicit analysis flag. The default is ``1``.

            - EQ.0: No implicit analysis
            - EQ.1: Implicit analysis on
            - EQ.2: Explicit/implicit switching
            - EQ.4: Implicit with automatic implicit/explicit switching
            - EQ.5: Implicit SMP with automatic switching
            - EQ.6: Implicit MPP with automatic switching

        dt0 : float, optional
            Initial time step size for implicit analysis. The default is ``0.0``.
        imform : int, optional
            Implicit element formulation. The default is ``2``.

            - EQ.1: Type 16 shell, fully integrated brick
            - EQ.2: Type 16 shell, reduced integration brick (default)

        nsbs : int, optional
            Number of sub-steps per output time step. The default is ``1``.
        igs : int, optional
            Geometric (initial stress) stiffness flag. The default is ``2``.

            - EQ.1: Off
            - EQ.2: On (default)

        cnstn : int, optional
            Indicator for consistent tangent stiffness. The default is ``0``.
        form : int, optional
            Element formulation when using IMFORM flag. The default is ``0``.
        zero_v : int, optional
            Zero velocity flag. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.defined = True
        self.defined_general = True
        self.imflag = imflag
        self.dt0 = dt0
        self.imform = imform
        self.nsbs = nsbs
        self.igs = igs
        self.cnstn = cnstn
        self.form = form
        self.zero_v = zero_v

    def set_solution(
        self,
        solution_method=12,
        iteration_limit=11,
        stiffness_reformation_limit=55,
        absolute_convergence_tolerance=1e-10,
        displacement_convergence_tolerance=0.001,
        energy_convergence_tolerance=0.01,
        residual_convergence_tolerance=1.0e10,
        line_search_tolerance=0.9,
        displacement_norm=2,
        divergence_flag=1,
        initial_stiffness_flag=1,
        nonlinear_print_flag=0,
        nonlinear_norm=0.0,
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
        displacement_convergence_tolerance : float, optional
            Displacement convergence tolerance (dctol). The default is ``0.001``.
        energy_convergence_tolerance : float, optional
            Energy convergence tolerance (ectol). The default is ``0.01``.
        residual_convergence_tolerance : float, optional
            Residual (force) convergence tolerance (rctol). The default is ``1.0e10``.
        line_search_tolerance : float, optional
            Line search convergence tolerance (lstol). The default is ``0.9``.
        displacement_norm : int, optional
            Displacement norm for convergence test (dnorm). The default is ``2``.
        divergence_flag : int, optional
            Divergence flag (diverg). The default is ``1``.
        initial_stiffness_flag : int, optional
            Initial stiffness formation flag (istif). The default is ``1``.
        nonlinear_print_flag : int, optional
            Nonlinear solver print level (nlprint). The default is ``0``.
        nonlinear_norm : float, optional
            Nonlinear residual norm type (nlnorm). The default is ``0.0``.

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
        self.dctol = displacement_convergence_tolerance
        self.ectol = energy_convergence_tolerance
        self.rctol = residual_convergence_tolerance
        self.lstol = line_search_tolerance
        self.dnorm = displacement_norm
        self.diverg = divergence_flag
        self.istif = initial_stiffness_flag
        self.nlprint = nonlinear_print_flag
        self.nlnorm = nonlinear_norm

    def set_consistent_mass_matrix(self):
        """Use the consistent mass matrix in implicit dynamics and eigenvalue solutions."""
        self.defined_mass_matrix = True

    def create(self):
        """Create an implicit analysis."""
        if self.defined == False:
            return
        if self.defined:
            # Use extended parameters if set_general was called, otherwise use basic parameters
            if self.defined_general:
                # Create a simple object with all attributes for keywords backend compatibility
                request = type(
                    "Request",
                    (),
                    {
                        "imflag": self.imflag,
                        "dt0": self.dt0,
                        "imform": self.imform,
                        "nsbs": self.nsbs,
                        "igs": self.igs,
                        "cnstn": self.cnstn,
                        "form": self.form,
                        "zero_v": self.zero_v,
                    },
                )()
                self.stub.CreateControlImplicitGeneral(request)
            else:
                self.stub.CreateControlImplicitGeneral(ControlImplicitGeneralRequest(imflag=self.imflag, dt0=self.dt0))
        if self.defined_auto:
            self.stub.CreateControlImplicitAuto(ControlImplicitAutoRequest(iauto=self.iauto, iteopt=self.iteopt))
        if self.defined_dynamic:
            # Check if using keywords backend (has _backend attribute) vs gRPC stub
            if hasattr(self.stub, "_backend"):
                # Keywords backend - pass all extended parameters
                request = type(
                    "Request",
                    (),
                    {
                        "imass": self.imass,
                        "gamma": self.gamma,
                        "beta": self.beta,
                        "tdybir": self.tdybir,
                        "tdydth": self.tdydth,
                        "tdybur": self.tdybur,
                        "irate": self.irate,
                        "alpha": self.alpha,
                    },
                )()
                self.stub.CreateControlImplicitDynamics(request)
            else:
                # gRPC stub - only pass original 3 parameters that protobuf supports
                self.stub.CreateControlImplicitDynamic(
                    ControlImplicitDynamicRequest(imass=self.imass, gamma=self.gamma, beta=self.beta)
                )
        if self.defined_eigenvalue:
            # Create a simple object with all attributes for keywords backend compatibility
            request = type(
                "Request",
                (),
                {
                    "neig": self.neig,
                    "shfscl": self.shfscl,
                    "center": self.center,
                    "eigmth": self.eigmth,
                },
            )()
            self.stub.CreateControlImplicitEigenvalue(request)
        if self.defined_solution:
            # Check if using keywords backend (has _backend attribute) vs gRPC stub
            if hasattr(self.stub, "_backend"):
                # Keywords backend - pass all extended parameters
                request = type(
                    "Request",
                    (),
                    {
                        "nsolvr": self.nsolver,
                        "ilimit": self.ilimit,
                        "maxref": self.maxref,
                        "abstol": self.abstol,
                        "dctol": self.dctol,
                        "ectol": self.ectol,
                        "rctol": self.rctol,
                        "lstol": self.lstol,
                        "dnorm": self.dnorm,
                        "diverg": self.diverg,
                        "istif": self.istif,
                        "nlprint": self.nlprint,
                        "nlnorm": self.nlnorm,
                    },
                )()
                self.stub.CreateControlImplicitSolution(request)
            else:
                # gRPC stub - only pass original 4 parameters that protobuf supports
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

    def create(self, stub=None):
        """Create a thermal analysis.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().
        """
        if stub is None:
            stub = self.stub

        # Check if using keywords backend
        if hasattr(stub, "_backend"):
            backend = stub._backend
            if self.defined_timestep:
                backend.create_control_thermal_timestep(its=self.its)
            if self.defined_solver:
                backend.create_control_thermal_solver(atype=self.atype)
            if self.defined_timestep or self.defined_solver:
                backend.create_control_solution(soln=2)
            if self.defined_nonlinear:
                backend.create_control_thermal_nonlinear(tol=self.tol, dcp=self.dcp)
        else:
            # gRPC stub
            if self.defined_timestep:
                stub.CreateControlThermalTimestep(ControlThermalTimestepRequest(its=self.its))
            if self.defined_solver:
                stub.CreateControlThermalSolver(ControlThermalSolverRequest(atype=self.atype))
            if self.defined_timestep or self.defined_solver:
                stub.CreateControlSolution(ControlSolutionRequest(soln=2))
            if self.defined_nonlinear:
                stub.CreateControlThermalNonlinear(ControlThermalNonlinearRequest(tol=self.tol, dcp=self.dcp))


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


class BoundaryPrescribedMotionRigid:
    """Defines prescribed motion for rigid bodies.

    This class creates BOUNDARY_PRESCRIBED_MOTION_RIGID keywords.

    Parameters
    ----------
    pid : int
        Part ID for the rigid body.
    dof : int
        Degree of freedom:
        - 1: x-translational
        - 2: y-translational
        - 3: z-translational
        - 4: x-rotational
        - 5: y-rotational
        - 6: z-rotational
    vad : int
        Velocity/Acceleration/Displacement flag:
        - 0: velocity (rigid body only)
        - 1: acceleration
        - 2: displacement
    curve : Curve or int
        Curve object or curve ID defining the motion.
    scale_factor : float, optional
        Scale factor for the curve. Default is 1.0.
    birth : float, optional
        Birth time for the motion. Default is 0.0.
    death : float, optional
        Death time for the motion. Default is 0.0 (inactive).
    """

    def __init__(
        self,
        pid: int,
        dof: int,
        vad: int,
        curve,
        scale_factor: float = 1.0,
        birth: float = 0.0,
        death: float = 0.0,
    ):
        self.pid = pid
        self.dof = dof
        self.vad = vad
        self.curve = curve
        self.scale_factor = scale_factor
        self.birth = birth
        self.death = death

    def create(self, stub=None):
        """Create the boundary prescribed motion rigid keyword.

        Parameters
        ----------
        stub : object, optional
            The stub to use for creation. If not provided, uses DynaBase.get_stub().

        Returns
        -------
        bool
            True if successful.
        """
        if stub is None:
            stub = DynaBase.get_stub()

        # Get curve ID if a Curve object was passed
        if hasattr(self.curve, "id"):
            lcid = self.curve.id
        else:
            lcid = self.curve

        # Check if using keywords backend
        if hasattr(stub, "_backend"):
            # Keywords backend - use direct method
            stub._backend.create_boundary_prescribed_motion_rigid(
                pid=self.pid,
                dof=self.dof,
                vad=self.vad,
                lcid=lcid,
                sf=self.scale_factor,
                birth=self.birth,
                death=self.death,
            )
        else:
            # gRPC stub - use BdyPrescribedMotionRequest with option="RIGID"
            stub.CreateBdyPrescribedMotion(
                BdyPrescribedMotionRequest(
                    id=0,
                    heading="",
                    option="RIGID",
                    typeid=self.pid,
                    dof=self.dof,
                    vad=self.vad,
                    lcid=lcid,
                    sf=self.scale_factor,
                    vid=0,
                    birth=self.birth,
                    death=self.death,
                )
            )
        logging.info(f"BoundaryPrescribedMotionRigid for part {self.pid} created...")
        return True


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
