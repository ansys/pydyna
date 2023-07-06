"""
Mechanical API
==============

Module to setup Explicit or Implicit analysis
"""

from .dynabase import *  # noqa : F403


class AnalysisType(Enum):
    EXPLICIT = 1
    IMPLICIT = 2
    NONE = 3


class DynaMech(DynaBase):
    """Define an Mechanical analysis."""

    def __init__(self, analysis=AnalysisType.EXPLICIT):
        DynaBase.__init__(self)
        self.casetype = CaseType.STRUCTURE
        self.analysis = analysis.value

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
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateControlOutput(ControlOutputRequest(npopt=npopt, neecho=neecho))
        logging.info("Control Output Created...")
        return ret

    def set_init_velocity(self, translational=Velocity(0, 0, 0), rotational=RotVelocity(0, 0, 0)):
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
            ``True`` when successful, ``False`` when failed.
        """
        velocity = [
            translational.x,
            translational.y,
            translational.z,
            rotational.x,
            rotational.y,
            rotational.z,
        ]
        ret = self.stub.CreateInitVel(InitVelRequest(nsid=0, velocity=velocity))
        logging.info("Initial velocity Created...")
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
            ``True`` when successful, ``False`` when failed.
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
            ``True`` when successful, ``False`` when failed.
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
            ``True`` when successful, ``False`` when failed.
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
            ``True`` when successful, ``False`` when failed.
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
            ``True`` when successful, ``False`` when failed.
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
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateHourglass(HourglassRequest(ghid=ghid, ihq=ihq, qm=qm, q1=q1, q2=q2, qb=qb, qw=qw))
        logging.info("Hourglass 1 Created...")
        return ret

    def save_file(self, defaultsetting=1):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if self.analysis == 1:
            self.set_accuracy(
                objective_stress_updates=Switch.OFF,
                invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,
                implicit_accuracy_flag=Switch.OFF,
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
            if self.parts.get_num_shellpart() > 0:
                self.create_control_shell(
                    wrpang=0,
                    esort=1,
                    irnxx=0,
                    istupd=0,
                    theory=0,
                    bwc=1,
                    miter=1,
                    proj=1,
                    irquad=0,
                )
        elif self.analysis == 2:
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
            if self.parts.get_num_shellpart() > 0:
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
        else:
            pass
        DynaBase.save_file(self)


class Airbag:
    """Define an airbag or control volume.

    Parameters
    ----------
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
        ``True`` when successful, ``False`` when failed..
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
        self.stub = DynaBase.get_stub()
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

    def create(self):
        """Create airbag."""
        self.stub.CreateAirbagModel(
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
