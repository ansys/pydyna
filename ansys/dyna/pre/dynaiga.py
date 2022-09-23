"""
IGA API
==========

Module to create IGA dyna input deck
"""

import logging

from .dynabase import *  # noqa : F403
from .dynamech import DynaMech


class DynaIGA(DynaMech):
    """Contains methods to create keyword related to IGA."""

    def __init__(self):
        DynaMech.__init__(self)
        self.casetype = CaseType.IGA

    def create_section_igashell(self, secid, elform, shrf, thickness):
        """Define section properties for isogeometric shell elements.

        Parameters
        ----------
        secid : int
            Section ID. SECID is referenced on the \*PART card. A unique number or label must be specified.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor which scales the transverse shear stress.
        thickness : float
            Shell thickness.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionIGAShell(
            SectionIGAShellRequest(secid=secid, elform=elform, shrf=shrf, thickness=thickness)
        )
        logging.info("Section IGAShell 1 Created...")
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
        self.create_control_contact(rwpnal=1.0, ignore=1, igactc=1)
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