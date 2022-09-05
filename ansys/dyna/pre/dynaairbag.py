"""Module to create Airbag dyna input deck"""

import logging

from .dynabase import *


class DynaAirbag(DynaBase):
    """Contains methods to create keyword related to airbag"""

    def __init__(self, hostname="localhost"):
        DynaBase.__init__(self, hostname)

    def create_simple_airbag_model(
        self, modeltype, sid, sidtyp, cv, cp, t, lcid, mu, area, pe, ro
    ):
        """Define an airbag or control volume.
        Refer to: *SIMPLE_AIRBAG_MODEL
        Parameters
        ----------
        modeltype : string
            specifies one of the following thermodynamic models for modeling airbags using a control volume (CV) approach.
            "SIMPLE_AIRBAG_MODEL"
        sid : int
            Set ID.
        sidtyp : int
            Set type: EQ.0:segment EQ.1: part set ID.
        cv : float
            Heat capacity at constant volume.
        cp : float
            Heat capacity at constant pressure.
        t : float
            Temperature of input gas.
        lcid : int
            Load curve ID specifying input mass flow rate.
        mu : float
            Shape factor for exit hole.
        area : float
            Exit area.
        pe : float
            Ambient pressure.
        ro : float
            Ambient density.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateAirbagModel(
            AirbagModelRequest(
                modeltype=modeltype,
                sid=sid,
                sidtyp=sidtyp,
                cv=cv,
                cp=cp,
                t=t,
                lcid=lcid,
                mu=mu,
                area=area,
                pe=pe,
                ro=ro,
            )
        )
        logging.info("Airbag Model Created...")
        return ret

    def save_file(self):
        """Save keyword files."""
        #self.set_accuracy(objective_stress_updates=Switch.ON,invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,implicit_accuracy_flag=Switch.ON)
        #self.set_bulk_viscosity(bulk_viscosity_type=BulkViscosity.COMPUTE_INTERNAL_ENERGY_DISSIPATED)
        self.set_energy(hourglass_energy=EnergyFlag.COMPUTED,sliding_interface_energy=EnergyFlag.COMPUTED,)
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

class Airbag:
    def __init__(self,set,
    heat_capacity_at_constant_volume=0,
    heat_capacity_at_constant_pressure=0,
    input_gas_temperature=0,
    input_mass_flow_rate=Curve(x=[],y=[]),
    shape_factor_for_exit_hole=0,
    ambient_pressure=0,
    ambient_density=0):
        self.stub = DynaBase.get_stub()
        self.cv = heat_capacity_at_constant_volume
        self.cp = heat_capacity_at_constant_pressure
        self.t = input_gas_temperature
        self.lcid = input_mass_flow_rate.create(self.stub)
        self.mu = shape_factor_for_exit_hole
        self.pe = ambient_pressure
        self.ro = ambient_density
        self.sid = set.create(self.stub)
        if(set.type=="SEGMENTSET"):
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