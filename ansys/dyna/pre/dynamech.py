"""
Mechanical API
==============

Module to setup Explicit or Implicit analysis
"""

from .dynabase import *  # noqa : F403

class DynaMech(DynaBase):
    """Define an Mechanical analysis.

    Parameters
    ----------
    
    Returns
    -------
   
    """

    def __init__(self):
        DynaBase.__init__(self)

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