"""Module to create Structural ALE dyna input deck"""

import logging

from .dynabase import *


class Air():
    def __init__(self,mass_density=1.280E-03,
    pressure_cutoff=-1.0E-9,
    initial_internal_energy = 2.5331E-6,
    initial_relative_volume=1.0,
    equation_coefficient=[0,0,0,0,0.4,0.4,0]
    ):
        """Create material of air
        refer to:*EOS_LINEAR_POLYNOMIAL
                 *MAT_NULL
        Parameters
        ----------
        name : string
            material name.
        mass_density : float
            mass density.
        pressure_cutoff : float
            pressure cutoff.   
        initial_internal_energy : float
            Initial internal energy per unit reference volume.
        initial_relative_volume : float
            Initial relative volume.
        equation_coefficient : list
            six polynomial equation coefficient.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.mass_density=mass_density
        self.pressure_cutoff=pressure_cutoff
        self.initial_internal_energy=initial_internal_energy
        self.initial_relative_volume=initial_relative_volume
        self.equation_coefficient=equation_coefficient

    def create(self,stub):
        ret = stub.CreateEOSLinearPolynomial(
            EOSLinearPolynomialRequest(ci=self.equation_coefficient, e0=self.initial_internal_energy, v0=self.initial_relative_volume)
        )
        self.eos_id = ret.eosid
        ret = stub.CreateMatNull(
            MatNullRequest(ro=self.mass_density,pc=self.pressure_cutoff)
        )
        self.material_id = ret.mid
        self.name = "air"
        logging.info(f"Material {self.name} Created...")

class Liner():
    def __init__(self,mass_density=8.96,
        shear_modulus=0.46,
        youngs_modulus=0,
        poissons_ratio = 0.34,
        constants = [90e-5,292e-5,0.31,0.025,1.09],
        melt_temperature =  1356,
        room_temperature = 293,
        strain_rate = 1e-6,
        specific_heat = 383.0E-8,
        tensile_failure_stress = -1.2E-2,
        spall_type = 2,
        iteration_option = 0,
        failure_parameters = [0.54,4.89,3.03,0.014,1.12],
        equation_constants =  [0.394,1.489,0,0,2.02],
        volume_correction_coefficient =0.47,
        initial_internal_energy = 0):       
        """Create material of liner
        refer to:*EOS_GRUNEISEN
                 *MAT_JOHNSON_COOK

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.mass_density=mass_density
        self.shear_modulus=shear_modulus
        self.youngs_modulus=youngs_modulus
        self.poissons_ratio = poissons_ratio
        self.constants = constants
        self.melt_temperature =  melt_temperature
        self.room_temperature = room_temperature
        self.strain_rate = strain_rate
        self.specific_heat = specific_heat
        self.tensile_failure_stress = tensile_failure_stress
        self.spall_type = spall_type
        self.iteration_option = iteration_option
        self.failure_parameters = failure_parameters
        self.equation_constants =  equation_constants
        self.volume_correction_coefficient =volume_correction_coefficient
        self.initial_internal_energy = initial_internal_energy  

    def create(self,stub):   
        ret = stub.CreateMatJohnsonCook(
            MatJohnsonCookRequest(
        ro=self.mass_density,g=self.shear_modulus,e=self.youngs_modulus,pr=self.poissons_ratio,constants=self.constants,
        tm=self.melt_temperature,tr=self.room_temperature,eps0=self.strain_rate,cp=self.specific_heat,pc=self.tensile_failure_stress,
        spall=self.spall_type,it=self.iteration_option,failure=self.failure_parameters)
        )
        self.material_id = ret.mid
        ret = stub.CreateEOSGruneisen(
            EOSGruneisenRequest(constants=self.equation_constants,
        a = self.volume_correction_coefficient,e0=self.initial_internal_energy)
        )
        self.eos_id = ret.eosid
        self.name="liner"
        logging.info(f"Material {self.name} Created...")

class HighExplosive():
    def __init__(self,mass_density = 1.835,
        detonation_velocity = 0.88,
        chapman_jouget_pressure = 0.37,
        jwl_equation_parameters = [8.261,0.1724,4.55,1.32,0.38,0.102,1.0]):          
        """Create material of high explosive
        refer to:*EOS_JWL
                 *MAT_HIGH_EXPLOSIVE_BURN
        Parameters
        ----------
        name : string
            material name.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        self.mass_density = mass_density
        self.detonation_velocity = detonation_velocity
        self.chapman_jouget_pressure = chapman_jouget_pressure
        self.jwl_equation_parameters = jwl_equation_parameters

    def create(self,stub):      
        ret = stub.CreateMatHighExplosiveBurn(
            MatHighExplosiveBurnRequest(ro=self.mass_density,d=self.detonation_velocity,pcj=self.chapman_jouget_pressure)
        )
        self.material_id = ret.mid
        ret = stub.CreateEOSJWL(
            EOSJWLRequest(jwl_equation=self.jwl_equation_parameters)
        )
        self.eos_id = ret.eosid
        self.name="HE"
        logging.info(f"Material {self.name} Created...")

class Vacuum():
    def __init__(self,estimated_material_density=1e-9):  
        self.estimated_material_density = estimated_material_density
        """Create material null.
        refer to:*MAT_VACUUM
        Parameters
        ----------
        name : string
            Material name.
        estimated_material_density : float
            Estimated material density.
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """

    def create(self,stub):
        ret = stub.CreateMatVacuum(
            MatVacuumRequest(rho=self.estimated_material_density)
        )
        self.material_id=ret.mid
        self.eos_id = 0
        self.name = "vacuum"
        logging.info(f"Material {self.name} Created...")

class MatNull():
    def __init__(self,mass_density=0,pressure_cutoff=0):  
        self.ro = mass_density
        self.pc = pressure_cutoff

    def create(self,stub):
        ret = stub.CreateMatNull(
            MatNullRequest(ro=self.ro,pc=self.pc)
        )
        self.material_id=ret.mid
        self.name = "NULL"
        logging.info(f"Material {self.name} Created...")

   
class MatRigid():
    def __init__(self,mass_density=0,young_modulus=0,poisson_ratio=0,
    center_of_mass_constraint=0,
    translational_constraint=0,
    rotational_constraint=0):  
        self.ro = mass_density
        self.e = young_modulus
        self.pr=poisson_ratio
        self.cmo = center_of_mass_constraint
        self.con1 = translational_constraint
        self.con2 = rotational_constraint

    def create(self,stub):
        ret = stub.CreateMatRigid(
            MatRigidRequest(ro=self.ro,e=self.e,pr=self.pr,cmo=self.cmo,con1=self.con1,con2=self.con2)
        )
        self.material_id=ret.mid
        self.name = "RIGID"
        logging.info(f"Material {self.name} Created...")

class MatPiecewiseLinearPlasticity():
    def __init__(self,mass_density=0,young_modulus=0,poisson_ratio=0,
    yield_stress=0,
    tangent_modulus=0):  
        self.ro = mass_density
        self.e = young_modulus
        self.pr=poisson_ratio
        self.sigy = yield_stress
        self.etan = tangent_modulus

    def create(self,stub):
        ret = stub.CreateMatPiecewiseLinearPlasticity(
            MatPiecewiseLinearPlasticityRequest(ro=self.ro,e=self.e,pr=self.pr,sigy=self.sigy,etan=self.etan)
        )
        self.material_id=ret.mid
        self.name = "Piecewise Linear Plasticity"
        logging.info(f"Material {self.name} Created...")

class MatModifiedPiecewiseLinearPlasticity():
    def __init__(self,mass_density=0,young_modulus=0,poisson_ratio=0,
    yield_stress=0,
    tangent_modulus=0,
    plastic_strain_to_failure=0,
    integration_points_number=0):  
        self.ro = mass_density
        self.e = young_modulus
        self.pr=poisson_ratio
        self.sigy = yield_stress
        self.etan = tangent_modulus
        self.fail = plastic_strain_to_failure
        self.numint = integration_points_number

    def create(self,stub):
        ret = stub.CreateMatModifiedPiecewiseLinearPlasticity(
            MatModifiedPiecewiseLinearPlasticityRequest(ro=self.ro,e=self.e,pr=self.pr,sigy=self.sigy,etan=self.etan,fail=self.fail,numint=self.numint)
        )
        self.material_id=ret.mid
        self.name = "Modified Piecewise Linear Plasticity"
        logging.info(f"Material {self.name} Created...")

class MatSpotweld():
    def __init__(self,mass_density=0,young_modulus=0,poisson_ratio=0,
    yield_stress=0,
    plastic_hardening_modulus=0):  
        self.ro = mass_density
        self.e = young_modulus
        self.pr=poisson_ratio
        self.sigy = yield_stress
        self.eh = plastic_hardening_modulus

    def create(self,stub):
        ret = stub.CreateMatSpotweld(
            MatSpotweldRequest(ro=self.ro,e=self.e,pr=self.pr,sigy=self.sigy,eh=self.eh)
        )
        self.material_id=ret.mid
        self.name = "Spotweld"
        logging.info(f"Material {self.name} Created...")

    
