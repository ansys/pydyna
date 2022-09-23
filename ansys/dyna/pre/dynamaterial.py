"""
Material API
=============

Module to create material
"""

import logging
from enum import Enum

from .dynabase import *  # noqa : F403


class Air:
    """Create material of air.

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

    def __init__(
        self,
        mass_density=1.280e-03,
        pressure_cutoff=-1.0e-9,
        initial_internal_energy=2.5331e-6,
        initial_relative_volume=1.0,
        equation_coefficient=[0, 0, 0, 0, 0.4, 0.4, 0],
    ):

        self.mass_density = mass_density
        self.pressure_cutoff = pressure_cutoff
        self.initial_internal_energy = initial_internal_energy
        self.initial_relative_volume = initial_relative_volume
        self.equation_coefficient = equation_coefficient

    def create(self, stub):
        """Create air material."""
        ret = stub.CreateEOSLinearPolynomial(
            EOSLinearPolynomialRequest(
                ci=self.equation_coefficient,
                e0=self.initial_internal_energy,
                v0=self.initial_relative_volume,
            )
        )
        self.eos_id = ret.eosid
        ret = stub.CreateMatNull(MatNullRequest(ro=self.mass_density, pc=self.pressure_cutoff))
        self.material_id = ret.mid
        self.name = "air"
        logging.info(f"Material {self.name} Created...")


class Liner:
    """Define material of liner.

    Returns
    -------
    bool
        "True" when successful, "False" when failed
    """

    def __init__(
        self,
        mass_density=8.96,
        shear_modulus=0.46,
        youngs_modulus=0,
        poissons_ratio=0.34,
        constants=[90e-5, 292e-5, 0.31, 0.025, 1.09],
        melt_temperature=1356,
        room_temperature=293,
        strain_rate=1e-6,
        specific_heat=383.0e-8,
        tensile_failure_stress=-1.2e-2,
        spall_type=2,
        iteration_option=0,
        failure_parameters=[0.54, 4.89, 3.03, 0.014, 1.12],
        equation_constants=[0.394, 1.489, 0, 0, 2.02],
        volume_correction_coefficient=0.47,
        initial_internal_energy=0,
    ):

        self.mass_density = mass_density
        self.shear_modulus = shear_modulus
        self.youngs_modulus = youngs_modulus
        self.poissons_ratio = poissons_ratio
        self.constants = constants
        self.melt_temperature = melt_temperature
        self.room_temperature = room_temperature
        self.strain_rate = strain_rate
        self.specific_heat = specific_heat
        self.tensile_failure_stress = tensile_failure_stress
        self.spall_type = spall_type
        self.iteration_option = iteration_option
        self.failure_parameters = failure_parameters
        self.equation_constants = equation_constants
        self.volume_correction_coefficient = volume_correction_coefficient
        self.initial_internal_energy = initial_internal_energy

    def create(self, stub):
        """Create material of liner."""
        ret = stub.CreateMatJohnsonCook(
            MatJohnsonCookRequest(
                ro=self.mass_density,
                g=self.shear_modulus,
                e=self.youngs_modulus,
                pr=self.poissons_ratio,
                constants=self.constants,
                tm=self.melt_temperature,
                tr=self.room_temperature,
                eps0=self.strain_rate,
                cp=self.specific_heat,
                pc=self.tensile_failure_stress,
                spall=self.spall_type,
                it=self.iteration_option,
                failure=self.failure_parameters,
            )
        )
        self.material_id = ret.mid
        ret = stub.CreateEOSGruneisen(
            EOSGruneisenRequest(
                constants=self.equation_constants,
                a=self.volume_correction_coefficient,
                e0=self.initial_internal_energy,
            )
        )
        self.eos_id = ret.eosid
        self.name = "liner"
        logging.info(f"Material {self.name} Created...")


class HighExplosive:
    """Define material of high explosive.

    Parameters
    ----------
    name : string
        material name.

    Returns
    -------
    bool
        "True" when successful, "False" when failed
    """

    def __init__(
        self,
        mass_density=1.835,
        detonation_velocity=0.88,
        chapman_jouget_pressure=0.37,
        jwl_equation_parameters=[8.261, 0.1724, 4.55, 1.32, 0.38, 0.102, 1.0],
    ):
        self.mass_density = mass_density
        self.detonation_velocity = detonation_velocity
        self.chapman_jouget_pressure = chapman_jouget_pressure
        self.jwl_equation_parameters = jwl_equation_parameters

    def create(self, stub):
        """Create material of high explosive."""
        ret = stub.CreateMatHighExplosiveBurn(
            MatHighExplosiveBurnRequest(
                ro=self.mass_density,
                d=self.detonation_velocity,
                pcj=self.chapman_jouget_pressure,
            )
        )
        self.material_id = ret.mid
        ret = stub.CreateEOSJWL(EOSJWLRequest(jwl_equation=self.jwl_equation_parameters))
        self.eos_id = ret.eosid
        self.name = "HE"
        logging.info(f"Material {self.name} Created...")


class Vacuum:
    """Define material null.

    Parameters
    ----------
    estimated_material_density : float
        Estimated material density.

    Returns
    -------
    bool
        "True" when successful, "False" when failed
    """

    def __init__(self, estimated_material_density=1e-9):
        self.estimated_material_density = estimated_material_density

    def create(self, stub):
        """Create null material."""
        ret = stub.CreateMatVacuum(MatVacuumRequest(rho=self.estimated_material_density))
        self.material_id = ret.mid
        self.eos_id = 0
        self.name = "vacuum"
        logging.info(f"Material {self.name} Created...")


class MatNull:
    """Define null material,The Young's modulus and Poisson's ratio are used only for setting the contact stiffness."""

    def __init__(self, mass_density=0, pressure_cutoff=0):
        self.ro = mass_density
        self.pc = pressure_cutoff

    def create(self, stub):
        """Create null material."""
        ret = stub.CreateMatNull(MatNullRequest(ro=self.ro, pc=self.pc))
        self.material_id = ret.mid
        self.name = "NULL"
        logging.info(f"Material {self.name} Created...")


class EMMATTYPE(Enum):
    AIR_OR_VACUUM = 0
    INSULATOR = 1
    CONDUCTOR = 2


class MatAdditional:
    """Define additional properties for material."""

    def __init__(self):
        self.em = False
        self.thermal = False

    def set_electromagnetic_property(self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability."""
        self.em = True
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity

    def create(self, stub, matid):
        """Define additional properties for material."""
        if self.em:
            stub.CreateMatEM(
                MatEMRequest(
                    mid=matid,
                    mtype=self.em_material_type,
                    sigma=self.em_initial_conductivity,
                )
            )
            logging.info(f"Material EM Created...")


class MatElastic(MatAdditional):
    """Define an isotropic hypoelastic material."""

    def __init__(self, mass_density=0, young_modulus=0, poisson_ratio=0.3):
        MatAdditional.__init__(self)
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio

    def create(self, stub):
        """Create elastic material."""
        ret = stub.CreateMatElastic(MatElasticRequest(ro=self.ro, e=self.e, pr=self.pr))
        self.material_id = ret.mid
        self.name = "Elastic"
        MatAdditional.create(self, stub, self.material_id)
        logging.info(f"Material {self.name} Created...")


class MatRigid(MatAdditional):
    """Define rigid material,Parts made from this material are considered to belong to a rigid body."""

    def __init__(
        self,
        mass_density=0,
        young_modulus=0,
        poisson_ratio=0.3,
        center_of_mass_constraint=0,
        translational_constraint=0,
        rotational_constraint=0,
    ):
        MatAdditional.__init__(self)
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.cmo = center_of_mass_constraint
        self.con1 = translational_constraint
        self.con2 = rotational_constraint

    def create(self, stub):
        """Create rigid material."""
        ret = stub.CreateMatRigid(
            MatRigidRequest(
                ro=self.ro,
                e=self.e,
                pr=self.pr,
                cmo=self.cmo,
                con1=self.con1,
                con2=self.con2,
            )
        )
        self.material_id = ret.mid
        self.name = "RIGID"
        MatAdditional.create(self, stub, self.material_id)
        logging.info(f"Material {self.name} Created...")


class MatPiecewiseLinearPlasticity:
    """Define an elasto-plastic material with an arbitrary stress
    as a function of strain curve that can also have an arbitrary
    strain rate dependency."""

    def __init__(
        self,
        mass_density=0,
        young_modulus=0,
        poisson_ratio=0.3,
        yield_stress=0,
        tangent_modulus=0,
    ):
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.sigy = yield_stress
        self.etan = tangent_modulus

    def create(self, stub):
        """Create piecewise linear plasticity material."""
        ret = stub.CreateMatPiecewiseLinearPlasticity(
            MatPiecewiseLinearPlasticityRequest(ro=self.ro, e=self.e, pr=self.pr, sigy=self.sigy, etan=self.etan)
        )
        self.material_id = ret.mid
        self.name = "Piecewise Linear Plasticity"
        logging.info(f"Material {self.name} Created...")


class MatModifiedPiecewiseLinearPlasticity:
    """Define an elasto-plastic material supporting an arbitrary
    stress as a function of strain curve as well as arbitrary strain rate dependency."""

    def __init__(
        self,
        mass_density=0,
        young_modulus=0,
        poisson_ratio=0,
        yield_stress=0,
        tangent_modulus=0,
        plastic_strain_to_failure=0,
        integration_points_number=0,
    ):
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.sigy = yield_stress
        self.etan = tangent_modulus
        self.fail = plastic_strain_to_failure
        self.numint = integration_points_number

    def create(self, stub):
        """Create modified piecewise linear plasticity."""
        ret = stub.CreateMatModifiedPiecewiseLinearPlasticity(
            MatModifiedPiecewiseLinearPlasticityRequest(
                ro=self.ro,
                e=self.e,
                pr=self.pr,
                sigy=self.sigy,
                etan=self.etan,
                fail=self.fail,
                numint=self.numint,
            )
        )
        self.material_id = ret.mid
        self.name = "Modified Piecewise Linear Plasticity"
        logging.info(f"Material {self.name} Created...")


class MatSpotweld:
    """Define material for spotweld."""

    def __init__(
        self,
        mass_density=0,
        young_modulus=0,
        poisson_ratio=0,
        yield_stress=0,
        plastic_hardening_modulus=0,
        axial_force_resultant_at_failure=0,
        force_resultant_nrs_at_failure=0,
        force_resultant_nrt_at_failure=0,
    ):
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.sigy = yield_stress
        self.eh = plastic_hardening_modulus
        self.nrr = axial_force_resultant_at_failure
        self.nrs = force_resultant_nrs_at_failure
        self.nrt = force_resultant_nrt_at_failure

    def create(self, stub):
        """Create material for spotweld."""
        ret = stub.CreateMatSpotweld(
            MatSpotweldRequest(
                ro=self.ro,
                e=self.e,
                pr=self.pr,
                sigy=self.sigy,
                eh=self.eh,
                nrr=self.nrr,
                nrs=self.nrs,
                nrt=self.nrt,
            )
        )
        self.material_id = ret.mid
        self.name = "Spotweld"
        logging.info(f"Material {self.name} Created...")


class MatFabric:
    """Define material for airbag.

    Parameters
    ----------
    mass_density : float
        Mass density.
    young_modulus_longitudinal_direction : float
        Young's modulus-longitudinal direction.
    young_modulus_transverse_direction : float
        Young's modulus-transverse direction.
    minor_poisson_ratio : float
        Minor Poisson's ratio ba direction.
    major_poisson_ratio : float
        Major Poisson's ratio ab direction.
    shear_modulus : float
        shear modulus in the ab direction.
    """

    def __init__(
        self,
        mass_density=0,
        young_modulus_longitudinal_direction=0,
        young_modulus_transverse_direction=0,
        minor_poisson_ratio=0.35,
        major_poisson_ratio=0.35,
        shear_modulus=0,
    ):
        self.ro = mass_density
        self.ea = young_modulus_longitudinal_direction
        self.eb = young_modulus_transverse_direction
        self.prba = minor_poisson_ratio
        self.prab = major_poisson_ratio
        self.gab = shear_modulus

    def create(self, stub):
        """Create material for airbag."""
        ret = stub.CreateMatFabric(
            MatFabricRequest(
                ro=self.ro,
                ea=self.ea,
                eb=self.eb,
                prba=self.prba,
                prab=self.prab,
                gab=self.gab,
            )
        )
        self.material_id = ret.mid
        logging.info("Material Fabric Created...")

class MatSpringNonlinearElastic:
    """Provide a nonlinear elastic translational and rotational spring with arbitrary force as a function of displacement and moment as a function of rotation.
        
        Parameters
        ----------
        curve : Curve
            Load curve describing force as a function of displacement or moment as a function of rotation relationship.
        """
    def __init__(self,curve):
        self.curve = curve

    def create(self, stub):
        """Create material spring nonlinear elastic."""
        lcid = self.curve.create(stub)
        ret = stub.CreateMatSpringNonlinearElastic(
            MatSpringNonlinearElasticRequest(lcid=lcid)
        )
        self.material_id = ret.mid
        logging.info("Material Spring Nonlinear Elastic Created...") 

class MatDamperViscous:
    """Provide a linear translational or rotational damper located between two nodes."""
    def __init__(self,damping_constant=0):
        self.dc = damping_constant

    def create(self, stub):
        """Create material damper viscous."""
        ret = stub.CreateMatDamperViscous(MatDamperViscousRequest( dc=self.dc))
        self.material_id = ret.mid
        logging.info("Material damper viscous Created...")

class MatDamperNonlinearViscous:
    """Provide a viscous translational damper with an arbitrary force as a function of velocity dependency or a rotational damper with an arbitrary moment as a function of rotational velocity dependency.
    
    Parameters
    ----------
    curve : Curve
        Load curve defining force as a function of rate-of-displacement relationship or a moment as a function of rate-of-rotation relationship.
    """
    def __init__(self,curve):
        self.curve = curve

    def create(self, stub):
        """Create material damper nonlinear viscous."""
        lcdr = self.curve.create(stub)
        ret = stub.CreateMatDamperNonlinearViscous(
            MatDamperNonlinearViscousRequest( lcdr=lcdr)
        )
        self.material_id = ret.mid
        logging.info("Material damper viscous Created...") 
