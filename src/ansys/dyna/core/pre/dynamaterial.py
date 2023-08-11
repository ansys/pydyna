"""
Material API
=============

Module for creating a material.
"""

from enum import Enum
import logging

from .dynabase import *  # noqa : F403


class Air:
    """Defines an air material.

    Parameters
    ----------
    name : string
        Material name.
    mass_density : float, optional
        Mass density. The default is ``0.00128``.
    pressure_cutoff : float, optional
        pressure cutoff. The default is ``1e-09``.
    initial_internal_energy : float, optional
        Initial internal energy per unit reference volume.
        The default is ``2.5331e-06``.
    initial_relative_volume : float, optional
        Initial relative volume. The default is ``1.0``.
    equation_coefficient : list, optional
        Six polynomial equation coefficient. The default is
        ``[0, 0, 0, 0, 0.4, 0.4, 0])``.

    Returns
    -------
    bool
        ``True`` when successful, ``False`` when failed.
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
        """Create an air material."""
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
    """Defines a liner material.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``8.96``.
    shear_modulus : float, optional
    youngs_modulus : float, optional
    poissons_ratio : float optional
    constants : list, optional
    melt_temperature : int, optional
    room_temperature : int, optional
    strain_rate : float, optional
    specific_heat : float, optional
    tensile_failure_stress : float, optional
    spall_type, int, optional
    iteration_option ; int, optional
    failure_parameters : list, optional
    equation_constants, list, optional
    volume_correction_coefficient : float, optional
    initial_internal_energy : float, optional
        Initial internal energy per unit reference volume.
        The default is ``0``.

    Returns
    -------
    bool
        ``True`` when successful, ``False`` when failed.
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
        """Create a liner material."""
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
    """Defines a high-explosive material.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``8.96``.
    detonation_velocity : float, optional
    chapman_jouget_pressure : float, optional
    jwl_equation_parameters : list, optional

    Returns
    -------
    bool
        ``True`` when successful, ``False`` when failed.
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
        """Create a high-explosive material."""
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
    """Defines a vacuum material.

    Parameters
    ----------
    estimated_material_density : float
        Estimated material density. The default is ``1e-09``.

    Returns
    -------
    bool
        ``True`` when successful, ``False`` when failed.
    """

    def __init__(self, estimated_material_density=1e-9):
        self.estimated_material_density = estimated_material_density

    def create(self, stub):
        """Create a null material."""
        ret = stub.CreateMatVacuum(MatVacuumRequest(rho=self.estimated_material_density))
        self.material_id = ret.mid
        self.eos_id = 0
        self.name = "vacuum"
        logging.info(f"Material {self.name} Created...")


class MatNull:
    """Defines a null material.

    Young's modulus and Poisson's ratio are used to set the contact stiffness.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``0``.
    pressure_cutoff :

    """

    def __init__(self, mass_density=0, pressure_cutoff=0):
        self.ro = mass_density
        self.pc = pressure_cutoff

    def create(self, stub):
        """Create a null material."""
        ret = stub.CreateMatNull(MatNullRequest(ro=self.ro, pc=self.pc))
        self.material_id = ret.mid
        self.name = "NULL"
        logging.info(f"Material {self.name} Created...")


class EMEOSTabulated1:
    """Defines electrical conductivity or permeability.

    Parameters
    ----------
    curve : Curve

    """

    def __init__(self, curve=None):
        self.curve = curve

    def create(self, stub):
        """Create an EM EOS tabulated1."""
        lcid = self.curve.create(stub)
        ret = stub.CreateEMEOSTabulated1(
            EMEOSTabulated1Request(
                lcid=lcid,
            )
        )
        self.id = ret.id
        self.name = "tabulated1"
        logging.info(f"EOS {self.name} Created...")
        return self.id


class EMMATTYPE(Enum):
    AIR_OR_VACUUM = 0
    INSULATOR = 1
    CONDUCTOR = 2


class MatAdditional:
    """Defines additional properties for a material."""

    def __init__(self):
        self.em = False
        self.em_mat_type = 0
        self.em_eos = None
        self.thermal_isotropic = False

    def set_electromagnetic_property(self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0, eos=None):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        material_type :
        initial_conductivity :
        eos :

        """
        self.em = True
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity
        self.em_eos = eos

    def set_em_permeability_equal(self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0, eos=None):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        material_type :
        initial_conductivity :
        eos :

        """
        self.em_mat_type = 1
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity
        self.em_eos = eos

    def set_em_permeability_different(
        self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0, eos=None, murel=0
    ):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        material_type :
        initial_conductivity :
        eos :
        murel :

        """
        self.em_mat_type = 2
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity
        self.em_eos = eos
        self.murel = murel

    def set_em_conducting_shells_3d(self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        material_type :
        initial_conductivity :

        """
        self.em_mat_type = 4
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity

    def set_em_resistive_heating_2d(self, material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=0):
        """Define the electromagnetic material type and properties
        for a material whose permeability equals the free space permeability.

        Parameters
        ----------
        material_type :
        initial_conductivity :

        """
        self.em_mat_type = 4
        self.em_material_type = material_type.value
        self.em_initial_conductivity = initial_conductivity

    def set_em_randles_batmac(
        self, positive_current_collector_conductivity=0, negative_current_collector_conductivity=0
    ):
        """Define two conductivities per EM node for special applications (Randles Batmac).

        Parameters
        ----------
        positive_current_collector_conductivity : float
            conductivities of the positive current collector materials
        negative_current_collector_conductivity : float
            conductivities of the negative current collector materials

        """
        self.em_mat_type = 6
        self.em_material_type = 5
        self.sigp = positive_current_collector_conductivity
        self.sign = negative_current_collector_conductivity

    def set_thermal_isotropic(
        self, density=0, generation_rate=0, generation_rate_multiplier=0, specific_heat=0, conductivity=0
    ):
        """Define isotropic thermal properties.

        Parameters
        ----------
        density :
        generation_rate :
        generation_rate_multiplier :
        specific_heat :
        conductivity :
        """
        self.thermal_isotropic = True
        self.tro = density
        self.tgrlc = generation_rate
        self.tgmult = generation_rate_multiplier
        self.hc = specific_heat
        self.tc = conductivity

    def create(self, stub, matid):
        """Define additional properties for a material.

        Parameters
        ----------
        matid :
            Material ID.
        """
        if self.em_mat_type:
            if self.em_eos is not None:
                eosid = self.em_eos.create(stub)
            else:
                eosid = 0
            if self.em_mat_type == 1:
                stub.CreateEMMat001(
                    EMMat001Request(
                        mid=matid, mtype=self.em_material_type, sigma=self.em_initial_conductivity, eosid=eosid
                    )
                )
            elif self.em_mat_type == 2:
                stub.CreateEMMat002(
                    EMMat002Request(
                        mid=matid,
                        mtype=self.em_material_type,
                        sigma=self.em_initial_conductivity,
                        eosid=eosid,
                        murel=self.murel,
                    )
                )
            elif self.em_mat_type == 4:
                stub.CreateEMMat004(
                    EMMat004Request(mid=matid, mtype=self.em_material_type, sigma=self.em_initial_conductivity)
                )
            elif self.em_mat_type == 6:
                stub.CreateEMMat006(
                    EMMat006Request(mid=matid, mtype=self.em_material_type, sigp=self.sigp, sign=self.sign)
                )
        if self.em:
            if self.em_eos is not None:
                eosid = self.em_eos.create(stub)
            else:
                eosid = 0
            stub.CreateMatEM(
                MatEMRequest(mid=matid, mtype=self.em_material_type, sigma=self.em_initial_conductivity, eosid=eosid)
            )
            logging.info(f"Material EM Created...")
        if self.thermal_isotropic:
            stub.CreateMatThermalIsotropic(
                MatThermalIsotropicRequest(
                    mid=matid,
                    tro=self.tro,
                    tgrlc=self.tgrlc,
                    tgmult=self.tgmult,
                    hc=self.hc,
                    tc=self.tc,
                )
            )
            logging.info(f"Material thermal isotropic Created...")


class MatElastic(MatAdditional):
    """Defines an isotropic hypoelastic material."""

    def __init__(self, mass_density=0, young_modulus=0, poisson_ratio=0.3):
        MatAdditional.__init__(self)
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio

    def create(self, stub):
        """Create an elastic material."""
        ret = stub.CreateMatElastic(MatElasticRequest(ro=self.ro, e=self.e, pr=self.pr))
        self.material_id = ret.mid
        self.name = "Elastic"
        MatAdditional.create(self, stub, self.material_id)
        logging.info(f"Material {self.name} Created...")


class MatPlasticKinematic:
    """Define material of modelling isotropic and kinematic hardening plasticity."""

    def __init__(
        self, mass_density=0, young_modulus=0, poisson_ratio=0.3, yield_stress=0, tangent_modulus=0, hardening=0
    ):
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.sigy = yield_stress
        self.etan = tangent_modulus
        self.beta = hardening

    def create(self, stub):
        """Create plastic kinematic material."""
        ret = stub.CreateMatPlasticKinematic(
            MatPlasticKinematicRequest(ro=self.ro, e=self.e, pr=self.pr, sigy=self.sigy, etan=self.etan, beta=self.beta)
        )
        self.material_id = ret.mid
        self.name = "Plastic Kinematic"
        logging.info(f"Material {self.name} Created...")


class MatElasticPlasticThermal(MatAdditional):
    """Defines temperature-dependent material coefficients."""

    def __init__(
        self,
        mass_density=0,
        temperatures=None,
        young_modulus=None,
        poisson_ratio=None,
        thermal_expansion=None,
        yield_stress=None,
    ):
        MatAdditional.__init__(self)
        self.ro = mass_density
        self.ti = temperatures
        self.ei = young_modulus
        self.pri = poisson_ratio
        self.alphai = thermal_expansion
        self.sigyi = yield_stress

    def create(self, stub):
        """Create elastic plastic thermal material."""
        ret = stub.CreateMatElasticPlasticThermal(
            MatElasticPlasticThermalRequest(
                ro=self.ro,
                ti=self.ti,
                ei=self.ei,
                pri=self.pri,
                alphai=self.alphai,
                sigyi=self.sigyi,
            )
        )
        self.material_id = ret.mid
        self.name = "ElasticPlasticThermal"
        MatAdditional.create(self, stub, self.material_id)
        logging.info(f"Material {self.name} Created...")


class MatRigid(MatAdditional):
    """Defines a rigid material.

    Parts made from a rigid material are considered to belong to a rigid body."""

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


class MatCrushableFoam(MatAdditional):
    """Define material of modelling crushable foam."""

    def __init__(
        self, mass_density=0, young_modulus=0, poisson_ratio=0.3, yield_stress_curve=None, tensile_stress_cutoff=0
    ):
        MatAdditional.__init__(self)
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio
        self.lcid = yield_stress_curve
        self.tsl = tensile_stress_cutoff

    def create(self, stub):
        """Create crushable foam material."""
        if self.lcid is not None:
            cid = self.lcid.create(stub)
        else:
            cid = 0
        ret = stub.CreateMatCrushableFoam(
            MatCrushableFoamRequest(ro=self.ro, e=self.e, pr=self.pr, lcid=cid, tsl=self.tsl)
        )
        self.material_id = ret.mid
        self.name = "Crushable Foam"
        MatAdditional.create(self, stub, self.material_id)
        logging.info(f"Material {self.name} Created...")


class MatThermalIsotropic:
    """Defines isotropic thermal properties."""

    def __init__(self, density=0, generation_rate=0, generation_rate_multiplier=0, specific_heat=0, conductivity=0):
        self.tro = density
        self.tgrlc = generation_rate
        self.tgmult = generation_rate_multiplier
        self.hc = specific_heat
        self.tc = conductivity

    def create(self, stub):
        """Create isotropic thermal material."""
        ret = stub.CreateMatThermalIsotropic(
            MatThermalIsotropicRequest(
                tro=self.tro,
                tgrlc=self.tgrlc,
                tgmult=self.tgmult,
                hc=self.hc,
                tc=self.tc,
            )
        )
        self.material_id = ret.mid
        self.name = "Isotropic thermal"
        logging.info(f"Material {self.name} Created...")


class MatThermalOrthotropic:
    """Defines orthotropic thermal properties."""

    def __init__(self, specific_heat=0, conductivity_x=0, conductivity_y=0, conductivity_z=0):
        self.hc = specific_heat
        self.k1 = conductivity_x
        self.k2 = conductivity_y
        self.k3 = conductivity_z

    def create(self, stub):
        """Create orthotropic thermal material."""
        ret = stub.CreateMatThermalOrthotropic(
            MatThermalOrthotropicRequest(
                hc=self.hc,
                k1=self.k1,
                k2=self.k2,
                k3=self.k3,
            )
        )
        self.material_id = ret.mid
        self.name = "Orthotropic thermal"
        logging.info(f"Material {self.name} Created...")


class MatRigidDiscrete:
    """Defines a rigid material for shells or solids.

    Parameters
    ----------
    mass_density :
    young_modulus :
    poisson_ratio :

    """

    def __init__(self, mass_density=0, young_modulus=0, poisson_ratio=0.3):
        self.ro = mass_density
        self.e = young_modulus
        self.pr = poisson_ratio

    def create(self, stub):
        """Create a rigid material."""
        ret = stub.CreateMatRigidDiscrete(MatRigidDiscreteRequest(ro=self.ro, e=self.e, pr=self.pr))
        self.material_id = ret.mid
        self.name = "RIGID Discrete"
        logging.info(f"Material {self.name} Created...")


class MatPiecewiseLinearPlasticity:
    """Defines an elasto-plastic material with an arbitrary stress.

    The arbitrary stress is defined as a function of strain curve
    that can also have an arbitrary strain rate dependency.

    Parameters
    ----------
    mass_density :
    young_modulus :
    poisson_ratio :
    yield_stress :
    tangent_modulus :

    """

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
        """Create a piecewise linear plasticity material."""
        ret = stub.CreateMatPiecewiseLinearPlasticity(
            MatPiecewiseLinearPlasticityRequest(ro=self.ro, e=self.e, pr=self.pr, sigy=self.sigy, etan=self.etan)
        )
        self.material_id = ret.mid
        self.name = "Piecewise Linear Plasticity"
        logging.info(f"Material {self.name} Created...")


class MatModifiedPiecewiseLinearPlasticity:
    """Defines an elasto-plastic material supporting an arbitrary stress.

    The arbitrary stress is defined as a function of strain curve and arbitrary
    strain rate dependency.

    Parameters
    ----------
    mass_density :
    young_modulus :
    poisson_ratio :
    yield_stress :
    tangent_modulus :
    plastic_strain_to_failure :
    integration_points_number :

    """

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
        """Create a modified piecewise linear plasticity."""
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
    """Defines a spotweld material.

    Parameters
    ----------
    mass_density :
    young_modulus :
    poisson_ratio :
    yield_stress :
    plastic_hardening_modulus :
    axial_force_resultant_at_failure :
    force_resultant_nrs_at_failure :
    force_resultant_nrt_at_failure :
    """

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
        """Create a spotweld material."""
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
    """Define an airbag material.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``0``.
    young_modulus_longitudinal_direction : float, optional
        Young's modulus-longitudinal direction. The default is ``0``.
    young_modulus_transverse_direction : float, optional
        Young's modulus-transverse direction. The default is ``0``.
    minor_poisson_ratio : float, optional
        Minor Poisson's ratio ba direction. The default is ``0.35``.
    major_poisson_ratio : float, optional
        Major Poisson's ratio ab direction. The default is ``0.35``.
    shear_modulus : float, optional
        Shear modulus in the ab direction. The default is ``0``.
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
        """Create an airbag material."""
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
    """Provides a nonlinear elastic translational and rotational spring with an arbitrary force.

    The arbitrary force is defined as a function of displacement. The moment is defined
    as a function of rotation.

    Parameters
    ----------
    curve : Curve
        Load curve describing force as a function of displacement or moment as a function of rotation relationship.
    """

    def __init__(self, curve):
        self.curve = curve

    def create(self, stub):
        """Create material spring nonlinear elastic."""
        lcid = self.curve.create(stub)
        ret = stub.CreateMatSpringNonlinearElastic(MatSpringNonlinearElasticRequest(lcid=lcid))
        self.material_id = ret.mid
        logging.info("Material Spring Nonlinear Elastic Created...")


class MatDamperViscous:
    """Provides a linear translational or rotational damper located between two nodes.

    Parameters
    ----------
    damping_constant :
    """

    def __init__(self, damping_constant=0):
        self.dc = damping_constant

    def create(self, stub):
        """Create a material damper viscous."""
        ret = stub.CreateMatDamperViscous(MatDamperViscousRequest(dc=self.dc))
        self.material_id = ret.mid
        logging.info("Material damper viscous Created...")


class MatDamperNonlinearViscous:
    """Provides a viscous translational damper with an arbitrary force.

    The arbitrary force is defined as a function of velocity dependency or a rotational
    damper with an arbitrary moment as a function of rotational velocity dependency.

    Parameters
    ----------
    curve : Curve
        Load curve defining force as a function of rate-of-displacement
        relationship or a moment as a function of rate-of-rotation relationship.
    """

    def __init__(self, curve):
        self.curve = curve

    def create(self, stub):
        """Create a material damper nonlinear viscous."""
        lcdr = self.curve.create(stub)
        ret = stub.CreateMatDamperNonlinearViscous(MatDamperNonlinearViscousRequest(lcdr=lcdr))
        self.material_id = ret.mid
        logging.info("Material damper viscous Created...")


class MatSPHIncompressibleFluid:
    """Defines an ISPH (incompressible smooth particle hyrodynamics) fluid material.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``0``.
    dynamic_viscosity :
    tension_coefficient1 :
    tension_coefficient2 :
    """

    def __init__(self, mass_density=0, dynamic_viscosity=0, tension_coefficient1=0, tension_coefficient2=0):
        self.ro = mass_density
        self.mu = dynamic_viscosity
        self.gamma1 = tension_coefficient1
        self.gamma2 = tension_coefficient2

    def create(self, stub):
        """Create an ISPH fluid material."""
        ret = stub.CreateMatSPHIncompressibleFluid(
            MatSPHIncompressibleFluidRequest(ro=self.ro, mu=self.mu, gamma1=self.gamma1, gamma2=self.gamma2)
        )
        self.material_id = ret.mid
        self.name = "SPH incompressible fluid"
        logging.info(f"Material {self.name} Created...")


class MatSPHIncompressibleStructure:
    """Defines an ISPH structure material.

    Parameters
    ----------
    mass_density : float, optional
        Mass density. The default is ``0``.
    adhension_coefficient :
    roughness_coefficient :
    adhesion_scaling_coefficient :
    """

    def __init__(
        self,
        mass_density=0,
        adhesion_coefficient=0,
        roughness_coefficient=0,
        adhesion_scaling_coefficient=0,
    ):
        self.ro = mass_density
        self.beta = adhesion_coefficient
        self.rough = roughness_coefficient
        self.adh = adhesion_scaling_coefficient

    def create(self, stub):
        """Create an ISPH structure material."""
        ret = stub.CreateMatSPHIncompressibleStructure(
            MatSPHIncompressibleStructureRequest(ro=self.ro, beta=self.beta, rough=self.rough, adh=self.adh)
        )
        self.material_id = ret.mid
        self.name = "SPH incompressible structure"
        logging.info(f"Material {self.name} Created...")
