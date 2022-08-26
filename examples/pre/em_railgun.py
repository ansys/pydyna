import os
import sys

from pydyna.dynaem import *
from pydyna.dynamaterial import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    railgun = DynaEM(hostname=hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "em" + os.sep
    fns.append(path + "em_railgun.k")
    railgun.open_files(fns)
    
    railgun.create_em_control(emsol=1)
    railgun.create_em_control_contact(emct=1)
    railgun.create_em_timestep(tstype=1,dtconst=5e-6)
    railgun.create_circuit_rogo(rogid=4,setid=4,settype=1,curtyp=1)
    abs = [0,8e-5,2e-4,4e-4,6e-4,1e-3]
    ord = [0,350,450,310,230,125]
    railgun.create_definecurve(lcid=4, sfo=2e6, abscissa=abs, ordinate=ord)
    railgun.create_circuit(circid=1,circtyp=1,lcid=4,sidcurr=4,sidvin=1,sidvout=2)

    railgun.create_em_mat001(mid=1,mtype=2,sigma=25)
    railgun.create_em_mat001(mid=2,mtype=2,sigma=25)
    railgun.create_em_mat001(mid=3,mtype=2,sigma=25)
    railgun.create_em_solver_bemmat(matid=1)
    railgun.create_em_solver_bemmat(matid=2)
    railgun.create_em_solver_bem(ncylbem=3)
    railgun.create_em_solver_fem(reltol=1e-3,stype=1,precon=1,ncylbem=3)

    railgun.create_em_output(mats=2,matf=2,sols=2,solf=2)
    railgun.create_em_database_globalenergy(outlv=1)
    railgun.create_database_binary(dt=5e-6)
    railgun.create_termination(endtim=3e-4)
    railgun.create_timestep()

    abs = [0,3e-4]
    ord = [5e-6,5e-6]
    railgun.create_definecurve(lcid=5, sfo=1, abscissa=abs, ordinate=ord)

    matelastic = MatElastic(mass_density=2.64e-3,young_modulus=9.7e+10,poisson_ratio=0.31)
    matelastic.set_electromagnetic_property(material_type=2,initial_conductivity=25)
    matrigid = MatRigid(mass_density=2.64e-3,young_modulus=9.7e+10,poisson_ratio=0.31,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    matrigid.set_electromagnetic_property(material_type=2,initial_conductivity=25)
    
    coil = SolidPart(1)
    coil.set_material(matelastic)
    coil.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)

    workpiece1 = SolidPart(2)
    workpiece1.set_material(matrigid)
    workpiece1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)

    workpiece2 = SolidPart(3)
    workpiece2.set_material(matrigid)
    workpiece2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)

    railgun.create_boundary_spc(option1="SET",birthdeath=True,nid=1,dofz=1,dofrx=1,dofry=1,death=0)
    railgun.create_boundary_spc(option1="SET",birthdeath=True,nid=2,dofz=1,dofrx=1,dofry=1,death=0)

    railgun.save_file()
