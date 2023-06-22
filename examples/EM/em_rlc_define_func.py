"""
Define R,L,C circuit by function
================================

This example shows a way of defining a R,L,C circuit as an inlet boundary condition using the 'Function' object. \n
LS-DYNA version : ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe
"""

import os
import sys

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    NodeSet,
    SegmentSet,
    SolidPart,
    SolidFormulation,
    EMType,
    Isopotential_ConnType,
    Isopotential,
    RogoCoil,
    Function
)
from ansys.dyna.core.pre.dynamaterial import MatRigid,EMMATTYPE
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/em/rlc_isopotential.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

solution = DynaSolution(hostname)
fns = []
path = examples.em_rlc_define_func + os.sep
fns.append(path + "em_rlc_define_func.k")
solution.open_files(fns)
solution.set_termination(termination_time=0.01)
solution.create_database_binary(dt=1e-4)

emobj = DynaEM()
solution.add(emobj)

emobj.set_timestep(tssfac=1,timestep_size_for_mass_scaled=1e-4)

emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

matrigid = MatRigid(mass_density=7000, young_modulus=2e11,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
matrigid.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

part1 = SolidPart(1)
part1.set_material(matrigid)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

nset1 = NodeSet([429,433,437,441,445,449,453,457,461,465,469,473,477,481,485,489,493,497,501,505,509,513,517,521,525])
nset2 = NodeSet([26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101,106,111,116,121,126,131,136,141,146])
isopos_conn1 = Isopotential(nset1)
isopos_conn2 = Isopotential(nset2)
fn = """float rlc(float time,float emdt,float curr,float curr1,
        float curr2,float pot1,float pot2, float rmesh)
        {
        float fac,R,C,Vc,L,xi ; 
        R = 0.5e-3; L = 78.e-6; 
        fac =1.e-6; C=363.e-4;
        float q= 181.5;
        if(time<emdt) return fac;
        q=q+emdt*curr;
        Vc=q/C;
        xi=(Vc*emdt-L*curr)/((R+rmesh)*emdt+L);
        return xi*rmesh;
        }"""
emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = isopos_conn1,func = Function(fn))
emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = isopos_conn2)

emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

solution.save_file()
