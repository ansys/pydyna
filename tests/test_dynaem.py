import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaem import (
    DynaEM, 
    BEMSOLVER, 
    FEMSOLVER, 
    EMContact,
    Curve,
    Function,
    EMType,
    ThermalAnalysis,
    ThermalAnalysisType,
    SolidPart,
    SolidFormulation,
    ShellPart,
    ShellFormulation,
    NodeSet,
    PartSet,
    SegmentSet,
    Isopotential_ConnType,
    Isopotential,
    EMDimension,
    RogoCoil
) 
from ansys.dyna.core.pre.dynamaterial import MatElastic,MatThermalIsotropic,EMMATTYPE,EMEOSTabulated1,MatRigid

def comparefile(outputf, standardf):
    with open(outputf, "r") as fp1, open(standardf, "r") as fp2:
        line = fp1.readline()
        line = fp1.readline()
        while True:
            line1 = fp1.readline()
            line2 = fp2.readline()
            if line1 == "" or line2 == "":
                break
            if line1 != line2:
                return False
    return True

rogoseg = [[544,575,575,575],
           [545,544,544,544],
           [575,595,595,595],
           [595,615,615,615]]

rlc_rogoseg = [[248,252,272,268],
                [252,256,276,272],
                [256,260,280,276],
                [4,9,252,248],
                [260,264,284,280],
                [9,14,256,252],
                [268,272,292,288],
                [14,19,260,256],
                [272,276,296,292],
                [19,24,264,260],
                [276,280,300,296],
                [280,284,304,300],
                [288,292,312,308],
                [292,296,316,312],
                [296,300,320,316],
                [300,304,324,320]]

def test_em_railgun(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_railgun.k")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    em = DynaEM()
    solution.add(em)
    em.analysis.set_timestep(timestep=5e-6)
    em.analysis.set_solver_bem(solver=BEMSOLVER.PCG)
    em.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)
    contact = EMContact()
    em.contacts.add(contact)
    em.create_em_output(mats=2, matf=2, sols=2, solf=2)
    em.create_em_database_globalenergy(outlv=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_railgun.k")
    standardfile = os.path.join(resolve_standard_path,"em", "railgun.k")
    assert comparefile(outputfile, standardfile)

def test_em_resistive_heating(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_resistive_heating.k")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=20)
    solution.create_database_binary(dt=0.1)

    emobj = DynaEM()
    solution.add(emobj)

    emobj.set_timestep(timestep_size_for_mass_scaled=0.01,max_timestep=Curve(x=[0,9.9999997474e-5],y=[0.01,0.01]))

    emobj.analysis.set_timestep(timestep=0.01)
    emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)
    emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

    tanalysis = ThermalAnalysis()
    tanalysis.set_timestep(initial_timestep=0.05)
    tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
    emobj.add(tanalysis)

    matelastic1 = MatElastic(mass_density=8000, young_modulus=1e11, poisson_ratio=0.33)
    matelastic2 = MatElastic(mass_density=7000, young_modulus=1e11, poisson_ratio=0.33)
    matelastic1.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=6e7)
    matelastic2.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=4e6,eos=EMEOSTabulated1(Curve(x=[0, 25, 50, 100], y=[4e6, 4e6, 4e5, 4e5])))
    matthermaliso1 = MatThermalIsotropic(density=8000,specific_heat=400, conductivity=400)
    matthermaliso2 = MatThermalIsotropic(density=7000,specific_heat=450, conductivity=40)

    part2 = SolidPart(2)
    part2.set_material(matelastic1,matthermaliso1)
    part2.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    emobj.parts.add(part2)
    part3 = SolidPart(3)
    part3.set_material(matelastic1,matthermaliso1)
    part3.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    emobj.parts.add(part3)
    part1 = SolidPart(1)
    part1.set_material(matelastic2,matthermaliso2)
    part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    emobj.parts.add(part1)
    resistive_heating_tmp = [4507,4508]
    emobj.boundaryconditions.create_temperature(NodeSet(resistive_heating_tmp), scalefactor=50)
    emobj.set_init_temperature(temp=25)
    emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)
    solution.save_file()

    outputfile = os.path.join(resolve_server_path, "output", "test_resistive_heating.k")
    standardfile = os.path.join(resolve_standard_path,"em", "resistive_heating.k")
    assert comparefile(outputfile, standardfile)

def test_em_resistive_heating_2d(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_resistive_heating_2d.k")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=0.0101)
    solution.create_database_binary(dt=1e-4)

    emobj = DynaEM()
    solution.add(emobj)

    emobj.set_timestep(tssfac=1,timestep_size_for_mass_scaled=1e-4)

    emobj.analysis.set_timestep(timestep=1e-4)
    emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)
    emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

    tanalysis = ThermalAnalysis()
    tanalysis.set_timestep(initial_timestep=1e-4)
    tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
    emobj.add(tanalysis)

    matrigid = MatRigid(mass_density=1, young_modulus=2e11)
    matrigid.set_em_resistive_heating_2d(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

    matthermaliso = MatThermalIsotropic(density=100,specific_heat=10, conductivity=7)

    part = ShellPart(1)
    part.set_material(matrigid,matthermaliso)
    part.set_element_formulation(ShellFormulation.PLANE_STRESS)
    emobj.parts.add(part)

    emobj.boundaryconditions.create_imposed_motion(PartSet([1]), Curve(x=[0,10],y=[10,10]))
    emobj.set_init_temperature(temp=25)
    emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = Isopotential(NodeSet([521,517,513,509,525])),value=500)
    emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = Isopotential(NodeSet([585,605,625,564,565])))

    emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

    solution.save_file()

    outputfile = os.path.join(resolve_server_path, "output", "test_resistive_heating_2d.k")
    standardfile = os.path.join(resolve_standard_path,"em", "resistive_heating_2d.k")
    assert comparefile(outputfile, standardfile)

def test_em_resistive_heating_2d_isopots(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_resistive_heating_2d_isopots.k")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=0.0101)
    solution.create_database_binary(dt=1e-4)

    emobj = DynaEM()
    solution.add(emobj)

    emobj.set_timestep(tssfac=1,timestep_size_for_mass_scaled=1e-4)

    emobj.analysis.set_timestep(timestep=1e-4)
    emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING,dimtype=EMDimension.PLANAR_2D)
    emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

    tanalysis = ThermalAnalysis()
    tanalysis.set_timestep(initial_timestep=1e-4)
    tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
    emobj.add(tanalysis)

    matrigid = MatRigid(mass_density=1, young_modulus=2e11)
    matrigid.set_em_resistive_heating_2d(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

    matthermaliso = MatThermalIsotropic(density=100,specific_heat=10, conductivity=7)

    part = ShellPart(1)
    part.set_material(matrigid,matthermaliso)
    part.set_element_formulation(ShellFormulation.PLANE_STRESS)
    emobj.parts.add(part)

    emobj.boundaryconditions.create_imposed_motion(PartSet([1]), Curve(x=[0,10],y=[10,10]))
    emobj.set_init_temperature(temp=25)

    emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = Isopotential(NodeSet([521,517,513,509,525])),value=500)
    emobj.connect_isopotential(contype = Isopotential_ConnType.SHORT_CIRCUIT,isopotential1 = Isopotential(NodeSet([642,652,661,670,643])),isopotential2 = Isopotential(NodeSet([549,548,577,597,617])),value=0.01)
    emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = Isopotential(NodeSet([653,644,626,627,662])))
    emobj.add(RogoCoil(SegmentSet(rogoseg)))

    emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

    solution.save_file()

    outputfile = os.path.join(resolve_server_path, "output", "test_resistive_heating_2d_isopots.k")
    standardfile = os.path.join(resolve_standard_path,"em", "resistive_heating_2d_isopots.k")
    assert comparefile(outputfile, standardfile)

def test_em_rlc_isopotential(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_rlc_isopotential.k")
    fns = []
    fns.append(em_initialfile)
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
    emobj.connect_isopotential(contype = Isopotential_ConnType.RLC_CIRCUIT,isopotential1 = isopos_conn1,value=5e-4,inductance=7.8e-5,capacity=0.0363,initial_voltage=5000)
    emobj.connect_isopotential(contype = Isopotential_ConnType.VOLTAGE_SOURCE,isopotential1 = isopos_conn2)
    emobj.add(RogoCoil(SegmentSet(rlc_rogoseg)))

    emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

    solution.save_file()

    outputfile = os.path.join(resolve_server_path, "output", "test_rlc_isopotential.k")
    standardfile = os.path.join(resolve_standard_path,"em", "rlc_isopotential.k")
    assert comparefile(outputfile, standardfile)

def test_em_rlc_define_func(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_rlc_define_func.k")
    fns = []
    fns.append(em_initialfile)
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

    outputfile = os.path.join(resolve_server_path, "output", "test_rlc_define_func.k")
    standardfile = os.path.join(resolve_standard_path,"em", "rlc_define_func.k")
    assert comparefile(outputfile, standardfile)

def test_em_resistive_heating_2d_multi_isopots(resolve_em_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    em_initialfile = os.path.join(resolve_em_path, "test_resistive_heating_2d_multi_isopots.k")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=0.0101)
    solution.create_database_binary(dt=1e-4)

    emobj = DynaEM()
    solution.add(emobj)

    emobj.set_timestep(tssfac=1,timestep_size_for_mass_scaled=1e-4)

    emobj.analysis.set_timestep(timestep=1e-4)
    emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING,dimtype=EMDimension.PLANAR_2D)

    tanalysis = ThermalAnalysis()
    tanalysis.set_timestep(initial_timestep=1e-4)
    tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
    emobj.add(tanalysis)

    matrigid = MatRigid(mass_density=1, young_modulus=2e11)
    matrigid.set_em_resistive_heating_2d(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

    matthermaliso = MatThermalIsotropic(density=100,specific_heat=10, conductivity=7)

    part = ShellPart(1)
    part.set_material(matrigid,matthermaliso)
    part.set_element_formulation(ShellFormulation.PLANE_STRESS)
    emobj.parts.add(part)

    emobj.boundaryconditions.create_imposed_motion(PartSet([1]), Curve(x=[0,10],y=[10,10]))
    emobj.set_init_temperature(temp=25)

    crv = Curve(func="-5./0.01*EXP(-TIME/((5.e-4+0.05+0.01)*0.04))")
    nset1 = NodeSet([521,517,513,509,525])
    nset2 = NodeSet([549,548,577,597,617])  
    nset3 = NodeSet([653,644,626,627,662])    
    nset4 = NodeSet([642,652,661,670,643])       
    emobj.connect_isopotential(contype = Isopotential_ConnType.CURRENT_SOURCE,isopotential1 = Isopotential(nset4),isopotential2 = Isopotential(nset2),curve=crv)
    emobj.connect_isopotential(contype = Isopotential_ConnType.RESISTANCE,isopotential1 = Isopotential(nset4),isopotential2 = Isopotential(nset2),value=0.01)
    emobj.connect_isopotential(contype = Isopotential_ConnType.RESISTANCE,isopotential1 = Isopotential(nset3),isopotential2 = Isopotential(nset1),value=0.05)
    emobj.add(RogoCoil(SegmentSet(rogoseg)))

    emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

    solution.save_file()

    outputfile = os.path.join(resolve_server_path, "output", "test_resistive_heating_2d_multi_isopots.k")
    standardfile = os.path.join(resolve_standard_path,"em", "resistive_heating_2d_multi_isopots.k")
    assert comparefile(outputfile, standardfile)