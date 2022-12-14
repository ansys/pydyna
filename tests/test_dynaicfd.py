import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaicfd import (
    DynaICFD,
    MatICFD,
    ICFDPart,
    ICFDDOF,
    Curve,
    ICFDVolumePart,
    MeshedVolume,
    ICFDAnalysis,
    ICFD_SurfRemeshMethod,
    Gravity,
    GravityOption,
    Compressible,
    ICFD_AnalysisType,
    ICFD_MessageLevel

)


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
                print(line1)
                print(line2)
                return False
    return True


def test_icfd(icfd_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)
    icfd = DynaICFD()
    solution.add(icfd)
    solution.set_termination(termination_time=100)
    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep()
    icfd.add(icfdanalysis)
    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    icfd.parts.add(partvol)
    meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
    icfd.add(meshvol)
    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_icfd.k")
    standardfile = os.path.join(resolve_standard_path, "icfd.k")
    assert comparefile(outputfile, standardfile)


def test_icfd_internal_3d_flow(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_internal_3d_flow.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=10)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep(timestep=0.05)
    icfdanalysis.set_volume_mesh(mesh_growth_scale_factor=1.1)
    icfdanalysis.set_surface_mesh(remesh_method=ICFD_SurfRemeshMethod.LAPLACIAN_SMOOTHING)
    icfd.add(icfdanalysis)
    # define model
    mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
    part_inflow.set_boundary_layer_symmetry_condition()
    icfd.parts.add(part_inflow)

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
    part_outflow.set_boundary_layer_symmetry_condition()
    part_outflow.compute_flux()
    icfd.parts.add(part_outflow)

    part_wall = ICFDPart(3)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.set_boundary_layer(2)
    icfd.parts.add(part_wall)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2, 3])
    icfd.add(meshvol)

    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_internal_3d_flow.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "internal_3d_flow.k")
    assert comparefile(outputfile, standardfile)


def test_icfd_plate_flow(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_plate_flow.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)

    solution.set_termination(termination_time=100)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep()
    icfd.add(icfdanalysis)

    # define model
    mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_inflow)

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_outflow)

    part_symmetric = ICFDPart(3)
    part_symmetric.set_material(mat)
    part_symmetric.set_free_slip()
    icfd.parts.add(part_symmetric)

    part_wall = ICFDPart(4)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.compute_drag_force()
    part_wall.set_boundary_layer(number=2)
    icfd.parts.add(part_wall)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2, 3])
    meshvol.embed_shell([4])
    icfd.add(meshvol)

    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_plate_flow.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "plate_flow.k")
    assert comparefile(outputfile, standardfile)

def test_icfd_meshsize(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_mesh_size.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)

    solution.set_termination(termination_time=50)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep()
    icfd.add(icfdanalysis)

    # define model
    mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_inflow)

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_outflow)

    part_symmetric = ICFDPart(3)
    part_symmetric.set_material(mat)
    part_symmetric.set_free_slip()
    icfd.parts.add(part_symmetric)

    part_wall = ICFDPart(4)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.compute_drag_force()
    part_wall.set_boundary_layer(number=2)
    icfd.parts.add(part_wall)

    part_meshsize = ICFDPart(5)
    part_meshsize.set_material(mat)
    icfd.parts.add(part_meshsize)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
    meshvol.set_meshsize([5])
    icfd.add(meshvol)

    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_mesh_size.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "mesh_size.k")
    assert comparefile(outputfile, standardfile)

def test_icfd_thermal_flow(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_thermal_flow.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)

    solution.set_termination(termination_time=100)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep()
    icfd.add(icfdanalysis)

    # define model
    mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005,heat_capacity = 1000,thermal_conductivity=200)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
    part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
    part_inflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[20, 20]))
    icfd.parts.add(part_inflow)

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_outflow)

    part_symmetric = ICFDPart(3)
    part_symmetric.set_material(mat)
    part_symmetric.set_free_slip()
    icfd.parts.add(part_symmetric)

    part_wall = ICFDPart(4)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[80, 80]))
    part_wall.compute_drag_force()
    part_wall.set_boundary_layer(number=3)
    icfd.parts.add(part_wall)

    icfd.set_initial(temperature=10)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2, 3,4])
    icfd.add(meshvol)

    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_thermal_flow.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "thermal_flow.k")
    assert comparefile(outputfile, standardfile)

def test_icfd_free_convection_flow(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_free_convection_flow.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)

    solution.set_termination(termination_time=30)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep(0.01)
    icfd.add(icfdanalysis)

    # define model
    mat = MatICFD(flow_density=37.799999, dynamic_viscosity=1,heat_capacity = 0.7,thermal_conductivity=1.0,thermal_expansion_coefficient=1)

    part_inflow = ICFDPart(1)
    part_inflow.set_material(mat)
    part_inflow.set_non_slip()
    part_inflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[1, 1]))
    icfd.parts.add(part_inflow)

    part_outflow = ICFDPart(2)
    part_outflow.set_material(mat)
    part_outflow.set_non_slip()
    part_outflow.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[0, 0]))
    icfd.parts.add(part_outflow)

    part_symmetric = ICFDPart(3)
    part_symmetric.set_material(mat)
    part_symmetric.set_non_slip()
    part_symmetric.compute_temperature()
    icfd.parts.add(part_symmetric)

    part_wall = ICFDPart(4)
    part_wall.set_material(mat)
    part_wall.set_non_slip()
    part_wall.compute_temperature()
    icfd.parts.add(part_wall)

    icfd.set_initial()

    g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[1, 1]))
    icfd.add(g)

    partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2, 3,4])
    icfd.add(meshvol)

    solution.create_database_binary(dt=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_free_convection_flow.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "free_convection_flow.k")
    assert comparefile(outputfile, standardfile)

def test_icfd_dam_break(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_dam_break.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)
    solution.set_termination(termination_time=50)
    icfd = DynaICFD()
    solution.add(icfd)
    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_timestep()
    icfd.add(icfdanalysis)
    # define model
    mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
    mat2 = MatICFD(flag=Compressible.VACUUM)

    part1 = ICFDPart(1)
    part1.set_material(mat1)
    part1.set_free_slip()
    icfd.parts.add(part1)

    part2 = ICFDPart(2)
    part2.set_material(mat2)
    part2.set_free_slip()
    icfd.parts.add(part2)

    part3 = ICFDPart(3)
    part3.set_material(mat1)
    icfd.parts.add(part3)

    g = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[9.81, 9.81]))
    icfd.add(g)

    partvol1 = ICFDVolumePart(surfaces=[1, 3])
    partvol1.set_material(mat1)
    icfd.parts.add(partvol1)

    partvol2 = ICFDVolumePart(surfaces=[2, 3])
    partvol2.set_material(mat2)
    icfd.parts.add(partvol2)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2])
    meshvol.set_fluid_interfaces([3])
    icfd.add(meshvol)

    solution.create_database_binary(dt=0.2)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_dam_break.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "dam_break.k")
    assert comparefile(outputfile, standardfile)

def test_icfd_driven_cavity(resolve_icfd_path, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    icfd_initialfile = os.path.join(resolve_icfd_path, "test_driven_cavity.k")
    fns = []
    fns.append(icfd_initialfile)
    solution.open_files(fns)
    icfd = DynaICFD()
    solution.add(icfd)

    icfdanalysis = ICFDAnalysis()
    icfdanalysis.set_type(analysis_type=ICFD_AnalysisType.STEADY_STATE_ANALYSIS)
    icfdanalysis.set_output(messagelevel=ICFD_MessageLevel.FULL_OUTPUT_INFORMATION,iteration_interval=250)
    icfdanalysis.set_steady_state(max_iteration=2500,momentum_tol_limit=1e-8,pressure_tol_limit=1e-8,velocity_relax_param=1,pressure_relax_param=1)
    icfd.add(icfdanalysis)

    # define model
    mat = MatICFD(flow_density=1, dynamic_viscosity=0.001)

    part1 = ICFDPart(1)
    part1.set_material(mat)
    part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
    icfd.parts.add(part1)

    part2 = ICFDPart(2)
    part2.set_material(mat)
    part2.set_non_slip()
    icfd.parts.add(part2)

    partvol = ICFDVolumePart(surfaces=[1, 2])
    partvol.set_material(mat)
    icfd.parts.add(partvol)
    # define the volume space that will be meshed,The boundaries
    # of the volume are the surfaces "spids"
    meshvol = MeshedVolume(surfaces=[1, 2])
    icfd.add(meshvol)

    solution.create_database_binary(dt=250)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_driven_cavity.k")
    standardfile = os.path.join(resolve_standard_path, "icfd", "driven_cavity.k")
    assert comparefile(outputfile, standardfile)