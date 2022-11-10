"""
FRF for a rectangular plate
===========================

This example shows how to setup the keywords for FRF computation.
"""

import os
import sys

sys.path.append(os.path.join(sys.path[0],'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynamaterial import *
from ansys.dyna.pre.dynanvh import *
from ansys.dyna.pre import examples


if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    solution = DynaSolution(hostname)
    fns = []
    path = examples.nvh_frf_plate_damping+ os.sep
    fns.append(path + "frf_plate_damping.k")
    solution.open_files(fns)
    
    nvhobj = DynaNVH()
    solution.add(nvhobj)

    nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
    nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
    nvhobj.implicitanalysis.set_solution(solution_method=1)

    fd = FrequencyDomain()
    fd.set_frequency_response_function(excitation_input_set=NodeSet([131]),max_natural_frequency=2000,
    modal_damping_coefficient_curve_type = 1,
    response_output_set = NodeSet([131,651]),
    response_output_dof = 3,
    response_output_type = 1,
    frf_output_min_frequency=1,
    frf_output_max_frequency=400,
    frf_output_num_frequency=400
    )
    nvhobj.add(fd)

    matelastic = MatElastic(mass_density=7870,young_modulus=2.07e11,poisson_ratio=0.292)

    boxshell = ShellPart(1)
    boxshell.set_material(matelastic)
    boxshell.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    boxshell.set_thickness(0.002)
    nvhobj.parts.add(boxshell)
    
    solution.save_file()
