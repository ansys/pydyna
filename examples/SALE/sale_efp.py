"""
Cylinder flow example
=====================

This example demonstrates how to create a SALE input deck.
"""

import os
import sys

from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynasale import (
    DynaSALE,
    ControlPoint,
    AdvectionMethod,
    StructuredMesh,
    FillDirection,
    Point,
)
from ansys.dyna.core.pre import dynamaterial as matDB
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/sale/efpcase.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

efp_solution = DynaSolution(hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.sale_efp + os.sep
fns.append(path + "efpcase.k")
efp_solution.open_files(fns)

# set termination
efp_solution.set_termination(280)

efp = DynaSALE()
efp_solution.add(efp)

# set post result ouput interval
efp.set_output_interval(5.0)

# set analysis type
efp.set_analysis_type(method=AdvectionMethod.VAN_LEER_WITH_HIS)

# define mesh
control_points_x = [
    ControlPoint(number=1, position=0, ratio=1),
    ControlPoint(number=11, position=-2.5, ratio=0.5),
    ControlPoint(number=21, position=0, ratio=0.5),
    ControlPoint(number=31, position=0, ratio=1),
]

control_points_y = [
    ControlPoint(number=1, position=0, ratio=1),
    ControlPoint(number=11, position=-2.5, ratio=0.5),
    ControlPoint(number=21, position=0, ratio=0.5),
    ControlPoint(number=31, position=0, ratio=1),
]

control_points_z = [
    ControlPoint(number=1, position=0, ratio=0.5),
    ControlPoint(number=269, position=11, ratio=0.25),
    ControlPoint(number=309, position=21, ratio=0.25),
    ControlPoint(number=339, position=0, ratio=5),
]

mesh = StructuredMesh(control_points_x, control_points_y, control_points_z)
efp.add(mesh)

# fill material
vacuum = matDB.Vacuum()
mesh.fill(vacuum)
air = matDB.Air()
mesh.fill(air, geometry_type="ALL", reference_pressure=1.01325e-6)
he = matDB.HighExplosive()
mesh.fill(
    he,
    geometry_type="PART",
    define_geometry_parameters=[23],
    inout=FillDirection.OUTSIDE_THE_GEOMETRY,
)
liner = matDB.Liner()
mesh.fill(
    liner,
    geometry_type="PART",
    define_geometry_parameters=[22],
    inout=FillDirection.OUTSIDE_THE_GEOMETRY,
)

# Set the initial conditions
mesh.initial_detonation(Point(0, 0, 19.33))

# set outut datebase
efp_solution.set_output_database(matsum=0.2, glstat=0.2)

# save file on server end
efp_solution.save_file()
