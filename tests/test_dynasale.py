import os
import sys
import pytest


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre import dynamaterial as matDB
from ansys.dyna.core.pre.dynasale import *


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


def test_sale(sale_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(sale_initialfile)
    solution.open_files(fns)
    sale = DynaSALE()
    solution.add(sale)
    sale.set_output_interval(5.0)
    sale.set_analysis_type(method=AdvectionMethod.VAN_LEER_WITH_HIS)
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
    mesh.initial_detonation(Point(0, 0, 19.33))
    sale.add(mesh)
    solution.set_output_database(matsum=0.2, glstat=0.2)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_sale.k")
    standardfile = os.path.join(resolve_standard_path, "sale.k")
    assert comparefile(outputfile, standardfile)
