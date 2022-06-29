"""
This example demonstrates how to create a SALE input deck.
"""
import os
import sys

from pydyna.dynasale import *
from pydyna.dynamaterial import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    #Import the initial mesh data(nodes and elements)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep+"sale"+os.sep
    fns.append(path + "efpcase.k")
    efp = DynaSALE(hostname=hostname,filenames=fns)

    efp.set_termination(280)
    efp.set_output_interval(5.0)

    #set analysis type
    efp.set_analysis_type(method=AdvectionMethod.VAN_LEER_WITH_HIS)

    #define material  
    air = Air()
    efp.add_material(air,reference_pressure = 1.01325E-6)
    he = HighExplosive()
    efp.add_material(he)
    liner = Liner()
    efp.add_material(liner)
    vacuum = Vacuum()
    efp.add_material(vacuum)
    
    #define mesh
    control_points_x=[ControlPoint(number=1,position=0,ratio=1),
                      ControlPoint(number=11,position=-2.5,ratio=0.5),
                      ControlPoint(number=21,position=0,ratio=0.5),
                      ControlPoint(number=31,position=0,ratio=1)]

    control_points_y=[ControlPoint(number=1,position=0,ratio=1),
                      ControlPoint(number=11,position=-2.5,ratio=0.5),
                      ControlPoint(number=21,position=0,ratio=0.5),
                      ControlPoint(number=31,position=0,ratio=1)]

    
    control_points_z=[ControlPoint(number=1,position=0,ratio=0.5),
                      ControlPoint(number=269,position=11,ratio=0.25),
                      ControlPoint(number=309,position=21,ratio=0.25),
                      ControlPoint(number=339,position=0,ratio=5)]
    
    mesh = efp.create_mesh(control_points_x,control_points_y,control_points_z)
    mesh.filling(material_name = air.name,geometry_type="ALL")
    mesh.filling(material_name="HE",geometry_type="PART",define_geometry_parameters = [23],inout=FillDirection.OUTSIDE_THE_GEOMETRY)
    mesh.filling(material_name="liner",geometry_type="PART",define_geometry_parameters = [22],inout=FillDirection.OUTSIDE_THE_GEOMETRY)
    #Set the initial conditions
    mesh.initial_detonation(detonation_point=[0,0,19.33])

    efp.set_output_database(matsum=0.2,glstat=0.2)
    efp.save_file()
