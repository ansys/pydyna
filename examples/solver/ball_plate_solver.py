"""
Cylinder flow example
=====================
"""

import ansys.dyna.core.solver as solver

hostname = "localhost"
port = "5000"
dyna=solver.DynaSolver(hostname,port)           # connect to the container
dyna.push("./output/ball_plate.k")                            # push an input file
dyna.start(4)                                   # start 4 ranks of mppdyna
dyna.run("i=ball_plate.k memory=10m ncycle=20000")   # begin execution
