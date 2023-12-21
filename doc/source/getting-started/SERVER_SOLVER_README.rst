Start pydyna solver server locally
==================================

Prerequisites
-------------

Start server on Windows
~~~~~~~~~~~~~~~~~~~~~~~

#. If you want to start the server on Windows,please ensure that you have installed the ANSYS locally.

Start server on Linux(Centos7)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. If you want to start the server on Linux,please ensure that you have installed the openmpi package.

   .. code:: bash

       yum install openmpi3 openmpi3-dev

#. set environment variable for openmpi

   .. code:: bash

       export LD_LIBRARY_PATH=/usr/lib64/openmpi3/lib:$LD_LIBRARY_PATH
       export PATH=/usr/lib64/openmpi3/bin:$PATH


There are two ways to start the pydyna solver server
====================================================

1.Start server manually
-----------------------

* Run this command in the current folder:

  .. code:: console

   python server.py

2.Start server automatically
----------------------------

Start server on Windows
~~~~~~~~~~~~~~~~~~~~~~~

#. Set environment variable:

   .. code:: bash
  
      Variable name:  ANSYS_PYDYNA_SOLVER_SERVER_PATH
      variable value: <The file path of this package>

      example of variable value: C:\pydyna\ansys-pydyna-solver-server

Start server on Linux(Centos7)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. Set environment variable:

   .. code:: bash
  
      ANSYS_PYDYNA_SOLVER_SERVER_PATH=<The file path of this package>

      example of variable value: /home/lstc/ansys-pydyna-solver-server

Run an example on the client side
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
   .. code:: bash

       import ansys.dyna.core.solver as solver

       hostname = "localhost"
       port = "5000"
       dyna=solver.DynaSolver(hostname,port)     # connect to the server
       dyna.push("./output/ball_plate.k")        # push an input file
       dyna.start_locally(input = "ball_plate.k",nproc=1) 

#. The function of solver.DynaSolver() will start the solver server automatically.