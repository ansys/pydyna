PyDynaSolver
#############

This repository holds Python code for a simple gRPC interface
to the LS-DYNA solver.


Project Overview
----------------
As LS-DYNA is primarily a batch solver with very limited interactive
capabilities, the code here is similarly limited.  The target
use case is that LS-DYNA will be running in a container environment
such as Docker or Kubernetes.  The code here then allows for pushing
input files to the container, starting LS-DYNA and monitoring its
progress, and retrieving results files.


Installation
------------
This package is not yet available on PyPI, so for now the only real
option is for you to download the code from GitHub:

.. code::

   git clone https://github.com/pyansys/pyDynaSolver

and copy the required files

Documentation
-------------


Usage
-----
Here is a basic example:

.. code:: python

   >>> import ansys.dyna.solver as solver
   >>> dyna=solver.DynaSovler(hostname,port)           # connect to the container
   >>> dyna.push("input.k")                            # push an input file
   >>> dyna.start(4)                                   # start 4 ranks of mppdyna
   >>> dyna.run("i=input.k memory=10m ncycle=20000")   # begin execution


License
-------
Distributed under the MIT license.  See LICENSE in the root directory
of the repository for details.
