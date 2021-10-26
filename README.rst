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
I have as yet no idea how to set this up under PyPI, nor exactly when
it will be ready for that.  So for now the only real option is for
you to download the code from GitHub:

.. code::

   git clone https://github.com/pyansys/pyDynaSolver

and copy the required files

Documentation
-------------


Usage
-----
Here is a basic example:

.. code:: python

   >>> from DynaSolver import *
   >>> dyna=DynaSovler(hostname,port)                  # connect to the container
   >>> dyna.push("input.k")                            # push an input file
   >>> dyna.start(4)                                   # start 4 ranks of mppdyna
   >>> dyna.run("i=input.k memory=10m ncycle=20000")   # begin execution


License
-------
Distributed under the MIT license.  See LICENSE in the root directory
of the repository for details.
