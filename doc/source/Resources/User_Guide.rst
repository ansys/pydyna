User Guide
----------

There are 3 related packages here, all under the ansys/dyna/ directory.

PyDyna-pre and PyDyna-post are for code related to pre and post processing support for LS-DYNA.

PyDyna-solver contains code for interfacing with the LS-DYNA solver directly.
As LS-DYNA is primarily a batch solver with very limited interactive
capabilities, the code here is similarly limited.  The target
use case is that LS-DYNA will be running in a container environment
such as Docker or Kubernetes.  The code here then allows for pushing
input files to the container, starting LS-DYNA and monitoring its
progress, and retrieving results files.

.. toctree::
   :hidden:
   
   pydyna_pre
   pydyna_solver
   

