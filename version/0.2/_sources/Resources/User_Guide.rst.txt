User guide
----------

There are 3 related packages here, all under the ``ansys/dyna`` directory.

PyDyna-pre provides interface to create DYNA input deck.

PyDyna-solver contains code for interfacing with the LS-DYNA solver directly.
As LS-DYNA is primarily a batch solver with very limited interactive
capabilities, the code here is similarly limited. The target
use case is that LS-DYNA is running in a container environment
such as Docker or Kubernetes. The code here then allows for pushing
input files to the container, starting LS-DYNA and monitoring its
progress, and retrieving results files.

The Data Processing Framework (DPF) is designed to provide numerical
simulation users/engineers with a toolbox for accessing and
transforming simulation data. DPF can access data from solver result
files as well as several neutral formats (``.csv``, ``.hdf5``, ``.vtk``,
etc.). Various operators are available allowing the manipulation and
the transformation of this data.

The Python `ansys-dpf-post` package provides a simplified Python
interface to DPF, thus enabling rapid postprocessing without ever
leaving a Python environment.

Visit the `DPF-Post Documentation <https://post.docs.pyansys.com>`_ for a
detailed description of the package

.. toctree::
   :hidden:
   
   pydyna_pre
   pydyna_solver
   pydyna_post
   

