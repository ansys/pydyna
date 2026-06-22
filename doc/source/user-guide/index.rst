User guide
----------

In the PyDYNA installation, the ``docker`` directory has one child 
directory, ``run``, which contains the necessary files to run LS-DYNA in a Docker container.
The ``run`` module provides the ability to start the LS-DYNA solver without requiring any client-server library or Docker container.

Once you have results, you can use the Ansys Data Processing Framework (DPF),
which is designed to provide numerical simulation users and engineers
with a toolbox for accessing and transforming simulation data. DPF
can access data from Ansys solver RST files and from several
files with neutral formats, including CSV, HDF5, and VTK. Using DPF's
various operators, you can manipulate and transform this data.

The `ansys-dpf-post package <https://github.com/ansys/pydpf-post>`_ provides
a simplified Python interface to DPF, thus enabling rapid postprocessing
without ever leaving a Python environment. For more information on DPF-Post,
see the `DPF-Post documentation <https://post.docs.pyansys.com>`_.
