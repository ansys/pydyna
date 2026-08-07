User guide
----------

In the PyDYNA installation, the ``docker`` directory has two child
directories:

- ``pre``: Provides the interface for creating DYNA input decks.
  This service includes highly abstracted APIs for setting up
  LSN-DYNA input decks. Included are DynaMech, DynaIGA, DynaICFD,
  DynaSALE, DynaEM, and DynaAirbag.
- ``solver``: Contains the code for interfacing directly with
  the Ansys LS-DYNA solver. Because LS-DYNA is primarily a batch
  solver with very limited interactive capabilities, the code in
  this directory is similarly limited. The target use case is that
  LS-DYNA is running in a container environment such as Docker or
  Kubernetes. The code in the ``solver`` directory allows you to push
  input files to the container, start LS-DYNA and monitor its progress,
  and then retrieve results (RST) files.

Once you have results, you can use the Ansys Data Processing Framework (DPF),
which is designed to provide numerical simulation users and engineers
with a toolbox for accessing and transforming simulation data. DPF
can access data from Ansys solver result files and from several
files with neutral formats, including CSV, HDF5, and VTK. Using DPF's
various operators, you can manipulate and transform this data.

The `ansys-dpf-post package <https://github.com/ansys/pydpf-post>`_ provides
a simplified Python interface to DPF, thus enabling rapid postprocessing
without ever leaving a Python environment. For more information on DPF-Post,
see the `DPF-Post documentation <https://post.docs.pyansys.com>`_.
