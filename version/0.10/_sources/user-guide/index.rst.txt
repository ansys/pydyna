User guide
----------

In the PyDYNA installation, the ``docker`` directory has three child
directories:

- ``pre``: Contains the package with the ``pre`` Docker image for the
  ``pre`` service. This service provides highly abstracted APIs for creating and
  setting up DYNA input decks for DynaMech, DynaIGA, DynaICFD, DynaSALE, DynaEM,
  and DynaAirbag.
- ``solver``: Contains the package with the ``dynasolver`` Docker image
  for the ``solver`` service. This service provides highly abstracted
  APIs for interacting directly with the Ansys LS-DYNA solver. Because LS-DYNA
  is primarily a batch solver with very limited interactive capabilities, the
  ``solver`` service is similarly limited. The target use case is that LS-DYNA is
  running in a container environment such as Docker or Kubernetes. Using this
  service, you can push input files to the container, start LS-DYNA
  and monitor its progress, and then retrieve Ansys solver results (RST)
  files.
- ``run``: This module provides the ability to start the LS-DYNA solver. This does not require any
  client-server library or Docker container.

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
