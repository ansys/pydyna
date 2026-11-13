Run
====

Use PyDYNA to run LSDYNA locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run DYNA using ``ansys.dyna.core.run`` on a local machine,
this does not require Docker.


Run LS-DYNA using ansys.dyna.core.run
*************************************

.. code:: python

    import os
    from ansys.dyna.core.run import run_dyna

    dynafile = "input.k"
    dynadir = os.getcwd()
    filepath = run_dyna(dynafile, working_directory=dynadir)
    ......

How it works
************

``run_dyna`` attempts to find an installation of the LS-DYNA solver on your machine.
It uses the Python dependency ``ansys-tools-path`` to discover where LS-DYNA is installed.
After installing ``ansys-tools-path``, the location of LS-DYNA can be saved by running
``save-ansys-path --name dyna {path/to/dyna}`` so that subsequent usages of ``run_dyna``
look there.
