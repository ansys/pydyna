Run
===

Run LS-DYNA locally
~~~~~~~~~~~~~~~~~~~

Use ``ansys.dyna.core.run`` to run LS-DYNA on a local machine.
This does not require Docker.


Prerequisites
*************

A local LS-DYNA solver installation is required. ``run_dyna`` discovers the
solver through the ``ansys-tools-common`` dependency, which looks for an
Ansys unified installation or a path saved with the ``save-ansys-path`` CLI.


Solver discovery
****************

Both platforms try an explicit executable, a saved solver path, a requested
version, and then the latest installation.

Windows
^^^^^^^

1. If ``executable`` is provided, it is used directly. The file must exist.
2. Otherwise ``get_dyna_path(find=True, allow_input=False)`` is called to
   retrieve a path previously saved with ``save-ansys-path --name dyna``.
   The saved path is used only if the file exists.
3. Otherwise, if ``version`` is provided (for example ``241`` for 2024 R1),
   the matching unified installation is located.
4. Otherwise the latest discoverable Ansys installation is used.

Unified Ansys installations ship an environment script under the
``lsprepost*/LS-Run`` directory (``lsdynaintelvar.bat`` for Intel MPI,
``lsdynamsvar.bat`` otherwise). When that script is found, the run calls it
before launching the solver. If no matching ``lsprepost`` directory is found,
the solver is launched directly and a warning is logged. If that directory
exists, the expected script must be present and usable. MPI modes additionally
need a working ``mpiexec`` command and the required libraries on ``PATH``.

Linux
^^^^^

1. If ``executable`` is provided, it is used directly. The file must exist.
2. Otherwise ``get_dyna_path(find=True, allow_input=False)`` is called to
   retrieve a path previously saved with ``save-ansys-path --name dyna``.
3. Otherwise, if ``version`` is provided, the matching unified installation
   is located.
4. Otherwise the latest discoverable Ansys installation is used.

When ``mpi_option`` is ``MPP_INTEL_MPI``, the solver is launched through
``mpirun``, which must be available on ``PATH``.


Parameters
**********

The most commonly used parameters are:

``mpi_option``
   MPI mode. One of ``MpiOption.SMP`` (default), ``MpiOption.MPP_INTEL_MPI``,
   or ``MpiOption.MPP_MS_MPI`` (Windows only).

``precision``
   ``Precision.DOUBLE`` (default) or ``Precision.SINGLE``.

``version``
   Integer identifying the Ansys release, for example ``241`` for 2024 R1.
   Used when neither an explicit executable nor a saved path is selected.
   When also omitted, the latest discoverable installation is used.

``executable``
   Full path to the LS-DYNA solver binary. Accepted on both platforms.

``ncpu``
   Number of CPU cores. Defaults to ``1``.

``memory``
   Solver memory amount. Defaults to ``20``.

``memory_unit``
   ``MemoryUnit.MB`` (default) or ``MemoryUnit.GB``.

``working_directory``
   Working directory for the solve. Defaults to the directory containing the
   input file when ``input`` is a path, or to a temporary folder under
   ``$TMP/ansys/pydyna/jobs`` when ``input`` is a ``Deck``.


Examples
********

Windows
^^^^^^^

.. code:: python

    from ansys.dyna.core.run import run_dyna

    run_dyna(
        "input.k",
        executable=r"C:\Program Files\ANSYS Inc\v241\ansys\bin\winx64\lsdyna_dp.exe",
        working_directory=r"C:\work\my-simulation",
    )

This example uses a unified installation. A standalone executable can also
be selected. Without an ``LS-Run`` environment script, the required solver
libraries must already be available in the process environment.

Linux
^^^^^

.. code:: python

    from ansys.dyna.core.run import run_dyna, MpiOption

    run_dyna(
        "input.k",
        executable="/usr/ansys_inc/v241/ansys/bin/linx64/lsdyna_dp.e",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        ncpu=4,
        working_directory="/home/user/my-simulation",
    )

The Intel MPI runtime must be available through ``mpirun`` on ``PATH``.

.. note::
    These examples require a working LS-DYNA installation. They are not
    executed automatically by the documentation build.


Saving a custom solver path
***************************

You can persist the location of a solver executable so that subsequent
calls to ``run_dyna`` do not need ``executable`` every time::

    save-ansys-path --name dyna /path/to/dyna

Both platforms pick up the saved path through
``get_dyna_path(find=True, allow_input=False)``. Windows additionally checks
that the returned file exists. Linux uses the path returned by the dependency.
When no usable path is selected, discovery continues with ``version`` or the
latest discoverable installation.


Current limitations
*******************

- On Windows, MPI modes need a working ``mpiexec`` command and the required
  libraries. An ``LS-Run`` environment script can configure these when present,
  but the runner can also launch directly with a prepared environment.
- On Windows, paths containing commas are converted to a short filename
  when one is available. If conversion fails or the result still contains
  commas, the original path is retained with a warning. Such paths can still
  fail in LS-DYNA; use a directory without commas in that case.
- Custom or non-default MPI installations are not configurable through
  ``run_dyna``.
- Docker-based runs are configured separately through the ``container``
  parameter and are outside the scope of this page.
