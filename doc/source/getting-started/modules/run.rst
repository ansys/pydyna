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

The discovery order depends on the platform.

Windows
^^^^^^^

1. If ``executable`` is provided, it is used directly. The file must exist.
2. Otherwise, if ``version`` is provided (for example ``241`` for 2025 R1),
   the matching unified installation is located.
3. Otherwise the latest discoverable Ansys installation is used.

After the solver is located, the run still requires an environment script
shipped under the ``lsprepost*/LS-Run`` directory of the installation
(``lsdynaintelvar.bat`` for Intel MPI, ``lsdynamsvar.bat`` otherwise).
This means arbitrary standalone binaries are not supported on Windows.

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
   Integer identifying the Ansys release, for example ``241`` for 2025 R1.
   When omitted, the latest discoverable installation is used.

``executable``
   Full path to the LS-DYNA solver binary. Accepted on both platforms.

``ncpu``
   Number of CPUs. Defaults to ``1``.

``memory``
   Solver memory amount. Defaults to ``20``.

``memory_unit``
   ``MemoryUnit.MB`` (default), ``MemoryUnit.KB``, or ``MemoryUnit.GB``.

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

The ``executable`` path must point to a solver installed as part of an
Ansys unified installation. The run relies on the ``LS-Run`` environment
script found alongside the solver.

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

On Linux, ``run_dyna`` picks up the saved path through
``get_dyna_path(find=True, allow_input=False)``. On Windows, the saved
path is not used directly; ``version`` or ``executable`` must still
resolve to an installation that provides the ``LS-Run`` environment
script.


Current limitations
*******************

- On Windows, only installations that ship the ``lsprepost*/LS-Run``
  environment scripts are supported. Standalone binaries outside a
  unified Ansys installation may not work.
- Input paths containing commas are not handled; LS-DYNA cannot parse
  them. This affects Windows paths with certain cloud-sync locations.
- Custom MPI installations or non-default MPI runtimes are not
  configurable through ``run_dyna``.
- Docker-based runs are configured separately through the ``container``
  parameter and are outside the scope of this page.
