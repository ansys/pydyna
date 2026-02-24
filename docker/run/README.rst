PyDyna Run Docker Image
=======================

This directory contains Docker images for running LS-DYNA through PyDYNA's ``run`` module.

Overview
--------

The **unified Docker image** (``Dockerfile``) includes both SMP and MPP LS-DYNA executables
in a single container, providing:

- **SMP executable**: ``ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2``
- **MPP executable**: ``ls-dyna_mpp_d_R16_1_1_x64_centos79_ifort190_sse2_openmpi405_sharelib``
- **OpenMPI** support for MPP runs
- **Auto-detection**: PyDyna automatically selects the appropriate executable based on your solver options

Directory Structure
-------------------

.. code:: text

    docker/run/
    ├── Dockerfile              # Unified image (SMP + MPP) - RECOMMENDED
    ├── SMP/
    │   └── Dockerfile          # Legacy SMP-only image
    └── MPP/
        └── Dockerfile          # Legacy MPP-only image

The ``SMP/`` and ``MPP/`` subdirectories contain legacy single-executable images
for backward compatibility and are retained for reference purposes.

Prerequisites
-------------

* Ensure that you have cloned the PyDYNA repository locally:

  .. code:: console

   git clone https://github.com/ansys/pydyna.git
   cd pydyna

* If building on Windows, install Windows Subsystem for Linux (WSL).
  See `Install Linux on Windows with WSL <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

* Install Docker Engine. See `Docker installation guide <https://docs.docker.com/engine/install/>`_.

* You will need FTP credentials to download LS-DYNA executables from LSTC.

Build the Unified Docker Image (Recommended)
---------------------------------------------

The unified image contains both SMP and MPP executables, eliminating mode mismatch issues.

To build the unified Docker image:

#. Navigate to the ``pydyna/docker/run`` directory:

   .. code:: bash

      cd docker/run

#. Build the image with FTP credentials:

   .. code:: bash

      docker build \
        --build-arg FTP_USER=your_ftp_username \
        --build-arg FTP_LOGIN=your_ftp_password \
        -t pydyna-run:latest \
        -f Dockerfile \
        .

#. Verify the image was built successfully:

   .. code:: bash

      docker images pydyna-run

   Your output should look similar to:

   .. code:: console

      REPOSITORY     TAG      IMAGE ID       CREATED          SIZE
      pydyna-run     latest   defbadbeee8e   16 minutes ago   12.4GB

Build Legacy Single-Mode Images (Optional)
-------------------------------------------

For specialized use cases, you can build SMP-only or MPP-only images.

**SMP-only image:**

.. code:: bash

   cd docker/run/SMP
   docker build \
     --build-arg FTP_USER=your_ftp_username \
     --build-arg FTP_LOGIN=your_ftp_password \
     -t pydyna-run-smp:latest \
     .

**MPP-only image:**

.. code:: bash

   cd docker/run/MPP
   docker build \
     --build-arg FTP_USER=your_ftp_username \
     --build-arg FTP_LOGIN=your_ftp_password \
     -t pydyna-run-mpp:latest \
     .

Usage Examples
--------------

Auto-Detection with Unified Image
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``DockerRunner`` class automatically detects and selects the correct executable:

**Example 1: SMP execution**

.. code:: python

    from ansys.dyna.core.run import run_dyna
    from ansys.dyna.core.run.options import MpiOption, Precision, MemoryUnit

    # Auto-selects SMP executable
    run_dyna(
        "input.k",
        mpi_option=MpiOption.SMP,
        precision=Precision.DOUBLE,
        ncpu=4,
        memory=20,
        memory_unit=MemoryUnit.MB,
        container="pydyna-run:latest",
        working_directory="run"
    )

**Example 2: MPP execution**

.. code:: python

    from ansys.dyna.core.run import run_dyna
    from ansys.dyna.core.run.options import MpiOption, Precision, MemoryUnit

    # Auto-selects MPP executable
    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        precision=Precision.DOUBLE,
        ncpu=8,
        memory=20,
        memory_unit=MemoryUnit.MB,
        container="pydyna-run:latest",
        working_directory="run"
    )

License Configuration
~~~~~~~~~~~~~~~~~~~~~

**Option 1: Pass licenses via container_env (recommended)**

.. code:: python

    from ansys.dyna.core.run import run_dyna
    from ansys.dyna.core.run.options import MpiOption

    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        ncpu=2,
        container="pydyna-run:latest",
        working_directory="run",
        container_env={
            "LSTC_LICENSE": "ansys",
            "ANSYSLI_SERVERS": "2325@your_license_server",
            "ANSYSLMD_LICENSE_FILE": "1055@your_license_server",
        }
    )

**Option 2: Build licenses into the image**

.. code:: bash

   docker build \
     --build-arg FTP_USER=your_ftp_username \
     --build-arg FTP_LOGIN=your_ftp_password \
     --build-arg LSTC_LICENSE=ansys \
     --build-arg ANSYSLI_SERVERS=2325@your_license_server \
     --build-arg ANSYSLMD_LICENSE_FILE=1055@your_license_server \
     -t pydyna-run:latest \
     -f Dockerfile \
     .

Then use without ``container_env``:

.. code:: python

    from ansys.dyna.core.run import run_dyna
    from ansys.dyna.core.run.options import MpiOption

    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        ncpu=2,
        container="pydyna-run:latest",
        working_directory="run"
    )

Advanced: Using Legacy Single-Mode Images
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you built separate SMP/MPP images:

.. code:: python

    from ansys.dyna.core.run import run_dyna
    from ansys.dyna.core.run.options import MpiOption

    # Use SMP-only image
    run_dyna(
        "input.k",
        mpi_option=MpiOption.SMP,
        container="pydyna-run-smp:latest",
        working_directory="run"
    )

    # Use MPP-only image
    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        ncpu=4,
        container="pydyna-run-mpp:latest",
        working_directory="run"
    )

CI/CD Integration
-----------------

The nightly workflow ``.github/workflows/ci_cd_night.yml`` automatically builds
the unified image and pushes it to ``ghcr.io/ansys/pydyna-run:dev``.

To use the pre-built image from GitHub Container Registry:

.. code:: bash

   docker pull ghcr.io/ansys/pydyna-run:dev

Then reference it in your Python code:

.. code:: python

    run_dyna(
        "input.k",
        container="ghcr.io/ansys/pydyna-run:dev",
        working_directory="run"
    )

Troubleshooting
---------------

**Issue: "No LS-DYNA executable found in container"**

- Ensure the image was built with valid FTP credentials
- Check that the executable downloaded successfully during build
- Verify the image with: ``docker run --rm pydyna-run:latest which ls-dyna_smp_d_R16_1_1_x64_centos79_ifort190_sse2``

**Issue: "mpirun: command not found"**

- Use the unified image (``Dockerfile``) which includes OpenMPI
- Or use the MPP-specific image (``MPP/Dockerfile``)

**Issue: License errors**

- Verify license server accessibility from within the container
- Check environment variables are correctly passed via ``container_env``
- Ensure firewall rules allow license server connections