Build the Docker image compatible with the `run` module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This container is used by PyDYNA's `run` module.
The `run_dyna` function can run LS-DYNA in a Docker container, though it assumes that the container was built
from this Dockerfile.


Prerequisites
~~~~~~~~~~~~~

* Ensure that you have cloned the PyDYNA repository locally with these commands:

  .. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna

  The ``docker`` file in the  ``docker/run`` directory is used to build the
  Linux-based Docker image.

* If you are building the image on Windows, ensure that the Windows Subsystem for Linux (WSL)
  is installed. For installation information, see Microsoft's
  `Install Linux on Windows with WSL <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

* Install ``docker`` engine. Based on the Linux distro you can use the corresponding installation
  instructions from `this page <https://docs.docker.com/engine/install/>`_.

Build the Docker image
~~~~~~~~~~~~~~~~~~~~~~

To build the ``run`` Docker image, perform these steps:

#. In your terminal, go to the ``pydyna/docker/run`` directory.

#. Either run this Docker command (if defining licensing environment variables from Python):

   .. code:: bash

      docker build -t dyna_run .

#. Or run this Docker command (if defining licensing environment variables in the container itself):

.. code:: bash

      docker build -t dyna_run . --build-arg LSTC_LICENSE=ansys --build-arg ANSYSLI_SERVERS=*** --build-arg ANSYSLMD_LICENSE_FILE=***

#. Check that the image has been built successfully by running this command:

   .. code:: bash

       docker images


   Your output should look similar to this:

   .. code:: bash

       >>> REPOSITORY                        TAG                                        IMAGE ID       CREATED          SIZE
       >>> dyna_run                          latest                                     defbadbeee8e   16 minutes ago   12.4GB
       >>> ......                            ......                                     ............   ..............   ......


Use the container to run a dyna input deck
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This examples assumes that the LS-DYNA deck "input.k" exists in the directory "run". It will use the Docker
container to run LS-DYNA. If licensing was configured in the container, it can be used like this.

   .. code:: python

    from ansys.dyna.core.run import run_dyna, MpiOption, Precision, MemoryUnit

    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        precision=Precision.DOUBLE,
        ncpu=2,
        memory=20,
        memory_unit=MemoryUnit.MB,
        container="dyna_run:latest",
        working_directory="run"
    )

If licenses were not configured into the container, they can be passed as environment variables in the ``docker run`` command, like below:

.. code:: python

    from ansys.dyna.core.run import run_dyna, MpiOption, Precision, MemoryUnit

    run_dyna(
        "input.k",
        mpi_option=MpiOption.MPP_INTEL_MPI,
        precision=Precision.DOUBLE,
        ncpu=2,
        memory=20,
        memory_unit=MemoryUnit.MB,
        container="dyna_run_v04:latest",
        working_directory="run",
        container_env = {
            "LSTC_LICENSE": "ansys",
            "ANSYSLI_SERVERS": "***",
            "ANSYSLMD_LICENSE_FILE": "***",
        }
    )