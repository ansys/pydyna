Build the Docker image for the ``solver`` service
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You must build the Docker image for the PyDYNA ``solver`` service and then
run the image as a container.

Prerequisites
~~~~~~~~~~~~~

* Ensure that you have cloned the PyDYNA repository locally with these commands:

  .. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna

  The ``docker`` file in the  ``docker/solver`` directory is used to build the
  Linux-based Docker image.

* If you are building the image on Windows, ensure that the Windows Subsystem for Linux (WSL)
  is installed. For installation information, see Microsoft's
  `Install Linux on Windows with WSL <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

* Install ``docker`` engine. Based on the Linux distro you can use the corresponding installation
  instructions from `this page <https://docs.docker.com/engine/install/>`_.

* Download the latest Linux release artifacts for the ``solver`` Docker container:
  `mppdyna_docker_centos7.zip <https://github.com/ansys/pydyna/releases/download/v0.4.3/mppdyna_docker_centos7.zip>`_.

* Move this ZIP file to the ``docker/solver`` directory.

  The files in this folder should look similar to this:

  .. code:: bash

     >>> Dockerfile README.rst docker-compose.yml  mppdyna_docker_centos7.zip


Once all prerequisites are met, you can build the Docker image for the ``solver`` service.

Build the Docker image
~~~~~~~~~~~~~~~~~~~~~~

To build the Docker image for the ``solver`` service, perform these steps:

#. In your terminal, go to the ``pydyna/docker/solver`` directory.

#. Run this Docker command:

   .. code:: bash
  
      docker build -t dyna_solver_v04 .

#. Check that the image has been built successfully by running this command:

   .. code:: bash

       docker images


   Your output should look similar to this:

   .. code:: bash

       >>> REPOSITORY                        TAG                                        IMAGE ID       CREATED          SIZE
       >>> dyna_solver_v04                   latest                                     defbadbeee8e   16 minutes ago   730MB
       >>> ......                                                   ......                             ............   ..............   ......


Start the container from a ``docker-compose.yml`` file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternatively, you can start the container for the ``pre`` service from a
``docker-compose.yml`` file.
	 
#. Ensure that Docker Compose has been installed on your computer. If Docker Compose is not
   installed, see `Overview of installing Docker Compose <https://docs.docker.com/compose/install/>`_
   in the Docker documentation.

#. In the ``docker-compose.yml`` file, replace ``<license_server_name>`` with the correct
   license server hosting the LS-DYNA license.
  
#. In your terminal, go to the ``docker/solver`` directory and run this Docker command:
 
   .. code:: bash

      docker compose up -d

#. Check that the image is running successfully by running this command.

   .. code:: bash

       docker ps
	   
   Your output should look similar to this:  

   .. code:: bash

       >>> CONTAINER ID   IMAGE             COMMAND                  CREATED          STATUS         PORTS                            NAMES
       >>> be84c95db31d   dyna_solver_v04   "/ansys_inc/server.p…"   18 minutes ago   Up 8 seconds   22/tcp, 0.0.0.0:5000->5000/tcp   mppdyna_docker_centos7_dyna_1

Copy files from Docker
~~~~~~~~~~~~~~~~~~~~~~
To copy files back from the ``solver`` container to your host machine use the command below:

  .. code:: bash

     docker cp <containerId>:/file/path/within/container /host/target/path

The path within the container is ``/rundir``.
