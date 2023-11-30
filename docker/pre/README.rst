Build the Docker image for the ``pre`` service
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You must build the Docker image for the PyDYNA ``pre`` service and then
run the image as a container.

Prerequisites
~~~~~~~~~~~~~

* Ensure that you have cloned the PyDYNA repository locally with these commands:

  .. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna

  The ``docker`` file in the  ``docker/pre`` directory is used to build the
  Linux-based Docker image.

* If you are building the image on Windows, ensure that the Windows Subsystem for Linux (WSL)
  is installed. For installation information, see Microsoft's
  `Install Linux on Windows with WSL <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

* Install ``docker`` engine. Based on the Linux distro you can use the corresponding installation
  instructions from `this page <https://docs.docker.com/engine/install/>`_.

* Download the latest Linux release artifacts for the ``pre`` Docker container:
  `linux-binaries.zip <https://github.com/ansys/pydyna/releases/download/v0.4.2/linux-binaries.zip>`_.

* Move this ZIP file to the ``docker/pre`` directory.


Once all prerequisites are met, you can build the Docker container for the ``pre`` service.

Build the Docker container for the ``pre`` service
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To build the Docker image, perform these steps:

#. In your terminal, go to the ``docker`` directory.
#. Run the following Docker command, replacing ``<DOCKERFILE_NAME>``
   with ``Dockerfile`` and ``<DOCKER_IMAGE_TAG>`` with ``latest``.

   .. code:: bash

       docker build -t ghcr.io/ansys/ls-pre:<DOCKER_IMAGE_TAG> -f <DOCKERFILE_NAME> .

#. Check that the image has been built successfully by running this command:

   .. code:: bash

       docker images


   Your output should look similar to this:

   .. code:: bash
 
       >>> REPOSITORY                                               TAG                                IMAGE ID       CREATED          SIZE
       >>> ghcr.io/ansys/ls-pre                                     *******-latest                     ............   X seconds ago    188MB
       >>> ......                                                   ......                             ............   ..............   ......


Run the image as a container
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once the Docker image of the ``pre`` service is built successfully, perform these steps to
run this image as a container:

#. Run this Docker command:
 
   .. code:: bash

      docker run -d -p 50051:50051 ghcr.io/ansys/ls-pre 

#. Check that the image is running successfully by running this command.

   .. code:: bash

       docker ps

   Your output should look similar to this:

   .. code:: bash

     >>> CONTAINER ID   IMAGE                  COMMAND                  CREATED         STATUS         PORTS                                           NAMES
     >>> c77ffd67f9fa   ghcr.io/ansys/ls-pre   "python3 ./linux-bin…"   7 seconds ago   Up 7 seconds   0.0.0.0:50051->50051/tcp, :::50051->50051/tcp   hardcore_margulis
	 
	 
Alternatively, you can start the container for the ``pre`` service from a
``docker-compose.yml`` file.
	 
#. Ensure that Docker Compose has been installed on your computer. If Docker Compose is not
   installed, see `Overview of installing Docker Compose <https://docs.docker.com/compose/install/>`_
   in the Docker documentation.

#. In your terminal, go to the ``docker/pre`` directory and run this Docker command:

  .. code:: bash

     docker compose up -d

Copy files from Docker
~~~~~~~~~~~~~~~~~~~~~~
To copy files back from the ``pre`` docker container to your host machine use the command below:

  .. code:: bash

     docker cp <containerId>:/file/path/within/container /host/target/path

The path within the container is ``/server/output``.