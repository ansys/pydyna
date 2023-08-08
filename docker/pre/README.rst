Build the Docker image for the ``pre`` service
==============================================

You must build the Docker image for the PyDYNA ``pre`` service and then
run the image as a container.

Prerequisites
-------------

* Ensure that you have cloned the PyDYNA repository locally with these commands:

  .. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna

  The ``docker`` file in the  ``docker/pre`` directory is used to build the
  Linux-based Docker image.
  

* Ensure that Docker is installed on your machine. If you do not have Docker installed,
  see the `Docker website <https://www.docker.com>`_ for more information.

* Download the latest Linux release artifacts for the Linux Docker container:
  `linux-binaries.zip <https://github.com/ansys/pydyna/releases/download/v0.3.4/linux-binaries.zip>`_.

* Move these ZIP files to the current location (``<repository-root-folder>/docker/pre``).

Starting the docker container
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are two ways to start docker container.

1.build image and run container


Build the Docker image
::::::::::::::::::::::

Once all prerequisites are met, perform these steps to build the Docker image:

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
::::::::::::::::::::::::::::

Perform these steps to run the image as a container:

#. Run this Docker command:
 
   .. code:: bash

      docker run -d -p 50051:50051 ghcr.io/ansys/ls-pre 

#. Check that the image is running successfully.

   Your output should look similar to this:

   .. code:: bash

     >>> CONTAINER ID   IMAGE                  COMMAND                  CREATED         STATUS         PORTS                                           NAMES
     >>> c77ffd67f9fa   ghcr.io/ansys/ls-pre   "python3 ./linux-bin…"   7 seconds ago   Up 7 seconds   0.0.0.0:50051->50051/tcp, :::50051->50051/tcp   hardcore_margulis
	 
	 
2.Start the container from docker-compose.yml file

	 
Make sure the docker compose have been installed on your computer.
For more information: https://docs.docker.com/compose/install/
Ensure that Docker compose is installed on your machine. If you do not have Docker compose installed,
see the `Docker website <https://docs.docker.com/compose/install/>`_ for more information.

* Locate yourself at ``<repository-root-folder>/docker/pre`` in your terminal.
* Run this Docker command:

  .. code:: bash

     docker compose up -d
