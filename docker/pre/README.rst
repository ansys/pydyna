Create your own pydyna-pre service docker container
===================================================

The pydyna-pre service Docker containers can be easily built by following
these steps.

Inside this folder, the instructions (i.e. ``Dockerfile`` files) for
building the pydyna-pre service Docker containers are made available. 

* ``Dockerfile``: this file builds the Linux-based Docker image.

Prerequisites
^^^^^^^^^^^^^

* Ensure that ``docker`` is installed in your machine.
  If you do not have ``docker`` available, please refer to the
  `official Docker site <https://www.docker.com>`_.
  Note that the container can also be started on Windows if the Docker Desktop has been installed.
  How to install the Docker Desktop: https://docs.docker.com/desktop/install/windows-install/

* Download the latest release artifacts. You can do this as follows:

  * Latest Linux artifacts: `linux-binaries.zip <https://github.com/ansys/pydyna/releases/download/v0.2.1/linux-binaries.zip>`_

* Move these ``.zip`` files to the current location (i.e. ``<repository-root-folder>/docker/pre``).

Starting the docker container
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are two ways to start docker container.

1.bulid image and run container
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Building the Docker images
::::::::::::::::::::::::::

In order to build your images, follow the next instructions:

* Locate yourself at ``<repository-root-folder>/docker/pre`` in your terminal.
* Run the following Docker command:

  .. code:: bash

     docker build -t ghcr.io/ansys/ls-pre:<DOCKER_IMAGE_TAG> -f <DOCKERFILE_NAME> .

  Bear in mind that you will need to substitute the following entries in the previous command:

  * ``<DOCKERFILE_NAME>``: this will be ``Dockerfile``
  * ``<DOCKER_IMAGE_TAG>``: this will be ``latest`` 

* Check that the image has been created successfully. You should see an output similar
  to this one when running the following command:

  .. code:: bash

     docker images

     >>> REPOSITORY                                               TAG                                IMAGE ID       CREATED          SIZE
     >>> ghcr.io/ansys/ls-pre                                     *******-latest                     ............   X seconds ago    188MB
     >>> ......                                                   ......                             ............   ..............   ......

Run the image as a container
::::::::::::::::::::::::::::

* Run the following Docker command:
 
  .. code:: bash

     docker run -d -p 50051:50051 ghcr.io/ansys/ls-pre

* Check that the image has been created successfully.   


.. code:: bash


     >>> CONTAINER ID   IMAGE                  COMMAND                  CREATED         STATUS         PORTS                                           NAMES
     >>> c77ffd67f9fa   ghcr.io/ansys/ls-pre   "python3 ./linux-bin…"   7 seconds ago   Up 7 seconds   0.0.0.0:50051->50051/tcp, :::50051->50051/tcp   hardcore_margulis
	 
	 
2.Start the container from docker-compose.yml file
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	 
Make sure the docker compose have been installed on your computer.
For more information: https://docs.docker.com/compose/install/

* Locate yourself at ``<repository-root-folder>/docker/pre`` in your terminal.
* Run the following Docker command:

  .. code:: bash

     docker compose up -d
     