Create your own pydyna-solver service docker container
::::::::::::::::::::::::::::::::::::::::::::::::::::::

The pydyna-solver service Docker containers can be easily built by following
these steps.

* ``docker/solver/Dockerfile``: this file builds the Linux-based Docker image.

Prerequisites
^^^^^^^^^^^^^

* Ensure that ``docker`` is installed in your machine.
  If you do not have ``docker`` available, please refer to the
  `official Docker site <https://www.docker.com>`_.

* If you are building the image on Windows, you will need to have 
  Windows Subsystem for Linux (WSL) installed. The instructions for that can be found `here <https://learn.microsoft.com/en-us/windows/wsl/install>`
  
* Download the latest release artifacts for the Linux
  Docker container. You can do this as follows:

  * Latest Linux artifacts: `mppdyna_docker_centos7.zip <https://github.com/ansys/pydyna/releases/download/v0.2.1/mppdyna_docker_centos7.zip>`_

* Move these ``.zip`` files to a local directory ``local_image_build_dir``.

Building the Docker images
^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to build your images, follow the next instructions:

* cd to ``local_image_build_dir``.
* Run the following Docker command:

  .. code:: bash
  
     ./do_build 

* Check that the image has been created successfully. You should see an output similar
  to this one when running the following command:

  .. code:: bash

     docker images

     >>> REPOSITORY                        TAG                                        IMAGE ID       CREATED          SIZE
     >>> dyna_solver_v04                   latest                                     defbadbeee8e   16 minutes ago   730MB
     >>> ......                                                   ......                             ............   ..............   ......

Run the image as a container
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Edit the docker-compose.yml file and replace ``<license_server_name>`` with the correct license server hosting the DYNA license.
  If you are using Ansy Flexlm license 
  
* Run the following Docker command:
 
  .. code:: bash

     docker-compose up

* Check that the image has been created successfully.   


.. code:: bash

     >>> CONTAINER ID   IMAGE             COMMAND                  CREATED          STATUS         PORTS                            NAMES
     >>> be84c95db31d   dyna_solver_v04   "/ansys_inc/server.p…"   18 minutes ago   Up 8 seconds   22/tcp, 0.0.0.0:5000->5000/tcp   mppdyna_docker_centos7_dyna_1
