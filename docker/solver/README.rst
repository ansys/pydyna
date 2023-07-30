Build the Docker image for the ``solver`` service
=================================================

You must build the Docker image for the PyDNA ``solver`` service and then
run the image as a container.

Prerequisites
-------------

* Ensure that you have cloned the PyDYNA repository locally with these commands:

  .. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna

  The ``docker`` file in the the ``docker/solver`` directory is used to build the
  Linux-based Docker image.

* Ensure that Docker is installed on your machine. If you do not have Docker installed,
  see the `Docker website <https://www.docker.com>`_ for more information.

* If you are building the image on Windows, ensure that the Windows Subsystem for Linux (WSL)
  is installed. For installation information, see Microsoft's
  `Install Linux on Windows with WSL <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

* Download the latest Linux release artifacts for the Linux Docker container:
  `mppdyna_docker_centos7.zip <https://github.com/ansys/pydyna/releases/tag/v0.3.1/mppdyna_docker_centos7.zip>`_.

* Move these ZIP files to a a local directory ``local_image_build_dir``. 


Build the Docker image
----------------------

Once all prerequisites are met, perform these steps to build the Docker image:

#. In your terminal, go to the ``local_image_build_dir`` directory.
#. Run this Docker command:

   .. code:: bash
  
      ./do_build 

#. Check that the image has been built successfully by running this command:

   .. code:: bash

       docker images


   Your output should look similar to this:

   .. code:: bash

       >>> REPOSITORY                        TAG                                        IMAGE ID       CREATED          SIZE
       >>> dyna_solver_v04                   latest                                     defbadbeee8e   16 minutes ago   730MB
       >>> ......                                                   ......                             ............   ..............   ......


Run the image as a container
----------------------------

Perform these steps to run the image as a container:

#. In the ``docker-compose.yml`` file, replace ``<license_server_name>`` with the correct
   license server hosting the DYNA license.
   If you are using Ansy Flexlm license 
  
#. Run this Docker command:
 
   .. code:: bash

      docker compose up

#. Check that the image is running successfully.   

   .. code:: bash

       >>> CONTAINER ID   IMAGE             COMMAND                  CREATED          STATUS         PORTS                            NAMES
       >>> be84c95db31d   dyna_solver_v04   "/ansys_inc/server.p…"   18 minutes ago   Up 8 seconds   22/tcp, 0.0.0.0:5000->5000/tcp   mppdyna_docker_centos7_dyna_1
