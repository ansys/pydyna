Create your own pydyna-pre service docker container
=================================================

The pydyna-pre service Docker containers can be easily built by following
these steps.

Inside this folder, the instructions (i.e. ``Dockerfile.*`` files) for
building the pydyna-pre service Docker containers are made available. 

* ``Dockerfile``: this file builds the Linux-based Docker image.

Prerequisites
^^^^^^^^^^^^^

* Ensure that ``docker`` is installed in your machine.
  If you do not have ``docker`` available, please refer to the
  `official Docker site <https://www.docker.com>`_.

* Download the latest release artifacts for the Linux
  Docker container. You can do this as follows:

  * Latest Linux artifacts: `linux-binaries.zip <https://github.com/ansys/pydyna/releases/latest/download/linux-binaries.zip>`_

* Move these ``.zip`` files to the current location (i.e. ``<repository-root-folder>/docker``).

Building the Docker images
^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to build your images, follow the next instructions:

* Locate yourself at ``<repository-root-folder>/docker`` in your terminal.
* Run the following Docker command:

  .. code:: bash

     docker build -t ls-pre .

* Check that the image has been created successfully. You should see an output similar
  to this one when running the following command:

  .. code:: bash

     docker images

     >>> REPOSITORY                                               TAG                                IMAGE ID       CREATED          SIZE
     >>> ls-pre                                                   *******-latest                     ............   X seconds ago    187MB
     >>> ......                                                   ......                             ............   ..............   ......
