Server
=======

Run PyDYNA server locally
-------------------------
Launching the servers directly on local machines.

1. Start PyDYNA preprocessing server locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run an example on the client side
*********************************

.. code:: bash

    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
        solution = launch_dynapre(ip = hostname)
        ......

#. The function of launch_dynapre() can download and start the preprocessing server automatically.

2. Start PyDYNA solver server locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prerequisites
*************

Start server on Windows
+++++++++++++++++++++++

#. If you want to start the server on Windows,please ensure that you have installed the ANSYS locally.

Start server on Linux(Centos7)
++++++++++++++++++++++++++++++

#. If you want to start the server on Linux,please ensure that you have installed the Open MPI package.

   .. code:: bash

       yum install openmpi3 openmpi3-dev

#. set environment variable for Open MPI

   .. code:: bash

       export LD_LIBRARY_PATH=/usr/lib64/openmpi3/lib:$LD_LIBRARY_PATH
       export PATH=/usr/lib64/openmpi3/bin:$PATH


Run PyDYNA Server in a Docker container
---------------------------------------
PyDYNA server can be run in a Docker container.

.. include:: ../../../../docker/pre/README.rst
