Start PyDYNA Pre server locally
===============================

There are two ways to start the PyDYNA Pre server

1.Start server manually
-----------------------

* Run this command in the current folder:

  .. code:: console

   python kwserver.py

2.Start server automatically
----------------------------

Start server on Windows
~~~~~~~~~~~~~~~~~~~~~~~
   
#. Set environment variable:

   .. code:: bash
  
      Variable name:  ANSYS_PYDYNA_PRE_SERVER_PATH
      variable value: <The file path of this package>

      example of variable value: C:\pydyna\ansys-pydyna-pre-server

Start server on Linux
~~~~~~~~~~~~~~~~~~~~~

#. Set environment variable:

   .. code:: bash
  
      ANSYS_PYDYNA_PRE_SERVER_PATH=<The file path of this package>

      example of variable value: /home/lstc/ansys-pydyna-pre-server

Run an example on the client side
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
   .. code:: bash

       hostname = "localhost"
       if len(sys.argv) > 1:
           hostname = sys.argv[1]
           solution = DynaSolution(hostname)
           ......

#. The function of DynaSolution() can start the Pre server automatically.
