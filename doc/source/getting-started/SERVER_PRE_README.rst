Start PyDYNA preprocessing server locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run an example on the client side
*********************************

   .. code:: bash

       hostname = "localhost"
       if len(sys.argv) > 1:
           hostname = sys.argv[1]
           solution = DynaSolution(hostname)
           ......

#. The function of DynaSolution() can download and start the preprocessing server automatically.
