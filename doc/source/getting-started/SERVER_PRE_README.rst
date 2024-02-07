Start PyDYNA preprocessing server locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run an example on the client side
*********************************

   .. code:: bash

       hostname = "localhost"
       if len(sys.argv) > 1:
           hostname = sys.argv[1]
           solution = launch_dynapre(ip = hostname)
           ......

#. The function of launch_dynapre() can download and start the preprocessing server automatically.
