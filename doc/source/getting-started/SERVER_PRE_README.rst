Start pydyna pre server locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run an example on the client side
*********************************
 
   .. code:: bash

       hostname = "localhost"
       if len(sys.argv) > 1:
           hostname = sys.argv[1]
           solution = DynaSolution(hostname)
           ......

#. The function of DynaSolution() will download and start the pre server automatically.