PyDyna-solver
=============

Here is a quick preview for how Python code looks using the
``pyansys-DynaSolver``.  For more examples, click the links at the
top of the page to see function, method, and class documentation.


Rendered Python code
--------------------

.. code:: python

   >>> import ansys.dyna.core.solver as solver
   >>> hostname="localhost"
   >>> port="5000"
   >>> dyna=solver.DynaSolver(hostname,port)
   >>> dyna.push("input.k")
   >>> dyna.start(3)
   >>> dyna.run("i=input.k")


Usage
-----
Here is a basic example:

.. code:: python

   >>> import ansys.dyna.core.solver as solver
   >>> dyna=solver.DynaSovler(hostname,port)           # connect to the container
   >>> dyna.push("input.k")                            # push an input file
   >>> dyna.start(4)                                   # start 4 ranks of mppdyna
   >>> dyna.run("i=input.k memory=10m ncycle=20000")   # begin execution