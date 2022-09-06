PyDyna Documentation  0.0.1

.. include:: ../../README.rst

.. toctree::
   :hidden:
   :maxdepth: 3

   Resources/Getting_Started
   Resources/User_Guide
   API/index
   examples/index
   Resources/Code_Guidelines
   Resources/Contributing


Code Examples
~~~~~~~~~~~~~
Here's a quick preview for how Python code looks using the
``pyansys-DynaSolver``.  For more examples, click the links at the
top of the page to see function, method, and class documentation.


Rendered Python Code
--------------------

.. code:: python

   >>> import ansys.dyna.solver as solver
   >>> hostname="localhost"
   >>> port="5000"
   >>> dyna=solver.DynaSolver(hostname,port)
   >>> dyna.push("input.k")
   >>> dyna.start(3)
   >>> dyna.run("i=input.k")
