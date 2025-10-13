





:class:`ControlPzelectric`
==========================


.. py:class:: control_pzelectric.ControlPzelectric(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_PZELECTRIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlPzelectric

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~solver`
            - Get or set the Piezoelectric solver type:
          * - :py:attr:`~msgitr`
            - Get or set the Output iteration message level for SOLVER = 12:
          * - :py:attr:`~maxitr`
            - Get or set the Maximum number of iterations for SOLVER = 12.
          * - :py:attr:`~abstol`
            - Get or set the Absolute convergence tolerance, for SOLVER =12.
          * - :py:attr:`~reltol`
            - Get or set the Relative convergence tolerance, for SOLVER = 12.
          * - :py:attr:`~ndtrfk`
            - Get or set the Reform the dielectric stiffness matrix for every NDTRFK time steps.
          * - :py:attr:`~epzmsg`
            - Get or set the Flag to determine if electric flux and electric field at the element center of piezoelectric material is output to d3plot:


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from control_pzelectric import ControlPzelectric

Property detail
---------------

.. py:property:: solver
   :type: int


   
   Get or set the Piezoelectric solver type:
   EQ.11:  Direct solver
   EQ.12 : Diagonal scaling conjugate gradient iterative, recommended for MPP for better scalability
















   ..
       !! processed by numpydoc !!

.. py:property:: msgitr
   :type: int


   
   Get or set the Output iteration message level for SOLVER = 12:
   EQ.0:   No output(default)
   EQ.1 : Summary information
















   ..
       !! processed by numpydoc !!

.. py:property:: maxitr
   :type: int


   
   Get or set the Maximum number of iterations for SOLVER = 12.
   EQ.0:   Use default value 100.
















   ..
       !! processed by numpydoc !!

.. py:property:: abstol
   :type: float


   
   Get or set the Absolute convergence tolerance, for SOLVER =12.
   EQ.0.0: Use default value 10 - 20.
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: int


   
   Get or set the Relative convergence tolerance, for SOLVER = 12.
   EQ.0.0: Use default value 10 - 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndtrfk
   :type: int


   
   Get or set the Reform the dielectric stiffness matrix for every NDTRFK time steps.
   LT.0:   Curve |NDTRFK | defines the stiffness reformation time step as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: epzmsg
   :type: int


   
   Get or set the Flag to determine if electric flux and electric field at the element center of piezoelectric material is output to d3plot:
   EQ.0:   No electric flux or electric field output to d3plot
   EQ.1 : x, y,and z strain slots in d3plot store the electric flux along the x, y,and z directions, respectively.xy, yz,and zx strain slots in d3plot store the electric field along the x, y,and z directions, respectively
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'PZELECTRIC'






