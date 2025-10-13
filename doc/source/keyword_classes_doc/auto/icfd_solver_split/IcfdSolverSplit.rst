





:class:`IcfdSolverSplit`
========================


.. py:class:: icfd_solver_split.IcfdSolverSplit(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_SOLVER_SPLIT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdSolverSplit

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nit`
            - Get or set the Maximum Number of iterations of the system for each fluid time step. If TOL criteria is not reached after NIT iterations, the run will proceed.
          * - :py:attr:`~tol`
            - Get or set the Tolerance Criteria for the pressure residual during the fluid system solve.


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

    from icfd_solver_split import IcfdSolverSplit

Property detail
---------------

.. py:property:: nit
   :type: int


   
   Get or set the Maximum Number of iterations of the system for each fluid time step. If TOL criteria is not reached after NIT iterations, the run will proceed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the Tolerance Criteria for the pressure residual during the fluid system solve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'SOLVER_SPLIT'






