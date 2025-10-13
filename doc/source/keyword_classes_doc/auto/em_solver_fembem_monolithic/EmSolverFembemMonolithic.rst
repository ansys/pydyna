





:class:`EmSolverFembemMonolithic`
=================================


.. py:class:: em_solver_fembem_monolithic.EmSolverFembemMonolithic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_SOLVER_FEMBEM_MONOLITHIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmSolverFembemMonolithic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mtype`
            - Get or set the Monolithic solver type (See Remark 1):
          * - :py:attr:`~stype`
            - Get or set the Solver type:
          * - :py:attr:`~abstol`
            - Get or set the Absolute tolerance
          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance
          * - :py:attr:`~maxit`
            - Get or set the Maximum number of iterations


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

    from em_solver_fembem_monolithic import EmSolverFembemMonolithic

Property detail
---------------

.. py:property:: mtype
   :type: int


   
   Get or set the Monolithic solver type (See Remark 1):
   EQ.0:   Direct symmetric solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Solver type:
   EQ.0: MINRES iterative solver.
   EQ.1: GMRES iterative solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: abstol
   :type: float


   
   Get or set the Absolute tolerance
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance
















   ..
       !! processed by numpydoc !!

.. py:property:: maxit
   :type: int


   
   Get or set the Maximum number of iterations
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'SOLVER_FEMBEM_MONOLITHIC'






