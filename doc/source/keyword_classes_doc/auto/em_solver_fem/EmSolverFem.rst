





:class:`EmSolverFem`
====================


.. py:class:: em_solver_fem.EmSolverFem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_SOLVER_FEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmSolverFem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance for the solver. The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
          * - :py:attr:`~maxite`
            - Get or set the Maximal number of iterations.
          * - :py:attr:`~stype`
            - Get or set the Solver type:
          * - :py:attr:`~precon`
            - Get or set the Preconditioner type for PCG.
          * - :py:attr:`~uselast`
            - Get or set the This is used only for iterative solvers (PCG).
          * - :py:attr:`~ncyclfem`
            - Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.


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

    from em_solver_fem import EmSolverFem

Property detail
---------------

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance for the solver. The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxite
   :type: int


   
   Get or set the Maximal number of iterations.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Solver type:
   EQ.1: Direct solve
   EQ.2: Conditioned Gradient Method (PCG)
















   ..
       !! processed by numpydoc !!

.. py:property:: precon
   :type: int


   
   Get or set the Preconditioner type for PCG.
   EQ.0: no preconditioner
   EQ.1: Diagonal line
















   ..
       !! processed by numpydoc !!

.. py:property:: uselast
   :type: int


   
   Get or set the This is used only for iterative solvers (PCG).
   EQ.-1 : starts from 0 as initial solution of the linear system.
   EQ.1: starts from previous solution normalized by the rhs change.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyclfem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'SOLVER_FEM'






