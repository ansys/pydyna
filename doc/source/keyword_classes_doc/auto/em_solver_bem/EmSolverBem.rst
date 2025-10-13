





:class:`EmSolverBem`
====================


.. py:class:: em_solver_bem.EmSolverBem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_SOLVER_BEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmSolverBem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance for the iterative solvers (PCG or GMRES). The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
          * - :py:attr:`~maxite`
            - Get or set the Maximal number of iterations.
          * - :py:attr:`~stype`
            - Get or set the Solver type:
          * - :py:attr:`~precon`
            - Get or set the Preconditioner type for PCG or GMRES iterative solves:
          * - :py:attr:`~uselast`
            - Get or set the This is used only for iterative solvers (PCG or GMRES).
          * - :py:attr:`~ncyclbem`
            - Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.


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

    from em_solver_bem import EmSolverBem

Property detail
---------------

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance for the iterative solvers (PCG or GMRES). The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
















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
   EQ 1: Direct solve - the matrices will then be dense.
   EQ 2: Pre-Conditioned Gradient method (PCG) - this allows to have block matrices with low rank blocks and thus to reduce the memory.
   EQ 3 : GMRES method - this allows to have block matrices with low rank blocks and thus to reduce the memory. The GMRES option only works in Serial for now.
   Note: the GMRES capability is not fully implemented yet, so we advise to use either the PCG method or the direct solve for now
















   ..
       !! processed by numpydoc !!

.. py:property:: precon
   :type: int


   
   Get or set the Preconditioner type for PCG or GMRES iterative solves:
   EQ 0: no preconditioner
   EQ 1: Diagonal line
   EQ 2: diagonal block
   EQ.3: broad diagonal including all neighbor faces
   EQ.4: LLT factorization. The LLT factorization option only works in Serial for now
















   ..
       !! processed by numpydoc !!

.. py:property:: uselast
   :type: int


   
   Get or set the This is used only for iterative solvers (PCG or GMRES).
   EQ.-1 : starts from 0 as initial solution of the linear system.
   EQ.1: starts from previous solution normalized by the rhs change. Note: using USELAST=1 can save many iterations in the further solves if the vector solution of the present solve is assumed to be nearly parallel to the vector solution of the previous solve, like it usually happens in time domain eddy-current problems
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyclbem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'SOLVER_BEM'






