





:class:`IcfdSolverTolLset`
==========================


.. py:class:: icfd_solver_tol_lset.IcfdSolverTolLset(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_SOLVER_TOL_LSET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdSolverTolLset

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~atol`
            - Get or set the Absolute convergence criteria. Convergence is achieved when Residual_(i+1)-Residual_i≤ATOL . If a negative integer is entered, then that value will be used as a load curve ID for ATOL.
          * - :py:attr:`~rtol`
            - Get or set the Relative convergence criteria. Convergence is achieved when ((Residual_(i+1)-Residual_i ))⁄Residual_initial  ≤RTOL. If a negative integer is entered, then that value will be used as a load curve ID for RTOL.
          * - :py:attr:`~maxit`
            - Get or set the Maximum number of iterations allowed to achieve convergence. If a negative integer is entered, then that value will be used as a load curve ID for MAXIT.


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

    from icfd_solver_tol_lset import IcfdSolverTolLset

Property detail
---------------

.. py:property:: atol
   :type: float


   
   Get or set the Absolute convergence criteria. Convergence is achieved when Residual_(i+1)-Residual_i≤ATOL . If a negative integer is entered, then that value will be used as a load curve ID for ATOL.
















   ..
       !! processed by numpydoc !!

.. py:property:: rtol
   :type: float


   
   Get or set the Relative convergence criteria. Convergence is achieved when ((Residual_(i+1)-Residual_i ))⁄Residual_initial  ≤RTOL. If a negative integer is entered, then that value will be used as a load curve ID for RTOL.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxit
   :type: int


   
   Get or set the Maximum number of iterations allowed to achieve convergence. If a negative integer is entered, then that value will be used as a load curve ID for MAXIT.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'SOLVER_TOL_LSET'






