





:class:`EmSolverFembem`
=======================


.. py:class:: em_solver_fembem.EmSolverFembem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_SOLVER_FEMBEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmSolverFembem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance for the FEM/BEM system solve. If the results are not accurate enough, try decreasing this tolerance. A smaller tolerance will, however, require more iterations.
          * - :py:attr:`~maxite`
            - Get or set the Maximal number of iterations.
          * - :py:attr:`~forcon`
            - Get or set the Force Convergence :


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

    from em_solver_fembem import EmSolverFembem

Property detail
---------------

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance for the FEM/BEM system solve. If the results are not accurate enough, try decreasing this tolerance. A smaller tolerance will, however, require more iterations.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxite
   :type: int


   
   Get or set the Maximal number of iterations.
















   ..
       !! processed by numpydoc !!

.. py:property:: forcon
   :type: int


   
   Get or set the Force Convergence :
   EQ.0: The code stops with an error if no convergence
   EQ.1: The code continues to the next time step even if the RELTOL convergence criteria has not been reached.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'SOLVER_FEMBEM'






