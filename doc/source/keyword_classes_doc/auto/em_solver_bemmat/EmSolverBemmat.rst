





:class:`EmSolverBemmat`
=======================


.. py:class:: em_solver_bemmat.EmSolverBemmat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_SOLVER_BEMMAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmSolverBemmat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~matid`
            - Get or set the Defines which BEM matrix the card refers to:
          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance on the sub-blocks of the matrix when doing low rank approximations.The user should try to decrease these tolerances if the results are not accurate enough.More memory will then be needed.


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

    from em_solver_bemmat import EmSolverBemmat

Property detail
---------------

.. py:property:: matid
   :type: int


   
   Get or set the Defines which BEM matrix the card refers to:
   EQ.1: P matrix
   EQ.2: Q matrix
   EQ.3: W matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance on the sub-blocks of the matrix when doing low rank approximations.The user should try to decrease these tolerances if the results are not accurate enough.More memory will then be needed.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'SOLVER_BEMMAT'






