





:class:`EmControlSolution`
==========================


.. py:class:: em_control_solution.EmControlSolution(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_SOLUTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlSolution

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ncylfem`
            - Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLFEM function of time.
          * - :py:attr:`~ncylbem`
            - Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLBEM function of time.
          * - :py:attr:`~autofem`
            - Get or set the In addition to NCYLFEM, this triggers an automatic recomputation of the FEM matrices based on an error calculation of the conductors' relative deformation and electrical conductivity changes.
          * - :py:attr:`~autobem`
            - Get or set the In addition to NCYLBEM, this triggers an automatic recomputation of the BEM matrices based on an error calculation of the conductors' relative displacements.
          * - :py:attr:`~tol1fem`
            - Get or set the If a conducting element sees a deformation or a conductivity change that reaches an error higher than TOL1FEM, then the FEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1FEM function of time.
          * - :py:attr:`~tol2fem`
            - Get or set the If TOL2FEM*Number-of-conducting-elements see a deformation or a conductivity change that reaches an error higher than TOL2FEM, then the FEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2FEM function of time.
          * - :py:attr:`~tol1bem`
            - Get or set the If a conducting element sees a displacement that reaches an error higher than TOL1BEM, then the BEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1BEM function of time.
          * - :py:attr:`~tol2bem`
            - Get or set the If TOL2BEM*Number-of-conducting-elements see a displacement that reaches an error higher than TOL2BEM, then the BEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2BEM function of time.


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

    from em_control_solution import EmControlSolution

Property detail
---------------

.. py:property:: ncylfem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLFEM function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncylbem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLBEM function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: autofem
   :type: int


   
   Get or set the In addition to NCYLFEM, this triggers an automatic recomputation of the FEM matrices based on an error calculation of the conductors' relative deformation and electrical conductivity changes.
   EQ.0:Autorecomputation off.
   EQ.1:Autorecomputation on.
















   ..
       !! processed by numpydoc !!

.. py:property:: autobem
   :type: int


   
   Get or set the In addition to NCYLBEM, this triggers an automatic recomputation of the BEM matrices based on an error calculation of the conductors' relative displacements.
   EQ.0:Autorecomputation off.
   EQ.1:Autorecomputation on.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol1fem
   :type: float


   
   Get or set the If a conducting element sees a deformation or a conductivity change that reaches an error higher than TOL1FEM, then the FEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1FEM function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol2fem
   :type: float


   
   Get or set the If TOL2FEM*Number-of-conducting-elements see a deformation or a conductivity change that reaches an error higher than TOL2FEM, then the FEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2FEM function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol1bem
   :type: float


   
   Get or set the If a conducting element sees a displacement that reaches an error higher than TOL1BEM, then the BEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1BEM function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol2bem
   :type: float


   
   Get or set the If TOL2BEM*Number-of-conducting-elements see a displacement that reaches an error higher than TOL2BEM, then the BEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2BEM function of time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SOLUTION'






