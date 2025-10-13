





:class:`AleMappingFromLagrangian`
=================================


.. py:class:: ale_mapping_from_lagrangian.AleMappingFromLagrangian(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_MAPPING_FROM_LAGRANGIAN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleMappingFromLagrangian

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lagpid`
            - Get or set the Part or part set ID for Lagrangian parts involved in the mapping
          * - :py:attr:`~lagpty`
            - Get or set the Type of LARGPID:
          * - :py:attr:`~nx`
            - Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
          * - :py:attr:`~ny`
            - Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
          * - :py:attr:`~npx`
            - Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
          * - :py:attr:`~npy`
            - Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
          * - :py:attr:`~npz`
            - Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
          * - :py:attr:`~aleid`
            - Get or set the Part ID of the ALE mesh.
          * - :py:attr:`~method`
            - Get or set the Method to compute volumes at the intersection of Lagrangian and ALE elements :
          * - :py:attr:`~div`
            - Get or set the Division of ALE element edges to create subcells, which volumes inside Lagrangian elements are added up by MTH=2 to approximate the intersection volumes at the intersection between ALE and Lagrangian elements


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

    from ale_mapping_from_lagrangian import AleMappingFromLagrangian

Property detail
---------------

.. py:property:: lagpid
   :type: Optional[int]


   
   Get or set the Part or part set ID for Lagrangian parts involved in the mapping
















   ..
       !! processed by numpydoc !!

.. py:property:: lagpty
   :type: int


   
   Get or set the Type of LARGPID:
   EQ.0: ID is a part set id(see * SET_PART)
   EQ.1 : ID is a part id(see * PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: Optional[int]


   
   Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: Optional[int]


   
   Get or set the Number of ALE elements in each direction of the global coordinate system. These parameters create a structured box mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: npx
   :type: Optional[int]


   
   Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: npy
   :type: Optional[int]


   
   Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: npz
   :type: Optional[int]


   
   Get or set the Number of extra elements to pad the box mesh beyond its lower and upper limits in each direction of the global coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: aleid
   :type: Optional[int]


   
   Get or set the Part ID of the ALE mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: Optional[int]


   
   Get or set the Method to compute volumes at the intersection of Lagrangian and ALE elements :
   EQ.0: Both METHOD = 1 and METHOD = 2 are applied by default.
   EQ.1 : The intersection volumes are exactly computed
   EQ.2 : The intersection volumes are evaluated with DIV.
















   ..
       !! processed by numpydoc !!

.. py:property:: div
   :type: Optional[int]


   
   Get or set the Division of ALE element edges to create subcells, which volumes inside Lagrangian elements are added up by MTH=2 to approximate the intersection volumes at the intersection between ALE and Lagrangian elements
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'MAPPING_FROM_LAGRANGIAN'






