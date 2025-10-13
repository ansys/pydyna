





:class:`AleStructuredMeshMotion`
================================


.. py:class:: ale_structured_mesh_motion.AleStructuredMeshMotion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MESH_MOTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMeshMotion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mshid`
            - Get or set the S-ALE Mesh ID.  A unique number must be specified.
          * - :py:attr:`~option`
            - Get or set the FOLLOW_GC/COVER_LAG
          * - :py:attr:`~ammgsid`
            - Get or set the The set of ALE multi-material group list IDs which the mesh follows.
          * - :py:attr:`~explim`
            - Get or set the Limit ratio for mesh expansion and contraction. The distance between the nodes is not allowed to increase by
          * - :py:attr:`~symcod`
            - Get or set the A three digit number to define symmetry. Each digit specifies one direction (local x,y,z defined in *ALE_STRUCTURED_MESH) and can be of 0,1 or 2. Code 0 means no symmetry; 1 symmetry defined at minus face; 2 plus face


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

    from ale_structured_mesh_motion import AleStructuredMeshMotion

Property detail
---------------

.. py:property:: mshid
   :type: Optional[int]


   
   Get or set the S-ALE Mesh ID.  A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the FOLLOW_GC/COVER_LAG
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgsid
   :type: int


   
   Get or set the The set of ALE multi-material group list IDs which the mesh follows.
   Please refer to *SET_MULTI-MATERIAL_GROUP_LIST card for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: explim
   :type: float


   
   Get or set the Limit ratio for mesh expansion and contraction. The distance between the nodes is not allowed to increase by
   more than a factor EXPLIM or decrease to less than a factor 1/EXPLIM.  Default value of 1.0 means no expansion/contraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: symcod
   :type: int


   
   Get or set the A three digit number to define symmetry. Each digit specifies one direction (local x,y,z defined in *ALE_STRUCTURED_MESH) and can be of 0,1 or 2. Code 0 means no symmetry; 1 symmetry defined at minus face; 2 plus face
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MESH_MOTION'






