





:class:`AleStructuredMesh`
==========================


.. py:class:: ale_structured_mesh.AleStructuredMesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mshid`
            - Get or set the S-ALE Mesh ID. A unique number must be specified.
          * - :py:attr:`~dpid`
            - Get or set the Default Part ID. The elements generated will be with DPID.
          * - :py:attr:`~nbid`
            - Get or set the Nodes are generated and assigned with node IDs starting from NBID.
          * - :py:attr:`~ebid`
            - Get or set the Elements are generated and assigned with element IDs starting from EBID.
          * - :py:attr:`~tdeath`
            - Get or set the Death time for this mesh.Please see Remark 3.
          * - :py:attr:`~cpidx`
            - Get or set the Control point IDs defining node ID/value pairs along each local  axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
          * - :py:attr:`~cpidy`
            - Get or set the Control point IDs defining node ID/value pairs along each local axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
          * - :py:attr:`~cpidz`
            - Get or set the Control point IDs defining node ID/value pairs along each local  axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
          * - :py:attr:`~nid0`
            - Get or set the NID0 specifies the mesh origin node at the input phase. Later
          * - :py:attr:`~lcsid`
            - Get or set the Local coordinate system ID. Please see Remark 2.


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

    from ale_structured_mesh import AleStructuredMesh

Property detail
---------------

.. py:property:: mshid
   :type: int


   
   Get or set the S-ALE Mesh ID. A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: dpid
   :type: Optional[int]


   
   Get or set the Default Part ID. The elements generated will be with DPID.
   DPID refers to an empty part contains no material and used to
   reference the mesh only. This part definition is automatically
   generated during the input phase and contains no material and
   element formulation information. Please see remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: nbid
   :type: int


   
   Get or set the Nodes are generated and assigned with node IDs starting from NBID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ebid
   :type: int


   
   Get or set the Elements are generated and assigned with element IDs starting from EBID.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time for this mesh.Please see Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpidx
   :type: Optional[int]


   
   Get or set the Control point IDs defining node ID/value pairs along each local  axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpidy
   :type: Optional[int]


   
   Get or set the Control point IDs defining node ID/value pairs along each local axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpidz
   :type: Optional[int]


   
   Get or set the Control point IDs defining node ID/value pairs along each local  axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid0
   :type: Optional[int]


   
   Get or set the NID0 specifies the mesh origin node at the input phase. Later
   during the simulation, prescribed motion applied to this node
   gives the generated mesh the translational motion.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID. Please see Remark 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MESH'






