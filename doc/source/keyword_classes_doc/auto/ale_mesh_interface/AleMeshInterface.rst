





:class:`AleMeshInterface`
=========================


.. py:class:: ale_mesh_interface.AleMeshInterface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_MESH_INTERFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleMeshInterface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mmgset`
            - Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
          * - :py:attr:`~nowrt`
            - Get or set the Three digit flag to deselect which file to output:
          * - :py:attr:`~volrat`
            - Get or set the Mesh volume ratio beyond which the mesh is output (see Remark 3)
          * - :py:attr:`~interp`
            - Get or set the Interpolating method :
          * - :py:attr:`~edgmin`
            - Get or set the Minimum triangle edge applied during remeshing (see Remark 2).
          * - :py:attr:`~edgmax`
            - Get or set the Maximum triangle edge applied during remeshing (see Remark 2).


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

    from ale_mesh_interface import AleMeshInterface

Property detail
---------------

.. py:property:: mmgset
   :type: Optional[int]


   
   Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
   The materials (or ALE groups) in this set are selected to be meshed.
















   ..
       !! processed by numpydoc !!

.. py:property:: nowrt
   :type: int


   
   Get or set the Three digit flag to deselect which file to output:
   EQ.__0: Write a first try of the triangular meshes for the material interfaces(see Remark 1).The mesh is output in a keyword file called alemeshmatint.k.
   EQ.__1 : Do not output alemeshmatint.k.
   EQ._0_ : Write triangular meshes of the material interfaces, after  their remeshing(see Remark 2), in a keyword file called aleremeshmatint.k.
   EQ._1_ : Do not output aleremeshmatint.k.
   EQ.0__ : Write tetrahedral meshes of the material volumes in a keyword file called alemeshmatvol.k.
   EQ.1__ : Do not output alemeshmatvol.k.
















   ..
       !! processed by numpydoc !!

.. py:property:: volrat
   :type: float


   
   Get or set the Mesh volume ratio beyond which the mesh is output (see Remark 3)
















   ..
       !! processed by numpydoc !!

.. py:property:: interp
   :type: int


   
   Get or set the Interpolating method :
   EQ.0‌:     The ALE hexahedron data are interpolated at the Lagrangian tetrahedron centers.
   EQ.1‌ : The intersection volumes between ALE hexahedra and Lagrangian tetrahedra are computed and the ALE data are mapped to the Lagrangian elements with a volume - averaged method.
















   ..
       !! processed by numpydoc !!

.. py:property:: edgmin
   :type: float


   
   Get or set the Minimum triangle edge applied during remeshing (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: edgmax
   :type: float


   
   Get or set the Maximum triangle edge applied during remeshing (see Remark 2).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'MESH_INTERFACE'






