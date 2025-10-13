





:class:`IcfdControlMesh`
========================


.. py:class:: icfd_control_mesh.IcfdControlMesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_MESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlMesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mgsf`
            - Get or set the Mesh Growth Scale Factor : Specifies the maximum mesh size that the volume mesher is allowed to use when generating the volume mesh based on the mesh surface element sizes defined in *MESH_SURFACE_ELEMENT. Values between 1 and 2 are allowed. Values closer to 1 will result in a finer volume mesh (1 means the volume mesh is not allowed to be coarser than the element size from the closest surface meshes) and val# ues closer to 2 will result in a coarser volume mesh (2 means the volume can use elements as much as twice as coarse as those from the closest surface mesh).
          * - :py:attr:`~mstrat`
            - Get or set the Mesh generation strategy:
          * - :py:attr:`~nrmsh`
            - Get or set the Flag to turn off any remeshing:
          * - :py:attr:`~aver`
            - Get or set the Automatic Volume Mesher version :


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

    from icfd_control_mesh import IcfdControlMesh

Property detail
---------------

.. py:property:: mgsf
   :type: float


   
   Get or set the Mesh Growth Scale Factor : Specifies the maximum mesh size that the volume mesher is allowed to use when generating the volume mesh based on the mesh surface element sizes defined in *MESH_SURFACE_ELEMENT. Values between 1 and 2 are allowed. Values closer to 1 will result in a finer volume mesh (1 means the volume mesh is not allowed to be coarser than the element size from the closest surface meshes) and val# ues closer to 2 will result in a coarser volume mesh (2 means the volume can use elements as much as twice as coarse as those from the closest surface mesh).
















   ..
       !! processed by numpydoc !!

.. py:property:: mstrat
   :type: int


   
   Get or set the Mesh generation strategy:
   EQ.0: Mesh generation based on Delaunay criteria.
   EQ.1: Mesh generation based on octree (See Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: nrmsh
   :type: int


   
   Get or set the Flag to turn off any remeshing:
   EQ.0:Remeshing possible
   EQ.1:Remeshing impossible
















   ..
       !! processed by numpydoc !!

.. py:property:: aver
   :type: int


   
   Get or set the Automatic Volume Mesher version :
   EQ.14 : Version 14.
   EQ.16 : Version 16. (See Remark 4)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_MESH'






