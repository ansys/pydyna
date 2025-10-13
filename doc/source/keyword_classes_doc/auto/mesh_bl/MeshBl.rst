





:class:`MeshBl`
===============


.. py:class:: mesh_bl.MeshBl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MESH_BL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MeshBl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part identification.
          * - :py:attr:`~nelth`
            - Get or set the Number of elements normal to the surface.
          * - :py:attr:`~blth`
            - Get or set the Boundary layer mesh thickness.
          * - :py:attr:`~blfe`
            - Get or set the Option to impose the distance between the wall and the first volume mesh node.
          * - :py:attr:`~blst`
            - Get or set the Boundary layer mesh generation strategy:


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

    from mesh_bl import MeshBl

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: nelth
   :type: Optional[int]


   
   Get or set the Number of elements normal to the surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: blth
   :type: float


   
   Get or set the Boundary layer mesh thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: blfe
   :type: float


   
   Get or set the Option to impose the distance between the wall and the first volume mesh node.
















   ..
       !! processed by numpydoc !!

.. py:property:: blst
   :type: int


   
   Get or set the Boundary layer mesh generation strategy:
   A default boundary layer mesh thickness based on the surface mesh size will be chosen.BLTH and BLFE are not needed.
   EQ.1:Constant element size using BLFE and NELTH.
   EQ.2: Repartition following a quadratic polynomial and using BLFE, NELTH and BLTH.
   EQ.3: Repartition following a growth scale factor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MESH'


.. py:attribute:: subkeyword
   :value: 'BL'






