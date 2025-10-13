





:class:`DualceseBoundaryCyclicMsurf`
====================================


.. py:class:: dualcese_boundary_cyclic_msurf.DualceseBoundaryCyclicMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_CYCLIC_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryCyclicMsurf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mspid1`
            - Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
          * - :py:attr:`~mspid2`
            - Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
          * - :py:attr:`~cyctyp`
            - Get or set the Relationship between the two cyclic boundary condition surfaces:


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

    from dualcese_boundary_cyclic_msurf import DualceseBoundaryCyclicMsurf

Property detail
---------------

.. py:property:: mspid1
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: mspid2
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: cyctyp
   :type: int


   
   Get or set the Relationship between the two cyclic boundary condition surfaces:
   EQ.0:   none assumed(default)
   EQ.1 : The first surface is rotated about an axis to match the second surface.
   EQ.2 : The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CYCLIC_MSURF'






