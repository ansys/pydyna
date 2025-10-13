





:class:`CeseBoundarySolidWallMsurfSet`
======================================


.. py:class:: cese_boundary_solid_wall_msurf_set.CeseBoundarySolidWallMsurfSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_SOLID_WALL_MSURF_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundarySolidWallMsurfSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfsid`
            - Get or set the Identifier of a set of surface part IDs created with a *LSO_ID_SET card, where each surface part ID in the set is referenced in *MESH_SURFACE_ELEMENT cards.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to define this solid wall boundary movement.
          * - :py:attr:`~vx`
            - Get or set the velocity vector of the solid wall:
          * - :py:attr:`~vy`
            - Get or set the velocity vector of the solid wall:
          * - :py:attr:`~vz`
            - Get or set the velocity vector of the solid wall:


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

    from cese_boundary_solid_wall_msurf_set import CeseBoundarySolidWallMsurfSet

Property detail
---------------

.. py:property:: surfsid
   :type: Optional[int]


   
   Get or set the Identifier of a set of surface part IDs created with a *LSO_ID_SET card, where each surface part ID in the set is referenced in *MESH_SURFACE_ELEMENT cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID to define this solid wall boundary movement.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the velocity vector of the solid wall:
   LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
   LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the velocity vector of the solid wall:
   LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
   LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the velocity vector of the solid wall:
   LCID.EQ.0: it is defined by (Vx,Vy,Vz) itself.
   LCID.NE.0: it will be defined by both of the load curve and (Vx,Vy,Vz).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_SOLID_WALL_MSURF_SET'






