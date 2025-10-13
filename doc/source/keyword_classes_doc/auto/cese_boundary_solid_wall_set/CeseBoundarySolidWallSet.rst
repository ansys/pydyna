





:class:`CeseBoundarySolidWallSet`
=================================


.. py:class:: cese_boundary_solid_wall_set.CeseBoundarySolidWallSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_SOLID_WALL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundarySolidWallSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set  ID.
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

    from cese_boundary_solid_wall_set import CeseBoundarySolidWallSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set  ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
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
   :value: 'BOUNDARY_SOLID_WALL_SET'






