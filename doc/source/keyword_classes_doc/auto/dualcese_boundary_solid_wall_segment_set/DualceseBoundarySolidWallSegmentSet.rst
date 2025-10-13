





:class:`DualceseBoundarySolidWallSegmentSet`
============================================


.. py:class:: dualcese_boundary_solid_wall_segment_set.DualceseBoundarySolidWallSegmentSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_SOLID_WALL_SEGMENT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundarySolidWallSegmentSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to define this solid wall boundary movement
          * - :py:attr:`~vx`
            - Get or set the Velocity vector of the solid wall boundary condition:
          * - :py:attr:`~vy`
            - Get or set the Velocity vector of the solid wall boundary condition:
          * - :py:attr:`~vz`
            - Get or set the Velocity vector of the solid wall boundary condition:


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

    from dualcese_boundary_solid_wall_segment_set import DualceseBoundarySolidWallSegmentSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to define this solid wall boundary movement
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the Velocity vector of the solid wall boundary condition:
   LCID.EQ.0:      Constant velocity vector specified with VX, VY,and VZ.
   LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the Velocity vector of the solid wall boundary condition:
   LCID.EQ.0:      Constant velocity vector specified with VX, VY,and VZ.
   LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the Velocity vector of the solid wall boundary condition:
   LCID.EQ.0:      Constant velocity vector specified with VX, VY,and VZ.
   LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_SOLID_WALL_SEGMENT_SET'






