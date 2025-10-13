





:class:`DualceseBoundarySolidWallMsurf`
=======================================


.. py:class:: dualcese_boundary_solid_wall_msurf.DualceseBoundarySolidWallMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_SOLID_WALL_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundarySolidWallMsurf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mspid`
            - Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID scales the velocity vector specified with   to give the solid wall boundary movement. If not defined, the solid wall boundary moves with a constant velocity vector specified by VX,VY,VZ
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

    from dualcese_boundary_solid_wall_msurf import DualceseBoundarySolidWallMsurf

Property detail
---------------

.. py:property:: mspid
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID scales the velocity vector specified with   to give the solid wall boundary movement. If not defined, the solid wall boundary moves with a constant velocity vector specified by VX,VY,VZ
















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
   :value: 'BOUNDARY_SOLID_WALL_MSURF'






