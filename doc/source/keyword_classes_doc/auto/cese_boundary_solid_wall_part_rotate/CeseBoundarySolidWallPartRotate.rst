





:class:`CeseBoundarySolidWallPartRotate`
========================================


.. py:class:: cese_boundary_solid_wall_part_rotate.CeseBoundarySolidWallPartRotate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_SOLID_WALL_PART_ROTATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundarySolidWallPartRotate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfprt`
            - Get or set the Surface part ID referenced in *MESH_SURFACE_ELEMENT cards.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to define this solid wall boundary movement.
          * - :py:attr:`~vx`
            - Get or set the x-,y- & z-coordinates of a point in the rotating axis.
          * - :py:attr:`~vy`
            - Get or set the x-,y- & z-coordinates of a point in the rotating axis.
          * - :py:attr:`~vz`
            - Get or set the x-,y- & z-coordinates of a point in the rotating axis.
          * - :py:attr:`~nx`
            - Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
          * - :py:attr:`~ny`
            - Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
          * - :py:attr:`~nz`
            - Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.


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

    from cese_boundary_solid_wall_part_rotate import CeseBoundarySolidWallPartRotate

Property detail
---------------

.. py:property:: surfprt
   :type: Optional[int]


   
   Get or set the Surface part ID referenced in *MESH_SURFACE_ELEMENT cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID to define this solid wall boundary movement.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the x-,y- & z-coordinates of a point in the rotating axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the x-,y- & z-coordinates of a point in the rotating axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the x-,y- & z-coordinates of a point in the rotating axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: float


   
   Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: float


   
   Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: float


   
   Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_SOLID_WALL_PART_ROTATE'






