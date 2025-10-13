





:class:`DefineDeActiveRegion`
=============================


.. py:class:: define_de_active_region.DefineDeActiveRegion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_ACTIVE_REGION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeActiveRegion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Set ID/Box ID/Node ID
          * - :py:attr:`~type`
            - Get or set the EQ.0: Part set ID
          * - :py:attr:`~xm_r`
            - Get or set the For TYPE = 0 or 1, factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
          * - :py:attr:`~ym`
            - Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
          * - :py:attr:`~zm`
            - Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
          * - :py:attr:`~tbirth`
            - Get or set the Birth time for the active region when Node ID is used (TYPE=2).
          * - :py:attr:`~tdeath`
            - Get or set the Death time for the active region when Node ID is used (TYPE=2).
          * - :py:attr:`~nfreq`
            - Get or set the Number of cycles between updates the region's location for they =3
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_de_active_region import DefineDeActiveRegion

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID/Box ID/Node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the EQ.0: Part set ID
   1: BOX ID
   EQ.2: Node ID
   EQ.3: Noide ID (box shaped region that moves with this node)
















   ..
       !! processed by numpydoc !!

.. py:property:: xm_r
   :type: Optional[float]


   
   Get or set the For TYPE = 0 or 1, factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
   Limits for X-direction: Xmin, Xmax
   DX = Xmax-Xmin
   X_margin = Xm*DX
   Xmax = Xmax + X_margin
   Xmin = Xmin - X_margin,R is the radius of the region with center at Node ID (TYPE=2),
















   ..
       !! processed by numpydoc !!

.. py:property:: ym
   :type: Optional[float]


   
   Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
   Limits for Y-direction: Ymin, Ymax
   DY = Ymax-Ymin
   Y_margin = Ym*DY
   Ymax = Ymax + Y_margin
   Ymin = Ymin - Y_margin. R is the radius of the region with center at Node ID (TYPE=2),
















   ..
       !! processed by numpydoc !!

.. py:property:: zm
   :type: Optional[float]


   
   Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
   Limits for Z-direction: Zmin, Zmax
   DZ = Zmax-Zmin
   Z_margin = Zm*DZ
   Zmax = Zmax + Z_margin
   Zmin = Zmin - Z_margin. R is the radius of the region with center at Node ID (TYPE=2),
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: Optional[float]


   
   Get or set the Birth time for the active region when Node ID is used (TYPE=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time for the active region when Node ID is used (TYPE=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of cycles between updates the region's location for they =3
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'DE_ACTIVE_REGION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





