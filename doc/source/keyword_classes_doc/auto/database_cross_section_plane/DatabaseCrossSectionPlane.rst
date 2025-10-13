





:class:`DatabaseCrossSectionPlane`
==================================


.. py:class:: database_cross_section_plane.DatabaseCrossSectionPlane(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_CROSS_SECTION_PLANE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseCrossSectionPlane

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~csid`
            - Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
          * - :py:attr:`~title`
            - Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
          * - :py:attr:`~psid`
            - Get or set the Part set ID. If zero all parts are included.
          * - :py:attr:`~xct`
            - Get or set the x-coordinate of tail of any outward drawn normal vector, N, originating on wall (tail) and terminating in space (head), (see Figure 9.1 in user's manual).
          * - :py:attr:`~yct`
            - Get or set the y-coordinate of tail of normal vector, N.
          * - :py:attr:`~zct`
            - Get or set the z-coordinate of tail of normal vector, N.
          * - :py:attr:`~xch`
            - Get or set the x-coordinate of head of normal vector, N.
          * - :py:attr:`~ych`
            - Get or set the y-coordinate of head of normal vector, N.
          * - :py:attr:`~zch`
            - Get or set the z-coordinate of head of normal vector, N.
          * - :py:attr:`~radius`
            - Get or set the Optional radius.
          * - :py:attr:`~xhev`
            - Get or set the x-coordinate of head of edge vector, L.
          * - :py:attr:`~yhev`
            - Get or set the y-coordinate of head of edge vector, L.
          * - :py:attr:`~zhev`
            - Get or set the z-coordinate of head of edge vector, L.
          * - :py:attr:`~lenl`
            - Get or set the Length of edge a, in L direction (default is set to infinity).
          * - :py:attr:`~lenm`
            - Get or set the Length of edge b, in M direction (default is set to infinity).
          * - :py:attr:`~id`
            - Get or set the Rigid body or accelerometer ID. The force resultants are output in the updated local system of the rigid body or accelerometer.
          * - :py:attr:`~itype`
            - Get or set the Flag that specifies whether ID above pertains to a rigid body, an accelerometer, or a coordinate system:


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

    from database_cross_section_plane import DatabaseCrossSectionPlane

Property detail
---------------

.. py:property:: csid
   :type: Optional[int]


   
   Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID. If zero all parts are included.
















   ..
       !! processed by numpydoc !!

.. py:property:: xct
   :type: float


   
   Get or set the x-coordinate of tail of any outward drawn normal vector, N, originating on wall (tail) and terminating in space (head), (see Figure 9.1 in user's manual).
















   ..
       !! processed by numpydoc !!

.. py:property:: yct
   :type: float


   
   Get or set the y-coordinate of tail of normal vector, N.
















   ..
       !! processed by numpydoc !!

.. py:property:: zct
   :type: float


   
   Get or set the z-coordinate of tail of normal vector, N.
















   ..
       !! processed by numpydoc !!

.. py:property:: xch
   :type: float


   
   Get or set the x-coordinate of head of normal vector, N.
















   ..
       !! processed by numpydoc !!

.. py:property:: ych
   :type: float


   
   Get or set the y-coordinate of head of normal vector, N.
















   ..
       !! processed by numpydoc !!

.. py:property:: zch
   :type: float


   
   Get or set the z-coordinate of head of normal vector, N.
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the Optional radius.
   EQ.0.0: Not used.
   GT.0.0 : A circular cut plane will be created that is centered at(XCT ,YCT ,ZCT) with radius = RADIUS and has a normal vector originating at(XCT ,YCT ,ZCT) and pointing towards(XCH ,YCH ,ZCH).
   LT.0.0 : The radius will be the absolute value of RADIUS and XCT and XCH will be nodes IDs.The node with ID XCT is the center of the circular cut plane.The normal vector of the plane is the vector pointing from the node with ID XCT to the node with ID XCH.YCT, ZCT, YCH,and ZCH are ignored.
   If RADIUS != 0.0, the variables XHEV, YHEV, ZHEV, LENL,and LENM, which are specified on Card 1a.2, will be ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: xhev
   :type: float


   
   Get or set the x-coordinate of head of edge vector, L.
















   ..
       !! processed by numpydoc !!

.. py:property:: yhev
   :type: float


   
   Get or set the y-coordinate of head of edge vector, L.
















   ..
       !! processed by numpydoc !!

.. py:property:: zhev
   :type: float


   
   Get or set the z-coordinate of head of edge vector, L.
















   ..
       !! processed by numpydoc !!

.. py:property:: lenl
   :type: Optional[float]


   
   Get or set the Length of edge a, in L direction (default is set to infinity).
















   ..
       !! processed by numpydoc !!

.. py:property:: lenm
   :type: Optional[float]


   
   Get or set the Length of edge b, in M direction (default is set to infinity).
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Rigid body or accelerometer ID. The force resultants are output in the updated local system of the rigid body or accelerometer.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Flag that specifies whether ID above pertains to a rigid body, an accelerometer, or a coordinate system:
   EQ. 0: rigid body (default),
   EQ. 1: accelerometer,
   EQ. 2: coordinate ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'CROSS_SECTION_PLANE'






