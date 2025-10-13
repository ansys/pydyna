





:class:`BoundaryTemperatureRsw`
===============================


.. py:class:: boundary_temperature_rsw.BoundaryTemperatureRsw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_TEMPERATURE_RSW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryTemperatureRsw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Node Set ID; see *SET_‌NODE_‌OPTION. Nodes in the set will be checked to see if they are in the nugget or heat affected zone. If they are, the boundary condition will be applied. The boundary condition will not be applied to nodes in these regions if they are not included in the set..
          * - :py:attr:`~option`
            - Get or set the Option for heat affected zone around the weld nugget:
          * - :py:attr:`~nid1`
            - Get or set the Node defining the tail of the orientation vector (axis of rotation of
          * - :py:attr:`~nid2`
            - Get or set the Node defining the head of the orientation vector (axis of rotation
          * - :py:attr:`~tdeath`
            - Get or set the Deactivation time for temperature boundary condition. At this
          * - :py:attr:`~tbirth`
            - Get or set the Activation time for temperature boundary condition. Before this
          * - :py:attr:`~loc`
            - Get or set the Application of surface for thermal shell elements, see parameter,
          * - :py:attr:`~dist`
            - Get or set the Position of center of nugget on the axis of rotation. Parameter
          * - :py:attr:`~h1`
            - Get or set the Half width h1 of nugget in the lower half, i.e. in direction to NID1.    See Remark 2.
          * - :py:attr:`~h2`
            - Get or set the Half width h2 of nugget in the upper half, i.e. in direction to NID2. See Remark 2.
          * - :py:attr:`~r`
            - Get or set the Radius rweld of the nugget in surface normal to orientation vector. See Remark 2.
          * - :py:attr:`~tempc`
            - Get or set the Base temperature at the center of the nugget. See Remark 3.
          * - :py:attr:`~tempb`
            - Get or set the Base temperature at the boundary of the nugget. See Remark 3.
          * - :py:attr:`~lcidt`
            - Get or set the |LCIDT| refers to the load curve ID prescribing the temperature evolution in the nugget as a function of time. The abscissa of the load curve will be normalized between the birth and death times of the boundary condition.
          * - :py:attr:`~hz1`
            - Get or set the Half width hz1 of heat affected zone in the lower half, meaning in
          * - :py:attr:`~hz2`
            - Get or set the Half width hz2 of heat affected zone in the upper half, meaning in
          * - :py:attr:`~rz`
            - Get or set the Radius Rhaz of the heat affected zone in surface normal to
          * - :py:attr:`~tempzb`
            - Get or set the Base temperature at the boundary of the heat affected zone


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

    from boundary_temperature_rsw import BoundaryTemperatureRsw

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node Set ID; see *SET_‌NODE_‌OPTION. Nodes in the set will be checked to see if they are in the nugget or heat affected zone. If they are, the boundary condition will be applied. The boundary condition will not be applied to nodes in these regions if they are not included in the set..
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: int


   
   Get or set the Option for heat affected zone around the weld nugget:
   EQ.0: no heat affected zone
   EQ.1: ellipsoidal region considered
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: Optional[int]


   
   Get or set the Node defining the tail of the orientation vector (axis of rotation of
   the ellipsoidal region) and the base for positioning of the nugget.
   See Remarks 1 and 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the Node defining the head of the orientation vector (axis of rotation
   of the ellipsoidal region). See Remarks 1 and 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Deactivation time for temperature boundary condition. At this
   point in time the temperature constraint is removed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Activation time for temperature boundary condition. Before this
   point in time the temperature constraint is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the Application of surface for thermal shell elements, see parameter,
   THSHEL, in the *CONTROL_SHELL input:
   EQ.-1: lower surface of thermal shell element
   EQ.0: middle surface of thermal shell element
   EQ.1: upper surface of thermal shell element.
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: float


   
   Get or set the Position of center of nugget on the axis of rotation. Parameter
   defines the distance to NID1 along the orientation vector. See  Remark 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: h1
   :type: float


   
   Get or set the Half width h1 of nugget in the lower half, i.e. in direction to NID1.    See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: h2
   :type: float


   
   Get or set the Half width h2 of nugget in the upper half, i.e. in direction to NID2. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: float


   
   Get or set the Radius rweld of the nugget in surface normal to orientation vector. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: tempc
   :type: float


   
   Get or set the Base temperature at the center of the nugget. See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: tempb
   :type: float


   
   Get or set the Base temperature at the boundary of the nugget. See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the |LCIDT| refers to the load curve ID prescribing the temperature evolution in the nugget as a function of time. The abscissa of the load curve will be normalized between the birth and death times of the boundary condition.
   GT.0:   The ordinate values of the load curve scale the respective base temperature of a particular point.
   EQ.0:   No temperature evolution. Base temperatures are used.
   LT.0:   The ordinate values of the load curve are used to define a linear combination between the temperature at the birth time and the base temperature of a particular point.Load curve ordinate values should range between 0.0 and 1.0.We recommend LCIDT < 0 to ensure a smooth temperature evolution.
















   ..
       !! processed by numpydoc !!

.. py:property:: hz1
   :type: Optional[float]


   
   Get or set the Half width hz1 of heat affected zone in the lower half, meaning in
   direction to NID1. Only active for OPTION = 1. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: hz2
   :type: float


   
   Get or set the Half width hz2 of heat affected zone in the upper half, meaning in
   direction to NID1. Only active for OPTION = 1. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: rz
   :type: float


   
   Get or set the Radius Rhaz of the heat affected zone in surface normal to
   orientation vector. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: tempzb
   :type: float


   
   Get or set the Base temperature at the boundary of the heat affected zone
   for OPTION = 1. See Remark 4.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'TEMPERATURE_RSW'






