





:class:`ElementBearing`
=======================


.. py:class:: element_bearing.ElementBearing(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEARING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBearing

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the
          * - :py:attr:`~itype`
            - Get or set the Bearing type: EQ.0: default
          * - :py:attr:`~n1`
            - Get or set the Node on centerline of shaft
          * - :py:attr:`~cid1`
            - Get or set the Coordinate ID on shaft. The local z axis defines the axis of rotation.
          * - :py:attr:`~n2`
            - Get or set the Node on centerline of bearing. It should initially coincide with N1.
          * - :py:attr:`~ci2`
            - Get or set the Coordinate ID on bearing. The local z axis defines the axis of rotation.
          * - :py:attr:`~nb`
            - Get or set the Number of balls or rollers
          * - :py:attr:`~eball`
            - Get or set the Youngs modulus for balls or rollers.
          * - :py:attr:`~prball`
            - Get or set the Poissons ratio for balls or rollers.
          * - :py:attr:`~erace`
            - Get or set the Youngs modulus for races.
          * - :py:attr:`~prrace`
            - Get or set the Poissons ratio for races.
          * - :py:attr:`~stresl`
            - Get or set the Specified value of the bearing stress required to print a warning message that the value has been reached. If it is 0.0, then no message is printe.
          * - :py:attr:`~d`
            - Get or set the Diameter of balls or rollers.
          * - :py:attr:`~di`
            - Get or set the Bore inner diameter.
          * - :py:attr:`~do`
            - Get or set the Bore outer diameter.
          * - :py:attr:`~dm`
            - Get or set the Pitch diameter. If DM is not specified, it is calculated as the average of DI and DO.
          * - :py:attr:`~ao`
            - Get or set the Initial contact angle
          * - :py:attr:`~ai`
            - Get or set the Inner groove radius to ball diameter ratio.
          * - :py:attr:`~bo`
            - Get or set the Outer race groove radius to ball diameter ratio.
          * - :py:attr:`~pd`
            - Get or set the Bearing clearance when no load is applied.
          * - :py:attr:`~ipflag`
            - Get or set the Preload flag
          * - :py:attr:`~xtran`
            - Get or set the Displacement or force preload in the local X direction.
          * - :py:attr:`~ytran`
            - Get or set the Displacement or force preload in the local Y direction.
          * - :py:attr:`~ztran`
            - Get or set the Displacement or force preload in the local Z direction.
          * - :py:attr:`~xrot`
            - Get or set the Angle (in radians) or moment preload in local X direction.
          * - :py:attr:`~yrot`
            - Get or set the Angle (in radians) or moment preload in local Y direction.


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

    from element_bearing import ElementBearing

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Bearing type: EQ.0: default
   EQ.1:ball bearing
   EQ.2:  roller bearing
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node on centerline of shaft
















   ..
       !! processed by numpydoc !!

.. py:property:: cid1
   :type: Optional[int]


   
   Get or set the Coordinate ID on shaft. The local z axis defines the axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node on centerline of bearing. It should initially coincide with N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ci2
   :type: Optional[int]


   
   Get or set the Coordinate ID on bearing. The local z axis defines the axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nb
   :type: Optional[int]


   
   Get or set the Number of balls or rollers
















   ..
       !! processed by numpydoc !!

.. py:property:: eball
   :type: float


   
   Get or set the Youngs modulus for balls or rollers.
















   ..
       !! processed by numpydoc !!

.. py:property:: prball
   :type: float


   
   Get or set the Poissons ratio for balls or rollers.
















   ..
       !! processed by numpydoc !!

.. py:property:: erace
   :type: float


   
   Get or set the Youngs modulus for races.
















   ..
       !! processed by numpydoc !!

.. py:property:: prrace
   :type: float


   
   Get or set the Poissons ratio for races.
















   ..
       !! processed by numpydoc !!

.. py:property:: stresl
   :type: float


   
   Get or set the Specified value of the bearing stress required to print a warning message that the value has been reached. If it is 0.0, then no message is printe.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: float


   
   Get or set the Diameter of balls or rollers.
















   ..
       !! processed by numpydoc !!

.. py:property:: di
   :type: float


   
   Get or set the Bore inner diameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: do
   :type: float


   
   Get or set the Bore outer diameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: dm
   :type: float


   
   Get or set the Pitch diameter. If DM is not specified, it is calculated as the average of DI and DO.
















   ..
       !! processed by numpydoc !!

.. py:property:: ao
   :type: float


   
   Get or set the Initial contact angle
















   ..
       !! processed by numpydoc !!

.. py:property:: ai
   :type: float


   
   Get or set the Inner groove radius to ball diameter ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: bo
   :type: float


   
   Get or set the Outer race groove radius to ball diameter ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: pd
   :type: float


   
   Get or set the Bearing clearance when no load is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipflag
   :type: int


   
   Get or set the Preload flag
   EQ.0: no preload.
   EQ.1: displacement preload specified.
   EQ.2: force preload specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: xtran
   :type: float


   
   Get or set the Displacement or force preload in the local X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ytran
   :type: float


   
   Get or set the Displacement or force preload in the local Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ztran
   :type: float


   
   Get or set the Displacement or force preload in the local Z direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: xrot
   :type: float


   
   Get or set the Angle (in radians) or moment preload in local X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: yrot
   :type: float


   
   Get or set the Angle (in radians) or moment preload in local Y direction.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEARING'






