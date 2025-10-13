





:class:`ElementSeatbeltAccelerometer`
=====================================


.. py:class:: element_seatbelt_accelerometer.ElementSeatbeltAccelerometer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SEATBELT_ACCELEROMETER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSeatbeltAccelerometer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sbacid`
            - Get or set the Accelerometer ID. A unique number has to be used.
          * - :py:attr:`~nid1`
            - Get or set the Node 1 ID
          * - :py:attr:`~nid2`
            - Get or set the Node 2 ID
          * - :py:attr:`~nid3`
            - Get or set the Node 3 ID
          * - :py:attr:`~igrav`
            - Get or set the Gravitational accelerations due to body force loads.
          * - :py:attr:`~intopt`
            - Get or set the Integration option. If the accelerometer undergoes rigid body translation without rotation this option has no effect; however, if rotation occurs, option 1 may provide better agreement with the output of the accelerometer.
          * - :py:attr:`~mass`
            - Get or set the Optional added mass for accelerometer. This mass is equally distributed to nodal points NID1, NID2, and NID3.  This option avoids the need to use the *ELEMENT_MASS keyword input if additional mass is required.


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

    from element_seatbelt_accelerometer import ElementSeatbeltAccelerometer

Property detail
---------------

.. py:property:: sbacid
   :type: int


   
   Get or set the Accelerometer ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: int


   
   Get or set the Node 1 ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: int


   
   Get or set the Node 2 ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid3
   :type: int


   
   Get or set the Node 3 ID
















   ..
       !! processed by numpydoc !!

.. py:property:: igrav
   :type: int


   
   Get or set the Gravitational accelerations due to body force loads.
   EQ.-6:  Z and X components removed from acceleration output
   EQ.-5   Y and Z components removed from acceleration output
   EQ.-4:  X and Y components removed from acceleration output
   EQ.-3:  Z component removed from acceleration output
   EQ.-2:  Y component removed from acceleration output
   EQ.-1:  X component removed from acceleration output
   EQ. 0:  all components included in acceleration output
   EQ. 1:  all components removed from acceleration output.
















   ..
       !! processed by numpydoc !!

.. py:property:: intopt
   :type: int


   
   Get or set the Integration option. If the accelerometer undergoes rigid body translation without rotation this option has no effect; however, if rotation occurs, option 1 may provide better agreement with the output of the accelerometer.
   EQ.0: velocities are integrated from the global accelerations and transfromed into the local system of the accelerometer
   EQ.1: velocities are integrated directly from the local accelerations of the accelerometer.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: Optional[float]


   
   Get or set the Optional added mass for accelerometer. This mass is equally distributed to nodal points NID1, NID2, and NID3.  This option avoids the need to use the *ELEMENT_MASS keyword input if additional mass is required.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SEATBELT_ACCELEROMETER'






