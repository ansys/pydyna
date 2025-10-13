





:class:`BoundaryPrescribedAccelerometerRigid`
=============================================


.. py:class:: boundary_prescribed_accelerometer_rigid.BoundaryPrescribedAccelerometerRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_ACCELEROMETER_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedAccelerometerRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for rigid body whose motion is prescribed.
          * - :py:attr:`~nid`
            - Get or set the Node ID corresponding to the location of the accelerometer
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID describing the orientation of the accelerometer's local axes
          * - :py:attr:`~lcidx`
            - Get or set the Load curve ID containing the local x-acceleration time history from the accelerometer.
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID containing the local y-acceleration time history from the accelerometer.
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID containing the local z-acceleration time history from the accelerometer.


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

    from boundary_prescribed_accelerometer_rigid import BoundaryPrescribedAccelerometerRigid

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for rigid body whose motion is prescribed.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID corresponding to the location of the accelerometer
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID describing the orientation of the accelerometer's local axes
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: Optional[int]


   
   Get or set the Load curve ID containing the local x-acceleration time history from the accelerometer.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Load curve ID containing the local y-acceleration time history from the accelerometer.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Load curve ID containing the local z-acceleration time history from the accelerometer.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_ACCELEROMETER_RIGID'






