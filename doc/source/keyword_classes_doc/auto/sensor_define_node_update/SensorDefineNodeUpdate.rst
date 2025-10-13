





:class:`SensorDefineNodeUpdate`
===============================


.. py:class:: sensor_define_node_update.SensorDefineNodeUpdate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_NODE_UPDATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineNodeUpdate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sensid`
            - Get or set the Sensor ID.
          * - :py:attr:`~node1`
            - Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.  If CTYPE = TEMP, then the temperature at NODE1 will be output. If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
          * - :py:attr:`~node2`
            - Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.If CTYPE = TEMP, then the temperature at NODE1 will be output.If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
          * - :py:attr:`~vid`
            - Get or set the ID of vector along which the nodal values are measured, see *DEFINE_?VECTOR.  The magnitude of nodal values (coordinate, velocity, or acceleration) will be output if VID is 0 or undefined.
          * - :py:attr:`~ctype`
            - Get or set the Output component type:
          * - :py:attr:`~birth`
            - Get or set the Birth time of this sensor.
          * - :py:attr:`~death`
            - Get or set the Death time of this sensor.
          * - :py:attr:`~dtupd`
            - Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
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

    from sensor_define_node_update import SensorDefineNodeUpdate

Property detail
---------------

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the Sensor ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: Optional[int]


   
   Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.  If CTYPE = TEMP, then the temperature at NODE1 will be output. If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
   When the keyword option SET is active, NODE1 is a node set ID.If NODE2 is needed, it must be a node set of the same length as NODE1 with SETOPT defined, but it can be either a node or node set without SETOPT defined.
   When the SET option is active but SETOPT is not defined, determining the status of a related* SENSOR_SWITCH depends on the sign of NODE1.See Remark 2 for details
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: Optional[int]


   
   Get or set the For an accelerometer sensor, these fields are the nodes defining the accelerometer.If CTYPE = TEMP, then the temperature at NODE1 will be output.If both NODE1 and NODE2 are defined, then the difference in temperature between these two nodes will be output.
   When the keyword option SET is active, NODE1 is a node set ID.If NODE2 is needed, it must be a node set of the same length as NODE1 with SETOPT defined, but it can be either a node or node set without SETOPT defined.
   When the SET option is active but SETOPT is not defined, determining the status of a related * SENSOR_SWITCH depends on the sign of NODE1.See Remark 2 for details
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[str]


   
   Get or set the ID of vector along which the nodal values are measured, see *DEFINE_?VECTOR.  The magnitude of nodal values (coordinate, velocity, or acceleration) will be output if VID is 0 or undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: str


   
   Get or set the Output component type:
   EQ.ACC: acceleration
   EQ.VEL: velocity
   EQ.COORD: Coordinate
   EQ.TEMP:        Temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: Optional[float]


   
   Get or set the Birth time of this sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: Optional[float]


   
   Get or set the Death time of this sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtupd
   :type: Optional[float]


   
   Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
















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
   :value: 'SENSOR'


.. py:attribute:: subkeyword
   :value: 'DEFINE_NODE_UPDATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





