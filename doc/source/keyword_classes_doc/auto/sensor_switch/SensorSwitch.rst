





:class:`SensorSwitch`
=====================


.. py:class:: sensor_switch.SensorSwitch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_SWITCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorSwitch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~switid`
            - Get or set the Switch ID can be referred directly by *SENSOR_CONTROL to control the status of entities like CONTACT and AIRBAG, or can be referred to by *SENSOR_SWITCH_CALC-LOGIC for logic computation.
          * - :py:attr:`~type`
            - Get or set the Type:
          * - :py:attr:`~sensid`
            - Get or set the ID of the sensor whose value will be compared to the criterion to determine if a switch condition is met.
          * - :py:attr:`~logic`
            - Get or set the Logic:
          * - :py:attr:`~value`
            - Get or set the Critical value
          * - :py:attr:`~filtrid`
            - Get or set the Filter ID (optional).  Filters may be defined using *DEFINE_FILTER.
          * - :py:attr:`~timwin`
            - Get or set the Trigger a status change when the value given by the sensor is less than or greater than (depending on LOGIC) the VALUE for a duration defined by TIMWIN.
          * - :py:attr:`~tdelay`
            - Get or set the Optional time delay. The status change will not happen immediately when
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

    from sensor_switch import SensorSwitch

Property detail
---------------

.. py:property:: switid
   :type: Optional[int]


   
   Get or set the Switch ID can be referred directly by *SENSOR_CONTROL to control the status of entities like CONTACT and AIRBAG, or can be referred to by *SENSOR_SWITCH_CALC-LOGIC for logic computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: str


   
   Get or set the Type:
   EQ.Sensor:
   EQ.Time:
















   ..
       !! processed by numpydoc !!

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the ID of the sensor whose value will be compared to the criterion to determine if a switch condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: logic
   :type: str


   
   Get or set the Logic:
   EQ.LT: less than
   EQ.GT: greater than
















   ..
       !! processed by numpydoc !!

.. py:property:: value
   :type: Optional[float]


   
   Get or set the Critical value
















   ..
       !! processed by numpydoc !!

.. py:property:: filtrid
   :type: Optional[int]


   
   Get or set the Filter ID (optional).  Filters may be defined using *DEFINE_FILTER.
















   ..
       !! processed by numpydoc !!

.. py:property:: timwin
   :type: Optional[float]


   
   Get or set the Trigger a status change when the value given by the sensor is less than or greater than (depending on LOGIC) the VALUE for a duration defined by TIMWIN.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdelay
   :type: Optional[float]


   
   Get or set the Optional time delay. The status change will not happen immediately when
   both the switch condition and TIMWIN are met. Instead the status change
   is delayed by TDELAY.
















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
   :value: 'SWITCH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





