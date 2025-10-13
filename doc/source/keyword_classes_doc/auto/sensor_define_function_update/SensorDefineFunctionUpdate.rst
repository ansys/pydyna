





:class:`SensorDefineFunctionUpdate`
===================================


.. py:class:: sensor_define_function_update.SensorDefineFunctionUpdate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_FUNCTION_UPDATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineFunctionUpdate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sensid`
            - Get or set the Sensor ID.
          * - :py:attr:`~func`
            - Get or set the Function ID.
          * - :py:attr:`~sens1`
            - Get or set the 1st Sensor ID, the value of which will be used as the 1st argument of
          * - :py:attr:`~sens2`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sens3`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sens4`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
          * - :py:attr:`~sens5`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sens6`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi_1`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi_2`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
          * - :py:attr:`~sensi_3`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi_4`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi_5`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
          * - :py:attr:`~sensi_6`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~sensi_7`
            - Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
          * - :py:attr:`~birth`
            - Get or set the Sensor IBirth time of this sensor.
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

    from sensor_define_function_update import SensorDefineFunctionUpdate

Property detail
---------------

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the Sensor ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: func
   :type: Optional[int]


   
   Get or set the Function ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sens1
   :type: Optional[int]


   
   Get or set the 1st Sensor ID, the value of which will be used as the 1st argument of
   function FUNC. If defined as negative, the absolute value of SENS1,
   |SENS1|, is the number of sensors to be input. If |SENS1| > 5,
   additional cards will be needed to input the ID of all sensors. The number of sensor is limited to 15
















   ..
       !! processed by numpydoc !!

.. py:property:: sens2
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sens3
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sens4
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
















   ..
       !! processed by numpydoc !!

.. py:property:: sens5
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sens6
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_1
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_2
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_3
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_4
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_5
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_6
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sensi_7
   :type: Optional[int]


   
   Get or set the Ith Sensor ID, the value of which will be used as the Ith argument of function FUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: Optional[float]


   
   Get or set the Sensor IBirth time of this sensor.
















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
   :value: 'DEFINE_FUNCTION_UPDATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





