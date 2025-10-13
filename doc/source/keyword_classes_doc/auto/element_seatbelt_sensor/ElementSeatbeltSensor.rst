





:class:`ElementSeatbeltSensor`
==============================


.. py:class:: element_seatbelt_sensor.ElementSeatbeltSensor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SEATBELT_SENSOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSeatbeltSensor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sbsid`
            - Get or set the Sensor ID. A unique number has to be used.
          * - :py:attr:`~sbstyp`
            - Get or set the Sensor type:
          * - :py:attr:`~sbsfl`
            - Get or set the Sensor flag:
          * - :py:attr:`~nid`
            - Get or set the Node ID of sensor
          * - :py:attr:`~dof`
            - Get or set the Degree of freedom:
          * - :py:attr:`~acc`
            - Get or set the Activating acceleration
          * - :py:attr:`~atime`
            - Get or set the Time over which acceleration must be exceeded
          * - :py:attr:`~sbrid`
            - Get or set the Retractor ID, see *ELEMENT_SEATBELT_RETRACTOR.
          * - :py:attr:`~pulrat`
            - Get or set the Rate of pull-out (length/time units)
          * - :py:attr:`~pultim`
            - Get or set the Time over which rate of pull-out must be exceeded
          * - :py:attr:`~time`
            - Get or set the Time at which sensor triggers
          * - :py:attr:`~nid1`
            - Get or set the Node 1 ID
          * - :py:attr:`~nid2`
            - Get or set the Node 2 ID
          * - :py:attr:`~dmx`
            - Get or set the Maximum distance
          * - :py:attr:`~dmn`
            - Get or set the Minimum distance
          * - :py:attr:`~pulmx`
            - Get or set the Maximum pull-out
          * - :py:attr:`~pulmn`
            - Get or set the Minimum pull-out


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

    from element_seatbelt_sensor import ElementSeatbeltSensor

Property detail
---------------

.. py:property:: sbsid
   :type: int


   
   Get or set the Sensor ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbstyp
   :type: int


   
   Get or set the Sensor type:
   EQ.1: acceleration of node (default),
   EQ.2: retractor pull-out rate,
   EQ.3: time,
   EQ.4: distance between nodes.
   EQ.5:   retractor pull-out
















   ..
       !! processed by numpydoc !!

.. py:property:: sbsfl
   :type: int


   
   Get or set the Sensor flag:
   EQ.0: sensor active during dynamic relaxation,
   EQ.1: sensor can be triggered during dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the Node ID of sensor
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Degree of freedom:
   EQ.1: x,
   EQ.2: y,
   EQ.3: z.
















   ..
       !! processed by numpydoc !!

.. py:property:: acc
   :type: float


   
   Get or set the Activating acceleration
















   ..
       !! processed by numpydoc !!

.. py:property:: atime
   :type: float


   
   Get or set the Time over which acceleration must be exceeded
















   ..
       !! processed by numpydoc !!

.. py:property:: sbrid
   :type: int


   
   Get or set the Retractor ID, see *ELEMENT_SEATBELT_RETRACTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: pulrat
   :type: float


   
   Get or set the Rate of pull-out (length/time units)
















   ..
       !! processed by numpydoc !!

.. py:property:: pultim
   :type: float


   
   Get or set the Time over which rate of pull-out must be exceeded
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: float


   
   Get or set the Time at which sensor triggers
















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

.. py:property:: dmx
   :type: float


   
   Get or set the Maximum distance
















   ..
       !! processed by numpydoc !!

.. py:property:: dmn
   :type: float


   
   Get or set the Minimum distance
















   ..
       !! processed by numpydoc !!

.. py:property:: pulmx
   :type: float


   
   Get or set the Maximum pull-out
















   ..
       !! processed by numpydoc !!

.. py:property:: pulmn
   :type: float


   
   Get or set the Minimum pull-out
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SEATBELT_SENSOR'






