





:class:`ElementSeatbeltPretensioner`
====================================


.. py:class:: element_seatbelt_pretensioner.ElementSeatbeltPretensioner(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SEATBELT_PRETENSIONER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSeatbeltPretensioner

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sbprid`
            - Get or set the Pretensioner ID. A unique number has to be used.
          * - :py:attr:`~sbprty`
            - Get or set the Pretensioner type (see Activation):
          * - :py:attr:`~sbsid1`
            - Get or set the Sensor 1, see *ELEMENT_SEATBELT_SENSOR.
          * - :py:attr:`~sbsid2`
            - Get or set the Sensor 2, see *ELEMENT_SEATBELT_SENSOR.
          * - :py:attr:`~sbsid3`
            - Get or set the Sensor 3, see *ELEMENT_SEATBELT_SENSOR.
          * - :py:attr:`~sbsid4`
            - Get or set the Sensor 4, see *ELEMENT_SEATBELT_SENSOR.
          * - :py:attr:`~sbrid`
            - Get or set the Retractor number (SBPRTY = 1, 4, 5, 6, 7 or 8) or spring element number (SBPRTY = 2, 3 or 9).
          * - :py:attr:`~time`
            - Get or set the Time between sensor triggering and pretensioner acting.
          * - :py:attr:`~ptlcid`
            - Get or set the Load curve for pretensioner (Time after activation, Pull-in) (SBPRTY = 1, 4, 5, 6, 7, 8 or 9).
          * - :py:attr:`~lmtfrc`
            - Get or set the Optional limiting force for retractor types 4 through 8.  If zero, this option is ignored.


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

    from element_seatbelt_pretensioner import ElementSeatbeltPretensioner

Property detail
---------------

.. py:property:: sbprid
   :type: int


   
   Get or set the Pretensioner ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbprty
   :type: int


   
   Get or set the Pretensioner type (see Activation):
   EQ.1:   Pyrotechnic retractor with force limits(see Type 1),
   EQ.2 : Pre - loaded spring becomes active(see Types 2 and 3),
   EQ.3 : Lock spring removed(see Types 2 and 3),
   EQ.4 : Force as a function of time retractor with optional force limiter, LMTFRC(see Type 4)
   EQ.5 : Pyrotechnic retractor(old type in version 950) but with optional force limiter, LMTFRC(see Type 1 and Type 5).
   EQ.6 : Combination of types 4 and 5 as described in Type 6 below.
   EQ.7 : Independent pretensioner / retractor with optional force limiter(see Type 7).
   EQ.8 : Energy as a function of time retractor pretensioner with optional force limiter, LMTFRC(see Type 8).
   EQ.9 : Energy as a function of time buckle or anchor pretensioner(see Type 9).
















   ..
       !! processed by numpydoc !!

.. py:property:: sbsid1
   :type: int


   
   Get or set the Sensor 1, see *ELEMENT_SEATBELT_SENSOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbsid2
   :type: int


   
   Get or set the Sensor 2, see *ELEMENT_SEATBELT_SENSOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbsid3
   :type: int


   
   Get or set the Sensor 3, see *ELEMENT_SEATBELT_SENSOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbsid4
   :type: int


   
   Get or set the Sensor 4, see *ELEMENT_SEATBELT_SENSOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbrid
   :type: int


   
   Get or set the Retractor number (SBPRTY = 1, 4, 5, 6, 7 or 8) or spring element number (SBPRTY = 2, 3 or 9).
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: float


   
   Get or set the Time between sensor triggering and pretensioner acting.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptlcid
   :type: int


   
   Get or set the Load curve for pretensioner (Time after activation, Pull-in) (SBPRTY = 1, 4, 5, 6, 7, 8 or 9).
















   ..
       !! processed by numpydoc !!

.. py:property:: lmtfrc
   :type: float


   
   Get or set the Optional limiting force for retractor types 4 through 8.  If zero, this option is ignored.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SEATBELT_PRETENSIONER'






