





:class:`EmVoltageDrop`
======================


.. py:class:: em_voltage_drop.EmVoltageDrop(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_VOLTAGE_DROP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmVoltageDrop

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vdid`
            - Get or set the Voltage Drop ID
          * - :py:attr:`~vdtype`
            - Get or set the Voltage Drop Type:EQ.1: Voltage drop between the two corresponding nodes of the two segment sets SSID1 and SSID2.
          * - :py:attr:`~ssid1`
            - Get or set the Segment Set ID 1
          * - :py:attr:`~ssid2_`
            - Get or set the Segment Set ID 2
          * - :py:attr:`~volt_`
            - Get or set the Value of the voltage drop


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

    from em_voltage_drop import EmVoltageDrop

Property detail
---------------

.. py:property:: vdid
   :type: Optional[int]


   
   Get or set the Voltage Drop ID
















   ..
       !! processed by numpydoc !!

.. py:property:: vdtype
   :type: Optional[int]


   
   Get or set the Voltage Drop Type:EQ.1: Voltage drop between the two corresponding nodes of the two segment sets SSID1 and SSID2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid1
   :type: Optional[int]


   
   Get or set the Segment Set ID 1
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid2_
   :type: Optional[int]


   
   Get or set the Segment Set ID 2
















   ..
       !! processed by numpydoc !!

.. py:property:: volt_
   :type: Optional[float]


   
   Get or set the Value of the voltage drop
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'VOLTAGE_DROP'






