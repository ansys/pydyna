





:class:`FrequencyDomainAccelerationUnit`
========================================


.. py:class:: frequency_domain_acceleration_unit.FrequencyDomainAccelerationUnit(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_ACCELERATION_UNIT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainAccelerationUnit

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~unit`
            - Get or set the Flag for acceleration unit conversion:
          * - :py:attr:`~umlt`
            - Get or set the Multiplier for converting g to [length unit]/[time unit]2 (used only for UNIT=-1).


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

    from frequency_domain_acceleration_unit import FrequencyDomainAccelerationUnit

Property detail
---------------

.. py:property:: unit
   :type: int


   
   Get or set the Flag for acceleration unit conversion:
   EQ.0: use [length unit]/[time unit]2 as unit of acceleration.
   EQ.1: use g as unit for acceleration, and SI units (Newton, kg, meter, second, etc.) elsewhere.
   EQ.2: use g as unit for acceleration, and Engineering units (lbf,lbf*second2/inch, inch, second, etc.) elsewhere.
   EQ.3: use g as unit for acceleration, and units (kN, kg, mm, ms, GPa, etc.) elsewhere.
   EQ4:use g as unit for acceleration, and units (Newton, ton, mm, second, MPa, etc.) elsewhere.
   EQ.-1: use g as unit for acceleration and provide the multiplier for converting g to [length unit]/[time unit]2.
















   ..
       !! processed by numpydoc !!

.. py:property:: umlt
   :type: Optional[float]


   
   Get or set the Multiplier for converting g to [length unit]/[time unit]2 (used only for UNIT=-1).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_ACCELERATION_UNIT'






