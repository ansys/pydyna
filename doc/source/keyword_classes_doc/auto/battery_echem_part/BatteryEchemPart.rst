





:class:`BatteryEchemPart`
=========================


.. py:class:: battery_echem_part.BatteryEchemPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part identifier (must be different from any PID on a *PART card)
          * - :py:attr:`~mid`
            - Get or set the Material identifier defined by *ECHEM_BATTERY_MAT_... card
          * - :py:attr:`~eosid`
            - Get or set the Equation of state identifier defined using a *ECHEM_BATTERY_EOS_... card


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

    from battery_echem_part import BatteryEchemPart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part identifier (must be different from any PID on a *PART card)
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identifier defined by *ECHEM_BATTERY_MAT_... card
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state identifier defined using a *ECHEM_BATTERY_EOS_... card
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_PART'






