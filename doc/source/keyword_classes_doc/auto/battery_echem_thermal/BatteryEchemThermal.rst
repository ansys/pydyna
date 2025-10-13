





:class:`BatteryEchemThermal`
============================


.. py:class:: battery_echem_thermal.BatteryEchemThermal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_THERMAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemThermal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~thame`
            - Get or set the Thermal material identifier
          * - :py:attr:`~tid`
            - Get or set the Material identifier
          * - :py:attr:`~iprt`
            - Get or set the Data print in ASCII format
          * - :py:attr:`~cp`
            - Get or set the The specific heat coefficient of the cell. (J/Kg K)
          * - :py:attr:`~hconv`
            - Get or set the Convective heat transfer coefficient with external medium. (W/m2K)
          * - :py:attr:`~temp`
            - Get or set the Ambient temperature around the cell stack. (K)
          * - :py:attr:`~dudt`
            - Get or set the The temperature coefficient of open circuit potential (V/K).
          * - :py:attr:`~filename`
            - Get or set the Name of the battery cell output file (ASCII)


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

    from battery_echem_thermal import BatteryEchemThermal

Property detail
---------------

.. py:property:: thame
   :type: Optional[str]


   
   Get or set the Thermal material identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: tid
   :type: Optional[int]


   
   Get or set the Material identifier
   EQ.0:   Constant temperature mode.
   EQ.1 : Isothermal temperature with time.
   EQ.2 : Thermal coupling with LS - DYNA thermal solver
















   ..
       !! processed by numpydoc !!

.. py:property:: iprt
   :type: Optional[int]


   
   Get or set the Data print in ASCII format
   EQ.0:   No data print out.
   EQ.1 : Time vs.heat flux print out for thermal solver.
   EQ.2 : Time vs.cell temperature print out
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the The specific heat coefficient of the cell. (J/Kg K)
















   ..
       !! processed by numpydoc !!

.. py:property:: hconv
   :type: Optional[float]


   
   Get or set the Convective heat transfer coefficient with external medium. (W/m2K)
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Ambient temperature around the cell stack. (K)
















   ..
       !! processed by numpydoc !!

.. py:property:: dudt
   :type: Optional[float]


   
   Get or set the The temperature coefficient of open circuit potential (V/K).
   EQ.0:   Constant coefficient given by MULT.
   EQ.1 : Coefficient as function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of the battery cell output file (ASCII)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_THERMAL'






