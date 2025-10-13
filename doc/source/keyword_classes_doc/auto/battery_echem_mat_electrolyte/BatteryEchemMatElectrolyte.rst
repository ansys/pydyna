





:class:`BatteryEchemMatElectrolyte`
===================================


.. py:class:: battery_echem_mat_electrolyte.BatteryEchemMatElectrolyte(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_MAT_ELECTROLYTE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemMatElectrolyte

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part number identifier
          * - :py:attr:`~ielyte`
            - Get or set the Material identifier for the open-circuit potential.
          * - :py:attr:`~etype`
            - Get or set the Type of electrolyte:  (Kg/m3):
          * - :py:attr:`~rhoe`
            - Get or set the Density of the electrolyte. (Kg/m3)
          * - :py:attr:`~rhop`
            - Get or set the Density of the polymer phase. (Kg/m3)
          * - :py:attr:`~rhos`
            - Get or set the Density of the separator material. (Kg/m3)
          * - :py:attr:`~calmax`
            - Get or set the Maximum concentration of the electrolyte
          * - :py:attr:`~vfes`
            - Get or set the Volume fraction of electrolyte in the separator
          * - :py:attr:`~vfps`
            - Get or set the Volume fraction of the polymer phase in the separator
          * - :py:attr:`~vfgs`
            - Get or set the Volume fraction of the gas in the separator


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

    from battery_echem_mat_electrolyte import BatteryEchemMatElectrolyte

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part number identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: ielyte
   :type: Optional[int]


   
   Get or set the Material identifier for the open-circuit potential.
   EQ.1:   Lithium Hexafluoroarsenate in Methyl acetate, LiAsF6.
   EQ.2 : Perchlorate in polyethylene oxide(PEO).
   EQ.3 : Sodium Triflate, CF3NaO3S in PEO.
   EQ.4 : Lithium Hexafluoroarsenate in propylene carbonate(PC).
   EQ.5 : Perchlorate in PC.
   EQ.6 : Triflate in PEO.
   EQ.7 : LiPF6 in ethylene carbonate(EC) / dimethyl carbonates(DMC) and p(VdF - HFP).
















   ..
       !! processed by numpydoc !!

.. py:property:: etype
   :type: Optional[int]


   
   Get or set the Type of electrolyte:  (Kg/m3):
   EQ.0:   Liquid electrolyte.
   EQ.1 : Solid electrolyte
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoe
   :type: Optional[float]


   
   Get or set the Density of the electrolyte. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhop
   :type: Optional[float]


   
   Get or set the Density of the polymer phase. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhos
   :type: Optional[float]


   
   Get or set the Density of the separator material. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: calmax
   :type: Optional[float]


   
   Get or set the Maximum concentration of the electrolyte
















   ..
       !! processed by numpydoc !!

.. py:property:: vfes
   :type: Optional[float]


   
   Get or set the Volume fraction of electrolyte in the separator
















   ..
       !! processed by numpydoc !!

.. py:property:: vfps
   :type: Optional[float]


   
   Get or set the Volume fraction of the polymer phase in the separator
















   ..
       !! processed by numpydoc !!

.. py:property:: vfgs
   :type: Optional[float]


   
   Get or set the Volume fraction of the gas in the separator
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_MAT_ELECTROLYTE'






