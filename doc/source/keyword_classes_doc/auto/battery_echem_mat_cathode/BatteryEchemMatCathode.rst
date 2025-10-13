





:class:`BatteryEchemMatCathode`
===============================


.. py:class:: battery_echem_mat_cathode.BatteryEchemMatCathode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_MAT_CATHODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemMatCathode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part number identifier
          * - :py:attr:`~iocpa`
            - Get or set the Material identifier for the open-circuit potential.
          * - :py:attr:`~capta`
            - Get or set the Coulombic capacity of anode material.
          * - :py:attr:`~s_yc`
            - Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
          * - :py:attr:`~s_rad`
            - Get or set the Radius of spherical particle in the cathode side active material. (m)
          * - :py:attr:`~ratec`
            - Get or set the Reaction rate constant for the cathode electrode
          * - :py:attr:`~rcath`
            - Get or set the Film resistance for the cathode electrode
          * - :py:attr:`~rhoec`
            - Get or set the Density of the cathode insertion material (electrode particles). (Kg/m3)
          * - :py:attr:`~rhofc`
            - Get or set the Density of the cathode side inert filler. (Kg/m3)
          * - :py:attr:`~rhoccc`
            - Get or set the Density of the cathode side current collector. (Kg/m3)
          * - :py:attr:`~diffc`
            - Get or set the Diffusion coefficient of Lithium ions in the cathode insertion material. (m2/s)
          * - :py:attr:`~condc`
            - Get or set the Effective electronic conductivity of the cathode porous electrode
          * - :py:attr:`~vfec`
            - Get or set the Volume fraction of electrolyte in the anode electrode
          * - :py:attr:`~vfpc`
            - Get or set the Volume fraction of the polymer phase in the anode electrode
          * - :py:attr:`~vffc`
            - Get or set the Volume fraction of the inert filler in the anode electrode
          * - :py:attr:`~vfgc`
            - Get or set the Volume fraction of the gas in the anode electrode


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

    from battery_echem_mat_cathode import BatteryEchemMatCathode

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part number identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: iocpa
   :type: Optional[int]


   
   Get or set the Material identifier for the open-circuit potential.
   EQ.1: Titanium disulfide, LiyTiS2(0 < y < 1).
   EQ.2 : Spinel Mn2O4(lower plateau) (1.1 < y < 1.99).
   EQ.3 : Cobalt dioxide, LiyCoO2(0.0 < y < 0.99).
   EQ.4 : Spinel Mn2O4(upper plateau) (0.17 < y < 0.99).
   EQ.5 : NMC - 111 (not working).
   EQ.6 : NMC - 811 (not working).
   EQ.7 : LFP(not working).
















   ..
       !! processed by numpydoc !!

.. py:property:: capta
   :type: Optional[float]


   
   Get or set the Coulombic capacity of anode material.
















   ..
       !! processed by numpydoc !!

.. py:property:: s_yc
   :type: Optional[float]


   
   Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
















   ..
       !! processed by numpydoc !!

.. py:property:: s_rad
   :type: Optional[float]


   
   Get or set the Radius of spherical particle in the cathode side active material. (m)
















   ..
       !! processed by numpydoc !!

.. py:property:: ratec
   :type: Optional[float]


   
   Get or set the Reaction rate constant for the cathode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: rcath
   :type: Optional[float]


   
   Get or set the Film resistance for the cathode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoec
   :type: Optional[float]


   
   Get or set the Density of the cathode insertion material (electrode particles). (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhofc
   :type: Optional[float]


   
   Get or set the Density of the cathode side inert filler. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoccc
   :type: Optional[float]


   
   Get or set the Density of the cathode side current collector. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: diffc
   :type: Optional[float]


   
   Get or set the Diffusion coefficient of Lithium ions in the cathode insertion material. (m2/s)
















   ..
       !! processed by numpydoc !!

.. py:property:: condc
   :type: Optional[float]


   
   Get or set the Effective electronic conductivity of the cathode porous electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfec
   :type: Optional[float]


   
   Get or set the Volume fraction of electrolyte in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfpc
   :type: Optional[float]


   
   Get or set the Volume fraction of the polymer phase in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vffc
   :type: Optional[float]


   
   Get or set the Volume fraction of the inert filler in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfgc
   :type: Optional[float]


   
   Get or set the Volume fraction of the gas in the anode electrode
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_MAT_CATHODE'






