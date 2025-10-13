





:class:`BatteryEchemMatAnode`
=============================


.. py:class:: battery_echem_mat_anode.BatteryEchemMatAnode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_MAT_ANODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemMatAnode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part IDr
          * - :py:attr:`~iocpa`
            - Get or set the Material type for the open-circuit potential.
          * - :py:attr:`~capta`
            - Get or set the Coulombic capacity of anode material.
          * - :py:attr:`~s_xa`
            - Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
          * - :py:attr:`~rada`
            - Get or set the Radius of spherical particles in the anode side active material. (m)
          * - :py:attr:`~ratea`
            - Get or set the Reaction rate constant for the anode electrode
          * - :py:attr:`~ranode`
            - Get or set the Film resistance for the anode electrode
          * - :py:attr:`~rhoea`
            - Get or set the Density of anode insertion material (electrode particles). (Kg/m3)
          * - :py:attr:`~rhofa`
            - Get or set the Density of the anode side inert filler. (Kg/m3)
          * - :py:attr:`~rhocca`
            - Get or set the Density of the anode side current collector. (Kg/m3)
          * - :py:attr:`~diffa`
            - Get or set the Diffusion coefficient of Lithium ions in the anode insertion material
          * - :py:attr:`~conda`
            - Get or set the Effective electronic conductivity of the anode porous electrode
          * - :py:attr:`~vfea`
            - Get or set the Volume fraction of electrolyte in the anode electrode
          * - :py:attr:`~vfpa`
            - Get or set the Volume fraction of the polymer phase in the anode electrode
          * - :py:attr:`~vffa`
            - Get or set the Volume fraction of the inert filler in the anode electrode
          * - :py:attr:`~vfga`
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

    from battery_echem_mat_anode import BatteryEchemMatAnode

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part IDr
















   ..
       !! processed by numpydoc !!

.. py:property:: iocpa
   :type: Optional[int]


   
   Get or set the Material type for the open-circuit potential.
   EQ.1:   Lithium metal foil.
   EQ.2 : Titanium disulfide, LixTiS2(0 < x < 1).
   EQ.3 : Petroleum coke, Carbon.
   EQ.4 : MCMB 2510 carbon.
   EQ.5 : MCMB 2528 carbon
















   ..
       !! processed by numpydoc !!

.. py:property:: capta
   :type: Optional[float]


   
   Get or set the Coulombic capacity of anode material.
















   ..
       !! processed by numpydoc !!

.. py:property:: s_xa
   :type: Optional[float]


   
   Get or set the Initial Lithium stoichiometric coefficient of the anode side active material. For example LixWO3 (0<x<0.67).
















   ..
       !! processed by numpydoc !!

.. py:property:: rada
   :type: Optional[float]


   
   Get or set the Radius of spherical particles in the anode side active material. (m)
















   ..
       !! processed by numpydoc !!

.. py:property:: ratea
   :type: Optional[float]


   
   Get or set the Reaction rate constant for the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: ranode
   :type: Optional[float]


   
   Get or set the Film resistance for the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoea
   :type: Optional[float]


   
   Get or set the Density of anode insertion material (electrode particles). (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhofa
   :type: Optional[float]


   
   Get or set the Density of the anode side inert filler. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rhocca
   :type: Optional[float]


   
   Get or set the Density of the anode side current collector. (Kg/m3)
















   ..
       !! processed by numpydoc !!

.. py:property:: diffa
   :type: Optional[float]


   
   Get or set the Diffusion coefficient of Lithium ions in the anode insertion material
















   ..
       !! processed by numpydoc !!

.. py:property:: conda
   :type: Optional[float]


   
   Get or set the Effective electronic conductivity of the anode porous electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfea
   :type: Optional[float]


   
   Get or set the Volume fraction of electrolyte in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfpa
   :type: Optional[float]


   
   Get or set the Volume fraction of the polymer phase in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vffa
   :type: Optional[float]


   
   Get or set the Volume fraction of the inert filler in the anode electrode
















   ..
       !! processed by numpydoc !!

.. py:property:: vfga
   :type: Optional[float]


   
   Get or set the Volume fraction of the gas in the anode electrode
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_MAT_ANODE'






