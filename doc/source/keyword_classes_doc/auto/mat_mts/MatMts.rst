





:class:`MatMts`
===============


.. py:class:: mat_mts.MatMts(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatMts

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~siga`
            - Get or set the Dislocation interactions with long-range barriers (force/area).
          * - :py:attr:`~sigi`
            - Get or set the Dislocation interactions with interstitial atoms (force/area).
          * - :py:attr:`~sigs`
            - Get or set the Dislocation interactions with solute atoms (force/area).
          * - :py:attr:`~sig0`
            - Get or set the Initial value of  SIGA at zero plastic strain (force/area). NOT USED.
          * - :py:attr:`~bulk`
            - Get or set the Bulk modulus defined for shell elements only. Do not input for solid elements.
          * - :py:attr:`~hf0`
            - Get or set the Dislocation generation material constant (force/area).
          * - :py:attr:`~hf1`
            - Get or set the Dislocation generation material constant (force/area).
          * - :py:attr:`~hf2`
            - Get or set the Dislocation generation material constant (force/area).
          * - :py:attr:`~sigs0`
            - Get or set the Saturation threshold stress at 0  Kelvin (force/area).
          * - :py:attr:`~edots0`
            - Get or set the Reference strain-rate (1/time).
          * - :py:attr:`~burg`
            - Get or set the Magnitude of Burgers vector (interatomic slip distance).
          * - :py:attr:`~capa`
            - Get or set the Material constant, A.
          * - :py:attr:`~boltz`
            - Get or set the Boltzmann's constant ,k (energy/degree).
          * - :py:attr:`~sm0`
            - Get or set the G0, shear modulus at zero degrees Kelvin (force/area).
          * - :py:attr:`~sm1`
            - Get or set the b1 , shear modulus constant (force/area).
          * - :py:attr:`~sm2`
            - Get or set the b2 , shear modulus constant (degree).
          * - :py:attr:`~edot0`
            - Get or set the Reference strain-rate (1/time).
          * - :py:attr:`~go`
            - Get or set the g0 , normalized activation energy for a .dislocation/dislocation interaction.
          * - :py:attr:`~pinv`
            - Get or set the 1/p, material constant.
          * - :py:attr:`~qinv`
            - Get or set the 1/q, material constant.
          * - :py:attr:`~edoti`
            - Get or set the Reference strain-rate (1/time).
          * - :py:attr:`~g0i`
            - Get or set the g0,i, normalized activation energy for a dislocation/interstitial interaction.
          * - :py:attr:`~pinvi`
            - Get or set the 1/pi, material constant.
          * - :py:attr:`~qinvi`
            - Get or set the 1/qi, material constant.
          * - :py:attr:`~edots`
            - Get or set the Reference strain-rate (1/time).
          * - :py:attr:`~g0s`
            - Get or set the g0,snormalized activation energy for a dislocation/solute interaction.
          * - :py:attr:`~pinvs`
            - Get or set the 1/ps, material constant.
          * - :py:attr:`~qinvs`
            - Get or set the 1/qs, material constant.
          * - :py:attr:`~rhocpr`
            - Get or set the Product of density and specific heat.
          * - :py:attr:`~temprf`
            - Get or set the Initial element temperature in degrees K.
          * - :py:attr:`~alpha`
            - Get or set the Material constant (typical value is between 0 and 2).
          * - :py:attr:`~eps0`
            - Get or set the Factor to normalize strain rate in the calculation of Teta-0. Use 1, 1/1000, 1/1000000 for the time units of seconds, milliseconds, microseconds, respectively.
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

    from mat_mts import MatMts

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: siga
   :type: Optional[float]


   
   Get or set the Dislocation interactions with long-range barriers (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigi
   :type: Optional[float]


   
   Get or set the Dislocation interactions with interstitial atoms (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigs
   :type: Optional[float]


   
   Get or set the Dislocation interactions with solute atoms (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sig0
   :type: Optional[float]


   
   Get or set the Initial value of  SIGA at zero plastic strain (force/area). NOT USED.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Bulk modulus defined for shell elements only. Do not input for solid elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: hf0
   :type: Optional[float]


   
   Get or set the Dislocation generation material constant (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: hf1
   :type: Optional[float]


   
   Get or set the Dislocation generation material constant (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: hf2
   :type: Optional[float]


   
   Get or set the Dislocation generation material constant (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigs0
   :type: Optional[float]


   
   Get or set the Saturation threshold stress at 0  Kelvin (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: edots0
   :type: Optional[float]


   
   Get or set the Reference strain-rate (1/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: burg
   :type: Optional[float]


   
   Get or set the Magnitude of Burgers vector (interatomic slip distance).
















   ..
       !! processed by numpydoc !!

.. py:property:: capa
   :type: Optional[float]


   
   Get or set the Material constant, A.
















   ..
       !! processed by numpydoc !!

.. py:property:: boltz
   :type: Optional[float]


   
   Get or set the Boltzmann's constant ,k (energy/degree).
















   ..
       !! processed by numpydoc !!

.. py:property:: sm0
   :type: Optional[float]


   
   Get or set the G0, shear modulus at zero degrees Kelvin (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sm1
   :type: Optional[float]


   
   Get or set the b1 , shear modulus constant (force/area).
















   ..
       !! processed by numpydoc !!

.. py:property:: sm2
   :type: Optional[float]


   
   Get or set the b2 , shear modulus constant (degree).
















   ..
       !! processed by numpydoc !!

.. py:property:: edot0
   :type: Optional[float]


   
   Get or set the Reference strain-rate (1/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: go
   :type: Optional[float]


   
   Get or set the g0 , normalized activation energy for a .dislocation/dislocation interaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pinv
   :type: Optional[float]


   
   Get or set the 1/p, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: qinv
   :type: Optional[float]


   
   Get or set the 1/q, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: edoti
   :type: Optional[float]


   
   Get or set the Reference strain-rate (1/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: g0i
   :type: Optional[float]


   
   Get or set the g0,i, normalized activation energy for a dislocation/interstitial interaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pinvi
   :type: Optional[float]


   
   Get or set the 1/pi, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: qinvi
   :type: Optional[float]


   
   Get or set the 1/qi, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: edots
   :type: Optional[float]


   
   Get or set the Reference strain-rate (1/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: g0s
   :type: Optional[float]


   
   Get or set the g0,snormalized activation energy for a dislocation/solute interaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pinvs
   :type: Optional[float]


   
   Get or set the 1/ps, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: qinvs
   :type: Optional[float]


   
   Get or set the 1/qs, material constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhocpr
   :type: Optional[float]


   
   Get or set the Product of density and specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: temprf
   :type: Optional[float]


   
   Get or set the Initial element temperature in degrees K.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Material constant (typical value is between 0 and 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps0
   :type: Optional[float]


   
   Get or set the Factor to normalize strain rate in the calculation of Teta-0. Use 1, 1/1000, 1/1000000 for the time units of seconds, milliseconds, microseconds, respectively.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'MTS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





