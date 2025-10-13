





:class:`MatThermalIsotropicTd`
==============================


.. py:class:: mat_thermal_isotropic_td.MatThermalIsotropicTd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_ISOTROPIC_TD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalIsotropicTd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal conductivity at T1al material identification, a unique number has to be used.
          * - :py:attr:`~tro`
            - Get or set the Thermal density:
          * - :py:attr:`~tgrlc`
            - Get or set the Thermal generation rate (see *DEFINE_CURVE).
          * - :py:attr:`~tgmult`
            - Get or set the Thermal generation rate multiplier:
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature.
          * - :py:attr:`~hlat`
            - Get or set the Latent heat.
          * - :py:attr:`~t1`
            - Get or set the Temperature T1.
          * - :py:attr:`~t2`
            - Get or set the Temperature T2.
          * - :py:attr:`~t3`
            - Get or set the Temperature T3.
          * - :py:attr:`~t4`
            - Get or set the Temperature T4.
          * - :py:attr:`~t5`
            - Get or set the Temperature T5.
          * - :py:attr:`~t6`
            - Get or set the Temperature T6.
          * - :py:attr:`~t7`
            - Get or set the Temperature T7.
          * - :py:attr:`~t8`
            - Get or set the Temperature T8.
          * - :py:attr:`~c1`
            - Get or set the Heat capacity at T1.
          * - :py:attr:`~c2`
            - Get or set the Heat capacity at T2.
          * - :py:attr:`~c3`
            - Get or set the Heat capacity at T3.
          * - :py:attr:`~c4`
            - Get or set the Heat capacity at T4.
          * - :py:attr:`~c5`
            - Get or set the Heat capacity at T5.
          * - :py:attr:`~c6`
            - Get or set the Heat capacity at T6.
          * - :py:attr:`~c7`
            - Get or set the Heat capacity at T7.
          * - :py:attr:`~c8`
            - Get or set the Heat capacity at T8.
          * - :py:attr:`~k1`
            - Get or set the Thermal conductivity at T1.
          * - :py:attr:`~k2`
            - Get or set the Thermal conductivity at T2.
          * - :py:attr:`~k3`
            - Get or set the Thermal conductivity at T3.
          * - :py:attr:`~k4`
            - Get or set the Thermal conductivity at T4.
          * - :py:attr:`~k5`
            - Get or set the Thermal conductivity at T5.
          * - :py:attr:`~k6`
            - Get or set the Thermal conductivity at T6.
          * - :py:attr:`~k7`
            - Get or set the Thermal conductivity at T7.
          * - :py:attr:`~k8`
            - Get or set the Thermal conductivity at T8.
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

    from mat_thermal_isotropic_td import MatThermalIsotropicTd

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal conductivity at T1al material identification, a unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tro
   :type: Optional[float]


   
   Get or set the Thermal density:
   EQ 0.0 structural density(default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tgrlc
   :type: Optional[float]


   
   Get or set the Thermal generation rate (see *DEFINE_CURVE).
   GT.0:   Load curve ID giving thermal generation rate as a function of time
   EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
   LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tgmult
   :type: Optional[float]


   
   Get or set the Thermal generation rate multiplier:
   EQ.0.0: no heat generation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlat
   :type: Optional[float]


   
   Get or set the Phase change temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlat
   :type: Optional[float]


   
   Get or set the Latent heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Temperature T1.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the Temperature T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: Optional[float]


   
   Get or set the Temperature T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: t4
   :type: Optional[float]


   
   Get or set the Temperature T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: t5
   :type: Optional[float]


   
   Get or set the Temperature T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: t6
   :type: Optional[float]


   
   Get or set the Temperature T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: t7
   :type: Optional[float]


   
   Get or set the Temperature T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: t8
   :type: Optional[float]


   
   Get or set the Temperature T8.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Heat capacity at T1.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Heat capacity at T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the Heat capacity at T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the Heat capacity at T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the Heat capacity at T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the Heat capacity at T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[float]


   
   Get or set the Heat capacity at T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: c8
   :type: Optional[float]


   
   Get or set the Heat capacity at T8.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T1.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: k4
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: k5
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: k6
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: k7
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: k8
   :type: Optional[float]


   
   Get or set the Thermal conductivity at T8.
















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
   :value: 'THERMAL_ISOTROPIC_TD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





