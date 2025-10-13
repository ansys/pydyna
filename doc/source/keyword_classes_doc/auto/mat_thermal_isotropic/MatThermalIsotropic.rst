





:class:`MatThermalIsotropic`
============================


.. py:class:: mat_thermal_isotropic.MatThermalIsotropic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_ISOTROPIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalIsotropic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification, a unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Thermal density:
          * - :py:attr:`~tgrlc`
            - Get or set the Thermal generation rate (see *DEFINE_CURVE).
          * - :py:attr:`~tgmult`
            - Get or set the Thermal generation rate multiplier:
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature.
          * - :py:attr:`~hlat`
            - Get or set the Latent heat.
          * - :py:attr:`~hc`
            - Get or set the Specific Heat.
          * - :py:attr:`~tc`
            - Get or set the Thermal conductivity.
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

    from mat_thermal_isotropic import MatThermalIsotropic

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification, a unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Thermal density:
   EQ 0.0 structural density (default).
















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
   EQ.0.0: no heat generation (default).
















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

.. py:property:: hc
   :type: Optional[float]


   
   Get or set the Specific Heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: Optional[float]


   
   Get or set the Thermal conductivity.
















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
   :value: 'THERMAL_ISOTROPIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





