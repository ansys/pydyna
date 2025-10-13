





:class:`MatThermalDiscreteBeam`
===============================


.. py:class:: mat_thermal_discrete_beam.MatThermalDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalDiscreteBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification. A unique number or label must be specified.
          * - :py:attr:`~tro`
            - Get or set the Thermal density: EQ.0.0: default to structural density.
          * - :py:attr:`~hc`
            - Get or set the Specific heat.
          * - :py:attr:`~tc`
            - Get or set the Thermal conductance (SI units are W/K).HC = (heat transfer coefficient)x(beam cross section area)
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

    from mat_thermal_discrete_beam import MatThermalDiscreteBeam

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: tro
   :type: Optional[float]


   
   Get or set the Thermal density: EQ.0.0: default to structural density.
















   ..
       !! processed by numpydoc !!

.. py:property:: hc
   :type: Optional[float]


   
   Get or set the Specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: Optional[float]


   
   Get or set the Thermal conductance (SI units are W/K).HC = (heat transfer coefficient)x(beam cross section area)
















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
   :value: 'THERMAL_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





