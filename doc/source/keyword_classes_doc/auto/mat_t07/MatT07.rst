





:class:`MatT07`
===============


.. py:class:: mat_t07.MatT07(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_T07 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatT07

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
          * - :py:attr:`~tgrlc`
            - Get or set the Thermal generation rate (see *DEFINE_‌CURVE):
          * - :py:attr:`~tgmult`
            - Get or set the Thermal generation rate multiplier: EQ.0.0: no heat generation.
          * - :py:attr:`~hdead`
            - Get or set the Specific heat for inactive material before birth time
          * - :py:attr:`~tdead`
            - Get or set the Thermal conductivity for inactive material before birth time
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature
          * - :py:attr:`~hlat`
            - Get or set the Latent heat
          * - :py:attr:`~lchc`
            - Get or set the Load curve for heat capacity as function of temperature.
          * - :py:attr:`~lctc`
            - Get or set the Load curve for thermal conductivity as function of temperature.
          * - :py:attr:`~tlstart`
            - Get or set the Birth temperature of material start.
          * - :py:attr:`~tlend`
            - Get or set the Birth temperature of material end.
          * - :py:attr:`~tistart`
            - Get or set the Birth time start.
          * - :py:attr:`~tiend`
            - Get or set the Birth time end.
          * - :py:attr:`~hghost`
            - Get or set the Heat capacity for ghost (quiet) material.
          * - :py:attr:`~tghost`
            - Get or set the Thermal conductivity for ghost (quiet) material.
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

    from mat_t07 import MatT07

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

.. py:property:: tgrlc
   :type: Optional[float]


   
   Get or set the Thermal generation rate (see *DEFINE_‌CURVE):
   GT.0:   load curve ID defining thermal generation rate as a function of time
   EQ.0 : thermal generation rate is the constant multiplier, TGMULT.
   LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
   Feature is similar to the volumetric heat generation rate in * LOAD_HEAT_GENERATION and has units W / m ^ 3 in the SI units system.
















   ..
       !! processed by numpydoc !!

.. py:property:: tgmult
   :type: Optional[float]


   
   Get or set the Thermal generation rate multiplier: EQ.0.0: no heat generation.
















   ..
       !! processed by numpydoc !!

.. py:property:: hdead
   :type: Optional[float]


   
   Get or set the Specific heat for inactive material before birth time
















   ..
       !! processed by numpydoc !!

.. py:property:: tdead
   :type: Optional[float]


   
   Get or set the Thermal conductivity for inactive material before birth time
















   ..
       !! processed by numpydoc !!

.. py:property:: tlat
   :type: Optional[float]


   
   Get or set the Phase change temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: hlat
   :type: Optional[float]


   
   Get or set the Latent heat
















   ..
       !! processed by numpydoc !!

.. py:property:: lchc
   :type: Optional[int]


   
   Get or set the Load curve for heat capacity as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctc
   :type: Optional[int]


   
   Get or set the Load curve for thermal conductivity as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlstart
   :type: Optional[float]


   
   Get or set the Birth temperature of material start.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlend
   :type: Optional[float]


   
   Get or set the Birth temperature of material end.
















   ..
       !! processed by numpydoc !!

.. py:property:: tistart
   :type: Optional[float]


   
   Get or set the Birth time start.
















   ..
       !! processed by numpydoc !!

.. py:property:: tiend
   :type: Optional[float]


   
   Get or set the Birth time end.
















   ..
       !! processed by numpydoc !!

.. py:property:: hghost
   :type: Optional[float]


   
   Get or set the Heat capacity for ghost (quiet) material.
















   ..
       !! processed by numpydoc !!

.. py:property:: tghost
   :type: Optional[float]


   
   Get or set the Thermal conductivity for ghost (quiet) material.
















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
   :value: 'T07'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





