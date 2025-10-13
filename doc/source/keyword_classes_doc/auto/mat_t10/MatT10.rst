





:class:`MatT10`
===============


.. py:class:: mat_t10.MatT10(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_T10 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatT10

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
            - Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature.
          * - :py:attr:`~hlat`
            - Get or set the Latent heat.
          * - :py:attr:`~hclc`
            - Get or set the Load curve ID specifying specific heat as a function of temperature, or, if |HCHSV| > 0:
          * - :py:attr:`~tclc`
            - Get or set the Load curve ID specifying thermal conductivity as a function of temperature, or if |TCHSV| > 0:
          * - :py:attr:`~hchsv`
            - Get or set the Optional:
          * - :py:attr:`~tchsv`
            - Get or set the Optional:
          * - :py:attr:`~tghsv`
            - Get or set the Optional:
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

    from mat_t10 import MatT10

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
   :type: Optional[int]


   
   Get or set the Thermal generation rate (see *DEFINE_CURVE).
   GT.0:   Load curve ID defining thermal generation rate as a function of time
   EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
   LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tgmult
   :type: Optional[float]


   
   Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
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

.. py:property:: hclc
   :type: Optional[int]


   
   Get or set the Load curve ID specifying specific heat as a function of temperature, or, if |HCHSV| > 0:
   GT.0:   Load curve specifying the specific heat as a function of the mechanical history variable specified by HCHSV.
   LT.0 : Table of load curves for different temperatures.Each curve specifies the specific heat as a function of the mechanical history variable specified by HCHSV
















   ..
       !! processed by numpydoc !!

.. py:property:: tclc
   :type: Optional[int]


   
   Get or set the Load curve ID specifying thermal conductivity as a function of temperature, or if |TCHSV| > 0:
   GT.0:   Load curve giving thermal conductivity as a function of the mechanical history variable specified by TCHSV.
   LT.0 : Table of load curves for different temperatures.Each curve is a function of the mechanical history variable specified by TCHSV
















   ..
       !! processed by numpydoc !!

.. py:property:: hchsv
   :type: Optional[float]


   
   Get or set the Optional:
   GT.0.0: mechanical history variable # used by HCL
   LT.0.0: as above but | HCHSV |= 1 - 6 points to the six stress components, | HCHSV |= 7 to plastic strain,and | HCHSV |= 7 + k points to history variable k
















   ..
       !! processed by numpydoc !!

.. py:property:: tchsv
   :type: Optional[float]


   
   Get or set the Optional:
   GT.0.0: mechanical history variable # used by TCLC
   LT.0.0: as above but | TCHSV |= 1 - 6 points to the six stress components, | TCHSV |= 7 to plastic strain,and | TCHSV |= 7 + k points to history variable k
















   ..
       !! processed by numpydoc !!

.. py:property:: tghsv
   :type: Optional[float]


   
   Get or set the Optional:
   GT.0.0: mechanical history variable # used by TGRLC
   LT.0.0: as above but | TGHSV |= 1 - 6 points to the six stress components, | TGHSV |= 7 to plastic strain,and | TGHSV |= 7 + k points to history variable k
















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
   :value: 'T10'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





