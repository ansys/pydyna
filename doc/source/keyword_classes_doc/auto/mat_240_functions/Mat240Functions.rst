





:class:`Mat240Functions`
========================


.. py:class:: mat_240_functions.Mat240Functions(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_240_FUNCTIONS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat240Functions

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~roflg`
            - Get or set the Flag for whether density is specified per unit area or volume. ROFLG=0 specified density per unit volume (default), and ROFLG=1 specifies the density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
          * - :py:attr:`~intfail`
            - Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
          * - :py:attr:`~emod`
            - Get or set the The Young's modulus of the material.It is a function ID.
          * - :py:attr:`~gmod`
            - Get or set the The shear modulus of the material.Function ID for FUNCTIONS keyword option.
          * - :py:attr:`~thick`
            - Get or set the GT.0.0: Cohesive thickness. LE.0.0: Initial thickness is calculated from nodal coordinates.
          * - :py:attr:`~inicrt`
            - Get or set the Yield and damage initiation criterion:
          * - :py:attr:`~g1c_0`
            - Get or set the GT 0.0: G1C_0 is a function ID . Energy release rate GIC in Mode I. LE. 0.0: Lower bound value of rate-dependent GIC.
          * - :py:attr:`~g1c_inf`
            - Get or set the Upper bound value of rate-dependent GIC (only considered if G1C_0<0).
          * - :py:attr:`~edot_g1`
            - Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIC (only considered if G1C_0<0).
          * - :py:attr:`~t0`
            - Get or set the T0 is a function ID
          * - :py:attr:`~t1`
            - Get or set the Parameter T1, only considered if T0 < 0:
          * - :py:attr:`~edot_t`
            - Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode I (only considered if T0<0).
          * - :py:attr:`~fg1`
            - Get or set the An function ID
          * - :py:attr:`~lcg1c`
            - Get or set the Load curve ID which defines fracture energy GIC as a function of cohesive element thickness. G1C_‌0 and G1C_‌INF are ignored in this case.
          * - :py:attr:`~g2c_0`
            - Get or set the An function ID .
          * - :py:attr:`~g2c_inf`
            - Get or set the Upper bound value of GIIC (only considered if G2C_0<0).
          * - :py:attr:`~edot_g2`
            - Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIIC (only considered if G2C_0<0).
          * - :py:attr:`~s0`
            - Get or set the An function ID
          * - :py:attr:`~s1`
            - Get or set the Parameter S1, only considered if S0<0:
          * - :py:attr:`~edot_s`
            - Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode II (only considered if S0<0).
          * - :py:attr:`~fg2`
            - Get or set the An function ID .
          * - :py:attr:`~lcg2c`
            - Get or set the Load curve ID which defines fracture energy GIIC as a function of cohesive element thickness. G2C_‌0 and G2C_‌INF are ignored in that case.
          * - :py:attr:`~rfiltf`
            - Get or set the Smoothing factor on the equivalent strain rate using an exponential moving average method:
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

    from mat_240_functions import Mat240Functions

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: roflg
   :type: int


   
   Get or set the Flag for whether density is specified per unit area or volume. ROFLG=0 specified density per unit volume (default), and ROFLG=1 specifies the density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: intfail
   :type: Optional[float]


   
   Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
   LT.0.0: Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
   EQ.0.0 : Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
   GT.0.0 : Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
















   ..
       !! processed by numpydoc !!

.. py:property:: emod
   :type: Optional[int]


   
   Get or set the The Young's modulus of the material.It is a function ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmod
   :type: Optional[int]


   
   Get or set the The shear modulus of the material.Function ID for FUNCTIONS keyword option.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the GT.0.0: Cohesive thickness. LE.0.0: Initial thickness is calculated from nodal coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: inicrt
   :type: float


   
   Get or set the Yield and damage initiation criterion:
   EQ.0.0: quadratic nominal stress(default)
   EQ.1.0 : maximum nominal stress.
   LT.0.0: mixed mode with flexible exponent |INICRT
















   ..
       !! processed by numpydoc !!

.. py:property:: g1c_0
   :type: Optional[int]


   
   Get or set the GT 0.0: G1C_0 is a function ID . Energy release rate GIC in Mode I. LE. 0.0: Lower bound value of rate-dependent GIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: g1c_inf
   :type: Optional[float]


   
   Get or set the Upper bound value of rate-dependent GIC (only considered if G1C_0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: edot_g1
   :type: Optional[float]


   
   Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIC (only considered if G1C_0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: t0
   :type: Optional[int]


   
   Get or set the T0 is a function ID
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Parameter T1, only considered if T0 < 0:
   GT.0.0: Quadratic logarithmic model
   LT.0.0: Linear logarithmic model.
















   ..
       !! processed by numpydoc !!

.. py:property:: edot_t
   :type: Optional[float]


   
   Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode I (only considered if T0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: fg1
   :type: Optional[int]


   
   Get or set the An function ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg1c
   :type: Optional[int]


   
   Get or set the Load curve ID which defines fracture energy GIC as a function of cohesive element thickness. G1C_‌0 and G1C_‌INF are ignored in this case.
















   ..
       !! processed by numpydoc !!

.. py:property:: g2c_0
   :type: Optional[int]


   
   Get or set the An function ID .
















   ..
       !! processed by numpydoc !!

.. py:property:: g2c_inf
   :type: Optional[float]


   
   Get or set the Upper bound value of GIIC (only considered if G2C_0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: edot_g2
   :type: Optional[float]


   
   Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of GIIC (only considered if G2C_0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: s0
   :type: Optional[int]


   
   Get or set the An function ID
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: Optional[float]


   
   Get or set the Parameter S1, only considered if S0<0:
   GT.0.0: Quadratic logarithmic model is applied
   LT.0.0: Linear logarithmic model is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: edot_s
   :type: Optional[float]


   
   Get or set the Equivalent strain rate at yield initiation to describe the rate dependency of the yield stress in Mode II (only considered if S0<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: fg2
   :type: Optional[int]


   
   Get or set the An function ID .
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg2c
   :type: Optional[int]


   
   Get or set the Load curve ID which defines fracture energy GIIC as a function of cohesive element thickness. G2C_‌0 and G2C_‌INF are ignored in that case.
















   ..
       !! processed by numpydoc !!

.. py:property:: rfiltf
   :type: Optional[float]


   
   Get or set the Smoothing factor on the equivalent strain rate using an exponential moving average method:
   This option invokes a modified handling of strain rates, see Remarks.
   GT.0.0: RFILTF applied on the equivalent plastic strain rate
   LT.0.0 : | RFILTF | applied on the equivalent total strain rate
















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
   :value: '240_FUNCTIONS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





