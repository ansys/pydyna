





:class:`MatElasticViscoplasticThermal`
======================================


.. py:class:: mat_elastic_viscoplastic_thermal.MatElasticViscoplasticThermal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ELASTIC_VISCOPLASTIC_THERMAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatElasticViscoplasticThermal

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress.
          * - :py:attr:`~alpha`
            - Get or set the Coefficient of thermal expansion.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain.
          * - :py:attr:`~fail`
            - Get or set the Effective plastic failure strain for erosion.
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter QR1.
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter CR1.
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter QR2.
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter CR2.
          * - :py:attr:`~qx1`
            - Get or set the Kinematic hardening parameter QX1.
          * - :py:attr:`~cx1`
            - Get or set the Kinematic hardening parameter CX1.
          * - :py:attr:`~qx2`
            - Get or set the Kinematic hardening parameter QX2.
          * - :py:attr:`~cx2`
            - Get or set the Kinematic hardening parameter CX2.
          * - :py:attr:`~c`
            - Get or set the Viscous material parameter C.
          * - :py:attr:`~p`
            - Get or set the Viscous material parameter P.
          * - :py:attr:`~lce`
            - Get or set the Load curve defining Young's modulus as a function of temperature.
          * - :py:attr:`~lcpr`
            - Get or set the Load curve defining Poisson's ratio as a function of temperature.
          * - :py:attr:`~lcsigy`
            - Get or set the Load curve defining the initial yield stress as a function of temperature.
          * - :py:attr:`~lcr`
            - Get or set the Load curve for scaling the isotropic hardening parameters QR1 and QR2 or the stress given by the load curve LCSS as a function of temperature.
          * - :py:attr:`~lcx`
            - Get or set the Load curve for scaling the isotropic hardening parameters QX1 and QX2 as a function of temperature.
          * - :py:attr:`~lcalph`
            - Get or set the Load curve defining the coefficient of thermal expansion as a function of temperature.
          * - :py:attr:`~lcc`
            - Get or set the Load curve for scaling the viscous materal parameter C as a function of temperature.
          * - :py:attr:`~lcp`
            - Get or set the Load curve for scaling the viscous material parameter P as a function of temperature.
          * - :py:attr:`~tref`
            - Get or set the Reference temperature required if and only if LCALPH is given with a negative curve ID.
          * - :py:attr:`~lcfail`
            - Get or set the Load curve defining the plastic failure strain as a function of temperature. FAIL on card 1 is ignored with this option.
          * - :py:attr:`~nuhis`
            - Get or set the Number of additional user defined history variables
          * - :py:attr:`~t1phas`
            - Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
          * - :py:attr:`~t2phas`
            - Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
          * - :py:attr:`~tol`
            - Get or set the Optional tolerance for plasticity update. The default is 10-6 for solid elements and 10-3 for shells. This parameter overrides the default tolerance for all element types.
          * - :py:attr:`~fushi1`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi2`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi3`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi4`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi5`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi6`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi7`
            - Get or set the Function ID for user defined history variables
          * - :py:attr:`~fushi8`
            - Get or set the Function ID for user defined history variables
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

    from mat_elastic_viscoplastic_thermal import MatElasticViscoplasticThermal

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Coefficient of thermal expansion.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain.
   Card 2 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: Optional[float]


   
   Get or set the Effective plastic failure strain for erosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter QR1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter CR1.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter QR2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter CR2.
















   ..
       !! processed by numpydoc !!

.. py:property:: qx1
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter QX1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cx1
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter CX1.
















   ..
       !! processed by numpydoc !!

.. py:property:: qx2
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter QX2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cx2
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter CX2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Viscous material parameter C.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Viscous material parameter P.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce
   :type: Optional[float]


   
   Get or set the Load curve defining Young's modulus as a function of temperature.
   E on card 1 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpr
   :type: Optional[float]


   
   Get or set the Load curve defining Poisson's ratio as a function of temperature.
   PR on card 1 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsigy
   :type: Optional[float]


   
   Get or set the Load curve defining the initial yield stress as a function of temperature.
   SIGY on card 1 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr
   :type: Optional[float]


   
   Get or set the Load curve for scaling the isotropic hardening parameters QR1 and QR2 or the stress given by the load curve LCSS as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcx
   :type: Optional[float]


   
   Get or set the Load curve for scaling the isotropic hardening parameters QX1 and QX2 as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcalph
   :type: Optional[float]


   
   Get or set the Load curve defining the coefficient of thermal expansion as a function of temperature.
   ALPHA on card 1 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcc
   :type: Optional[int]


   
   Get or set the Load curve for scaling the viscous materal parameter C as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcp
   :type: Optional[int]


   
   Get or set the Load curve for scaling the viscous material parameter P as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature required if and only if LCALPH is given with a negative curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfail
   :type: Optional[float]


   
   Get or set the Load curve defining the plastic failure strain as a function of temperature. FAIL on card 1 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: nuhis
   :type: Optional[int]


   
   Get or set the Number of additional user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: t1phas
   :type: Optional[float]


   
   Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
















   ..
       !! processed by numpydoc !!

.. py:property:: t2phas
   :type: Optional[float]


   
   Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: Optional[float]


   
   Get or set the Optional tolerance for plasticity update. The default is 10-6 for solid elements and 10-3 for shells. This parameter overrides the default tolerance for all element types.
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi1
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi2
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi3
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi4
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi5
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi6
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi7
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: fushi8
   :type: Optional[int]


   
   Get or set the Function ID for user defined history variables
















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
   :value: 'ELASTIC_VISCOPLASTIC_THERMAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





