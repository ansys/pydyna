





:class:`Mat224Gys`
==================


.. py:class:: mat_224_gys.Mat224Gys(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_224_GYS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat224Gys

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~cp`
            - Get or set the Specific heat.
          * - :py:attr:`~tr`
            - Get or set the Room temperature.
          * - :py:attr:`~beta`
            - Get or set the Fraction of plastic work converted into heat (superseded by FWORK in *CONTROL_THERMAL_SOLVER if a coupled thermal/structural analysis):
          * - :py:attr:`~numint`
            - Get or set the Number of integration points which must fail before the element is deleted. Available for shells and solids.
          * - :py:attr:`~lck1`
            - Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) effective stress as a function of effective plastic strain for that rate.
          * - :py:attr:`~lckt`
            - Get or set the Table ID defining for each temperature value a load curve ID giving the (qu asi-static) effective stress as a function of effective plastic strain for that temperature.
          * - :py:attr:`~lcf`
            - Get or set the Load curve ID or table ID. The load curve specifies plastic failure strain as a function of triaxiality. The table specifies for each Lode parameter a load curve ID giving the plastic failure strain versus triaxiality for that Lode parameter. (Table option not yet generally supported).
          * - :py:attr:`~lcg`
            - Get or set the Load curve ID for specifying plastic failure strain as a function of plastic strain rate.
          * - :py:attr:`~lch`
            - Get or set the Load curve ID for specifying plastic failure strain as a function of temperature
          * - :py:attr:`~lci`
            - Get or set the Load curve ID or table ID. The load curve ID defines plastic failure strain as a function of element size. The table ID defines for each triaxiality a load curve ID giving the plastic failure strain versus element size for that triaxiality.
          * - :py:attr:`~lccr`
            - Get or set the Table ID. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) compressive yield stress as a function of plastic strain or effective plastic strain for that rate.
          * - :py:attr:`~lcct`
            - Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) compressive yield stress as a function of strain for that temperature. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
          * - :py:attr:`~lcsr`
            - Get or set the Table ID. The load curves define shear yield stress in function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) shear yield stress as a function of plastic strain or effective plastic strain for that rate.
          * - :py:attr:`~lcst`
            - Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) shear yield stress as a function of strain for that temperature. The load curves define shear yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
          * - :py:attr:`~iflag`
            - Get or set the Flag to specify abscissa for LCCR, LCCT, LCSR, LCST:
          * - :py:attr:`~sfiepm`
            - Get or set the Scale factor on the initial estimate of the plastic multiplier.
          * - :py:attr:`~niter`
            - Get or set the Number of secant iterations to be performed.
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

    from mat_224_gys import Mat224Gys

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
   GT.0.0: Constant value is used.
   LT.0.0: Temperature dependent Young’s modulus given by load curve ID = -E
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: tr
   :type: float


   
   Get or set the Room temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Fraction of plastic work converted into heat (superseded by FWORK in *CONTROL_THERMAL_SOLVER if a coupled thermal/structural analysis):
   GT.0.0: Constant value is used
   LT.0.0 : -BETA gives a load curve ID for strain rate dependence, a table ID for strain rateand temperature dependence, or a 3 - dimensional table ID for temperature(TABLE_3D), strain rate(TABLE) and plastic strain(CURVE) dependence, or a 4 - dimensional table ID for triaxiality(TABLE_4D), temperature(TABLE_3D), strain rate(TABLE) and plastic strain(CURVE) dependence.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: float


   
   Get or set the Number of integration points which must fail before the element is deleted. Available for shells and solids.
   LT.0.0: |NUMINT| is percentage of integration points/layers which must fail before element fails. For fully integrated shells, a methodology is used where a layer fails if one integrationpoint fails and then the given percentage of layers must fail before the element fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck1
   :type: int


   
   Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) effective stress as a function of effective plastic strain for that rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lckt
   :type: int


   
   Get or set the Table ID defining for each temperature value a load curve ID giving the (qu asi-static) effective stress as a function of effective plastic strain for that temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcf
   :type: int


   
   Get or set the Load curve ID or table ID. The load curve specifies plastic failure strain as a function of triaxiality. The table specifies for each Lode parameter a load curve ID giving the plastic failure strain versus triaxiality for that Lode parameter. (Table option not yet generally supported).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg
   :type: int


   
   Get or set the Load curve ID for specifying plastic failure strain as a function of plastic strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch
   :type: int


   
   Get or set the Load curve ID for specifying plastic failure strain as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: lci
   :type: int


   
   Get or set the Load curve ID or table ID. The load curve ID defines plastic failure strain as a function of element size. The table ID defines for each triaxiality a load curve ID giving the plastic failure strain versus element size for that triaxiality.
















   ..
       !! processed by numpydoc !!

.. py:property:: lccr
   :type: Optional[int]


   
   Get or set the Table ID. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) compressive yield stress as a function of plastic strain or effective plastic strain for that rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcct
   :type: Optional[int]


   
   Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) compressive yield stress as a function of strain for that temperature. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: Optional[int]


   
   Get or set the Table ID. The load curves define shear yield stress in function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) shear yield stress as a function of plastic strain or effective plastic strain for that rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcst
   :type: Optional[int]


   
   Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) shear yield stress as a function of strain for that temperature. The load curves define shear yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Flag to specify abscissa for LCCR, LCCT, LCSR, LCST:
   EQ.0:   Compressive and shear yields are given as functions of plastic strain as defined in Remark 1 (default).
   EQ.1 : Compressive and shear yields are given as functions of effective plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: sfiepm
   :type: float


   
   Get or set the Scale factor on the initial estimate of the plastic multiplier.
















   ..
       !! processed by numpydoc !!

.. py:property:: niter
   :type: int


   
   Get or set the Number of secant iterations to be performed.
















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
   :value: '224_GYS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





