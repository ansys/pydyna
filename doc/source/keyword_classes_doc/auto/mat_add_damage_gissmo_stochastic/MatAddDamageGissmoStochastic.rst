





:class:`MatAddDamageGissmoStochastic`
=====================================


.. py:class:: mat_add_damage_gissmo_stochastic.MatAddDamageGissmoStochastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_DAMAGE_GISSMO_STOCHASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddDamageGissmoStochastic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
          * - :py:attr:`~dtyp`
            - Get or set the DTYP is interpreted digit-wise as follows:
          * - :py:attr:`~refsz`
            - Get or set the Reference element size, for which an additional output of damage will be generated.
          * - :py:attr:`~numfip`
            - Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
          * - :py:attr:`~lcsdg`
            - Get or set the Failure strain curve/table or function:
          * - :py:attr:`~ecrit`
            - Get or set the Critical plastic strain (material instability); see below.
          * - :py:attr:`~dmgexp`
            - Get or set the Exponent for nonlinear damage accumulation, see remarks..
          * - :py:attr:`~dcrit`
            - Get or set the Damage threshold value (critical damage). If a Load curve of critical plastic strain or fixed value is given by ECRIT, input is ignored.
          * - :py:attr:`~fadexp`
            - Get or set the Exponent for damage-related stress fadeout.
          * - :py:attr:`~lcregd`
            - Get or set the Load curve ID or Table ID defining element size dependent regulari-zation factors for equivalent plastic strain to failure.
          * - :py:attr:`~instf`
            - Get or set the Flag for governing the behavior of instability measure, F, and fading exponent, FADEXP (see Remarks):
          * - :py:attr:`~lcsrs`
            - Get or set the Load curve ID defining failure strain scaling factor for LCSDG vs. strain rate. If the first strain rate value in the curve is negative,
          * - :py:attr:`~shrf`
            - Get or set the Reduction factor for regularization for shear stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
          * - :py:attr:`~biaxf`
            - Get or set the Reduction factor for regularization for biaxial stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
          * - :py:attr:`~lcdlim`
            - Get or set the Load curve ID defining damage limit values as a function of triaxiality. Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities..
          * - :py:attr:`~midfail`
            - Get or set the Mid-plane failure option for shell elements.
          * - :py:attr:`~hisvn`
            - Get or set the History variable used to evaluate the 3D table LCSDG:
          * - :py:attr:`~soft`
            - Get or set the Softening reduction factor for failure strain in crashfront elements. Crashfront elements are elements that are direct neighbors of failed (deleted) elements.
          * - :py:attr:`~lp2bi`
            - Get or set the Option to use a bending indicator instead of the Lode parameter. Everyhwere in this keyword’s manual description, the term “Lode parameter” can/should be replaced by the expression “bending indicator”, which is adopted from *MAT_258 (cf. variable Ω). Only available for shell elements.
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

    from mat_add_damage_gissmo_stochastic import MatAddDamageGissmoStochastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtyp
   :type: float


   
   Get or set the DTYP is interpreted digit-wise as follows:
   DTYP=[NM]=M+10×N
   M.EQ.0: damage is accumulated, but there is no coupling to flow stress and no failure.
   M.EQ.1: damage is accumulated, and element failure occurs for D=1.  Coupling of damage to flow stress depending on parameters, see remarks below.
   N.EQ.0: equivalent plastic strain is the driving quantity for the damage.  (To be more precise, it’s the history variable that LS-PrePost blindly labels as “plastic strain.”  What this history variable actually represents depends on the material model.)
   N.GT.0: the Nth additional history variable is the driving quantity for damage.  These additional history variables are the same ones flagged by the *DATABASE_EXTENT_BINARY keyword’s NEIPS and NEIPH fields.  For example, for solid elements with *MAT_187, setting N=6 causes volumetric plastic strain to be the driving quantity for the GISSMO damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: refsz
   :type: Optional[float]


   
   Get or set the Reference element size, for which an additional output of damage will be generated.
   This is necessary to ensure the applicability of resulting damage quantities when transferred to different mesh sizes.
















   ..
       !! processed by numpydoc !!

.. py:property:: numfip
   :type: float


   
   Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
   GT.0.0: Number of integration points which must fail before element is deleted.
   LT.0.0: Applies only to shells. |NUMFIP| is the percentage of layers which must fail before element fails.
   For shell formulations with 4 integration points per layer, the layer is considered failed if any of the integration points in the layer fails
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsdg
   :type: int


   
   Get or set the Failure strain curve/table or function:
   GT.0.0: Load curve ID or table ID.As a load curve, it defines equivalent plastic strain to failure as a function of triaxiality.As a table, it defines for each Lode parameter value(between - 1 and 1) a load curve ID giving the equivalent plastic strain to failure as a function of triaxiality for that Lode parameter value.With HISVN ≠ 0, a 3D table can be used, where failure strain is a function of the history variable(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).With HISVN = 0, a 3D table introduces thermal effects, that is, failure strain is a function of temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).As a 4D table, failure strain is a function of strain rate(TABLE_4D), temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).
   LT.0.0 : | LCSDG | is the ID of a function(*DEFINE_FUNCTION) with the arguments triaxiality η, Lode parameter L, plastic strain rate ε ̇^ p, temperature T, history variable HISVN ,and element size l_e : f(η,L,ε ̇ ^ p,T,HISVN,l_e).Note that the sequence of the arguments is important, not their names.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecrit
   :type: Optional[float]


   
   Get or set the Critical plastic strain (material instability); see below.
   LT.0.0: | ECRIT | is either a load curve ID defining critical equivalent plastic strain versus triaxiality or a table ID defining critical equivalent plastic strain as a function of triaxiality and Lode parameter(as in LCSDG).With HISVN ≠ 0, a 3D table can be used, where critical strain is a function of the history variable(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).With HISVN = 0, a 3D table introduces thermal effects, that is, critical strain is a function of temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).As a 4D table, critical strain is a function of strain rate(TABLE_4D), temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).
   EQ.0.0 : Fixed value DCRIT defining critical damage is read(see below).
   GT.0.0 : Fixed value for stress - state independent critical equivalent plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: dmgexp
   :type: float


   
   Get or set the Exponent for nonlinear damage accumulation, see remarks..
















   ..
       !! processed by numpydoc !!

.. py:property:: dcrit
   :type: Optional[float]


   
   Get or set the Damage threshold value (critical damage). If a Load curve of critical plastic strain or fixed value is given by ECRIT, input is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: fadexp
   :type: float


   
   Get or set the Exponent for damage-related stress fadeout.
   LT.0.0: |FADEXP|  is a load curve ID or table ID. As a load curve it gives the fading exponent as a function of element size. As a table, it specifies the fading exponent as a function triaxiality (TABLE) and element size (CURVE). For 3D tables, it specifies the fading exponent as a function Lode parameter (TABLE_3D), triaxiality (TABLE), and element size (CURVE).
   GT.0.0: Constant fading exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcregd
   :type: int


   
   Get or set the Load curve ID or Table ID defining element size dependent regulari-zation factors for equivalent plastic strain to failure.
   GT.0.0: Load curve ID (reg. factor vs. element size) or Table ID (reg. factor vs. element size curves vs. effective rate)
   LT.0.0: |LCREGD| is Table ID (reg. factor vs. element size curves vs. triaxiality) or a 3D table ID (regularization factor as function of Lode parameter, triaxiality, and element size).
   This table provides an alternative to the use of SHRF and BIAXF for defining the effect of triaxiality on element size regularization of equivalent plastic strain to failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: instf
   :type: Optional[int]


   
   Get or set the Flag for governing the behavior of instability measure, F, and fading exponent, FADEXP (see Remarks):
   EQ.0:   F is incrementally updated,and FADEXP, if from a table, is allowed to vary.
   EQ.1 : F is incrementally updated,and FADEXP is kept constant after F = 1.
   EQ.0 : F is only 0 or 1 (after ECRIT is reached),and FADEXP, if from a table, is allowed to vary.
   EQ.1 : F is only 0 or 1 (after ECRIT is reached),and FADEXP is kept constant after F = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsrs
   :type: Optional[int]


   
   Get or set the Load curve ID defining failure strain scaling factor for LCSDG vs. strain rate. If the first strain rate value in the curve is negative,
   it is assumed that all strain rate values are given as natural logarithm of the strain rate. The curve should not extrapolate to zero or failure may occur at low strain.
   GT.0:   scale ECRIT, too
   LT.0:   do not scale ECRIT.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: Optional[float]


   
   Get or set the Reduction factor for regularization for shear stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: biaxf
   :type: Optional[float]


   
   Get or set the Reduction factor for regularization for biaxial stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdlim
   :type: int


   
   Get or set the Load curve ID defining damage limit values as a function of triaxiality. Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities..
















   ..
       !! processed by numpydoc !!

.. py:property:: midfail
   :type: Optional[float]


   
   Get or set the Mid-plane failure option for shell elements.
   If active, then critical strain is only checked at the mid-plane integration point, i.e., an odd number for NIP should be used.
   The other integration points compute their damage, but no coupling to the stresses is done first.
   As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IP's are also checked.
   Those of them that are already above their critical value immediately start to reduce the stresses.
   Those who are still below critical still do not couple, only if they reach their criterion.
   EQ.0.0: Inactive,
   EQ.1.0: Active.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisvn
   :type: Optional[float]


   
   Get or set the History variable used to evaluate the 3D table LCSDG:
   GT.0.0: constant value
   LT.0.0 : the constant value found at position  where  is the location in the history array of * INITIAL_STRESS_ SHELL / SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: Optional[float]


   
   Get or set the Softening reduction factor for failure strain in crashfront elements. Crashfront elements are elements that are direct neighbors of failed (deleted) elements.
   EQ.0.0: inactive
   GT.0.0 : plastic failure strain,  (LCSDG),and critical plastic strain,  (ECRIT), will be scaled by SOFT.
   LT.0.0 : only plastic failure strain,  (LCSDG), will be scaled by | SOFT |.
















   ..
       !! processed by numpydoc !!

.. py:property:: lp2bi
   :type: Optional[float]


   
   Get or set the Option to use a bending indicator instead of the Lode parameter. Everyhwere in this keyword’s manual description, the term “Lode parameter” can/should be replaced by the expression “bending indicator”, which is adopted from *MAT_258 (cf. variable Ω). Only available for shell elements.
   EQ.0.0: inactive.
   EQ.1.0: active. Constant regularization (LCREGD) applied.
   EQ.2.0: active. Regularization (LCRGED) fully applied under pure membrane loading (Ω=0), but not at all under pure bending (Ω=1). Linear interpolation in between.
















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
   :value: 'ADD_DAMAGE_GISSMO_STOCHASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





