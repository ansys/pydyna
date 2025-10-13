





:class:`MatDamage3`
===================


.. py:class:: mat_damage_3.MatDamage3(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DAMAGE_3 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDamage3

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
            - Get or set the Initial yield stress, sigma-0.
          * - :py:attr:`~hardi`
            - Get or set the Isotropic hardening modulus, H.
          * - :py:attr:`~beta`
            - Get or set the Isotropic hardening parameter,  beta.  Set  beta=0 for linear isotropic hardening.
          * - :py:attr:`~lcss`
            - Get or set the Load curve or table ID defining effective stress as a function of effective plastic strain (and optionally temperature) for isotropic hardening.  The first abscissa value in each curve must be zero corresponding to the initial yield stress. The first ordinate value in each curve is the initial yield stress
          * - :py:attr:`~hardk1`
            - Get or set the Kinematic hardening modulus C1.
          * - :py:attr:`~gamma1`
            - Get or set the Kinematic hardening parameter  Gamma1.γ_j.  Set γ_j = 0 for linear kinematic hardening. Ignored if HARDKj = 0.
          * - :py:attr:`~hardk2`
            - Get or set the Kinematic hardening modulus C2.
          * - :py:attr:`~gamma2`
            - Get or set the Kinematic hardening parameter  Gamma2.
          * - :py:attr:`~src`
            - Get or set the Strain rate parameter, C, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
          * - :py:attr:`~srp`
            - Get or set the Strain rate parameter, P, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
          * - :py:attr:`~hardk3`
            - Get or set the Kinematic hardening modulus C3
          * - :py:attr:`~gamma3`
            - Get or set the Kinematic hardening parameter GAMMA3. Set GAMMA3 = 0 for linear kinematic hardening. Ignored if (HARDK3.EQ.0) is defined.
          * - :py:attr:`~idamage`
            - Get or set the Isotropic damage flag
          * - :py:attr:`~ids`
            - Get or set the Output stress flag
          * - :py:attr:`~idep`
            - Get or set the Damaged plastic strain
          * - :py:attr:`~epsd`
            - Get or set the Damage threshold Rd .  Damage accumulation begins when  R>Rd.
          * - :py:attr:`~s`
            - Get or set the Damage material constant S.
          * - :py:attr:`~t`
            - Get or set the Damage material constant t.  Default = 1
          * - :py:attr:`~dc`
            - Get or set the Critical damage value Dc .  When damage value reaches critical, the element is deleted from calculation.  Default = 0.5.
          * - :py:attr:`~khflg`
            - Get or set the Kinematic hardening flag
          * - :py:attr:`~hardk4`
            - Get or set the Kinematic hardening modulus.
          * - :py:attr:`~gamma5`
            - Get or set the Kinematic hardening parameter.γ_4.  Set γ_4 = 0 for linear kinematic hardening. Ignored if HARDK4 = 0.
          * - :py:attr:`~lckh`
            - Get or set the Load curve ID defining kinematic hardening when KHFLG > 0. Gives either (C_j,γ_j) values or stress as a function of plastic strain (ε_p,σ) depending on KHFLG.
          * - :py:attr:`~nkh`
            - Get or set the Number of kinematic hardening parameters when KHFLG > 0. Up to 10 back stresses can be used.
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

    from mat_damage_3 import MatDamage3

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
   LT.0:   -E gives the curve ID for E as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
   LT.0:   -v gives the curve ID for v as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress, sigma-0.
















   ..
       !! processed by numpydoc !!

.. py:property:: hardi
   :type: Optional[float]


   
   Get or set the Isotropic hardening modulus, H.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter,  beta.  Set  beta=0 for linear isotropic hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve or table ID defining effective stress as a function of effective plastic strain (and optionally temperature) for isotropic hardening.  The first abscissa value in each curve must be zero corresponding to the initial yield stress. The first ordinate value in each curve is the initial yield stress
















   ..
       !! processed by numpydoc !!

.. py:property:: hardk1
   :type: Optional[float]


   
   Get or set the Kinematic hardening modulus C1.
   LT.0:  -C_j gives the curve ID for C_j as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma1
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter  Gamma1.γ_j.  Set γ_j = 0 for linear kinematic hardening. Ignored if HARDKj = 0.
   LT.0:  -γ_j gives the curve ID for γ_j as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: hardk2
   :type: Optional[float]


   
   Get or set the Kinematic hardening modulus C2.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma2
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter  Gamma2.
















   ..
       !! processed by numpydoc !!

.. py:property:: src
   :type: Optional[float]


   
   Get or set the Strain rate parameter, C, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
   LT.0: -SRC gives the curve ID for C as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: srp
   :type: Optional[float]


   
   Get or set the Strain rate parameter, P, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
   LT.0: -SRP gives the curve ID for p as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: hardk3
   :type: Optional[float]


   
   Get or set the Kinematic hardening modulus C3
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma3
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter GAMMA3. Set GAMMA3 = 0 for linear kinematic hardening. Ignored if (HARDK3.EQ.0) is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: idamage
   :type: int


   
   Get or set the Isotropic damage flag
   EQ. 0: damage is inactivated
   EQ. 1: damage is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: ids
   :type: int


   
   Get or set the Output stress flag
   EQ. 0: undamaged stress output
   EQ. 1: damaged stress output.
















   ..
       !! processed by numpydoc !!

.. py:property:: idep
   :type: int


   
   Get or set the Damaged plastic strain
   EQ. 0: plastic strain is accumulated
   EQ. 1: damaged plastic strain is accumulated.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd
   :type: Optional[float]


   
   Get or set the Damage threshold Rd .  Damage accumulation begins when  R>Rd.
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Damage material constant S.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the Damage material constant t.  Default = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Critical damage value Dc .  When damage value reaches critical, the element is deleted from calculation.  Default = 0.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: khflg
   :type: int


   
   Get or set the Kinematic hardening flag
   EQ.0:   Use kinematic hardening parameters HARDKj and GAMMAj (default).
   EQ.1:   Kinematic hardening parameters(C_j,γ_j) given by load curve or table.NKH data points used(with a maximum of 10) in each curve.HARDKj and GAMMAj fields are ignored.
   EQ.2 : Fits NKH kinematic hardening parameters(C_j,γ_j) to uniaxial stress - strain data at constant temperature for a half - cycle, that is, it fits
   EQ.3 : Fits NKH kinematic hardening parameters(C_j, γ_j) to uniaxial stress - strain data for the tensile part of a stabilized cycle, that is, it fits,to N stress as a function of plastic strain data(ε_i ^ p, σ_i, T) given by the load curve or table LCHK.Here the first data point is chosen such that ε_1 ^ p = 0. HARDKj and GAMMAj fields are ignored.
   EQ.4 : Fits NKH kinematic hardening parameters(C_j, γ_j) to uniaxial stress - strain data for different stabilized cycles, that is, it fits,to max stress as a function of max plastic strain data(ε_i ^ p, σ_i, T) over N cycles, given by the load curve or table LCHK.HARDKj and GAMMAj fields are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: hardk4
   :type: Optional[float]


   
   Get or set the Kinematic hardening modulus.
   LT.0:  -C_4 gives the curve ID for C_4 as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma5
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter.γ_4.  Set γ_4 = 0 for linear kinematic hardening. Ignored if HARDK4 = 0.
   LT.0:  -γ_4 gives the curve ID for γ_4 as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lckh
   :type: Optional[int]


   
   Get or set the Load curve ID defining kinematic hardening when KHFLG > 0. Gives either (C_j,γ_j) values or stress as a function of plastic strain (ε_p,σ) depending on KHFLG.
















   ..
       !! processed by numpydoc !!

.. py:property:: nkh
   :type: Optional[int]


   
   Get or set the Number of kinematic hardening parameters when KHFLG > 0. Up to 10 back stresses can be used.
















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
   :value: 'DAMAGE_3'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





