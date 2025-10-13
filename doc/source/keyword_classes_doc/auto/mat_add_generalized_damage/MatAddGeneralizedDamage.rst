





:class:`MatAddGeneralizedDamage`
================================


.. py:class:: mat_add_generalized_damage.MatAddGeneralizedDamage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_GENERALIZED_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddGeneralizedDamage

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID for which this generalized damage definition applies.
          * - :py:attr:`~idam`
            - Get or set the Flag for damage model.
          * - :py:attr:`~dtyp`
            - Get or set the Flag for damage behavior.
          * - :py:attr:`~refsz`
            - Get or set the Reference element size, for which an additional output of damage
          * - :py:attr:`~numfip`
            - Get or set the Number of failed integration points prior to element deletion. The default is unity.
          * - :py:attr:`~pddt`
            - Get or set the Pre-defined damage tensors. If non-zero, damage tensor coefficients D11 to D66 on cards 3 and 4 will be ignored.See remarks for details.
          * - :py:attr:`~nhis`
            - Get or set the Number of history variables as driving quantities (min = 1, max = 3).
          * - :py:attr:`~his1`
            - Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
          * - :py:attr:`~his2`
            - Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
          * - :py:attr:`~his3`
            - Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
          * - :py:attr:`~iflg1`
            - Get or set the Damage driving quantities
          * - :py:attr:`~iflg2`
            - Get or set the Damage strain coordinate system
          * - :py:attr:`~iflg3`
            - Get or set the Erosion criteria and damage coupling system
          * - :py:attr:`~d11`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d22`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d33`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d44`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d55`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d66`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d12`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d21`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d24`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d42`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d14`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~d41`
            - Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
          * - :py:attr:`~lcsdg`
            - Get or set the Load curve ID defining corresponding history value to failure vs. triaxiality.
          * - :py:attr:`~ecrit`
            - Get or set the Critical history value (material instability), see below.
          * - :py:attr:`~dmgexp`
            - Get or set the Exponent for nonlinear damage accumulation.
          * - :py:attr:`~dcrit`
            - Get or set the Damage threshold value (critical damage). If a Load curve of critical
          * - :py:attr:`~fadexp`
            - Get or set the Exponent for damage-related stress fadeout.
          * - :py:attr:`~lcreg`
            - Get or set the Load curve ID defining element size dependent regularization factors for history value to failure.
          * - :py:attr:`~lcsrs`
            - Get or set the Load curve ID defining failure history value scaling factor for
          * - :py:attr:`~shrf`
            - Get or set the Reduction factors for regularization at triaxiality = 0 (shear).
          * - :py:attr:`~biaxf`
            - Get or set the Reduction factors for regularization at triaxiality = 2/3 (biaxial).
          * - :py:attr:`~lcdlim`
            - Get or set the Load curve ID defining damage limit values as a function of triaxiality.Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities
          * - :py:attr:`~midfail`
            - Get or set the Mid-plane failure option for shell elements. If active, then critical strain is only checked at the mid-plane integration point, meaning an odd number for NIP should be used. Damage is computed at the other integration points, but no coupling to the stresses is done first. As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IPs are also checked (exception: MIDFAIL = 4).
          * - :py:attr:`~nfloc`
            - Get or set the Optional “local” number of failed integration points prior to element deletion. Overwrites the definition of NUMFIP for history variable HISn
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

    from mat_add_generalized_damage import MatAddGeneralizedDamage

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID for which this generalized damage definition applies.
















   ..
       !! processed by numpydoc !!

.. py:property:: idam
   :type: int


   
   Get or set the Flag for damage model.
   EQ.0: no damage model is used.
   EQ.1: GISSMO damage model..
















   ..
       !! processed by numpydoc !!

.. py:property:: dtyp
   :type: int


   
   Get or set the Flag for damage behavior.
   EQ.0: Damage is accumulated, no coupling to flow stress, no failure.
   EQ.1: Damage is accumulated, element failure occurs for D = 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: refsz
   :type: Optional[float]


   
   Get or set the Reference element size, for which an additional output of damage
   will be generated. This is necessary to ensure the applicability of
   resulting damage quantities when transferred to different mesh sizes.
















   ..
       !! processed by numpydoc !!

.. py:property:: numfip
   :type: float


   
   Get or set the Number of failed integration points prior to element deletion. The default is unity.
   LT.0: |NUMFIP| is the percentage of layers which must fail before element fails..
















   ..
       !! processed by numpydoc !!

.. py:property:: pddt
   :type: int


   
   Get or set the Pre-defined damage tensors. If non-zero, damage tensor coefficients D11 to D66 on cards 3 and 4 will be ignored.See remarks for details.
   EQ.0:   No pre-defined damage tensor is used.
   EQ.1:   Isotropic damage tensor.
   EQ.2:   2-parameter isotropic damage tensor for volumetric-deviatoric split.
   EQ.3:   Anisotropic damage tensor as in MAT_104 (FLAG = -1).
   EQ.4:   3-parameter damage tensor associated with IFLG1=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhis
   :type: int


   
   Get or set the Number of history variables as driving quantities (min = 1, max = 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: his1
   :type: int


   
   Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
   EQ.0.0: Equivalent plastic strain rate is the driving quantity for
   the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
   components of the plastic strain rate tensor are driving quantities for damage (see remarks).
   GT.0.0: The rate of the additional history variable HISn is the
   driving quantity for damage. IFLG1 should be set to 0.
   LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
   quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: his2
   :type: Optional[int]


   
   Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
   EQ.0.0: Equivalent plastic strain rate is the driving quantity for
   the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
   components of the plastic strain rate tensor are driving quantities for damage (see remarks).
   GT.0.0: The rate of the additional history variable HISn is the
   driving quantity for damage. IFLG1 should be set to 0.
   LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
   quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: his3
   :type: Optional[int]


   
   Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
   EQ.0.0: Equivalent plastic strain rate is the driving quantity for
   the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
   components of the plastic strain rate tensor are driving quantities for damage (see remarks).
   GT.0.0: The rate of the additional history variable HISn is the
   driving quantity for damage. IFLG1 should be set to 0.
   LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
   quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflg1
   :type: int


   
   Get or set the Damage driving quantities
   EQ.0.0: Rates of history variables HISn.
   EQ.1.0: Specific components of the plastic strain rate tensor, see remarks for details.
   EQ.2.0: Predefined functions of plastic strain rate components for
   orthotropic damage model, HISn inputs will be ignored, IFLG2 should be set to 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: iflg2
   :type: int


   
   Get or set the Damage strain coordinate system
   EQ.0.0: Local element system.
   EQ.1.0: Material system, only applicable for non-isotropic material models.Supported models for shell elements: all materials with AOPT feature. Supported models for solid elements: 22, 33, 41-50, 103, 122, 133, 157, 199, 233.
   EQ.2.0: Principal strain system (rotating).
   EQ.3.0: Principal strain system (fixed when instability/coupling starts)..
















   ..
       !! processed by numpydoc !!

.. py:property:: iflg3
   :type: int


   
   Get or set the Erosion criteria and damage coupling system
   EQ.0.0: Erosion occurs when one of the damage parameters
   computer reaches unity, the damage tensor components
   are based on the individual damage parameters d1 to d3.
   EQ.1.0: Erosion occurs when a single damage parameter D
   reaches unity, the damage tensor components are based   on this single damage parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: d11
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d22
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d33
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d44
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d55
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d66
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d12
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d21
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d24
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d42
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d14
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: d41
   :type: Optional[int]


   
   Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsdg
   :type: int


   
   Get or set the Load curve ID defining corresponding history value to failure vs. triaxiality.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecrit
   :type: Optional[float]


   
   Get or set the Critical history value (material instability), see below.
   LT.0.0: |ECRIT| is load curve ID defining critical history value vs. triaxiality.
   EQ.0.0: Fixed value DCRIT defining critical damage is read.
   GT.0.0: Fixed value for stress-state independent critical history value.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmgexp
   :type: float


   
   Get or set the Exponent for nonlinear damage accumulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dcrit
   :type: Optional[float]


   
   Get or set the Damage threshold value (critical damage). If a Load curve of critical
   history value or fixed value is given by ECRIT, input is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: fadexp
   :type: float


   
   Get or set the Exponent for damage-related stress fadeout.
   LT.0.0: |FADEXP| is load curve ID defining element-size dependent fading exponent.
   GT.0.0: Constant fading exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcreg
   :type: int


   
   Get or set the Load curve ID defining element size dependent regularization factors for history value to failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsrs
   :type: int


   
   Get or set the Load curve ID defining failure history value scaling factor for
   LCSDG vs. history value rate. If the first rate value in the curve is
   negative, it is assumed that all rate values are given as natural logarithm of the history rate.
   GT.0: scale ECRIT, too
   LT.0: do not scale ECRIT.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: Optional[float]


   
   Get or set the Reduction factors for regularization at triaxiality = 0 (shear).
















   ..
       !! processed by numpydoc !!

.. py:property:: biaxf
   :type: Optional[float]


   
   Get or set the Reduction factors for regularization at triaxiality = 2/3 (biaxial).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdlim
   :type: int


   
   Get or set the Load curve ID defining damage limit values as a function of triaxiality.Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities
















   ..
       !! processed by numpydoc !!

.. py:property:: midfail
   :type: float


   
   Get or set the Mid-plane failure option for shell elements. If active, then critical strain is only checked at the mid-plane integration point, meaning an odd number for NIP should be used. Damage is computed at the other integration points, but no coupling to the stresses is done first. As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IPs are also checked (exception: MIDFAIL = 4).
   EQ.0.0: Inactive
   EQ.1.0 : Active.The stresses immediately begin to reduce for non - mid - plane IPs that are already above their critical value.Coupling only occurs for IPs that reach their criterion.
   EQ.2.0 : Active.The stresses immediately begin to reduce for all the non - mid - plane IPs.NUMFIP is active
   EQ.3.0 : Active.Same as 2, but when D = 1 is reached in the middle integration point, the element is eroded instantaneously.NUMFIP is disregarded.
   EQ.4.0 : Active.Damage and failure is applied only on the midpoint.When D = 1 on the midpoint, the element is eroded.NUMFIP is disregarded.Integration points away from the midplane see no stress reduction and no failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfloc
   :type: Optional[float]


   
   Get or set the Optional “local” number of failed integration points prior to element deletion. Overwrites the definition of NUMFIP for history variable HISn
















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
   :value: 'ADD_GENERALIZED_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





