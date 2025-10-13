





:class:`MatModifiedJohnsonCook`
===============================


.. py:class:: mat_modified_johnson_cook.MatModifiedJohnsonCook(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MODIFIED_JOHNSON_COOK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatModifiedJohnsonCook

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
          * - :py:attr:`~beta`
            - Get or set the Damage coupling parameter.
          * - :py:attr:`~xsi`
            - Get or set the Taylor-Quinney coefficient. Gives the portion of plastic work converted into heat (normally taken to be 0.9).
          * - :py:attr:`~cp`
            - Get or set the Specific heat.
          * - :py:attr:`~alpha`
            - Get or set the Thermal expansion coefficient.
          * - :py:attr:`~e0dot`
            - Get or set the User-defined strain rate normalization factor
          * - :py:attr:`~tr`
            - Get or set the Room temperature
          * - :py:attr:`~tm`
            - Get or set the Melt temperature
          * - :py:attr:`~t0`
            - Get or set the Initial temperature
          * - :py:attr:`~flag1`
            - Get or set the Constitutive relation flag used for parameters on card 3 and 4.
          * - :py:attr:`~flag2`
            - Get or set the Fracture criterion flag used for parameters on card 5
          * - :py:attr:`~a_siga`
            - Get or set the If FLAG1=0: Johnson-Cook yield stress A.
          * - :py:attr:`~b_b`
            - Get or set the If FLAG1=0: Johnson-Cook hardening parameter B.
          * - :py:attr:`~n_beta0`
            - Get or set the If FLAG1=0: Johnson-Cook hardening parameter
          * - :py:attr:`~c_beta1`
            - Get or set the If FLAG1=0: Johnson-Cook hardening parameter C
          * - :py:attr:`~m_na`
            - Get or set the Define only if FLAG1=0: Johnson-Cook thermal softening parameter m
          * - :py:attr:`~q1_a`
            - Get or set the If FLAG1=0: Voce hardening parameter Q1.
          * - :py:attr:`~c1_n`
            - Get or set the If FLAG1=0: Voce hardening parameter C1.
          * - :py:attr:`~q2_alpha0`
            - Get or set the If FLAG1=0: Voce hardening parameter Q2
          * - :py:attr:`~c2_alpha1`
            - Get or set the If FLAG1=0: Voce hardening parameter C2
          * - :py:attr:`~dc_dc`
            - Get or set the Critical damage parameter Dc. When the damage value D reaches this value, the element is eroded from the calculation.
          * - :py:attr:`~pd_wc`
            - Get or set the If FLAG2=0: Damage threshold.
          * - :py:attr:`~d1_na`
            - Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
          * - :py:attr:`~d2_na`
            - Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
          * - :py:attr:`~d3_na`
            - Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
          * - :py:attr:`~d4_na`
            - Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
          * - :py:attr:`~d5_na`
            - Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
          * - :py:attr:`~tc`
            - Get or set the Critical temperature parameter. When the temperature T reaches this value, the element is eroded from the simulation.
          * - :py:attr:`~tauc`
            - Get or set the Critical shear stress parameter. When the maximum shear stress reaches this value, the element is eroded from the simulation.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from mat_modified_johnson_cook import MatModifiedJohnsonCook

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

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Damage coupling parameter.
   EQ.0.0 No coupling between ductile damage and the constitutive relation.
   EQ.1.0 Full coupling between ductile damage and the constitutive relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: xsi
   :type: Optional[float]


   
   Get or set the Taylor-Quinney coefficient. Gives the portion of plastic work converted into heat (normally taken to be 0.9).
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Thermal expansion coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0dot
   :type: Optional[float]


   
   Get or set the User-defined strain rate normalization factor
















   ..
       !! processed by numpydoc !!

.. py:property:: tr
   :type: Optional[float]


   
   Get or set the Room temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: Optional[float]


   
   Get or set the Melt temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: t0
   :type: Optional[float]


   
   Get or set the Initial temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: flag1
   :type: Optional[float]


   
   Get or set the Constitutive relation flag used for parameters on card 3 and 4.
   EQ.0.0 Modified Johnson-Cook constitutive relation.
   EQ.1.0 Zerilli-Armstrong constitutive relation.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag2
   :type: Optional[float]


   
   Get or set the Fracture criterion flag used for parameters on card 5
   EQ.0.0 Modified Johnson-Cook fracture criterion.
   EQ.1.0 Cockcroft-Latharn fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: a_siga
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Johnson-Cook yield stress A.
   If FLAG1=1 :Zerilli-Armstrong parameter alfa_a.
















   ..
       !! processed by numpydoc !!

.. py:property:: b_b
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Johnson-Cook hardening parameter B.
   If FLAG1=1: Zerilli-Armstrong parameter B.
















   ..
       !! processed by numpydoc !!

.. py:property:: n_beta0
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Johnson-Cook hardening parameter
   If FLAG1=1: Zerilli-Armstrong parameter beta_0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c_beta1
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Johnson-Cook hardening parameter C
   If FLAG1=1: Zerilli-Armstrong parameter beta_1.
















   ..
       !! processed by numpydoc !!

.. py:property:: m_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG1=0: Johnson-Cook thermal softening parameter m
















   ..
       !! processed by numpydoc !!

.. py:property:: q1_a
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Voce hardening parameter Q1.
   If FLAG1=1 :Zerilli-Armstrong parameter alfa_a.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1_n
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Voce hardening parameter C1.
   If FLAG1=1: Zerilli-Armstrong parameter B.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2_alpha0
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Voce hardening parameter Q2
   If FLAG1=1: Zerilli-Armstrong parameter beta_0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2_alpha1
   :type: Optional[float]


   
   Get or set the If FLAG1=0: Voce hardening parameter C2
   If FLAG1=1: Zerilli-Armstrong parameter beta_1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc_dc
   :type: Optional[float]


   
   Get or set the Critical damage parameter Dc. When the damage value D reaches this value, the element is eroded from the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: pd_wc
   :type: Optional[float]


   
   Get or set the If FLAG2=0: Damage threshold.
   If FLAG2=1: Critical Cockcroft-Latham parameter Wc. When the plastic work per volume reaches this value, the element is eroded from the simulation..
















   ..
       !! processed by numpydoc !!

.. py:property:: d1_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: d4_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: d5_na
   :type: Optional[float]


   
   Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: Optional[float]


   
   Get or set the Critical temperature parameter. When the temperature T reaches this value, the element is eroded from the simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tauc
   :type: Optional[float]


   
   Get or set the Critical shear stress parameter. When the maximum shear stress reaches this value, the element is eroded from the simulation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'MODIFIED_JOHNSON_COOK'






