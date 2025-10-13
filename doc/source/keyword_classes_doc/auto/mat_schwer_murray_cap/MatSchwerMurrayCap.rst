





:class:`MatSchwerMurrayCap`
===========================


.. py:class:: mat_schwer_murray_cap.MatSchwerMurrayCap(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SCHWER_MURRAY_CAP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSchwerMurrayCap

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~shear`
            - Get or set the Shear modulus, G.
          * - :py:attr:`~bulk`
            - Get or set the Bulk modulus, K.
          * - :py:attr:`~grun`
            - Get or set the Gruneisen ratio (typically=0).
          * - :py:attr:`~shock`
            - Get or set the Shock velocity parameter (typically 0), S1.
          * - :py:attr:`~pore`
            - Get or set the Flag for pore collapse
          * - :py:attr:`~alpha`
            - Get or set the Shear failure parameter.
          * - :py:attr:`~theta`
            - Get or set the Shear failure parameter.
          * - :py:attr:`~gamma`
            - Get or set the Shear failure parameter.
          * - :py:attr:`~beta`
            - Get or set the Shear failure parameter.
          * - :py:attr:`~efit`
            - Get or set the Dilitation damage mechanics parameter (no damage = 1).
          * - :py:attr:`~ffit`
            - Get or set the Dilitation damage mechanics parameter (no damage = 0).
          * - :py:attr:`~alphan`
            - Get or set the Kinematic strain hardening parameter, Na.
          * - :py:attr:`~calpha`
            - Get or set the Kinematic straing hardening parameter, Ca.
          * - :py:attr:`~xo`
            - Get or set the Initial cap surface J1 (mean stress) axis intercept, X.
          * - :py:attr:`~irock`
            - Get or set the EQ.0: soils (cap can contract)
          * - :py:attr:`~secp`
            - Get or set the Shear enhanced compaction.
          * - :py:attr:`~afit`
            - Get or set the Ductile damage mechanics parameter (1=no damage).
          * - :py:attr:`~bfit`
            - Get or set the Ductile damage mechanics parameter (0=no damage).
          * - :py:attr:`~rdamo`
            - Get or set the Ductile damage mechanics parameter.
          * - :py:attr:`~w`
            - Get or set the Plastic Volume Strain parameter, W.
          * - :py:attr:`~d1`
            - Get or set the Plastic Volume Strain patameter, D1.
          * - :py:attr:`~d2`
            - Get or set the Plastic Volume Strain parameter, D2.
          * - :py:attr:`~nplot`
            - Get or set the History variable post-processed as effective plastic strain (See Table 1 for history variables available for plotting).
          * - :py:attr:`~epsmax`
            - Get or set the Maximum permitted strain increment (default=0)
          * - :py:attr:`~cfit`
            - Get or set the Brittle damage mechanics parameter (1=no damage).
          * - :py:attr:`~dfit`
            - Get or set the Brittle damage mechanics parameter (0=no damage).
          * - :py:attr:`~tfail`
            - Get or set the Tensile failure stress.
          * - :py:attr:`~failfg`
            - Get or set the Failure Flag, failed element:
          * - :py:attr:`~dbeta`
            - Get or set the Rounded vertices parameter.
          * - :py:attr:`~ddelta`
            - Get or set the Rounded vertices parameter.
          * - :py:attr:`~vptau`
            - Get or set the Viscoplasticity relaxation time parameter.
          * - :py:attr:`~alpha1`
            - Get or set the Torsion scaling parameter, a1.
          * - :py:attr:`~theta1`
            - Get or set the Torsion scaling parameter, theta1.
          * - :py:attr:`~gamma1`
            - Get or set the Torsion scaling parameter, gamma1.
          * - :py:attr:`~beta1`
            - Get or set the Torsion scaling parameter, beta1.
          * - :py:attr:`~alpha2`
            - Get or set the Tri-axial extension scaling parameter, a2.
          * - :py:attr:`~theta2`
            - Get or set the Tri-axial extension scaling parameter, thetha2.
          * - :py:attr:`~gamma2`
            - Get or set the Tri-axial extension scaling parameter, gamma2.
          * - :py:attr:`~beta2`
            - Get or set the Tri-axial extension scaling parameter, beta2.
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

    from mat_schwer_murray_cap import MatSchwerMurrayCap

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: shear
   :type: Optional[float]


   
   Get or set the Shear modulus, G.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Bulk modulus, K.
















   ..
       !! processed by numpydoc !!

.. py:property:: grun
   :type: Optional[float]


   
   Get or set the Gruneisen ratio (typically=0).
















   ..
       !! processed by numpydoc !!

.. py:property:: shock
   :type: Optional[float]


   
   Get or set the Shock velocity parameter (typically 0), S1.
















   ..
       !! processed by numpydoc !!

.. py:property:: pore
   :type: float


   
   Get or set the Flag for pore collapse
   EQ.0.0 for Pore collapse.
   EQ.1.0 for Constant bulk modulus (typical)
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Shear failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta
   :type: Optional[float]


   
   Get or set the Shear failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Shear failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Shear failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: efit
   :type: Optional[float]


   
   Get or set the Dilitation damage mechanics parameter (no damage = 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: ffit
   :type: Optional[float]


   
   Get or set the Dilitation damage mechanics parameter (no damage = 0).
















   ..
       !! processed by numpydoc !!

.. py:property:: alphan
   :type: Optional[float]


   
   Get or set the Kinematic strain hardening parameter, Na.
















   ..
       !! processed by numpydoc !!

.. py:property:: calpha
   :type: Optional[float]


   
   Get or set the Kinematic straing hardening parameter, Ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: xo
   :type: Optional[float]


   
   Get or set the Initial cap surface J1 (mean stress) axis intercept, X.
















   ..
       !! processed by numpydoc !!

.. py:property:: irock
   :type: float


   
   Get or set the EQ.0: soils (cap can contract)
   EQ.1: rock/concrete.
















   ..
       !! processed by numpydoc !!

.. py:property:: secp
   :type: Optional[float]


   
   Get or set the Shear enhanced compaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: afit
   :type: Optional[float]


   
   Get or set the Ductile damage mechanics parameter (1=no damage).
















   ..
       !! processed by numpydoc !!

.. py:property:: bfit
   :type: Optional[float]


   
   Get or set the Ductile damage mechanics parameter (0=no damage).
















   ..
       !! processed by numpydoc !!

.. py:property:: rdamo
   :type: Optional[float]


   
   Get or set the Ductile damage mechanics parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Plastic Volume Strain parameter, W.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Plastic Volume Strain patameter, D1.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Plastic Volume Strain parameter, D2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplot
   :type: Optional[float]


   
   Get or set the History variable post-processed as effective plastic strain (See Table 1 for history variables available for plotting).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsmax
   :type: float


   
   Get or set the Maximum permitted strain increment (default=0)
















   ..
       !! processed by numpydoc !!

.. py:property:: cfit
   :type: Optional[float]


   
   Get or set the Brittle damage mechanics parameter (1=no damage).
















   ..
       !! processed by numpydoc !!

.. py:property:: dfit
   :type: Optional[float]


   
   Get or set the Brittle damage mechanics parameter (0=no damage).
















   ..
       !! processed by numpydoc !!

.. py:property:: tfail
   :type: Optional[float]


   
   Get or set the Tensile failure stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: failfg
   :type: float


   
   Get or set the Failure Flag, failed element:
   EQ.0: stresses zeroed (use for ALE and EFG).
   EQ.1: removed from database (preferred).
















   ..
       !! processed by numpydoc !!

.. py:property:: dbeta
   :type: Optional[float]


   
   Get or set the Rounded vertices parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: ddelta
   :type: Optional[float]


   
   Get or set the Rounded vertices parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: vptau
   :type: Optional[float]


   
   Get or set the Viscoplasticity relaxation time parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Torsion scaling parameter, a1.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta1
   :type: Optional[float]


   
   Get or set the Torsion scaling parameter, theta1.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma1
   :type: Optional[float]


   
   Get or set the Torsion scaling parameter, gamma1.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta1
   :type: Optional[float]


   
   Get or set the Torsion scaling parameter, beta1.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Tri-axial extension scaling parameter, a2.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta2
   :type: Optional[float]


   
   Get or set the Tri-axial extension scaling parameter, thetha2.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma2
   :type: Optional[float]


   
   Get or set the Tri-axial extension scaling parameter, gamma2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: Optional[float]


   
   Get or set the Tri-axial extension scaling parameter, beta2.
















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
   :value: 'SCHWER_MURRAY_CAP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





