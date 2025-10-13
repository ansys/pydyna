





:class:`MatPlasticityWithDamage`
================================


.. py:class:: mat_plasticity_with_damage.MatPlasticityWithDamage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PLASTICITY_WITH_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPlasticityWithDamage

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
            - Get or set the Yield stress.
          * - :py:attr:`~etan`
            - Get or set the Tangent modulus, ignored if (LCSS.GT.0) is defined.
          * - :py:attr:`~eppf`
            - Get or set the Plastic strain, fs, at which material softening begins (logritmic).
          * - :py:attr:`~tdel`
            - Get or set the Minimum time step size for automatic element deletion.
          * - :py:attr:`~c`
            - Get or set the Strain rate parameter, C, see keyword manual page 239 (volume two).
          * - :py:attr:`~p`
            - Get or set the Strain rate parameter, P, see keyword manual page 239 (volume two).
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored.
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID defining strain rate scaling effect on yield stress.
          * - :py:attr:`~eppfr`
            - Get or set the Plastic strain at which material ruptures (logrithmic).
          * - :py:attr:`~vp`
            - Get or set the Formulation for rate effects:
          * - :py:attr:`~lcdm`
            - Get or set the Load curve ID defining nonlinear damage curve.
          * - :py:attr:`~numint`
            - Get or set the Number of through thickness integration points which must fail before the element is deleted. (If zero, all points must fail.) The default of all integration points is not recommended since elements undergoing large strain are often not deleted due to nodal fiber rotations which limit strains at active integration points after most points have failed. Better results are obtained if NUMINT is set to 1 or a number less than one half of the number of through thickness points. For example, if four through thickness points are used, NUMINT should not exceed 2, even for fully integrated shells which have 16 integration points.
          * - :py:attr:`~eps1`
            - Get or set the First effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
          * - :py:attr:`~eps2`
            - Get or set the Second effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
          * - :py:attr:`~eps3`
            - Get or set the Third effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~eps4`
            - Get or set the Fourth effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~eps5`
            - Get or set the Fifth effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~eps6`
            - Get or set the Sixth effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~eps7`
            - Get or set the Seventh effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~eps8`
            - Get or set the Eighth effective plastic strain value (optional if SIGY is defined).
          * - :py:attr:`~es1`
            - Get or set the Corresponding yield stress value to EPS1
          * - :py:attr:`~es2`
            - Get or set the Corresponding yield stress value to EPS2
          * - :py:attr:`~es3`
            - Get or set the Corresponding yield stress value to EPS3
          * - :py:attr:`~es4`
            - Get or set the Corresponding yield stress value to EPS4
          * - :py:attr:`~es5`
            - Get or set the Corresponding yield stress value to EPS5
          * - :py:attr:`~es6`
            - Get or set the Corresponding yield stress value to EPS6
          * - :py:attr:`~es7`
            - Get or set the Corresponding yield stress value to EPS7
          * - :py:attr:`~es8`
            - Get or set the Corresponding yield stress value to EPS8
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

    from mat_plasticity_with_damage import MatPlasticityWithDamage

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


   
   Get or set the Yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Tangent modulus, ignored if (LCSS.GT.0) is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: eppf
   :type: float


   
   Get or set the Plastic strain, fs, at which material softening begins (logritmic).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdel
   :type: Optional[float]


   
   Get or set the Minimum time step size for automatic element deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate parameter, C, see keyword manual page 239 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Strain rate parameter, P, see keyword manual page 239 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: int


   
   Get or set the Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: int


   
   Get or set the Load curve ID defining strain rate scaling effect on yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: eppfr
   :type: float


   
   Get or set the Plastic strain at which material ruptures (logrithmic).
















   ..
       !! processed by numpydoc !!

.. py:property:: vp
   :type: Optional[float]


   
   Get or set the Formulation for rate effects:
   EQ.0.0: Scale yield stress (default),
   EQ.1.0: Viscoplastic formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdm
   :type: int


   
   Get or set the Load curve ID defining nonlinear damage curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: int


   
   Get or set the Number of through thickness integration points which must fail before the element is deleted. (If zero, all points must fail.) The default of all integration points is not recommended since elements undergoing large strain are often not deleted due to nodal fiber rotations which limit strains at active integration points after most points have failed. Better results are obtained if NUMINT is set to 1 or a number less than one half of the number of through thickness points. For example, if four through thickness points are used, NUMINT should not exceed 2, even for fully integrated shells which have 16 integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the First effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
   WARNING: If the first point is nonzero the yield stress is extrapolated to determine the initial yield. If this option is used SIGY and ETAN are ignored and may be input as zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: Optional[float]


   
   Get or set the Second effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: Optional[float]


   
   Get or set the Third effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps4
   :type: Optional[float]


   
   Get or set the Fourth effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps5
   :type: Optional[float]


   
   Get or set the Fifth effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps6
   :type: Optional[float]


   
   Get or set the Sixth effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps7
   :type: Optional[float]


   
   Get or set the Seventh effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps8
   :type: Optional[float]


   
   Get or set the Eighth effective plastic strain value (optional if SIGY is defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: es1
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS1
















   ..
       !! processed by numpydoc !!

.. py:property:: es2
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS2
















   ..
       !! processed by numpydoc !!

.. py:property:: es3
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS3
















   ..
       !! processed by numpydoc !!

.. py:property:: es4
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS4
















   ..
       !! processed by numpydoc !!

.. py:property:: es5
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS5
















   ..
       !! processed by numpydoc !!

.. py:property:: es6
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS6
















   ..
       !! processed by numpydoc !!

.. py:property:: es7
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS7
















   ..
       !! processed by numpydoc !!

.. py:property:: es8
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS8
















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
   :value: 'PLASTICITY_WITH_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





