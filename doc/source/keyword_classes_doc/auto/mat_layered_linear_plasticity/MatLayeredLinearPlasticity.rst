





:class:`MatLayeredLinearPlasticity`
===================================


.. py:class:: mat_layered_linear_plasticity.MatLayeredLinearPlasticity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_LAYERED_LINEAR_PLASTICITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatLayeredLinearPlasticity

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
            - Get or set the Tangent modulus.
          * - :py:attr:`~fail`
            - Get or set the Failure flag:
          * - :py:attr:`~tdel`
            - Get or set the Minimum time step size for automatic element deletion
          * - :py:attr:`~c`
            - Get or set the Strain rate parameter,
          * - :py:attr:`~p`
            - Get or set the Strain rate parameter,
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID or Table ID. Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored. The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value. The strain rate parameters: C and P;the curve ID, LCSR; EPS1-EPS8 and ES1-ES8 are ignored if a Table ID is defined.
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID defining strain rate scaling effect on yield stress.
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
            - Get or set the Corresponding yield stress value to EPS1.
          * - :py:attr:`~es2`
            - Get or set the Corresponding yield stress value to EPS2.
          * - :py:attr:`~es3`
            - Get or set the Corresponding yield stress value to EPS3.
          * - :py:attr:`~es4`
            - Get or set the Corresponding yield stress value to EPS4.
          * - :py:attr:`~es5`
            - Get or set the Corresponding yield stress value to EPS5.
          * - :py:attr:`~es6`
            - Get or set the Corresponding yield stress value to EPS6.
          * - :py:attr:`~es7`
            - Get or set the Corresponding yield stress value to EPS7.
          * - :py:attr:`~es8`
            - Get or set the Corresponding yield stress value to EPS8.
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

    from mat_layered_linear_plasticity import MatLayeredLinearPlasticity

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


   
   Get or set the Tangent modulus.
   Ignored if LCSS.GT.0 is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: float


   
   Get or set the Failure flag:
   LT.0.0: User defined failure subroutine, matusr_24 in dyn21.F, is called to determine failure.
   EQ.0.0 : Failure is not considered.This option is recommended if failure is not of interest since many calculations will be saved.
   GT.0.0 : Plastic strain to failure.When the plastic strain reaches this value, the element is deleted from the calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: tdel
   :type: Optional[float]


   
   Get or set the Minimum time step size for automatic element deletion
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate parameter,
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Strain rate parameter,
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID. Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored. The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value. The strain rate parameters: C and P;the curve ID, LCSR; EPS1-EPS8 and ES1-ES8 are ignored if a Table ID is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: Optional[int]


   
   Get or set the Load curve ID defining strain rate scaling effect on yield stress.
















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


   
   Get or set the Corresponding yield stress value to EPS1.
















   ..
       !! processed by numpydoc !!

.. py:property:: es2
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS2.
















   ..
       !! processed by numpydoc !!

.. py:property:: es3
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS3.
















   ..
       !! processed by numpydoc !!

.. py:property:: es4
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS4.
















   ..
       !! processed by numpydoc !!

.. py:property:: es5
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS5.
















   ..
       !! processed by numpydoc !!

.. py:property:: es6
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS6.
















   ..
       !! processed by numpydoc !!

.. py:property:: es7
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS7.
















   ..
       !! processed by numpydoc !!

.. py:property:: es8
   :type: Optional[float]


   
   Get or set the Corresponding yield stress value to EPS8.
















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
   :value: 'LAYERED_LINEAR_PLASTICITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





