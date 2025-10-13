





:class:`MatMomentCurvatureBeam`
===============================


.. py:class:: mat_moment_curvature_beam.MatMomentCurvatureBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MOMENT_CURVATURE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatMomentCurvatureBeam

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
            - Get or set the Young's modulus.  This variable controls the time step size and must be chosen carefully.  Increasing the value of E will decrease the time step size.
          * - :py:attr:`~elaf`
            - Get or set the Load curve ID for the axial force-strain curve
          * - :py:attr:`~epflg`
            - Get or set the Function flag
          * - :py:attr:`~cta`
            - Get or set the Type of axial force-strain, moment-curvature, and torque-twist rate curves
          * - :py:attr:`~ctb`
            - Get or set the
          * - :py:attr:`~ctt`
            - Get or set the
          * - :py:attr:`~n1`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n2`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n3`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n4`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric..
          * - :py:attr:`~n5`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n6`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n7`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~n8`
            - Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
          * - :py:attr:`~lcms1`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
          * - :py:attr:`~lcms2`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcms3`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcms4`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
          * - :py:attr:`~lcms5`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcms6`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcms7`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcms8`
            - Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
          * - :py:attr:`~lcmt1`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
          * - :py:attr:`~lcmt2`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lcmt3`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lcmt4`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
          * - :py:attr:`~lcmt5`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lcmt6`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lcmt7`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lcmt8`
            - Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
          * - :py:attr:`~lct1`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
          * - :py:attr:`~lct2`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~lct3`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~lct4`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
          * - :py:attr:`~lct5`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~lct6`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~lct7`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~lct8`
            - Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
          * - :py:attr:`~cfa`
            - Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value.
          * - :py:attr:`~cfb`
            - Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
          * - :py:attr:`~cft`
            - Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
          * - :py:attr:`~hrule`
            - Get or set the Hardening rule, for multi-linear plastic analysis only.
          * - :py:attr:`~reps`
            - Get or set the Rupture effective plastic axial strain
          * - :py:attr:`~rbeta`
            - Get or set the Rupture effective plastic twist rate
          * - :py:attr:`~rcapay`
            - Get or set the Rupture effective plastic curvature about axis S
          * - :py:attr:`~rcapaz`
            - Get or set the Rupture effective plastic curvature about axis T
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

    from mat_moment_curvature_beam import MatMomentCurvatureBeam

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


   
   Get or set the Young's modulus.  This variable controls the time step size and must be chosen carefully.  Increasing the value of E will decrease the time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: elaf
   :type: Optional[int]


   
   Get or set the Load curve ID for the axial force-strain curve
















   ..
       !! processed by numpydoc !!

.. py:property:: epflg
   :type: int


   
   Get or set the Function flag
   EQ.0.0: nonlinear elastic analysis
   EQ.1.0: multi-linear plastic analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: cta
   :type: Optional[float]


   
   Get or set the Type of axial force-strain, moment-curvature, and torque-twist rate curves
   EQ.0.0: curve is symmetric
   EQ.1.0: curve is asymmetric
   For symmetric curves, all data point must be in the first quadrant and at least three data points need to be given, starting from the origin, ensued by the yield point.
   For asymmetric curves, at least five data points are needed and exactly one point must be at the origin.  The two points on both sides of the origin record the positive and negative yield points.
   The last data point(s) has no physical meaning: it serves only as a control point for inter or extrapolation.
   The curves are input by the user and treated in LS-DYNA as a linearly piecewise function.  The curves must be monotonically increasing, while the slopes must be monotonically decreasing.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctb
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: ctt
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric..
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: Optional[float]


   
   Get or set the Axial forces at which moment-curvature curves are given  The axial forces must be ordered monotonically increasing.  At least two axial forces must be defined if the curves are symmetric.  At least three axial forces must be defined if the curves are asymmetric.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms1
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms2
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms3
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms4
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms5
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms6
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms7
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcms8
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis S under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt1
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt2
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt3
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt4
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt5
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt6
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt7
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt8
   :type: Optional[int]


   
   Get or set the Load curve IDs for the moment-curvature curves about axis T under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct1
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lct2
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct3
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct4
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces..
















   ..
       !! processed by numpydoc !!

.. py:property:: lct5
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct6
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct7
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct8
   :type: Optional[int]


   
   Get or set the Load curve IDs for the torque-twist rate curves under corresponding axial forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfa
   :type: float


   
   Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfb
   :type: float


   
   Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
















   ..
       !! processed by numpydoc !!

.. py:property:: cft
   :type: float


   
   Get or set the For multi-linear plastic analysis only.  Ratio of axial, bending and torsional elastic rigidities to their initial values, no less than 1.0 in value
















   ..
       !! processed by numpydoc !!

.. py:property:: hrule
   :type: Optional[float]


   
   Get or set the Hardening rule, for multi-linear plastic analysis only.
   EQ.0.0: isotropic hardening
   EQ.1.0: kinematic hardening
   In between: mixed hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: reps
   :type: float


   
   Get or set the Rupture effective plastic axial strain
















   ..
       !! processed by numpydoc !!

.. py:property:: rbeta
   :type: float


   
   Get or set the Rupture effective plastic twist rate
















   ..
       !! processed by numpydoc !!

.. py:property:: rcapay
   :type: float


   
   Get or set the Rupture effective plastic curvature about axis S
















   ..
       !! processed by numpydoc !!

.. py:property:: rcapaz
   :type: float


   
   Get or set the Rupture effective plastic curvature about axis T
















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
   :value: 'MOMENT_CURVATURE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





