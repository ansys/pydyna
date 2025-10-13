





:class:`MatSampLight`
=====================


.. py:class:: mat_samp_light.MatSampLight(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SAMP_LIGHT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSampLight

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
          * - :py:attr:`~emod`
            - Get or set the Young's modulus
          * - :py:attr:`~nue`
            - Get or set the Poisson's ratio
          * - :py:attr:`~lcemod`
            - Get or set the Load curve ID defining Young’s modulus as function of effective strain rate.. LCEMOD ≠ 0 activates viscoelasticity, see remark 3. The parameters BETA and RFILTF have to be defined too
          * - :py:attr:`~beta`
            - Get or set the Decay constant in viscoelastic law. See remark 3. BETA has the unit[1/time].
          * - :py:attr:`~lcid_t`
            - Get or set the Load curve or table ID giving the yield stress as a function of plastic strain.
          * - :py:attr:`~lcid_c`
            - Get or set the Optional load curve (or table) ID giving the yield stress as a function of plastic strain (and strain rate).
          * - :py:attr:`~ctflg`
            - Get or set the Curve treatment flag (for LCID-T, LCID-C, and LCID-
          * - :py:attr:`~rateop`
            - Get or set the Calculation of effective strain rate option:
          * - :py:attr:`~nuep`
            - Get or set the Plastic Poisson’s ratio: an estimated ratio of transversal to longitudinal plastic rate of deformation under uniaxial loading should be given.
          * - :py:attr:`~lcid_p`
            - Get or set the Load curve ID giving the plastic Poisson's ratio as a function of an equivalent plastic strain measure during uniaxial tensile and uniaxial compressive testing.The plastic strain measure on the abscissa is negative for compression and positive for tension.
          * - :py:attr:`~rfiltf`
            - Get or set the Smoothing factor on the effective strain rate.
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

    from mat_samp_light import MatSampLight

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

.. py:property:: emod
   :type: Optional[float]


   
   Get or set the Young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: nue
   :type: Optional[float]


   
   Get or set the Poisson's ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: lcemod
   :type: Optional[int]


   
   Get or set the Load curve ID defining Young’s modulus as function of effective strain rate.. LCEMOD ≠ 0 activates viscoelasticity, see remark 3. The parameters BETA and RFILTF have to be defined too
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Decay constant in viscoelastic law. See remark 3. BETA has the unit[1/time].
   If LCEMOD > >0 is used, a non-zero value for BETA is mandatory
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_t
   :type: Optional[int]


   
   Get or set the Load curve or table ID giving the yield stress as a function of plastic strain.
   These curves should be obtained from quasi-static and (optionally) dynamic uniaxial tensile tests.
   This input is mandatory.  If LCID-T is a table ID, the table values are effective strain rates
   , and a curve of yield stress versus plastic strain must be given for each of those strain rates.
   If the first value in the table is negative, LS-DYNA assumes that all the table values represent the natural logarithm of effective strain rate.
   When the highest effective strain rate is several orders of magnitude greater than the lowest strain rate,
   it is recommended that the natural log of strain rate be input in the table.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_c
   :type: int


   
   Get or set the Optional load curve (or table) ID giving the yield stress as a function of plastic strain (and strain rate).
   This curve (or table) should be obtained from uniaxial compression tests.
   If LCID-C is defined as a curve and LCID-T given as a table, then the rate dependence from the tension table is adopted in compression as well
















   ..
       !! processed by numpydoc !!

.. py:property:: ctflg
   :type: int


   
   Get or set the Curve treatment flag (for LCID-T, LCID-C, and LCID-
   EQ.0:   Rediscretized curves are used(default).We recommend usingIt is recommended to use this option together with an appropriate value of LCINT for accurate resolution of the curves(see * DEFINE_CURVE and *CONTROL_SOLUTION).
   EQ.1 : Original curve values from the input are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: rateop
   :type: int


   
   Get or set the Calculation of effective strain rate option:
   EQ.0:   Original method for calculating the effective strain rate
   EQ.2 : Improved method for calculating the effective strain rate.This method gives a slightly closer match to* MAT_SAMP - 1 and is thus recommended
















   ..
       !! processed by numpydoc !!

.. py:property:: nuep
   :type: Optional[float]


   
   Get or set the Plastic Poisson’s ratio: an estimated ratio of transversal to longitudinal plastic rate of deformation under uniaxial loading should be given.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_p
   :type: int


   
   Get or set the Load curve ID giving the plastic Poisson's ratio as a function of an equivalent plastic strain measure during uniaxial tensile and uniaxial compressive testing.The plastic strain measure on the abscissa is negative for compression and positive for tension.
   It is important to cover both tension and compression.  If LCID-P is given, NUEP is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: rfiltf
   :type: float


   
   Get or set the Smoothing factor on the effective strain rate.
















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
   :value: 'SAMP_LIGHT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





