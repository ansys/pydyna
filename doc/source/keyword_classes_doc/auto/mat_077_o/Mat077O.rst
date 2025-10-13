





:class:`Mat077O`
================


.. py:class:: mat_077_o.Mat077O(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_077_O keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat077O

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
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio ( => 0.49 is recommended, smaller values may not work and should not be used).
          * - :py:attr:`~n`
            - Get or set the Order of fit to the Ogden model, (currently <9, 2 generally works okay).  The constants generated during the fit are printed in the output file and can be directly input in future runs, thereby, saving the cost of performing the nonlinear fit.
          * - :py:attr:`~nv`
            - Get or set the Number of terms in fit. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
          * - :py:attr:`~g`
            - Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
          * - :py:attr:`~sigf`
            - Get or set the Limit stress for frequency independent, frictional, damping.
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor. The reference
          * - :py:attr:`~tbhys`
            - Get or set the Table ID for hysteresis.
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length.
          * - :py:attr:`~sw`
            - Get or set the Specimen width.
          * - :py:attr:`~st`
            - Get or set the Specimen thickness.
          * - :py:attr:`~lcid1`
            - Get or set the Load curve ID giving the force versus actual change in the gauge length.
          * - :py:attr:`~data`
            - Get or set the Type of experimental data:
          * - :py:attr:`~lcid2`
            - Get or set the Load curve ID of relaxation curve If constants beta-i are determined via a least squares fit.
          * - :py:attr:`~bstart`
            - Get or set the In the fit, beta-1 is set to zero, beta-2 is set to BSTART, beta-3 is 10 times beta-2, beta-4 is 100 times greater than beta-3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
          * - :py:attr:`~tramp`
            - Get or set the Optional ramp time for loading. If N=0, the constants MUi and ALPHAi have to be defined:
          * - :py:attr:`~mu1`
            - Get or set the mu-1, first shear modulus.
          * - :py:attr:`~mu2`
            - Get or set the mu-2, second shear modulus.
          * - :py:attr:`~mu3`
            - Get or set the mu-3, third shear modulus.
          * - :py:attr:`~mu4`
            - Get or set the mu-4, fourth shear modulus.
          * - :py:attr:`~mu5`
            - Get or set the mu-5, fifth shear modulus.
          * - :py:attr:`~mu6`
            - Get or set the mu-6, sixth shear modulus.
          * - :py:attr:`~mu7`
            - Get or set the mu-7, seventh shear modulus.
          * - :py:attr:`~mu8`
            - Get or set the mu-8, eighth shear modulus.
          * - :py:attr:`~alpha1`
            - Get or set the alpha-1, first exponent.
          * - :py:attr:`~alpha2`
            - Get or set the alpha-2, second exponent.
          * - :py:attr:`~alpha3`
            - Get or set the alpha-3, third exponent.
          * - :py:attr:`~alpha4`
            - Get or set the alpha-4, fourth exponent.
          * - :py:attr:`~alpha5`
            - Get or set the alpha-5, fifth exponent.
          * - :py:attr:`~alpha6`
            - Get or set the alpha-6, sixth exponent.
          * - :py:attr:`~alpha7`
            - Get or set the alpha-7, seventh exponent.
          * - :py:attr:`~alpha8`
            - Get or set the alpha-8, eighth exponent.
          * - :py:attr:`~constants`
            - Get the table of constants.
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

    from mat_077_o import Mat077O

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

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio ( => 0.49 is recommended, smaller values may not work and should not be used).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: int


   
   Get or set the Order of fit to the Ogden model, (currently <9, 2 generally works okay).  The constants generated during the fit are printed in the output file and can be directly input in future runs, thereby, saving the cost of performing the nonlinear fit.
















   ..
       !! processed by numpydoc !!

.. py:property:: nv
   :type: int


   
   Get or set the Number of terms in fit. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
   Default is set to 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigf
   :type: Optional[float]


   
   Get or set the Limit stress for frequency independent, frictional, damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the stress tensor. The reference
   geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
   EQ.0.0: off
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbhys
   :type: Optional[float]


   
   Get or set the Table ID for hysteresis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sgl
   :type: Optional[float]


   
   Get or set the Specimen gauge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: sw
   :type: Optional[float]


   
   Get or set the Specimen width.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: Optional[float]


   
   Get or set the Specimen thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid1
   :type: Optional[int]


   
   Get or set the Load curve ID giving the force versus actual change in the gauge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: data
   :type: float


   
   Get or set the Type of experimental data:
   EQ.1.0: uniaxial data (default),
   EQ.2.0: biaxial data.
   EQ.3.0: pure shear data
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: Optional[int]


   
   Get or set the Load curve ID of relaxation curve If constants beta-i are determined via a least squares fit.
   This model ignores the constant stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: bstart
   :type: Optional[float]


   
   Get or set the In the fit, beta-1 is set to zero, beta-2 is set to BSTART, beta-3 is 10 times beta-2, beta-4 is 100 times greater than beta-3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
















   ..
       !! processed by numpydoc !!

.. py:property:: tramp
   :type: Optional[float]


   
   Get or set the Optional ramp time for loading. If N=0, the constants MUi and ALPHAi have to be defined:
















   ..
       !! processed by numpydoc !!

.. py:property:: mu1
   :type: Optional[float]


   
   Get or set the mu-1, first shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu2
   :type: Optional[float]


   
   Get or set the mu-2, second shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu3
   :type: Optional[float]


   
   Get or set the mu-3, third shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu4
   :type: Optional[float]


   
   Get or set the mu-4, fourth shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu5
   :type: Optional[float]


   
   Get or set the mu-5, fifth shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu6
   :type: Optional[float]


   
   Get or set the mu-6, sixth shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu7
   :type: Optional[float]


   
   Get or set the mu-7, seventh shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu8
   :type: Optional[float]


   
   Get or set the mu-8, eighth shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the alpha-1, first exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the alpha-2, second exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the alpha-3, third exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha4
   :type: Optional[float]


   
   Get or set the alpha-4, fourth exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha5
   :type: Optional[float]


   
   Get or set the alpha-5, fifth exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha6
   :type: Optional[float]


   
   Get or set the alpha-6, sixth exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha7
   :type: Optional[float]


   
   Get or set the alpha-7, seventh exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha8
   :type: Optional[float]


   
   Get or set the alpha-8, eighth exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: constants
   :type: pandas.DataFrame


   
   Get the table of constants.
















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
   :value: '077_O'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





