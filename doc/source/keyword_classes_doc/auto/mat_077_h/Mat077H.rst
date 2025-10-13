





:class:`Mat077H`
================


.. py:class:: mat_077_h.Mat077H(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_077_H keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat077H

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
            - Get or set the Poisson's ratio (> .49 is recommended, smaller values may not work
          * - :py:attr:`~n`
            - Get or set the Number of constants to solve for:,
          * - :py:attr:`~nv`
            - Get or set the Number of Prony series terms in fit. The default is 6. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
          * - :py:attr:`~g`
            - Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
          * - :py:attr:`~sigf`
            - Get or set the Limit stress for frequency independent, frictional, damping.
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor. The reference
          * - :py:attr:`~tbhys`
            - Get or set the Table ID for hysteresis, see Remarks.
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length.
          * - :py:attr:`~sw`
            - Get or set the Specimen width.
          * - :py:attr:`~st`
            - Get or set the Specimen thickness.
          * - :py:attr:`~lcid1`
            - Get or set the Load curve ID giving the force versus actual change in the gauge length.
          * - :py:attr:`~data`
            - Get or set the Type of experimental data.
          * - :py:attr:`~lcid2`
            - Get or set the Load curve ID of relaxation curve.
          * - :py:attr:`~bstart`
            - Get or set the In the fit, β1 is set to zero, β2 is set to BSTART, β3 is 10 times β2, β4 is 100 times grater than β3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
          * - :py:attr:`~tramp`
            - Get or set the Optional ramp time for loading.
          * - :py:attr:`~c10`
            - Get or set the C10
          * - :py:attr:`~c01`
            - Get or set the C01
          * - :py:attr:`~c11`
            - Get or set the C11
          * - :py:attr:`~c20`
            - Get or set the C20
          * - :py:attr:`~c02`
            - Get or set the C02
          * - :py:attr:`~c30`
            - Get or set the C30
          * - :py:attr:`~therml`
            - Get or set the Flag for the thermal option. If THERML>0.0, then G, SIGF, C10 and C01 specify curve IDs giving the values as functions of temperature, otherwise they specify the constants.
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

    from mat_077_h import Mat077H

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


   
   Get or set the Poisson's ratio (> .49 is recommended, smaller values may not work
   and should not be used). If this is set to a negative number, then the
   absolute value is used and an extra card is read for Mullins effect.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: int


   
   Get or set the Number of constants to solve for:,
   EQ.1: Solve for C10 and C01,
   EQ.2: Solve for C10, C01, C11, C20, and C02,
   EQ.3: Solve for C10, C01, C11, C20, C02, and C30
















   ..
       !! processed by numpydoc !!

.. py:property:: nv
   :type: Optional[int]


   
   Get or set the Number of Prony series terms in fit. The default is 6. Currently, the maximum number is set to 6. Values less than 6, possibly 3-5 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
















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
   geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
   EQ.0.0: off,
   EQ.1.0: on..
















   ..
       !! processed by numpydoc !!

.. py:property:: tbhys
   :type: Optional[float]


   
   Get or set the Table ID for hysteresis, see Remarks.
















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
   :type: Optional[float]


   
   Get or set the Type of experimental data.
   EQ.0.0: uniaxial data (Only option for this model)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: Optional[int]


   
   Get or set the Load curve ID of relaxation curve.
   If constants βi are determined via a least squares fit. This relaxation curve is shown in Figure 20.25. This model ignores the constant stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: bstart
   :type: Optional[float]


   
   Get or set the In the fit, β1 is set to zero, β2 is set to BSTART, β3 is 10 times β2, β4 is 100 times grater than β3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
















   ..
       !! processed by numpydoc !!

.. py:property:: tramp
   :type: Optional[float]


   
   Get or set the Optional ramp time for loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10
   :type: Optional[float]


   
   Get or set the C10
















   ..
       !! processed by numpydoc !!

.. py:property:: c01
   :type: Optional[float]


   
   Get or set the C01
















   ..
       !! processed by numpydoc !!

.. py:property:: c11
   :type: Optional[float]


   
   Get or set the C11
















   ..
       !! processed by numpydoc !!

.. py:property:: c20
   :type: Optional[float]


   
   Get or set the C20
















   ..
       !! processed by numpydoc !!

.. py:property:: c02
   :type: Optional[float]


   
   Get or set the C02
















   ..
       !! processed by numpydoc !!

.. py:property:: c30
   :type: Optional[float]


   
   Get or set the C30
















   ..
       !! processed by numpydoc !!

.. py:property:: therml
   :type: Optional[float]


   
   Get or set the Flag for the thermal option. If THERML>0.0, then G, SIGF, C10 and C01 specify curve IDs giving the values as functions of temperature, otherwise they specify the constants.
















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
   :value: '077_H'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





