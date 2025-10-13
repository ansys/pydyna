





:class:`MatQuasilinearViscoelastic`
===================================


.. py:class:: mat_quasilinear_viscoelastic.MatQuasilinearViscoelastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_QUASILINEAR_VISCOELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatQuasilinearViscoelastic

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
          * - :py:attr:`~k`
            - Get or set the Bulk modulus.
          * - :py:attr:`~lc1`
            - Get or set the Load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
          * - :py:attr:`~lc2`
            - Get or set the Load curve ID that defines the instantaneous elastic response in shear. This curve is used to fit the coefficients Ci. If zero, define the coefficients directly. The latter is recommended.
          * - :py:attr:`~n`
            - Get or set the Number of terms used in the Prony series, a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number is LC1 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
          * - :py:attr:`~gstart`
            - Get or set the Starting value for the least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero.
          * - :py:attr:`~m`
            - Get or set the Number of terms used to determine the instantaneous elastic response. Define this number if LC2 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
          * - :py:attr:`~so`
            - Get or set the Strain (logarithmic) output option to be plotted as component 7 in LS-TAURUS (D3PLOT file) which is the effective plastic strain component. The maximum values are updated for each element each time step:
          * - :py:attr:`~e_min`
            - Get or set the Minimum strain used to generate the load curve from Ci. The default range is -0.9 to 5.1. The computed solution will be more accurate if the user specifies the range used to fit the Ci. Linear extrapolation is used outside the specified range
          * - :py:attr:`~e_max`
            - Get or set the Maximum strain used to generate the load curve from Ci.
          * - :py:attr:`~gama1`
            - Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER and Figure 23.181.1.
          * - :py:attr:`~gama2`
            - Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER
          * - :py:attr:`~eh`
            - Get or set the Damage parameter, see *MAT_SIMPLIFIED_RUBBER
          * - :py:attr:`~form`
            - Get or set the Formulation of model. FORM=0 gives the original model developed by Fung, which always relaxes to a zero stress state as time approaches infinity, and FORM=1 gives the alternative model, which relaxes to the quasi-static elastic response. In general, the two formulations won't give the same responses.  Formulation, FORM=-1, is an improvement on FORM=0 where the instantaneous elastic response is used in the viscoelastic stress update, not just in the relaxation, as in FORM=0.  Consequently, the constants for the elastic response do not need to be scaled
          * - :py:attr:`~g1`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta1`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g2`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta2`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g3`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta3`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g4`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta4`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g5`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta5`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g6`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta6`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g7`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta7`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g8`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta8`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g9`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta9`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g10`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta10`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g11`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta11`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~g12`
            - Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
          * - :py:attr:`~beta12`
            - Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
          * - :py:attr:`~c1`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
          * - :py:attr:`~c2`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
          * - :py:attr:`~c3`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
          * - :py:attr:`~c4`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
          * - :py:attr:`~c5`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
          * - :py:attr:`~c6`
            - Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
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

    from mat_quasilinear_viscoelastic import MatQuasilinearViscoelastic

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

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: int


   
   Get or set the Load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: int


   
   Get or set the Load curve ID that defines the instantaneous elastic response in shear. This curve is used to fit the coefficients Ci. If zero, define the coefficients directly. The latter is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the Number of terms used in the Prony series, a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number is LC1 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
















   ..
       !! processed by numpydoc !!

.. py:property:: gstart
   :type: Optional[float]


   
   Get or set the Starting value for the least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the Number of terms used to determine the instantaneous elastic response. Define this number if LC2 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
















   ..
       !! processed by numpydoc !!

.. py:property:: so
   :type: float


   
   Get or set the Strain (logarithmic) output option to be plotted as component 7 in LS-TAURUS (D3PLOT file) which is the effective plastic strain component. The maximum values are updated for each element each time step:
   EQ.0.0: maximum principal strain that occurs during the calculation,
   EQ.1.0: maximum magnitude of the principal strain values that occurs during the calculation,
   EQ.2.0: maximum effective strain that occurs during the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: e_min
   :type: float


   
   Get or set the Minimum strain used to generate the load curve from Ci. The default range is -0.9 to 5.1. The computed solution will be more accurate if the user specifies the range used to fit the Ci. Linear extrapolation is used outside the specified range
















   ..
       !! processed by numpydoc !!

.. py:property:: e_max
   :type: float


   
   Get or set the Maximum strain used to generate the load curve from Ci.
















   ..
       !! processed by numpydoc !!

.. py:property:: gama1
   :type: Optional[float]


   
   Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER and Figure 23.181.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: gama2
   :type: Optional[float]


   
   Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER
















   ..
       !! processed by numpydoc !!

.. py:property:: eh
   :type: Optional[float]


   
   Get or set the Damage parameter, see *MAT_SIMPLIFIED_RUBBER
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Formulation of model. FORM=0 gives the original model developed by Fung, which always relaxes to a zero stress state as time approaches infinity, and FORM=1 gives the alternative model, which relaxes to the quasi-static elastic response. In general, the two formulations won't give the same responses.  Formulation, FORM=-1, is an improvement on FORM=0 where the instantaneous elastic response is used in the viscoelastic stress update, not just in the relaxation, as in FORM=0.  Consequently, the constants for the elastic response do not need to be scaled
















   ..
       !! processed by numpydoc !!

.. py:property:: g1
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta1
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g2
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g3
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta3
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g4
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta4
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g5
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta5
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g6
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta6
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g7
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta7
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g8
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta8
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g9
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta9
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g10
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta10
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g11
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta11
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta12
   :type: Optional[float]


   
   Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
















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
   :value: 'QUASILINEAR_VISCOELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





