





:class:`MatAddErosion`
======================


.. py:class:: mat_add_erosion.MatAddErosion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_EROSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddErosion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which this erosion definition applies.
          * - :py:attr:`~excl`
            - Get or set the The exclusion number, which applies to the failure values defined on Cards 1, 2, and 7.
          * - :py:attr:`~mxpres`
            - Get or set the Maximum pressure at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
          * - :py:attr:`~mneps`
            - Get or set the Minimum principal strain at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
          * - :py:attr:`~effeps`
            - Get or set the Maximum effective strain at failure, . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
          * - :py:attr:`~voleps`
            - Get or set the Volumetric strain at failure,  .  VOLEPS can be a positive or negative number depending on whether the failure is in tension or compression, respectively.  If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
          * - :py:attr:`~numfip`
            - Get or set the Number or percentage of failed integration points prior to element deletion (default is 1).  See Remark 2.
          * - :py:attr:`~ncs`
            - Get or set the Number of failure conditions to satisfy before failure occurs.  For example, if SIGP1 and SIGVM are defined and if NCS=2, both failure criteria must be met before element deletion can occur.  The default is set to unity.
          * - :py:attr:`~mnpres`
            - Get or set the Minimum pressure at failure,
          * - :py:attr:`~sigp1`
            - Get or set the Maximum principal stress at failur, Sigma max .
          * - :py:attr:`~sigvm`
            - Get or set the Equivalent stress at failure, The equivalent stress at failure is made a function of the effective strain rate by setting SIGVM to the negative of the appropriate load curve ID, Sigma max
          * - :py:attr:`~mxeps`
            - Get or set the Variable to invoke a failure criterion based on maximum principal strain.
          * - :py:attr:`~epssh`
            - Get or set the Tensorial shear strain at failure, r max .
          * - :py:attr:`~sigth`
            - Get or set the Threshold stress, Sigma 0 .
          * - :py:attr:`~impulse`
            - Get or set the Stress impulse for failure, K f.
          * - :py:attr:`~failtm`
            - Get or set the Failure time. When the problem time exceeds the failure time, the material is removed.
          * - :py:attr:`~idam`
            - Get or set the Flag for damage model.
          * - :py:attr:`~lcregd`
            - Get or set the Load curve ID defining element size dependent regularization factors for equivalent plastic strain to failure.
          * - :py:attr:`~lcfld`
            - Get or set the Load curve ID or Table ID. Load curve defines the Forming Limit Diagram, where minor engineering strains in percent are defined as abscissa values and major engineering strains in percent are defined as ordinate values. Table defines for each strain rate an associated FLD curve. The forming limit diagram is shown in Figure Error! Reference source not found.. In defining the curve, list pairs of minor and major strains starting with the left most point and ending with the right most point. This criterion is only available for shell elements.
          * - :py:attr:`~nsff`
            - Get or set the Number of explicit time step cycles for stress fade-out used in the LCFLD criterion. Default is 10.
          * - :py:attr:`~epsthin`
            - Get or set the Thinning strain at failure for thin and thick shells.
          * - :py:attr:`~engcrt`
            - Get or set the Critical energy for nonlocal failure criterion
          * - :py:attr:`~radcrt`
            - Get or set the Critical radius for nonlocal failure criterion
          * - :py:attr:`~lceps12`
            - Get or set the Load curve ID defining in-plane shear strain limit_12^c vs. element size
          * - :py:attr:`~lceps13`
            - Get or set the Load curve ID defining through-thickness shear strain limit_13^c vs. element size
          * - :py:attr:`~lcepsmx`
            - Get or set the Load curve ID defining in-plane major strain limit_1^c vs. element size
          * - :py:attr:`~dteflt`
            - Get or set the The time period (or inverse of the cutoff frequency) for the low-pass filter applied to the effective strain rate when MXEPS is negative.
          * - :py:attr:`~volfrac`
            - Get or set the The volume fraction required to fail before the element is deleted. The default is 0.5. It is used for higher order solid element types 24, 25, 26, 27, 28, and 29, and all isogeometric solids and shell elements.
          * - :py:attr:`~mxtmp`
            - Get or set the Maximum temperature at failure
          * - :py:attr:`~dtmin`
            - Get or set the -
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

    from mat_add_erosion import MatAddErosion

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which this erosion definition applies.
   A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: excl
   :type: Optional[float]


   
   Get or set the The exclusion number, which applies to the failure values defined on Cards 1, 2, and 7.
   When any of the failure values on these cards are set to the exclusion number,
   the associated failure criterion is not invoked.  Or in other words, only the failure values not set to the exclusion number are invoked.
   The default value of EXCL is 0.0, which eliminates all failure criteria from consideration that have their constants left blank or set to 0.0.  As an example,
   to prevent a material from developing tensile pressure, the user could specify an unusual value for
   the exclusion number, e.g., 1234, set MNPRES to 0.0, and set all the remaining failure values to 1234.
   However, use of an exclusion number may be considered nonessential since the same effect
   could be achieved without use of the exclusion number by setting MNPRES to a very small negative value.
















   ..
       !! processed by numpydoc !!

.. py:property:: mxpres
   :type: Optional[float]


   
   Get or set the Maximum pressure at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
















   ..
       !! processed by numpydoc !!

.. py:property:: mneps
   :type: Optional[float]


   
   Get or set the Minimum principal strain at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
















   ..
       !! processed by numpydoc !!

.. py:property:: effeps
   :type: Optional[float]


   
   Get or set the Maximum effective strain at failure, . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
















   ..
       !! processed by numpydoc !!

.. py:property:: voleps
   :type: Optional[float]


   
   Get or set the Volumetric strain at failure,  .  VOLEPS can be a positive or negative number depending on whether the failure is in tension or compression, respectively.  If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
















   ..
       !! processed by numpydoc !!

.. py:property:: numfip
   :type: float


   
   Get or set the Number or percentage of failed integration points prior to element deletion (default is 1).  See Remark 2.
   NUMFIP does not apply to higher order solid element types 24, 25, 26, 27, 28, and 29, rather see the variable VOLFRAC.
   GT.0.0: Number of integration points which must fail before element is deleted.
   LT.0.0: Applies only to shells. "|NUMFIP|" is the percentage of integration points which must exceed the failure criterion before the element fails.
   If NUMFIP < -100, then "|NUMFIP|-100"  is the number of failed integration points prior to element deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncs
   :type: float


   
   Get or set the Number of failure conditions to satisfy before failure occurs.  For example, if SIGP1 and SIGVM are defined and if NCS=2, both failure criteria must be met before element deletion can occur.  The default is set to unity.
















   ..
       !! processed by numpydoc !!

.. py:property:: mnpres
   :type: Optional[float]


   
   Get or set the Minimum pressure at failure,
















   ..
       !! processed by numpydoc !!

.. py:property:: sigp1
   :type: Optional[float]


   
   Get or set the Maximum principal stress at failur, Sigma max .
















   ..
       !! processed by numpydoc !!

.. py:property:: sigvm
   :type: Optional[float]


   
   Get or set the Equivalent stress at failure, The equivalent stress at failure is made a function of the effective strain rate by setting SIGVM to the negative of the appropriate load curve ID, Sigma max
















   ..
       !! processed by numpydoc !!

.. py:property:: mxeps
   :type: Optional[float]


   
   Get or set the Variable to invoke a failure criterion based on maximum principal strain.
   GT.0:    Maximum principal strain at failure, ε_max.
   LT.0:    -MXEPS is the ID of a curve giving maximum principal strain at failure as a function of effective strain rate.
   A filter is applied to the effective strain rate according to DTEFLT; see Card 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: epssh
   :type: Optional[float]


   
   Get or set the Tensorial shear strain at failure, r max .
















   ..
       !! processed by numpydoc !!

.. py:property:: sigth
   :type: Optional[float]


   
   Get or set the Threshold stress, Sigma 0 .
















   ..
       !! processed by numpydoc !!

.. py:property:: impulse
   :type: Optional[float]


   
   Get or set the Stress impulse for failure, K f.
















   ..
       !! processed by numpydoc !!

.. py:property:: failtm
   :type: Optional[float]


   
   Get or set the Failure time. When the problem time exceeds the failure time, the material is removed.
   GT.0:   Failure time is active during any phase of the analysis.
   LT.0:   Failure time is set to |FAILTM| but this criterion in inactive during the dynamic relaxation phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: idam
   :type: Optional[int]


   
   Get or set the Flag for damage model.
   EQ.0: no damage model is used.
   NE.0:   Damage models GISSMO or DIEM, see manuals of R10 and before.
   Still available here for backward compatibility, but description actually moved to new keywords *MAT_ADD_DAMAGE_DIEM/GISSMO
   ,
















   ..
       !! processed by numpydoc !!

.. py:property:: lcregd
   :type: Optional[int]


   
   Get or set the Load curve ID defining element size dependent regularization factors for equivalent plastic strain to failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfld
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID. Load curve defines the Forming Limit Diagram, where minor engineering strains in percent are defined as abscissa values and major engineering strains in percent are defined as ordinate values. Table defines for each strain rate an associated FLD curve. The forming limit diagram is shown in Figure Error! Reference source not found.. In defining the curve, list pairs of minor and major strains starting with the left most point and ending with the right most point. This criterion is only available for shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsff
   :type: int


   
   Get or set the Number of explicit time step cycles for stress fade-out used in the LCFLD criterion. Default is 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsthin
   :type: Optional[float]


   
   Get or set the Thinning strain at failure for thin and thick shells.
   GT.0.0: individual thinning for each integration point from z-strain
   LT.0.0: averaged thinning strain from element thickness change
















   ..
       !! processed by numpydoc !!

.. py:property:: engcrt
   :type: Optional[float]


   
   Get or set the Critical energy for nonlocal failure criterion
















   ..
       !! processed by numpydoc !!

.. py:property:: radcrt
   :type: Optional[float]


   
   Get or set the Critical radius for nonlocal failure criterion
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps12
   :type: Optional[int]


   
   Get or set the Load curve ID defining in-plane shear strain limit_12^c vs. element size
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps13
   :type: Optional[int]


   
   Get or set the Load curve ID defining through-thickness shear strain limit_13^c vs. element size
















   ..
       !! processed by numpydoc !!

.. py:property:: lcepsmx
   :type: Optional[int]


   
   Get or set the Load curve ID defining in-plane major strain limit_1^c vs. element size
















   ..
       !! processed by numpydoc !!

.. py:property:: dteflt
   :type: Optional[float]


   
   Get or set the The time period (or inverse of the cutoff frequency) for the low-pass filter applied to the effective strain rate when MXEPS is negative.
   If DTEFLT is set to zero or left blank, no filtering of the effective strain rate is performed in the determination of the maximum principal strain to failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: volfrac
   :type: Optional[float]


   
   Get or set the The volume fraction required to fail before the element is deleted. The default is 0.5. It is used for higher order solid element types 24, 25, 26, 27, 28, and 29, and all isogeometric solids and shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: mxtmp
   :type: Optional[float]


   
   Get or set the Maximum temperature at failure
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmin
   :type: Optional[float]


   
   Get or set the -
















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
   :value: 'ADD_EROSION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





