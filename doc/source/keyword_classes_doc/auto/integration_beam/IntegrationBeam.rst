





:class:`IntegrationBeam`
========================


.. py:class:: integration_beam.IntegrationBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTEGRATION_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IntegrationBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~irid`
            - Get or set the Integration rule ID. (IRID refers to IRID on *SECTION_BEAM card).
          * - :py:attr:`~nip`
            - Get or set the Number of integration points, see also ICST.
          * - :py:attr:`~ra`
            - Get or set the Relative area of cross section, i.e., the actual cross-sectional area divided by the area defined by the product of the specified thickness in the s direction and the thickness in the t direction. See also ICST below.
          * - :py:attr:`~icst`
            - Get or set the Standard cross section type, ICST. If this type is nonzero then NIP and the relative area above should be input as zero.
          * - :py:attr:`~k`
            - Get or set the Integration refinement factor
          * - :py:attr:`~d1`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~d2`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~d3`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~d4`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~sref`
            - Get or set the sref, location of reference surface normal to s, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
          * - :py:attr:`~tref`
            - Get or set the tref, location of reference surface normal to t, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
          * - :py:attr:`~d5`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~d6`
            - Get or set the Cross-section dimensions.
          * - :py:attr:`~s`
            - Get or set the Normalized s coordinate of integration point, -1.0 <= s <= 1.0.
          * - :py:attr:`~t`
            - Get or set the Normalized t coordinate of integration point, -1.0 <= t <= 1.0.
          * - :py:attr:`~wf`
            - Get or set the Weighting factor, Ari, i.e., the area associated with the integration point divided by actual cross sectional area Ari = Ai/A.
          * - :py:attr:`~pid`
            - Get or set the Optional PID, used to identify material properties for this integration point.  If zero, the  master  PID (referenced on *ELEMENT) will be used.


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

    from integration_beam import IntegrationBeam

Property detail
---------------

.. py:property:: irid
   :type: Optional[int]


   
   Get or set the Integration rule ID. (IRID refers to IRID on *SECTION_BEAM card).
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: int


   
   Get or set the Number of integration points, see also ICST.
















   ..
       !! processed by numpydoc !!

.. py:property:: ra
   :type: float


   
   Get or set the Relative area of cross section, i.e., the actual cross-sectional area divided by the area defined by the product of the specified thickness in the s direction and the thickness in the t direction. See also ICST below.
















   ..
       !! processed by numpydoc !!

.. py:property:: icst
   :type: int


   
   Get or set the Standard cross section type, ICST. If this type is nonzero then NIP and the relative area above should be input as zero.
   EQ.1: W-section,
   EQ.2: C-section,
   EQ.3: Angle section,
   EQ.4: T-section,
   EQ.5: Rectangular tubing,
   EQ.6: Z-section,
   EQ.7: Trapezoidal section.
   For further information see Users Manual section 17.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: int


   
   Get or set the Integration refinement factor
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: d4
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: sref
   :type: float


   
   Get or set the sref, location of reference surface normal to s, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: float


   
   Get or set the tref, location of reference surface normal to t, for the Hughes-Liu beam only. This option is only useful if the beam is connected to a shell or another beam on its outer surface, see also *SECTION_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: d5
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: d6
   :type: Optional[float]


   
   Get or set the Cross-section dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Normalized s coordinate of integration point, -1.0 <= s <= 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Normalized t coordinate of integration point, -1.0 <= t <= 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: wf
   :type: Optional[float]


   
   Get or set the Weighting factor, Ari, i.e., the area associated with the integration point divided by actual cross sectional area Ari = Ai/A.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Optional PID, used to identify material properties for this integration point.  If zero, the  master  PID (referenced on *ELEMENT) will be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTEGRATION'


.. py:attribute:: subkeyword
   :value: 'BEAM'






