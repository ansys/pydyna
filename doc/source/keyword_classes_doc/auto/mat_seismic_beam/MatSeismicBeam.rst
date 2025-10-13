





:class:`MatSeismicBeam`
=======================


.. py:class:: mat_seismic_beam.MatSeismicBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SEISMIC_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSeismicBeam

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
            - Get or set the Young's modulus
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~asflag`
            - Get or set the Axial strain definition for force-strain curves, degradation and FEMA output:
          * - :py:attr:`~ftype`
            - Get or set the Formulation type for interaction:
          * - :py:attr:`~degrad`
            - Get or set the Flag for degrading moment behavior
          * - :py:attr:`~ifema`
            - Get or set the Flag for input of FEMA thresholds
          * - :py:attr:`~lcpms`
            - Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local s-axis.
          * - :py:attr:`~sfs`
            - Get or set the Scale factor on s-moment at node 2.
          * - :py:attr:`~lcpmt`
            - Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local t-axis.
          * - :py:attr:`~sft`
            - Get or set the Scale factor on t-moment at node 2.
          * - :py:attr:`~lcat`
            - Get or set the Load curve ID giving axial tensile yield force vs. total tensile (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
          * - :py:attr:`~sfat`
            - Get or set the Scale factor on axial tensile force.
          * - :py:attr:`~lcac`
            - Get or set the Load curve ID giving compressive yield force vs. total compressive (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
          * - :py:attr:`~sfac`
            - Get or set the Scale factor on axial tensile force.
          * - :py:attr:`~alpha`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~beta`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~gamma`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~delta`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~a`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~b`
            - Get or set the Parameter to define yield surface.
          * - :py:attr:`~foffs`
            - Get or set the
          * - :py:attr:`~sigy`
            - Get or set the Yield stress of material.
          * - :py:attr:`~d`
            - Get or set the Depth of section used to calculate interaction curve.
          * - :py:attr:`~w`
            - Get or set the Width of section used to calculate interaction curve.
          * - :py:attr:`~tf`
            - Get or set the Flange thickness of section used to calculate interaction curve.
          * - :py:attr:`~tw`
            - Get or set the Web thickness used to calculate interaction curve.
          * - :py:attr:`~phi_t`
            - Get or set the Factor on tensile capacity
          * - :py:attr:`~phi_c`
            - Get or set the Factor on compression capacity
          * - :py:attr:`~phi_b`
            - Get or set the Factor on bending capacity
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

    from mat_seismic_beam import MatSeismicBeam

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


   
   Get or set the Young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: asflag
   :type: float


   
   Get or set the Axial strain definition for force-strain curves, degradation and FEMA output:
   EQ.0.0: true (log) total strain
   EQ.1.0: change in length
   EQ.2.0: nominal total strain
   EQ.3.0: FEMA plastic strain ( = nominal total strain minus elastic strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: ftype
   :type: int


   
   Get or set the Formulation type for interaction:
   EQ:1 Parabolic coefficients, axial load and biaxial bending (default).
   EQ:2 Japanese code, axial force and major axis bending.
   EQ.4:   AISC utilization calculation but no yielding
   EQ.5:   AS4100 utilization calculation but no yielding
















   ..
       !! processed by numpydoc !!

.. py:property:: degrad
   :type: int


   
   Get or set the Flag for degrading moment behavior
   EQ.0: Behavior as in previous versions
   EQ.1: Fatigue-type degrading moment-rotation behavior
   EQ.2: FEMA-type degrading moment-rotation behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifema
   :type: int


   
   Get or set the Flag for input of FEMA thresholds
   EQ.0: No input
   EQ.1: Input of rotation thresholds only
   EQ.2: Input of rotation and axial strain thresholds.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpms
   :type: Optional[int]


   
   Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local s-axis.
   See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfs
   :type: float


   
   Get or set the Scale factor on s-moment at node 2.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpmt
   :type: Optional[int]


   
   Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local t-axis.
   See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: float


   
   Get or set the Scale factor on t-moment at node 2.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcat
   :type: Optional[int]


   
   Get or set the Load curve ID giving axial tensile yield force vs. total tensile (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
   See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfat
   :type: float


   
   Get or set the Scale factor on axial tensile force.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcac
   :type: Optional[int]


   
   Get or set the Load curve ID giving compressive yield force vs. total compressive (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
   See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfac
   :type: float


   
   Get or set the Scale factor on axial tensile force.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: delta
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to 4.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: float


   
   Get or set the Parameter to define yield surface.
   Default is set to -1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: foffs
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress of material.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Depth of section used to calculate interaction curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Width of section used to calculate interaction curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tf
   :type: Optional[float]


   
   Get or set the Flange thickness of section used to calculate interaction curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tw
   :type: Optional[float]


   
   Get or set the Web thickness used to calculate interaction curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_t
   :type: float


   
   Get or set the Factor on tensile capacity
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_c
   :type: float


   
   Get or set the Factor on compression capacity
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_b
   :type: float


   
   Get or set the Factor on bending capacity
















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
   :value: 'SEISMIC_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





