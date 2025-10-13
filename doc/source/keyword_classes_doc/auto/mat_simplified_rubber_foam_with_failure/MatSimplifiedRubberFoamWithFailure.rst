





:class:`MatSimplifiedRubberFoamWithFailure`
===========================================


.. py:class:: mat_simplified_rubber_foam_with_failure.MatSimplifiedRubberFoamWithFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SIMPLIFIED_RUBBER/FOAM_WITH_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSimplifiedRubberFoamWithFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~km`
            - Get or set the Linear bulk modulus.
          * - :py:attr:`~mu`
            - Get or set the Damping coefficient (0.05 < recommended value < 0.50; default is 0.10).
          * - :py:attr:`~g`
            - Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
          * - :py:attr:`~sigf`
            - Get or set the Limit stress for frequency independent, frictional, damping.
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY (see there for more details).
          * - :py:attr:`~prten`
            - Get or set the The tensile Poisson's ratio for shells (optional).  If PRTEN is zero, PR/BETA will serve as the Poisson's ratio for both tension and compression in shells.  If PRTEN is nonzero, PR/BETA will serve only as the compressive Poisson's ratio for shells.
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length.
          * - :py:attr:`~sw`
            - Get or set the Specimen width.
          * - :py:attr:`~st`
            - Get or set the Specimen thickness.
          * - :py:attr:`~lc_tbid`
            - Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the force versus actual change in the gauge length. If the table definition is used a family of curves are defined for discrete strain rates. The load curves should cover the complete range of expected loading, i.e., the smallest stretch ratio to the largest.
          * - :py:attr:`~tension`
            - Get or set the Parameter that controls how the rate effects are treated. Applicable to the table definition.
          * - :py:attr:`~rtype`
            - Get or set the Strain rate type if a table is defined:
          * - :py:attr:`~avgopt`
            - Get or set the Averaging option determine strain rate to reduce numerical noise.
          * - :py:attr:`~pra`
            - Get or set the Poisson ratio or viscosity coefficient, If the value is specified between 0 and 0.5 exclusive, i.e.,the number defined here is taken as Poisson's ratio.  If zero, an incompressible rubber like behavior is assumed and a default value of 0.495 is used internally.   If a Poisson's ratio of 0.0 is desired, input a small value for PR such as 0.001.  When fully integrated solid elements are used and when a nonzero Poisson's ratio is specified, a foam material is assumed and selective-reduced integration is not used due to the compressibility.  This is true even if PR approaches 0.500.  If any other value excluding zero is define, then BETA is taken as the absolute value of the given number and a nearly incompressible rubber like behavior is assumed.  An incrementally updated mean viscous stress develops according to the equation:The BETA parameter does not apply to highly compressible foam materials.Material failure parameter that controls the volume enclosed by the failure surface.
          * - :py:attr:`~k`
            - Get or set the Material failure parameter that controls the volume enclosed by the failure surface.
          * - :py:attr:`~gama1`
            - Get or set the Material failure parameter.
          * - :py:attr:`~gama2`
            - Get or set the Material failure parameter.
          * - :py:attr:`~eh`
            - Get or set the Damage parameter.
          * - :py:attr:`~lcunld`
            - Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual
          * - :py:attr:`~hu`
            - Get or set the Hysteretic unloading factor between 0 and 1 (default = 1., i.e. no
          * - :py:attr:`~shape`
            - Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor HU. Values less than one reduces the energy
          * - :py:attr:`~stol`
            - Get or set the Tolerance in stability check.
          * - :py:attr:`~visco`
            - Get or set the Flag to invoke visco-elastic formulation.  The visco-elastic formulation does not apply to shell elements and will be ignored for shells.
          * - :py:attr:`~hisout`
            - Get or set the History output flag.
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

    from mat_simplified_rubber_foam_with_failure import MatSimplifiedRubberFoamWithFailure

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: km
   :type: Optional[float]


   
   Get or set the Linear bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: float


   
   Get or set the Damping coefficient (0.05 < recommended value < 0.50; default is 0.10).
















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


   
   Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY (see there for more details).
   EQ.0.0:  off,
   EQ.1.0:  on.
















   ..
       !! processed by numpydoc !!

.. py:property:: prten
   :type: Optional[float]


   
   Get or set the The tensile Poisson's ratio for shells (optional).  If PRTEN is zero, PR/BETA will serve as the Poisson's ratio for both tension and compression in shells.  If PRTEN is nonzero, PR/BETA will serve only as the compressive Poisson's ratio for shells.
















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

.. py:property:: lc_tbid
   :type: Optional[int]


   
   Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the force versus actual change in the gauge length. If the table definition is used a family of curves are defined for discrete strain rates. The load curves should cover the complete range of expected loading, i.e., the smallest stretch ratio to the largest.
















   ..
       !! processed by numpydoc !!

.. py:property:: tension
   :type: float


   
   Get or set the Parameter that controls how the rate effects are treated. Applicable to the table definition.
   EQ.-1.-: rate effects are considered for loading either in tension or compression, but not for unloading,
   EQ.0.0: rate effects are considered for compressive loading only,
   EQ.1.0: rate effects are treated identically in tension and compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: rtype
   :type: float


   
   Get or set the Strain rate type if a table is defined:
   EQ.0.0: true strain rate,
   EQ.1.0: engineering strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: avgopt
   :type: Optional[float]


   
   Get or set the Averaging option determine strain rate to reduce numerical noise.
   LT.0.0: |AVGOPT| is a time window/interval over which the strain rates are averaged.
   EQ.0.0: simple average of twelve time steps,
   EQ.1.0: running average of last 12 averages.
















   ..
       !! processed by numpydoc !!

.. py:property:: pra
   :type: Optional[float]


   
   Get or set the Poisson ratio or viscosity coefficient, If the value is specified between 0 and 0.5 exclusive, i.e.,the number defined here is taken as Poisson's ratio.  If zero, an incompressible rubber like behavior is assumed and a default value of 0.495 is used internally.   If a Poisson's ratio of 0.0 is desired, input a small value for PR such as 0.001.  When fully integrated solid elements are used and when a nonzero Poisson's ratio is specified, a foam material is assumed and selective-reduced integration is not used due to the compressibility.  This is true even if PR approaches 0.500.  If any other value excluding zero is define, then BETA is taken as the absolute value of the given number and a nearly incompressible rubber like behavior is assumed.  An incrementally updated mean viscous stress develops according to the equation:The BETA parameter does not apply to highly compressible foam materials.Material failure parameter that controls the volume enclosed by the failure surface.
   LE.0.0: ignore failure criterion;
   GT.0.0: use actual K value for failure criterions..
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Material failure parameter that controls the volume enclosed by the failure surface.
   LE. 0.0: ignore failure criterion;
   GT. 0.0: use actual K value for failure criterions
















   ..
       !! processed by numpydoc !!

.. py:property:: gama1
   :type: Optional[float]


   
   Get or set the Material failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: gama2
   :type: Optional[float]


   
   Get or set the Material failure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: eh
   :type: Optional[float]


   
   Get or set the Damage parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcunld
   :type: Optional[int]


   
   Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual
   length during unloading. The unload curve should cover exactly
   the same range as LC or the load curves of TBID and its end points
   should have identical values, i.e., the combination of LC and
   LCUNLD or the first curve of TBID and LCUNLD describes a
   complete cycle of loading and unloading. See also material *MAT_        083.
















   ..
       !! processed by numpydoc !!

.. py:property:: hu
   :type: float


   
   Get or set the Hysteretic unloading factor between 0 and 1 (default = 1., i.e. no
   energy dissipation), see also material *MAT_083 and Figure M57-1. This option is ignored if LCUNLD is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: shape
   :type: Optional[float]


   
   Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor HU. Values less than one reduces the energy
   dissipation and greater than one increases dissipation, see also material *MAT_083 and Figure M57-1.
















   ..
       !! processed by numpydoc !!

.. py:property:: stol
   :type: Optional[float]


   
   Get or set the Tolerance in stability check.
















   ..
       !! processed by numpydoc !!

.. py:property:: visco
   :type: float


   
   Get or set the Flag to invoke visco-elastic formulation.  The visco-elastic formulation does not apply to shell elements and will be ignored for shells.
   EQ.0.0: purely elastic;
   EQ.1.0: visco-elastic formulation (solids only).
















   ..
       !! processed by numpydoc !!

.. py:property:: hisout
   :type: float


   
   Get or set the History output flag.
   EQ.0.0: default;
   EQ.1.0: principal strains are written to history variables 25, 26, 27.
















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
   :value: 'SIMPLIFIED_RUBBER/FOAM_WITH_FAILURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





