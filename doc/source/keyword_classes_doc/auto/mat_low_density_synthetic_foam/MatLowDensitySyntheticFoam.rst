





:class:`MatLowDensitySyntheticFoam`
===================================


.. py:class:: mat_low_density_synthetic_foam.MatLowDensitySyntheticFoam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_LOW_DENSITY_SYNTHETIC_FOAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatLowDensitySyntheticFoam

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~lcid1`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the first loading cycle.
          * - :py:attr:`~lcid2`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the loading cycles after the first loading cycle is completed.
          * - :py:attr:`~hu`
            - Get or set the Hysteretic unloading factor betwen 0 and 1, (default=1, i.e., no energy dissipation).
          * - :py:attr:`~beta`
            - Get or set the B, decay constant to model creep in unloading.
          * - :py:attr:`~damp`
            - Get or set the Viscous coefficient (.05< recommended value </.50) to model damping effects
          * - :py:attr:`~shape`
            - Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor. Values less than one reduces the energy dissipation and greater than one increases dissipation.
          * - :py:attr:`~fail`
            - Get or set the Failure option after the cutoff stress is reached:
          * - :py:attr:`~bvflag`
            - Get or set the Bulk viscosity activation flag:
          * - :py:attr:`~ed`
            - Get or set the Optional Young's relaxation modulus, Ed, for rate effects.
          * - :py:attr:`~beta1`
            - Get or set the Optional decay constant, B1.
          * - :py:attr:`~kcon`
            - Get or set the Stiffness coefficient for contact interface stiffness. If undefined the maximum slope in stress vs. strain curve is used. When the maximum slope is taken for the contact, the time step size for this material is reduced for stability. In some cases delta-t may be significantly smaller, and defining a reasonable stiffness is recommended.
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY. This option is currently restarted to 8-noded solid elements with one point integration.
          * - :py:attr:`~tc`
            - Get or set the Tension cut-off stress.
          * - :py:attr:`~rflag`
            - Get or set the Rate type for input:
          * - :py:attr:`~dtrt`
            - Get or set the Strain rate averaging flag:
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

    from mat_low_density_synthetic_foam import MatLowDensitySyntheticFoam

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid1
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the first loading cycle.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the loading cycles after the first loading cycle is completed.
















   ..
       !! processed by numpydoc !!

.. py:property:: hu
   :type: float


   
   Get or set the Hysteretic unloading factor betwen 0 and 1, (default=1, i.e., no energy dissipation).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the B, decay constant to model creep in unloading.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Viscous coefficient (.05< recommended value </.50) to model damping effects
   LT.0.0: |DAMP| is the load curve ID, which defines the damping constant as a function of the maximum strain in compression defined as: Emax = max(1-lambda1, 1-lambda2, 1-lambda3). In tension, the damping constant is set to the value corresponding to the strain at 0. The abcissia should be defined from 0 to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: shape
   :type: Optional[float]


   
   Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor. Values less than one reduces the energy dissipation and greater than one increases dissipation.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: float


   
   Get or set the Failure option after the cutoff stress is reached:
   EQ.0.0: tensile stress remains at cut-off value,
   EQ.1.0: tensile stress is reset to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: bvflag
   :type: float


   
   Get or set the Bulk viscosity activation flag:
   EQ.0.0: no bulk viscosity (recommended),
   EQ.1.0: bulk viscosity active.
















   ..
       !! processed by numpydoc !!

.. py:property:: ed
   :type: Optional[float]


   
   Get or set the Optional Young's relaxation modulus, Ed, for rate effects.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta1
   :type: Optional[float]


   
   Get or set the Optional decay constant, B1.
















   ..
       !! processed by numpydoc !!

.. py:property:: kcon
   :type: Optional[float]


   
   Get or set the Stiffness coefficient for contact interface stiffness. If undefined the maximum slope in stress vs. strain curve is used. When the maximum slope is taken for the contact, the time step size for this material is reduced for stability. In some cases delta-t may be significantly smaller, and defining a reasonable stiffness is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY. This option is currently restarted to 8-noded solid elements with one point integration.
   EQ.0.0: off,
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: Optional[float]


   
   Get or set the Tension cut-off stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: rflag
   :type: float


   
   Get or set the Rate type for input:
   EQ.0.0: LCID1 and LCID2 should be input as functions of true strain rate
   EQ.1.0: LCID1 and LCID2 should be input as functions of engineering strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtrt
   :type: Optional[float]


   
   Get or set the Strain rate averaging flag:
   EQ.0.0: use weighted running average
   LT.0.0: average the last 11 values
   GT.0.0: average over the last DTRT time units.
















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
   :value: 'LOW_DENSITY_SYNTHETIC_FOAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





