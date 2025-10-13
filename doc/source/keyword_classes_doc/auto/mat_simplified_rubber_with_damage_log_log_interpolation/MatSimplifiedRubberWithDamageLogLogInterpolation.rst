





:class:`MatSimplifiedRubberWithDamageLogLogInterpolation`
=========================================================


.. py:class:: mat_simplified_rubber_with_damage_log_log_interpolation.MatSimplifiedRubberWithDamageLogLogInterpolation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SIMPLIFIED_RUBBER_WITH_DAMAGE_LOG_LOG_INTERPOLATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSimplifiedRubberWithDamageLogLogInterpolation

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
            - Get or set the Linear bulk modulus.
          * - :py:attr:`~mu`
            - Get or set the Damping coefficient.
          * - :py:attr:`~g`
            - Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
          * - :py:attr:`~sigf`
            - Get or set the Limit stress for frequency independent, frictional, damping.
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
          * - :py:attr:`~lcunld`
            - Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual change in the gauge length during unloading. The unload curve should cover exactly the same range as LCLD and its end points should have identical values, i.e., the combination of LCLD and LCUNLD describes a complete cycle of loading and unloading.
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

    from mat_simplified_rubber_with_damage_log_log_interpolation import MatSimplifiedRubberWithDamageLogLogInterpolation

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


   
   Get or set the Linear bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Damping coefficient.
















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
   :type: Optional[float]


   
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
   :type: float


   
   Get or set the Averaging option determine strain rate to reduce numerical noise.
   EQ.0.0: simple average of twelve time steps,
   EQ.1.0: running 12 point average.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcunld
   :type: Optional[int]


   
   Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual change in the gauge length during unloading. The unload curve should cover exactly the same range as LCLD and its end points should have identical values, i.e., the combination of LCLD and LCUNLD describes a complete cycle of loading and unloading.
















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
   :value: 'SIMPLIFIED_RUBBER_WITH_DAMAGE_LOG_LOG_INTERPOLATION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





