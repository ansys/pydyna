





:class:`MatInelasticSpringDiscreteBeam`
=======================================


.. py:class:: mat_inelastic_spring_discrete_beam.MatInelasticSpringDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_INELASTIC_SPRING_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatInelasticSpringDiscreteBeam

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
            - Get or set the Mass density, see also volume in *SECTION_BEAM definition.
          * - :py:attr:`~k`
            - Get or set the Elastic loading/unloading stiffness. This is required input.
          * - :py:attr:`~f0`
            - Get or set the Optional initial force. This option is inactive if this material is referenced in a part referenced by material type *MAT_INELASTIC_6DOF_SPRING
          * - :py:attr:`~d`
            - Get or set the Optional viscous damping coefficient.
          * - :py:attr:`~cdf`
            - Get or set the Compressive displacement at failure. Input as a positive number. After failure, no forces are carried. This option does not apply to zero length springs.
          * - :py:attr:`~tdf`
            - Get or set the Tensile displacement at failure. After failure, no forces are carried.
          * - :py:attr:`~flcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE, defining the yield force versus plastic deflection. If the origin of the curve is at (0,0) the force magnitude is identical in tension and compression, i.e., only the sign changes. If not, the yield stress in the compression is used when the spring force is negative. The plastic displacement increases monotonically in this implementation.
          * - :py:attr:`~hlcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE, defining force versus relative velocity (optional).
          * - :py:attr:`~c1`
            - Get or set the Damping coefficient.
          * - :py:attr:`~c2`
            - Get or set the Damping coefficient.
          * - :py:attr:`~dle`
            - Get or set the Factor to scale time units.
          * - :py:attr:`~glcid`
            - Get or set the Optional load curve ID, see *DEFINE_CURVE, defining a scale factor versus deflection  for load curve ID, GLCID.
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

    from mat_inelastic_spring_discrete_beam import MatInelasticSpringDiscreteBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density, see also volume in *SECTION_BEAM definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Elastic loading/unloading stiffness. This is required input.
















   ..
       !! processed by numpydoc !!

.. py:property:: f0
   :type: Optional[float]


   
   Get or set the Optional initial force. This option is inactive if this material is referenced in a part referenced by material type *MAT_INELASTIC_6DOF_SPRING
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Optional viscous damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdf
   :type: Optional[float]


   
   Get or set the Compressive displacement at failure. Input as a positive number. After failure, no forces are carried. This option does not apply to zero length springs.
   EQ.0.0: inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdf
   :type: Optional[float]


   
   Get or set the Tensile displacement at failure. After failure, no forces are carried.
   EQ.0.0: inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: flcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, defining the yield force versus plastic deflection. If the origin of the curve is at (0,0) the force magnitude is identical in tension and compression, i.e., only the sign changes. If not, the yield stress in the compression is used when the spring force is negative. The plastic displacement increases monotonically in this implementation.
   The load curve is required input.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, defining force versus relative velocity (optional).
   If the origin of the curve is at (0,0) the force magnitude is identical for a given magnitude of the relative velocity, i.e., only the sign changes.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: dle
   :type: Optional[float]


   
   Get or set the Factor to scale time units.
















   ..
       !! processed by numpydoc !!

.. py:property:: glcid
   :type: Optional[int]


   
   Get or set the Optional load curve ID, see *DEFINE_CURVE, defining a scale factor versus deflection  for load curve ID, GLCID.
   If zero, a scale factor of unity is assumed.
















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
   :value: 'INELASTIC_SPRING_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





