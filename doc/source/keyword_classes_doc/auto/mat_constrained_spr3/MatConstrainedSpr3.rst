





:class:`MatConstrainedSpr3`
===========================


.. py:class:: mat_constrained_spr3.MatConstrainedSpr3(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CONSTRAINED_SPR3 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatConstrainedSpr3

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
          * - :py:attr:`~model`
            - Get or set the Material behavior and damage model, see remarks.
          * - :py:attr:`~stiff`
            - Get or set the Elastic stiffness. Function ID if MODEL > 10.
          * - :py:attr:`~rn`
            - Get or set the Tensile strength factor.
          * - :py:attr:`~rs`
            - Get or set the Shear strength factor. Function ID if MODEL > 10.
          * - :py:attr:`~alpha1`
            - Get or set the Scaling factor. Function ID if MODEL > 10.
          * - :py:attr:`~beta`
            - Get or set the Exponent for plastic potential. Function ID if MODEL > 10.
          * - :py:attr:`~lcf`
            - Get or set the Load curve ID describing force versus plastic displacement.
          * - :py:attr:`~lcupf`
            - Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD
          * - :py:attr:`~lcupr`
            - Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD.
          * - :py:attr:`~upfn`
            - Get or set the Plastic initiation displacement in normal direction.
          * - :py:attr:`~upfs`
            - Get or set the Plastic initiation displacement in shear direction.
          * - :py:attr:`~alpha2`
            - Get or set the Plastic initiation displacement scaling factor.
          * - :py:attr:`~beta2`
            - Get or set the Exponent for plastic initiation displacement.
          * - :py:attr:`~uprn`
            - Get or set the Plastic rupture displacement in normal direction.
          * - :py:attr:`~uprs`
            - Get or set the Plastic rupture displacement in shear direction.
          * - :py:attr:`~alpha3`
            - Get or set the Plastic rupture displacement scaling factor.
          * - :py:attr:`~beta3`
            - Get or set the Exponent for plastic rupture displacement.
          * - :py:attr:`~mrn`
            - Get or set the Proportionality factor for dependency RN.
          * - :py:attr:`~mrs`
            - Get or set the Proportionality factor for dependency RS.
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

    from mat_constrained_spr3 import MatConstrainedSpr3

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

.. py:property:: model
   :type: int


   
   Get or set the Material behavior and damage model, see remarks.
   EQ. 1:  SPR3 (default),
   EQ. 2:  SPR4,
   EQ.11:  same as 1 with selected material parameters as functions,
   EQ.12:  same as 2 with selected material parameters as functions,
   EQ.21:  same as 11 with slight modification, see remarks,
   EQ.22:  same as 12 with slight modification, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: stiff
   :type: Optional[float]


   
   Get or set the Elastic stiffness. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: rn
   :type: Optional[float]


   
   Get or set the Tensile strength factor.
   GT.0.0: Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD).
   LT.0.0: Load curve with ID | RN | giving R_n as a function of peel ratio(see Remarks section for* CONSTRAINED_INTERPOLATION_SPOTWELD)
















   ..
       !! processed by numpydoc !!

.. py:property:: rs
   :type: Optional[float]


   
   Get or set the Shear strength factor. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Scaling factor. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Exponent for plastic potential. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcf
   :type: Optional[int]


   
   Get or set the Load curve ID describing force versus plastic displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcupf
   :type: Optional[int]


   
   Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD
















   ..
       !! processed by numpydoc !!

.. py:property:: lcupr
   :type: Optional[int]


   
   Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves). See Remarks section for *CONSTRAINED_INTERPOLATION_SPOTWELD.
















   ..
       !! processed by numpydoc !!

.. py:property:: upfn
   :type: Optional[float]


   
   Get or set the Plastic initiation displacement in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: upfs
   :type: Optional[float]


   
   Get or set the Plastic initiation displacement in shear direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Plastic initiation displacement scaling factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: Optional[float]


   
   Get or set the Exponent for plastic initiation displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: uprn
   :type: Optional[float]


   
   Get or set the Plastic rupture displacement in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: uprs
   :type: Optional[float]


   
   Get or set the Plastic rupture displacement in shear direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Plastic rupture displacement scaling factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta3
   :type: Optional[float]


   
   Get or set the Exponent for plastic rupture displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: mrn
   :type: Optional[float]


   
   Get or set the Proportionality factor for dependency RN.
















   ..
       !! processed by numpydoc !!

.. py:property:: mrs
   :type: Optional[float]


   
   Get or set the Proportionality factor for dependency RS.
















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
   :value: 'CONSTRAINED_SPR3'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





