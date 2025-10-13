





:class:`ConstrainedInterpolationSpotweld`
=========================================


.. py:class:: constrained_interpolation_spotweld.ConstrainedInterpolationSpotweld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_INTERPOLATION_SPOTWELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedInterpolationSpotweld

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid1`
            - Get or set the Part ID or part set ID of first sheet
          * - :py:attr:`~pid2`
            - Get or set the Part ID or part set ID of second sheet. PID2 can be identical to PID1 if the spot weld location nodes from NSID lie in between the shell elements that should be self-connected.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID of spot weld location nodes.
          * - :py:attr:`~thick`
            - Get or set the Total thickness of both sheets.
          * - :py:attr:`~r`
            - Get or set the Spotweld radius.
          * - :py:attr:`~stiff`
            - Get or set the Elastic stiffness. Function ID if MODEL > 10.
          * - :py:attr:`~alpha1`
            - Get or set the Scaling factor. Function ID if MODEL > 10.
          * - :py:attr:`~model`
            - Get or set the Material behavior and damage model, see remarks.
          * - :py:attr:`~rn`
            - Get or set the Tensile strength factor.
          * - :py:attr:`~rs`
            - Get or set the Shear strength factor. Function ID if MODEL > 10.
          * - :py:attr:`~beta`
            - Get or set the Exponent for plastic potential β_1. Function ID if MODEL > 10.
          * - :py:attr:`~lcf`
            - Get or set the Load curve ID describing force versus plastic displacement.
          * - :py:attr:`~lcupf`
            - Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
          * - :py:attr:`~lcupr`
            - Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
          * - :py:attr:`~dens`
            - Get or set the Spotweld density (necessary for time step calculation).
          * - :py:attr:`~intp`
            - Get or set the Flag for interpolation.
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

    from constrained_interpolation_spotweld import ConstrainedInterpolationSpotweld

Property detail
---------------

.. py:property:: pid1
   :type: Optional[int]


   
   Get or set the Part ID or part set ID of first sheet
   GT.0:   Part ID
   LT.0 : | PID1 | is part set ID(for in - plane composed sheets such as Tailored Blanks)
















   ..
       !! processed by numpydoc !!

.. py:property:: pid2
   :type: Optional[int]


   
   Get or set the Part ID or part set ID of second sheet. PID2 can be identical to PID1 if the spot weld location nodes from NSID lie in between the shell elements that should be self-connected.
   GT.0:   Part ID
   LT.0 : |PID2 | is part set ID(for in - plane composed sheets sheets such as Tailored Blanks)
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID of spot weld location nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Total thickness of both sheets.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Spotweld radius.
















   ..
       !! processed by numpydoc !!

.. py:property:: stiff
   :type: Optional[float]


   
   Get or set the Elastic stiffness. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Scaling factor. Function ID if MODEL > 10.
















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

.. py:property:: rn
   :type: Optional[float]


   
   Get or set the Tensile strength factor.
   GT.0.0: Constant value unless MODEL > 10.  Function ID if MODEL > 10 (see Remark 2).
   LT.0.0: Load curve with ID | RN | giving R_n as a function of peel ratio(see Remark 5)
















   ..
       !! processed by numpydoc !!

.. py:property:: rs
   :type: Optional[float]


   
   Get or set the Shear strength factor. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Exponent for plastic potential β_1. Function ID if MODEL > 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcf
   :type: Optional[int]


   
   Get or set the Load curve ID describing force versus plastic displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcupf
   :type: Optional[int]


   
   Get or set the Load curve ID describing plastic initiation displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcupr
   :type: Optional[int]


   
   Get or set the Load curve ID describing plastic rupture displacement versus mode mixity. Only for MODEL=1.For MODEL = 1, LCUPF can also be a table ID giving plastic initiation displacement as a function of peel ratio (table values) and mode mixity (curves).
















   ..
       !! processed by numpydoc !!

.. py:property:: dens
   :type: Optional[float]


   
   Get or set the Spotweld density (necessary for time step calculation).
















   ..
       !! processed by numpydoc !!

.. py:property:: intp
   :type: int


   
   Get or set the Flag for interpolation.
   EQ.0:   linear (default),
   EQ.1:   uniform,
   EQ.2:   inverse distance weighting.
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'INTERPOLATION_SPOTWELD'






