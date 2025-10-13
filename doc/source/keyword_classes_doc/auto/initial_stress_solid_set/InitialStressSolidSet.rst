





:class:`InitialStressSolidSet`
==============================


.. py:class:: initial_stress_solid_set.InitialStressSolidSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_SOLID_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressSolidSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the solid set ID.
          * - :py:attr:`~nint`
            - Get or set the Number of integration points (should correspond to the solid element formulation).
          * - :py:attr:`~nhisv`
            - Get or set the Number of additional history variables, which is typically equal to the number of history variables stored at the integration point + IVEFLG. If NHISV exceeds the number of integration point history variables required by the constitutive model, only the number required is output; therefore, if in doubt, set NHISV to a large number
          * - :py:attr:`~large`
            - Get or set the Format size, if zero, NHISV must also set to zero, and, if 1, a larger format is used and NHISV is used. This is the format used by LS-DYNA version 970 and earlier.
          * - :py:attr:`~iveflg`
            - Get or set the Initial Volume/energy flag (only used in large format)
          * - :py:attr:`~ialegp`
            - Get or set the The ALE multi-material group (AMMG) ID; only if the element is of
          * - :py:attr:`~nthint`
            - Get or set the Number of thermal integration points
          * - :py:attr:`~nthhsv`
            - Get or set the Number of thermal history variables per thermal integration point
          * - :py:attr:`~sigxx`
            - Get or set the Defines the 11 stress component.
          * - :py:attr:`~sigyy`
            - Get or set the Defines the 22 stress component.
          * - :py:attr:`~sigzz`
            - Get or set the Defines the 33 stress component.
          * - :py:attr:`~sigxy`
            - Get or set the Defines the 12 stress component.
          * - :py:attr:`~sigyz`
            - Get or set the Defines the 23 stress component.
          * - :py:attr:`~sigzx`
            - Get or set the Defines the 31 stress component.
          * - :py:attr:`~eps`
            - Get or set the Effective plastic strain.


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

    from initial_stress_solid_set import InitialStressSolidSet

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nint
   :type: Optional[int]


   
   Get or set the Number of integration points (should correspond to the solid element formulation).
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: Optional[int]


   
   Get or set the Number of additional history variables, which is typically equal to the number of history variables stored at the integration point + IVEFLG. If NHISV exceeds the number of integration point history variables required by the constitutive model, only the number required is output; therefore, if in doubt, set NHISV to a large number
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: Optional[int]


   
   Get or set the Format size, if zero, NHISV must also set to zero, and, if 1, a larger format is used and NHISV is used. This is the format used by LS-DYNA version 970 and earlier.
















   ..
       !! processed by numpydoc !!

.. py:property:: iveflg
   :type: int


   
   Get or set the Initial Volume/energy flag (only used in large format)
   EQ.0:last history variable is used as normal,
   EQ.1:last history variable is used as the initial volume of the element.
   One additional history variable is required if IVFLG=1
   EQ.2:last two history variables are used to define the initial volume
   and the internal energy per unit initial volume. Two additional
   history variables are must be allocated, see NHISV above, if
   IVFLG=2. If the initial volume is set to zero, the actual element volume is used
















   ..
       !! processed by numpydoc !!

.. py:property:: ialegp
   :type: Optional[int]


   
   Get or set the The ALE multi-material group (AMMG) ID; only if the element is of
   ALE multi-material formulation (ELEFORM = 11). In this case, each AMMG has its own sets of stress and history variables so we must
   specify to which AMMG the stress data are assigned. For mixed elements, multiple cards are needed to complete the stress
   initialization in this element as each AMMG needs to have its own set of stress data.
   EQ.0: Assuming the element is fully filled by the AMMG that the
   element part belongs to. Please refer to *ALE_MULTI-MATERIAL_GROUP card.
   EQ.n: Assigning the stress to nth AMMG in that element.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthint
   :type: Optional[int]


   
   Get or set the Number of thermal integration points
















   ..
       !! processed by numpydoc !!

.. py:property:: nthhsv
   :type: Optional[int]


   
   Get or set the Number of thermal history variables per thermal integration point
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxx
   :type: float


   
   Get or set the Defines the 11 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyy
   :type: float


   
   Get or set the Defines the 22 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzz
   :type: float


   
   Get or set the Defines the 33 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxy
   :type: float


   
   Get or set the Defines the 12 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyz
   :type: float


   
   Get or set the Defines the 23 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzx
   :type: float


   
   Get or set the Defines the 31 stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps
   :type: float


   
   Get or set the Effective plastic strain.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_SOLID_SET'






