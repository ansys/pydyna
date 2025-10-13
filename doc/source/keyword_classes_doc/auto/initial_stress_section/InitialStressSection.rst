





:class:`InitialStressSection`
=============================


.. py:class:: initial_stress_section.InitialStressSection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_SECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressSection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~issid`
            - Get or set the Section stress initialization ID.
          * - :py:attr:`~csid`
            - Get or set the Cross Section ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining preload stress versus time. When the loas curve ends or goes to zero, the initialization is assumed to be completed.
          * - :py:attr:`~psid`
            - Get or set the Part set ID.
          * - :py:attr:`~vid`
            - Get or set the Vector ID defining the direction normal to the cross section.  This vector must be defined if *DATABASE_CROSS_SECTION_SET is used to define the cross section.  If the cross section is defined using the PLANE option, the normal used in the definition of the plane is used if VID is left undefined.
          * - :py:attr:`~izshear`
            - Get or set the Shear stress flag:
          * - :py:attr:`~istiff`
            - Get or set the Artificial stiffness. Simulates additional linearly elastic "ghost" elements in the cross section.


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

    from initial_stress_section import InitialStressSection

Property detail
---------------

.. py:property:: issid
   :type: Optional[int]


   
   Get or set the Section stress initialization ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: csid
   :type: Optional[int]


   
   Get or set the Cross Section ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining preload stress versus time. When the loas curve ends or goes to zero, the initialization is assumed to be completed.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID defining the direction normal to the cross section.  This vector must be defined if *DATABASE_CROSS_SECTION_SET is used to define the cross section.  If the cross section is defined using the PLANE option, the normal used in the definition of the plane is used if VID is left undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: izshear
   :type: int


   
   Get or set the Shear stress flag:
   EQ.0: Shear stresses are prescribed as zero during the time the
   curve is acting to prescribe normal stress.
   EQ.1: Shear stresses are allowed to develop during the time the
   curve is acting to prescribe normal stress.
   For implicit the section can also take bending and is identical to 2;
   EQ 2: Shear and bending stresses are allowed to develop
   during the time the curve is acting to prescribe normal stress
















   ..
       !! processed by numpydoc !!

.. py:property:: istiff
   :type: int


   
   Get or set the Artificial stiffness. Simulates additional linearly elastic "ghost" elements in the cross section.
   These elements prevent mesh distortion by stiffening up the structure.
   GT.0:   load curve ID defining stiffness fraction as a function of time.
   The stiffness of the ghost elements is the load curve value times the stiffness of the material in the part.
   Since the ghost element stress counteracts the preload stress the fraction should be low(1% or less).
   The total section stress is the preload stress minus the ghost element stress.
   LT.0 : |ISTIFF| is the load curve ID for the stiffness fraction as a function of time.
   The preload stress is here automatically adjusted(-/+10 % of original prestress values) such that the total section stress corresponds to the curve in LCID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_SECTION'






