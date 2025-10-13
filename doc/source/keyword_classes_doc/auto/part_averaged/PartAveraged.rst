





:class:`PartAveraged`
=====================


.. py:class:: part_averaged.PartAveraged(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_AVERAGED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartAveraged

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~title`
            - Get or set the Heading for the part.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~secid`
            - Get or set the Section ID defined in *SECTION section.
          * - :py:attr:`~mid`
            - Get or set the Material ID defined in *MAT section.
          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
          * - :py:attr:`~hgid`
            - Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
          * - :py:attr:`~grav`
            - Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
          * - :py:attr:`~adpopt`
            - Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
          * - :py:attr:`~tmid`
            - Get or set the Thermal material property identification defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.


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

    from part_averaged import PartAveraged

Property detail
---------------

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Heading for the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID defined in *SECTION section.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT section.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: int


   
   Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
   EQ.0: default values are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: int


   
   Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
   EQ.0: all parts initialized,
   EQ.1: only current material initialized.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpopt
   :type: Optional[int]


   
   Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
   LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
   EQ.0:Adaptive remeshing is inactive for this part ID.
   EQ.1:   h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
   EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
   EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
   EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid
   :type: int


   
   Get or set the Thermal material property identification defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
   EQ.0: defaults to MID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'AVERAGED'






