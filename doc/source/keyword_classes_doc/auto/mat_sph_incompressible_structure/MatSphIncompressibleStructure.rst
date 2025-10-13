





:class:`MatSphIncompressibleStructure`
======================================


.. py:class:: mat_sph_incompressible_structure.MatSphIncompressibleStructure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPH_INCOMPRESSIBLE_STRUCTURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSphIncompressibleStructure

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
            - Get or set the Mass density. This should be set to the rest density of the fluid.
          * - :py:attr:`~beta`
            - Get or set the Numerical surface adhesion coefficient. For water, a value of β=1000 m/s^2 is recommended. Only used if IMAT=0 in *CONTROL_SPH.
          * - :py:attr:`~rough`
            - Get or set the Surface roughness coefficient. A friction force between the structure and the fluid is generated based on the viscosity of the fluid, scaled by this coefficient. A value between 0.0 and 10.0 is usually recommended
          * - :py:attr:`~adh`
            - Get or set the Surface adhesion scaling coefficient. It is only used if IMAT=1 in *CONTROL_SPH. An attractive force between fluid and structure is calculated based on surface tension forces in the fluid and then, scaled by ADH.
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

    from mat_sph_incompressible_structure import MatSphIncompressibleStructure

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density. This should be set to the rest density of the fluid.
   The actual mass of the structure will be calculated from the parent surfaces sampled with the *DEFINE_SPH_MESH_SURFACE keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Numerical surface adhesion coefficient. For water, a value of β=1000 m/s^2 is recommended. Only used if IMAT=0 in *CONTROL_SPH.
















   ..
       !! processed by numpydoc !!

.. py:property:: rough
   :type: Optional[float]


   
   Get or set the Surface roughness coefficient. A friction force between the structure and the fluid is generated based on the viscosity of the fluid, scaled by this coefficient. A value between 0.0 and 10.0 is usually recommended
















   ..
       !! processed by numpydoc !!

.. py:property:: adh
   :type: Optional[float]


   
   Get or set the Surface adhesion scaling coefficient. It is only used if IMAT=1 in *CONTROL_SPH. An attractive force between fluid and structure is calculated based on surface tension forces in the fluid and then, scaled by ADH.
















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
   :value: 'SPH_INCOMPRESSIBLE_STRUCTURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





