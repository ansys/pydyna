





:class:`DefineDeHbond`
======================


.. py:class:: define_de_hbond.DefineDeHbond(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_HBOND keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeHbond

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the DES nodes
          * - :py:attr:`~stype`
            - Get or set the SID Type
          * - :py:attr:`~hbdfm`
            - Get or set the Bond formulation:
          * - :py:attr:`~idim`
            - Get or set the Space dimension for DES bonds:
          * - :py:attr:`~pbk_sf`
            - Get or set the Scale factor for volumetric stiffness of the bond.
          * - :py:attr:`~pbs_sf`
            - Get or set the Scale factor for shear stiffness of the bond.
          * - :py:attr:`~frgk`
            - Get or set the Critical fracture energy release rate for volumetric deformation due
          * - :py:attr:`~frgs`
            - Get or set the Critical fracture energy release rate for shear deformation. Special Cases:
          * - :py:attr:`~bondr`
            - Get or set the Influence radius of the DES nodes.
          * - :py:attr:`~alpha`
            - Get or set the Numerical damping.
          * - :py:attr:`~dmg`
            - Get or set the Continuous parameter for damage model.EQ.1.0: The bond breaks if the fracture energy in the bond
          * - :py:attr:`~frmdl`
            - Get or set the Fracture model:
          * - :py:attr:`~precrk`
            - Get or set the Shell set, define 3D surfaces of the pre-crack
          * - :py:attr:`~cktype`
            - Get or set the CKTYPE EQ. 0: Part set
          * - :py:attr:`~itfid`
            - Get or set the ID of the interface *INTERFACE_DE_HBOND, which defines
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

    from define_de_hbond import DefineDeHbond

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the DES nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the SID Type
   EQ.0: DES node set
   EQ.1: DES node
   EQ.2: DES part set
   EQ.3: DES part
















   ..
       !! processed by numpydoc !!

.. py:property:: hbdfm
   :type: int


   
   Get or set the Bond formulation:
   EQ.1: (Reserved)
   EQ.2: Nonlinear heterogeneous bond formulation for fracture analysis based on the general material models defined in the material cards. DES elements with different material models can be defined within one bond.
















   ..
       !! processed by numpydoc !!

.. py:property:: idim
   :type: int


   
   Get or set the Space dimension for DES bonds:
   EQ.2: for 2D plane stress problems
   EQ.3: for 3D problems.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbk_sf
   :type: float


   
   Get or set the Scale factor for volumetric stiffness of the bond.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs_sf
   :type: float


   
   Get or set the Scale factor for shear stiffness of the bond.
















   ..
       !! processed by numpydoc !!

.. py:property:: frgk
   :type: Optional[float]


   
   Get or set the Critical fracture energy release rate for volumetric deformation due
   to the hydrostatic pressure. Special Cases:
   EQ.0: A zero value specifies an infinite energy release rate for unbreakable bonds.
   LT.0: A negative value defines the energy release rate under volumetric
   compression (i.e. positive pressure) and FRGS defined
   below is used under volumetric expansion (i.e. negative pressure).
















   ..
       !! processed by numpydoc !!

.. py:property:: frgs
   :type: Optional[float]


   
   Get or set the Critical fracture energy release rate for shear deformation. Special Cases:
   EQ.0: A zero value specifies an infinite energy release rate for unbreakable bonds.
   FRGK.LT.0: See description for FRGK.
















   ..
       !! processed by numpydoc !!

.. py:property:: bondr
   :type: Optional[float]


   
   Get or set the Influence radius of the DES nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Numerical damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmg
   :type: float


   
   Get or set the Continuous parameter for damage model.EQ.1.0: The bond breaks if the fracture energy in the bond
   reaches the critical value. Microdamage is not calculated.
   ¦Å (0.5,1): Microdamage effects being once the fracture energy  reaches DMGxFMG[K,S]. Upon the onset of microdamage,
   the computed damage ratio will increase (monotonically) as the fracture energy grows. Bond
   weakening from microdamage is modeled by reducing the bond stiffness in proportion to the damage ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: frmdl
   :type: int


   
   Get or set the Fracture model:
   EQ.1: Fracture energy of shear deformation is calculated based on deviatoric stresses.
   EQ.2: Fracture energy of shear deformation is calculated based on deviatoric stresses, excluding the axial component (along the bond).
   EQ.3,4: Same as 1&2, respectively, but FRGK and FRGS are read
   as the total failure energy density and will be converted to the corresponding critical fracture energy release rate.
   The total failure energy density is calculated as the total area under uniaxial tension stress-strain curve.
   EQ.5,6: Same as 3&4, respectively, as FRGK and FRGS are read
   as the total failure energy density but will not be converted. Instead, the failure energy within the bond will be
   calculated. Models 1&2 are more suitable for brittle materials, and Models 5&6
   are easier for ductile materials. Models 3&4 can be used for moderately ductile fracture accordingly.
   This is the default fracture model and applied to all DES parts, even if they have different material models. More fracture models can be
   defined for different materials by specifying an interface ID (ITFID) in the optional card.
















   ..
       !! processed by numpydoc !!

.. py:property:: precrk
   :type: Optional[int]


   
   Get or set the Shell set, define 3D surfaces of the pre-crack
















   ..
       !! processed by numpydoc !!

.. py:property:: cktype
   :type: int


   
   Get or set the CKTYPE EQ. 0: Part set
   EQ. 1: Part
















   ..
       !! processed by numpydoc !!

.. py:property:: itfid
   :type: int


   
   Get or set the ID of the interface *INTERFACE_DE_HBOND, which defines
   different failure models for the heterogeneous bonds within each part and between two parts respectively
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'DE_HBOND'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





