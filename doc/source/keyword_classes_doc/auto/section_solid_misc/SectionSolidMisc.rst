





:class:`SectionSolidMisc`
=========================


.. py:class:: section_solid_misc.SectionSolidMisc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SOLID_MISC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionSolidMisc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~elform`
            - Get or set the Element formulation options.  Remark 2 enumerates the element formulations available for implicit calculations:
          * - :py:attr:`~aet`
            - Get or set the Ambient Element type: Can be defined for ELFORM 7, 11 and 12.
          * - :py:attr:`~cohoff`
            - Get or set the Applies to cohesive solid elements 20 and 22. COHOFF specifies the relative location of the cohesive layer. It must be a number between -1 and 1. A value of -1 will place it on the bottom face of the cohesive element, while a value of +1 will place it on the top face. This parameter is preferably used when the cohesive element is used for connecting shells with different thicknesses. In this case the cohesive layer should not be located exactly between the bottom and top layer which is the default location
          * - :py:attr:`~gaskeit`
            - Get or set the Gasket thickness for converting ELFORM 19, 20, 21 and 22 to gasket elements and use with *MAT_COHESIVE_GASKET
          * - :py:attr:`~cohthk`
            - Get or set the Cohesive thickness. This value supersedes the THICK value from *MAT_240 or *MAT_ADD_COHESIVE allowing a section- or part-wise definition of cohesive thickness
          * - :py:attr:`~nip`
            - Get or set the Number of integration points for user-defined solid (0 if resultant/discrete element).
          * - :py:attr:`~nxdof`
            - Get or set the Number of extra degrees of freedom per node for user-defined solid.
          * - :py:attr:`~ihgf`
            - Get or set the Flag for using hourglass stabilization (NIP.GT.0).
          * - :py:attr:`~itaj`
            - Get or set the Flag for setting up finite element matrices (NIP.GT.0).
          * - :py:attr:`~lmc`
            - Get or set the Number of property parameters.
          * - :py:attr:`~nhsv`
            - Get or set the Number of history variables.
          * - :py:attr:`~xi`
            - Get or set the First isoparametric coordinate.
          * - :py:attr:`~eta`
            - Get or set the Second isoparametric coordinate.
          * - :py:attr:`~zeta`
            - Get or set the Third isoparametric coordinate.
          * - :py:attr:`~wgt`
            - Get or set the Isoparametric weight.
          * - :py:attr:`~p1`
            - Get or set the property parameter.
          * - :py:attr:`~p2`
            - Get or set the property parameter.
          * - :py:attr:`~p3`
            - Get or set the property parameter.
          * - :py:attr:`~p4`
            - Get or set the property parameter.
          * - :py:attr:`~p5`
            - Get or set the property parameter.
          * - :py:attr:`~p6`
            - Get or set the property parameter.
          * - :py:attr:`~p7`
            - Get or set the property parameter.
          * - :py:attr:`~p8`
            - Get or set the property parameter.
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

    from section_solid_misc import SectionSolidMisc

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation options.  Remark 2 enumerates the element formulations available for implicit calculations:
   EQ. -18: 8 point enhanced strain solid element with 13 incompatible modes(see Remarks 4 and 22)
   EQ. -2: 8 point hexahedron intended for elements with poor aspect ratios, accurate formulation(see Remark 15)
   EQ. -1: 8 point hexahedron intended for elements with poor aspect ratios, efficient formulation(see Remark 15)
   EQ. 0: 1 point corotational for *MAT_MODIFIED_HONEYCOMB(see Remark 3)
   EQ.1: Constant stress solid element : default element type.By specifying hourglass type 10 with this element, a Cosserat Point  Element is invoked; see *CONTROL_HOURGLASS.
   EQ.2: 8 point hexahedron(see Remark 4)
   EQ.3: Fully integrated quadratic 8 node element with nodal rotations
   EQ.4: S/R quadratic tetrahedron element with nodal rotations
   EQ.5 : 1 point ALE
   EQ.6 : 1 point Eulerian
   EQ.7 : 1 point Eulerian ambient
   EQ.8 : Acoustic
   EQ.9 : 1 point corotational for *MAT_MODIFIED_HONEYCOMB(see Remark 3)
   EQ.10 : 1 point tetrahedron(see Remark 1)
   EQ.11 : 1 point ALE multi - material element
   EQ.12 : 1 point integration with single material and void
   EQ.13 : 1 point nodal pressure tetrahedron(see Remark 14)
   EQ.14 : 8 point acoustic
   EQ.15 : 2 point pentahedron element(see Remark 1)
   EQ.16 : 4 or 5 point 10 - noded tetrahedron(see Remark 13).By specifying hourglass type 10 with this element, a Cosserat Point Element is invoked; see *CONTROL_HOURGLASS.
   EQ.17: 10 - noded composite tetrahedron(see Remark 13)
   EQ.18 : 9 point enhanced strain solid element with 12 incompatible modes(implicit only; see Remarks 4 and 22)
   EQ.19 : 8 - noded, 4 point cohesive element(see Remarks 1 and 6)
   EQ.20 : 8 - noded, 4 point cohesive element with offsets for use with shells(see Remarks 1, 6,and 8)
   EQ.21 : 6 - noded, 1 point pentahedron cohesive element(see Remarks 1 and 7)
   EQ.22 : 6 - noded, 1 point pentahedron cohesive element with offsets for use with shells(see Remarks 1, 7,and 8)
   EQ.23 : 20 - node solid formulation
   EQ.24 : 27 - noded, fully integrated S / R quadratic solid element(see Remark 21)
   EQ.25 : 21 - noded, quadratic pentahedron(see Remark 21)
   EQ.26 : 15 - noded, quadratic tetrahedron(see Remark 21)
   EQ.27 : 20 - noded, cubic tetrahedron(see Remark 21)
   EQ.28 : 40 - noded, cubic pentrahedron(see Remark 21)
   EQ.29 : 64 - noded, cubic hexahedron(see Remark 21)
   EQ.41 : Mesh - free(EFG) solid formulation(see Remark 16)
   EQ.42 : Adaptive 4 - noded mesh - free(EFG) solid formulation(see Remark 16)
   EQ.43 : Mesh - free enriched finite element
   EQ.45 : Tied mesh - free enriched finite element
   EQ.47 : Smoothed Particle Galerkin(SPG) method(see Remark 17)
   EQ.60 : 1 point tetrahedron(see Remark 19)
   EQ.62:  8 point brick with incompatible modes by assumed strain
   EQ.98 : Interpolation solid
   EQ.99 : Simplified linear element for time - domain vibration studies(See Remark 5)
   EQ.101 : User defined solid
   EQ.102 : User defined solid
   EQ.103 : User defined solid
   EQ.104 : User defined solid
   EQ.105 : User defined solid
   EQ.115 : 1 point pentahedron element with hourglass control
   GE.201 : Isogeometric solids with NURBS. (see *ELEMENT_SOLID_NURBS_PATCH)
   GE.1000 : Generalized user - defined solid element formulation(see *DEFINE_ELEMENT_GENERALIZED_SOLID)
















   ..
       !! processed by numpydoc !!

.. py:property:: aet
   :type: int


   
   Get or set the Ambient Element type: Can be defined for ELFORM 7, 11 and 12.
   EQ.1: temperature (not currently available),
   EQ.2: pressure and temperature (not currently available),
   EQ.3: pressure outflow,
   EQ.4: pressure inflow (default for ELFORM 7).
   EQ.5: receptor for blast load (see *LOAD_BLAST_ENHANCED, available only for ELFORM=11).
















   ..
       !! processed by numpydoc !!

.. py:property:: cohoff
   :type: Optional[float]


   
   Get or set the Applies to cohesive solid elements 20 and 22. COHOFF specifies the relative location of the cohesive layer. It must be a number between -1 and 1. A value of -1 will place it on the bottom face of the cohesive element, while a value of +1 will place it on the top face. This parameter is preferably used when the cohesive element is used for connecting shells with different thicknesses. In this case the cohesive layer should not be located exactly between the bottom and top layer which is the default location
















   ..
       !! processed by numpydoc !!

.. py:property:: gaskeit
   :type: Optional[float]


   
   Get or set the Gasket thickness for converting ELFORM 19, 20, 21 and 22 to gasket elements and use with *MAT_COHESIVE_GASKET
















   ..
       !! processed by numpydoc !!

.. py:property:: cohthk
   :type: Optional[float]


   
   Get or set the Cohesive thickness. This value supersedes the THICK value from *MAT_240 or *MAT_ADD_COHESIVE allowing a section- or part-wise definition of cohesive thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: Optional[int]


   
   Get or set the Number of integration points for user-defined solid (0 if resultant/discrete element).
















   ..
       !! processed by numpydoc !!

.. py:property:: nxdof
   :type: Optional[int]


   
   Get or set the Number of extra degrees of freedom per node for user-defined solid.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihgf
   :type: int


   
   Get or set the Flag for using hourglass stabilization (NIP.GT.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: itaj
   :type: int


   
   Get or set the Flag for setting up finite element matrices (NIP.GT.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lmc
   :type: Optional[int]


   
   Get or set the Number of property parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhsv
   :type: Optional[int]


   
   Get or set the Number of history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: xi
   :type: Optional[float]


   
   Get or set the First isoparametric coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: eta
   :type: Optional[float]


   
   Get or set the Second isoparametric coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zeta
   :type: Optional[float]


   
   Get or set the Third isoparametric coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: wgt
   :type: Optional[float]


   
   Get or set the Isoparametric weight.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the property parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the property parameter.
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'SOLID_MISC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





