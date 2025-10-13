





:class:`SectionSolidSpg`
========================


.. py:class:: section_solid_spg.SectionSolidSpg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SOLID_SPG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionSolidSpg

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
          * - :py:attr:`~dx`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
          * - :py:attr:`~dy`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
          * - :py:attr:`~dz`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
          * - :py:attr:`~ispline`
            - Get or set the Type of kernel function:
          * - :py:attr:`~kernel`
            - Get or set the Type of kernel support update scheme:
          * - :py:attr:`~smstep`
            - Get or set the Interval of time steps to conduct displacement smoothing
          * - :py:attr:`~msc`
            - Get or set the Smoothing scheme for momentum consistent SPG only (ITB=3 on Card2b)
          * - :py:attr:`~idam`
            - Get or set the Option of bond failure mechanism
          * - :py:attr:`~fs`
            - Get or set the Critical value of the quantity indicated by IDAM for bond failure triggering. Default: 1.0E+10, i.e., no failure analysis
          * - :py:attr:`~stretch`
            - Get or set the Critical relative deformation (stretching or compression ratio) between the two nodes forming the bond for bond failure
          * - :py:attr:`~itb`
            - Get or set the Option of stabilization:
          * - :py:attr:`~msfac`
            - Get or set the For momentum consistent SPG invoked with ITB = 3 only, quadrature factor for surface nodes to suppress shear locking in thin structures. We recommend using the latest beta version or R14. The default for a regular solid structure is 1.00 while for a thin structure is 0.75.
          * - :py:attr:`~isc`
            - Get or set the Self-contact indicator:
          * - :py:attr:`~boxid`
            - Get or set the ID of a box defining the active SPG region. Outside this region, the particles are not included in the SPG calculation. See *DEFINE_BOX
          * - :py:attr:`~pdamp`
            - Get or set the Particle-to-particle damping coefficient. It is used for momentum consistent SPG (ITB = 3) only. The recommended range of values is -0.01 to -0.001. A positive value is not recommended
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

    from section_solid_spg import SectionSolidSpg

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

.. py:property:: dx
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: dy
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: dz
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions, respectively.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and locality on the construction of the mesh-free shape functions.  Values between 1.4 and 1.8 are recommended.  Values smaller than 1.0 are not allowed.  Larger values will increase the computation time and will sometimes result in divergence of the solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: ispline
   :type: int


   
   Get or set the Type of kernel function:
   EQ.0:   Cubic spline function with cubical support(default)
   EQ.1 : Quadratic spline function with cubical support
   EQ.2 : Cubic spline function with spherical support
















   ..
       !! processed by numpydoc !!

.. py:property:: kernel
   :type: int


   
   Get or set the Type of kernel support update scheme:
   EQ.0:   Updated Lagrangian, failure or no failure analysis, tension dominant problem
   EQ.1 : Eulerian, failure analysis, global extreme deformation
   EQ.2 : Pseudo Lagrangian, failure analysis, local extreme deformation
















   ..
       !! processed by numpydoc !!

.. py:property:: smstep
   :type: Optional[int]


   
   Get or set the Interval of time steps to conduct displacement smoothing
   Default: 15, if KERNEL = 0
   5, if KERNEL = 1
   30, if KERNEL = 2
   Code will determine according to KERNEL if SMSTEP = 0 from input
















   ..
       !! processed by numpydoc !!

.. py:property:: msc
   :type: Optional[float]


   
   Get or set the Smoothing scheme for momentum consistent SPG only (ITB=3 on Card2b)
   EQ.0:  Regular smoothing scheme
   EQ.1: New smoothing scheme for very low speed deformation, with better controls of the low energy modes than the regular smoothing scheme
















   ..
       !! processed by numpydoc !!

.. py:property:: idam
   :type: int


   
   Get or set the Option of bond failure mechanism
   EQ.1:   Effective plastic strain(phenomenological strain damage, default)
   EQ.2 : Maximum principal stress
   EQ.3 : Maximum shear strain
   EQ.4 : Minimum principal strain(input must be positive)
   EQ.5 : Effective plastic strain and maximum shear strain
   EQ.7 : Anisotropic damage for honeycomb modeled with* MAT_126 only.We recommend only using this with ITB = 3. This feature is available starting with R13.
   EQ.11 : Pre - damage model for brittle material failure(with crack propagation).It includes both bond failureand stress degradation.It is available as of R13.We recommend using this with ITB = 3.
   EQ.13 : Pre - damage model for ductile material failure.It includes stress degradation but not bond failure.It is available as of R13.We recommend using this with ITB = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Critical value of the quantity indicated by IDAM for bond failure triggering. Default: 1.0E+10, i.e., no failure analysis
   For * MAT_3 and *MAT_24, “FS” on material cards overwrites this value.
   When FS is defined on material cards, not only bond failure will occur when it is reached, but also the stress will be set to zero according to material law.
   If FS is defined on SPG card, only bond failure will occur without setting  stress to zero
















   ..
       !! processed by numpydoc !!

.. py:property:: stretch
   :type: float


   
   Get or set the Critical relative deformation (stretching or compression ratio) between the two nodes forming the bond for bond failure
















   ..
       !! processed by numpydoc !!

.. py:property:: itb
   :type: Optional[int]


   
   Get or set the Option of stabilization:
   EQ.1:   Fluid particle approximation(accurate but slow), used with KERNEL = 0 or 1
   EQ.2 : Simplified fluid particle approximation(efficient and robust), used with KERNEL = 2
   EQ.3 : Momentum consistent SPG(MCSPG) formulation(latest beta or R14 is recommended).MCSPG can be applied for large deformation, tension dominant problems.For coupled thermal mechanical problems, MCSPG is the only option.KERNEL = 1 is recommended for MCSPG.
   Default : 1, if KERNEL = 0 or 1
   2, if KERNEL = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: msfac
   :type: Optional[float]


   
   Get or set the For momentum consistent SPG invoked with ITB = 3 only, quadrature factor for surface nodes to suppress shear locking in thin structures. We recommend using the latest beta version or R14. The default for a regular solid structure is 1.00 while for a thin structure is 0.75.
















   ..
       !! processed by numpydoc !!

.. py:property:: isc
   :type: Optional[float]


   
   Get or set the Self-contact indicator:
   EQ.0:   No self - contact between the bond - failed particles in the same part.
   EQ.1 : Self - contact is defined between the bond - failed particles in the same part.The penalty factor in the self - contact is between 0.01 to 0.1 × Young’s modulus.This option is available for SMP only.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the ID of a box defining the active SPG region. Outside this region, the particles are not included in the SPG calculation. See *DEFINE_BOX
















   ..
       !! processed by numpydoc !!

.. py:property:: pdamp
   :type: float


   
   Get or set the Particle-to-particle damping coefficient. It is used for momentum consistent SPG (ITB = 3) only. The recommended range of values is -0.01 to -0.001. A positive value is not recommended
















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
   :value: 'SOLID_SPG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





