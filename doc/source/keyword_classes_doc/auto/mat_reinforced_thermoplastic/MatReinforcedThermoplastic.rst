





:class:`MatReinforcedThermoplastic`
===================================


.. py:class:: mat_reinforced_thermoplastic.MatReinforcedThermoplastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_REINFORCED_THERMOPLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatReinforcedThermoplastic

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
            - Get or set the Density
          * - :py:attr:`~em`
            - Get or set the Young's modulus of matrix material
          * - :py:attr:`~lcem`
            - Get or set the Curve ID for Young's modulus of matrix material as a function of temperature. With this option active, EM is ignored
          * - :py:attr:`~prm`
            - Get or set the Poisson's ratio for matrix material
          * - :py:attr:`~lcprm`
            - Get or set the Curve ID for Poisson's ratio of matrix material versus temperature. With this option active, EM is ignored
          * - :py:attr:`~lcsigy`
            - Get or set the Load curve or table ID for strain hardening of the matrix. If a curve, then it specifies yield stress as a function of effective plastic strain. If a table, then temperatures are the table values indexing curves giving yield stress as a function of effective plastic strain (see *DEFINE_‌TABLE).
          * - :py:attr:`~beta`
            - Get or set the Parameter for mixed hardening. Set BETA = 0 for pure kinematic hardening and BETA = 1 for pure isotropic hardening
          * - :py:attr:`~nfib`
            - Get or set the Number of fiber families to be considered
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
          * - :py:attr:`~a1`
            - Get or set the X-component of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Y-component of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Z-component of vector a for AOPT = 2
          * - :py:attr:`~v1`
            - Get or set the X-component of vector v for AOPT = 3
          * - :py:attr:`~v2`
            - Get or set the Y-component of vector v for AOPT = 3
          * - :py:attr:`~v3`
            - Get or set the Z-component of vector v for AOPT = 3
          * - :py:attr:`~d1`
            - Get or set the X-component of vector d for AOPT = 2
          * - :py:attr:`~d2`
            - Get or set the Y-component of vector d for AOPT = 2
          * - :py:attr:`~d3`
            - Get or set the Z-component of vector d for AOPT = 2
          * - :py:attr:`~mangl`
            - Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
          * - :py:attr:`~thick`
            - Get or set the Balance thickness changes of the material due to the matrix description by scaling fiber stresses:
          * - :py:attr:`~idf1`
            - Get or set the ID for 1st fiber family for post-processing
          * - :py:attr:`~alph1`
            - Get or set the Orientation angle ALPHA for 1st fiber with respect to overall material direction
          * - :py:attr:`~ef1`
            - Get or set the Young's modulus for 1st fiber family
          * - :py:attr:`~lcef1`
            - Get or set the Load curve for stress as a function of fiber strain of 1st fiber. With this option active, EF1 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
          * - :py:attr:`~g23_1`
            - Get or set the Transversal shear modulus orthogonal to direction of fiber 1
          * - :py:attr:`~g31_1`
            - Get or set the Transversal shear modulus in direction of fiber 1
          * - :py:attr:`~g12`
            - Get or set the Linear shear modulus for shearing between fiber 1 and 2
          * - :py:attr:`~lcg12`
            - Get or set the Curve ID for shear stress versus shearing between of 1st and 2nd fiber. With this option active, G12 is ignored. For details see parameter METH12
          * - :py:attr:`~aloc12`
            - Get or set the Locking angle (in radians) for shear between fiber families 1 and 2
          * - :py:attr:`~gloc12`
            - Get or set the Linear shear modulus for shear angles larger than ALOC12
          * - :py:attr:`~meth12`
            - Get or set the Option for shear between fiber 1 and 2:
          * - :py:attr:`~idf2`
            - Get or set the ID for 2nd fiber family for post-processing
          * - :py:attr:`~alph2`
            - Get or set the Orientation angle ALPHA for 2nd fiber with respect to overall material direction
          * - :py:attr:`~ef2`
            - Get or set the Young's modulus for 2nd fiber family
          * - :py:attr:`~lcef2`
            - Get or set the Load curve for stress as a function of fiber strain of 2nd fiber. With this option active, EF2 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
          * - :py:attr:`~g23_2`
            - Get or set the Transversal shear modulus orthogonal to direction of fiber 2
          * - :py:attr:`~g31_2`
            - Get or set the Transversal shear modulus in direction of fiber 2
          * - :py:attr:`~g23`
            - Get or set the Linear shear modulus for shearing between fiber 2 and 3
          * - :py:attr:`~lcg23`
            - Get or set the Curve ID for shear stress versus shearing between of 2nd and 3rd fiber. With this option active, G12 is ignored. For details see parameter METH23
          * - :py:attr:`~aloc23`
            - Get or set the Locking angle (in radians) for shear between fiber families 2 and 3
          * - :py:attr:`~gloc23`
            - Get or set the Linear shear modulus for shear angles larger than ALOC23
          * - :py:attr:`~meth23`
            - Get or set the Option for shear between fiber 2 and 3:
          * - :py:attr:`~idf3`
            - Get or set the ID for 3rd fiber family for post-processing
          * - :py:attr:`~alph3`
            - Get or set the Orientation angle ALPHA for 3rd fiber with respect to overall material direction
          * - :py:attr:`~ef3`
            - Get or set the Young's modulus for 3rd fiber family
          * - :py:attr:`~lcef3`
            - Get or set the Load curve for stress versus fiber strain of 3rd fiber. With this option active, EF3 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
          * - :py:attr:`~g23_3`
            - Get or set the Transversal shear modulus orthogonal to direction of fiber 3
          * - :py:attr:`~g31_3`
            - Get or set the Transversal shear modulus in direction of fiber 3
          * - :py:attr:`~postv`
            - Get or set the Defines additional history variables that might be useful for post-processing.
          * - :py:attr:`~ihis`
            - Get or set the Flag for material properties initialization:
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

    from mat_reinforced_thermoplastic import MatReinforcedThermoplastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Density
















   ..
       !! processed by numpydoc !!

.. py:property:: em
   :type: Optional[float]


   
   Get or set the Young's modulus of matrix material
















   ..
       !! processed by numpydoc !!

.. py:property:: lcem
   :type: Optional[int]


   
   Get or set the Curve ID for Young's modulus of matrix material as a function of temperature. With this option active, EM is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: prm
   :type: Optional[float]


   
   Get or set the Poisson's ratio for matrix material
















   ..
       !! processed by numpydoc !!

.. py:property:: lcprm
   :type: Optional[int]


   
   Get or set the Curve ID for Poisson's ratio of matrix material versus temperature. With this option active, EM is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsigy
   :type: Optional[int]


   
   Get or set the Load curve or table ID for strain hardening of the matrix. If a curve, then it specifies yield stress as a function of effective plastic strain. If a table, then temperatures are the table values indexing curves giving yield stress as a function of effective plastic strain (see *DEFINE_‌TABLE).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Parameter for mixed hardening. Set BETA = 0 for pure kinematic hardening and BETA = 1 for pure isotropic hardening
















   ..
       !! processed by numpydoc !!

.. py:property:: nfib
   :type: Optional[int]


   
   Get or set the Number of fiber families to be considered
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes, as with* DEFINE_COORDI - NATE_NODES,and then rotated about the shell element normal by the angle MANGL.
   EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR.
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle.The angle may be set in the keyword input for the element or in the input for this keyword with MANGL.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR).
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the X-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Y-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Z-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the X-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Y-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Z-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the X-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Y-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Z-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: mangl
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Balance thickness changes of the material due to the matrix description by scaling fiber stresses:
   EQ.0:   No scaling
   EQ.1 : Scaling
















   ..
       !! processed by numpydoc !!

.. py:property:: idf1
   :type: Optional[int]


   
   Get or set the ID for 1st fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph1
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for 1st fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef1
   :type: Optional[float]


   
   Get or set the Young's modulus for 1st fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: lcef1
   :type: Optional[int]


   
   Get or set the Load curve for stress as a function of fiber strain of 1st fiber. With this option active, EF1 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23_1
   :type: Optional[float]


   
   Get or set the Transversal shear modulus orthogonal to direction of fiber 1
















   ..
       !! processed by numpydoc !!

.. py:property:: g31_1
   :type: Optional[float]


   
   Get or set the Transversal shear modulus in direction of fiber 1
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the Linear shear modulus for shearing between fiber 1 and 2
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg12
   :type: Optional[int]


   
   Get or set the Curve ID for shear stress versus shearing between of 1st and 2nd fiber. With this option active, G12 is ignored. For details see parameter METH12
















   ..
       !! processed by numpydoc !!

.. py:property:: aloc12
   :type: Optional[float]


   
   Get or set the Locking angle (in radians) for shear between fiber families 1 and 2
















   ..
       !! processed by numpydoc !!

.. py:property:: gloc12
   :type: Optional[float]


   
   Get or set the Linear shear modulus for shear angles larger than ALOC12
















   ..
       !! processed by numpydoc !!

.. py:property:: meth12
   :type: Optional[int]


   
   Get or set the Option for shear between fiber 1 and 2:
   EQ.0: Elastic shear response, curve LCG12 defines shear stress as a function of scalar product of fibers directions. ALOC12 and GLOC12 are ignored.
   EQ.1: Elasto-plastic shear response, curve LCG12 defines shear stress as a function of the scalar product of fiber directions.
   EQ.2: Elastic shear response, curve LCG12 defines shear stress as a function of shear angle between fiber given in radians. ALOC12 and GLOC12 are ignored.
   EQ.3: Elasto-plastic shear response, curve LCGij defines shear stress vs. shear angle between fibers given in radians.
   EQ.4: Elastic shear response, curve LCG12 defines shear stress vs. shear angle between fiber given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching. ALOC12 and GLOC12 are ignored.
   EQ.5: Elasto-plastic shear response, curve LCG12 defines shear stress vs. shear angle between fibers given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching
















   ..
       !! processed by numpydoc !!

.. py:property:: idf2
   :type: Optional[int]


   
   Get or set the ID for 2nd fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph2
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for 2nd fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef2
   :type: Optional[float]


   
   Get or set the Young's modulus for 2nd fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: lcef2
   :type: Optional[int]


   
   Get or set the Load curve for stress as a function of fiber strain of 2nd fiber. With this option active, EF2 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23_2
   :type: Optional[float]


   
   Get or set the Transversal shear modulus orthogonal to direction of fiber 2
















   ..
       !! processed by numpydoc !!

.. py:property:: g31_2
   :type: Optional[float]


   
   Get or set the Transversal shear modulus in direction of fiber 2
















   ..
       !! processed by numpydoc !!

.. py:property:: g23
   :type: Optional[float]


   
   Get or set the Linear shear modulus for shearing between fiber 2 and 3
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg23
   :type: Optional[int]


   
   Get or set the Curve ID for shear stress versus shearing between of 2nd and 3rd fiber. With this option active, G12 is ignored. For details see parameter METH23
















   ..
       !! processed by numpydoc !!

.. py:property:: aloc23
   :type: Optional[float]


   
   Get or set the Locking angle (in radians) for shear between fiber families 2 and 3
















   ..
       !! processed by numpydoc !!

.. py:property:: gloc23
   :type: Optional[float]


   
   Get or set the Linear shear modulus for shear angles larger than ALOC23
















   ..
       !! processed by numpydoc !!

.. py:property:: meth23
   :type: Optional[int]


   
   Get or set the Option for shear between fiber 2 and 3:
   EQ.0: Elastic shear response, curve LCG23 defines shear stress as a function of scalar product of fibers directions. ALOC23 and GLOC23 are ignored.
   EQ.1: Elasto-plastic shear response, curve LCG23 defines shear stress as a function of the scalar product of fiber directions.
   EQ.2: Elastic shear response, curve LCG23 defines shear stress as a function of shear angle between fiber given in radians. ALOC23 and GLOC23 are ignored.
   EQ.3: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians.
   EQ.4: Elastic shear response, curve LCG23 defines shear stress vs. shear angle between fiber given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching. ALOC23 and GLOC23 are ignored.
   EQ.5: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching
















   ..
       !! processed by numpydoc !!

.. py:property:: idf3
   :type: Optional[int]


   
   Get or set the ID for 3rd fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph3
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for 3rd fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef3
   :type: Optional[float]


   
   Get or set the Young's modulus for 3rd fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: lcef3
   :type: Optional[int]


   
   Get or set the Load curve for stress versus fiber strain of 3rd fiber. With this option active, EF3 is ignored. If a curve, then it specifies input stress as a function of fiber strain. If a table, then temperatures are the table values indexing curves giving stress as function of fiber strain. The table data will be extrapolated for both strains and temperatures where necessary.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23_3
   :type: Optional[float]


   
   Get or set the Transversal shear modulus orthogonal to direction of fiber 3
















   ..
       !! processed by numpydoc !!

.. py:property:: g31_3
   :type: Optional[float]


   
   Get or set the Transversal shear modulus in direction of fiber 3
















   ..
       !! processed by numpydoc !!

.. py:property:: postv
   :type: Optional[int]


   
   Get or set the Defines additional history variables that might be useful for post-processing.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihis
   :type: Optional[float]


   
   Get or set the Flag for material properties initialization:
   EQ.0:   Material properties defined in Cards 1 - 9 are used.
   GE.1 : Use * INITIAL_‌STRESS_‌SHELL to initialize some material properties on an element - by - element basis
















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
   :value: 'REINFORCED_THERMOPLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





