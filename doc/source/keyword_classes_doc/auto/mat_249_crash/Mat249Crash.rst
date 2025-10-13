





:class:`Mat249Crash`
====================


.. py:class:: mat_249_crash.Mat249Crash(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_249_CRASH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat249Crash

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
            - Get or set the MAss density
          * - :py:attr:`~em`
            - Get or set the Young's modulus of matrix material
          * - :py:attr:`~prm`
            - Get or set the Poisson's ratio for matrix material
          * - :py:attr:`~lcsigy`
            - Get or set the Load curve or table ID for strain hardening of the matrix. If a curve, then it specifies yield stress as a function of effective plastic strain. If a table, then temperatures are the table values indexing curves giving yield stress as a function of effective plastic strain (see *DEFINE_‌TABLE).
          * - :py:attr:`~beta`
            - Get or set the Parameter for mixed hardening. Set BETA = 0 for pure kinematic hardening and BETA = 1 for pure isotropic hardening
          * - :py:attr:`~pfl`
            - Get or set the Percentage of layers that must fail to initiate failure of the element (Default is 100).
          * - :py:attr:`~visc`
            - Get or set the Viscous formulation for fibers:
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
            - Get or set the Balance thickness changes of the material due to the matrix response when calculating the fiber stresses. Stresses can be scaled to account for the fact that fiber cross-sectional usually does not change.
          * - :py:attr:`~vg1`
            - Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vb1`
            - Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vg2`
            - Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vb2`
            - Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vg3`
            - Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vb3`
            - Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vg4`
            - Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~vb4`
            - Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
          * - :py:attr:`~idf1`
            - Get or set the ID for 1st fiber family for post-processing
          * - :py:attr:`~alph1`
            - Get or set the Orientation angle ALPHA for 1st fiber with respect to overall material direction
          * - :py:attr:`~ef1`
            - Get or set the Young's modulus for 1st fiber family
          * - :py:attr:`~lcef1`
            - Get or set the Curve ID for stress versus fiber elongation of 1st fiber. With this option active, EF1 is ignored
          * - :py:attr:`~g23_1`
            - Get or set the Transversal shear modulus orthogonal to direction of fiber 1
          * - :py:attr:`~g31_1`
            - Get or set the Transversal shear modulus in direction of fiber 1
          * - :py:attr:`~daf1`
            - Get or set the Load curve or table ID for damage parameter d_1^ffor 1st fiber (see Remark 2). If a curve, DAF1 specifies damage as a function of fiber strain (for compression and elongation). If DAF1 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
          * - :py:attr:`~dam1`
            - Get or set the Load curve or table ID for damage parameter d_1^mfor matrix material based on the current deformation status of the 1st fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
          * - :py:attr:`~g12`
            - Get or set the Linear shear modulus for shearing between fiber 1 and 2
          * - :py:attr:`~lcg12`
            - Get or set the Curve ID for shear stress as a function of shearing type as specified with METH12 between the 1st and 2nd fibers.
          * - :py:attr:`~aloc12`
            - Get or set the Locking angle (in radians) for shear between fiber families 1 and 2
          * - :py:attr:`~gloc12`
            - Get or set the Linear shear modulus for shear angles larger than ALOC12
          * - :py:attr:`~meth12`
            - Get or set the Option for shear response between fiber 1 and 2 (see Remark 1):
          * - :py:attr:`~dam12`
            - Get or set the Load curve ID defining the damage parameter d_12^m for the matrix as function of shear angle (radians) between the 1st and 2nd fiber (see Remark 2). The damage parameter d_12^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_12^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
          * - :py:attr:`~idf2`
            - Get or set the ID for 2nd fiber family for post-processing
          * - :py:attr:`~alph2`
            - Get or set the Orientation angle ALPHA for 2nd fiber with respect to overall material direction
          * - :py:attr:`~ef2`
            - Get or set the Young's modulus for 2nd fiber family
          * - :py:attr:`~lcef2`
            - Get or set the Curve ID for stress as a functionof fiber elongation of 2nd fiber. With this option active, EF2 is ignored
          * - :py:attr:`~g23_2`
            - Get or set the Transversal shear modulus orthogonal to direction of fiber 2
          * - :py:attr:`~g31_2`
            - Get or set the Transversal shear modulus in direction of fiber 2
          * - :py:attr:`~daf2`
            - Get or set the Load curve or table ID for damage parameter d_2^ffor 2nd fiber (see Remark 2). If a curve, DAF2 specifies damage as a function of fiber strain (for compression and elongation). If DAF2 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
          * - :py:attr:`~dam2`
            - Get or set the Load curve or table ID for damage parameter d_2^mfor matrix material based on the current deformation status of the 2nd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
          * - :py:attr:`~g23`
            - Get or set the Linear shear modulus for shearing between fiber families 2 and 3
          * - :py:attr:`~lcg23`
            - Get or set the Curve ID for shear stress as a function of shearing type as specifies with METH23 between the 2nd and 3rd fibers.
          * - :py:attr:`~aloc23`
            - Get or set the Locking angle (in radians) for shear between fiber families 2 and 3
          * - :py:attr:`~gloc23`
            - Get or set the Linear shear modulus for shear angles larger than ALOC23
          * - :py:attr:`~meth23`
            - Get or set the Option for shear response between fibers 2 and 3:
          * - :py:attr:`~dam23`
            - Get or set the Load curve ID defining the damage parameter d_23^m for the matrix as function of shear angle (in rad) between 1st and 2nd fiber. The damager parameter d_23^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_23^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
          * - :py:attr:`~idf3`
            - Get or set the ID for 3rd fiber family for post-processing
          * - :py:attr:`~alph3`
            - Get or set the Orientation angle ALPHA for 3rd fiber with respect to overall material direction
          * - :py:attr:`~ef3`
            - Get or set the Young's modulus for 3rd fiber family
          * - :py:attr:`~lcef3`
            - Get or set the Curve ID for stress versus fiber elongation of 3rd fiber. With this option active, EF3 is ignored
          * - :py:attr:`~g23_3`
            - Get or set the Transverse shear modulus orthogonal to direction of fiber 3
          * - :py:attr:`~g31_3`
            - Get or set the Transverse shear modulus in direction of fiber 3
          * - :py:attr:`~daf3`
            - Get or set the Load curve or table ID for damage parameter d_3^ffor 3rd fiber (see Remark 2). If a curve, DAF3 specifies damage as a function of fiber strain (for compression and elongation). If DAF3 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
          * - :py:attr:`~dam3`
            - Get or set the Load curve or table ID for damage parameter d_3^mfor matrix material based on the current deformation status of the 3rd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
          * - :py:attr:`~postv`
            - Get or set the Parameter for outputting additional history variables that might be useful for post-processing.
          * - :py:attr:`~viscs`
            - Get or set the Portion of viscous relaxation moduli VGk that is accounted for in time step size calculation
          * - :py:attr:`~ihis`
            - Get or set the Flag for material properties initialization :
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

    from mat_249_crash import Mat249Crash

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the MAss density
















   ..
       !! processed by numpydoc !!

.. py:property:: em
   :type: Optional[float]


   
   Get or set the Young's modulus of matrix material
















   ..
       !! processed by numpydoc !!

.. py:property:: prm
   :type: Optional[float]


   
   Get or set the Poisson's ratio for matrix material
















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

.. py:property:: pfl
   :type: Optional[float]


   
   Get or set the Percentage of layers that must fail to initiate failure of the element (Default is 100).
















   ..
       !! processed by numpydoc !!

.. py:property:: visc
   :type: Optional[float]


   
   Get or set the Viscous formulation for fibers:
   EQ.0.0: Elastic behavior.
   EQ.1.0 : Viscoelastic behavior modeled with Prony series.
















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
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle MANGL.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















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


   
   Get or set the Balance thickness changes of the material due to the matrix response when calculating the fiber stresses. Stresses can be scaled to account for the fact that fiber cross-sectional usually does not change.
   EQ.0:   No scaling
   EQ.1 : Scaling
















   ..
       !! processed by numpydoc !!

.. py:property:: vg1
   :type: Optional[float]


   
   Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vb1
   :type: Optional[float]


   
   Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vg2
   :type: Optional[float]


   
   Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vb2
   :type: Optional[float]


   
   Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vg3
   :type: Optional[float]


   
   Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vb3
   :type: Optional[float]


   
   Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vg4
   :type: Optional[float]


   
   Get or set the Relaxation modulus G_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: vb4
   :type: Optional[float]


   
   Get or set the Decay constant β_k for the k-th term of the Prony series for viscoelastic fibers
















   ..
       !! processed by numpydoc !!

.. py:property:: idf1
   :type: Optional[int]


   
   Get or set the ID for 1st fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph1
   :type: Optional[int]


   
   Get or set the Orientation angle ALPHA for 1st fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef1
   :type: Optional[int]


   
   Get or set the Young's modulus for 1st fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: lcef1
   :type: Optional[int]


   
   Get or set the Curve ID for stress versus fiber elongation of 1st fiber. With this option active, EF1 is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: g23_1
   :type: Optional[int]


   
   Get or set the Transversal shear modulus orthogonal to direction of fiber 1
















   ..
       !! processed by numpydoc !!

.. py:property:: g31_1
   :type: Optional[int]


   
   Get or set the Transversal shear modulus in direction of fiber 1
















   ..
       !! processed by numpydoc !!

.. py:property:: daf1
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_1^ffor 1st fiber (see Remark 2). If a curve, DAF1 specifies damage as a function of fiber strain (for compression and elongation). If DAF1 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
   The damager parameter d_1 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dam1
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_1^mfor matrix material based on the current deformation status of the 1st fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
   The damager parameter d_1 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_1^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[int]


   
   Get or set the Linear shear modulus for shearing between fiber 1 and 2
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg12
   :type: Optional[int]


   
   Get or set the Curve ID for shear stress as a function of shearing type as specified with METH12 between the 1st and 2nd fibers.
















   ..
       !! processed by numpydoc !!

.. py:property:: aloc12
   :type: Optional[int]


   
   Get or set the Locking angle (in radians) for shear between fiber families 1 and 2
















   ..
       !! processed by numpydoc !!

.. py:property:: gloc12
   :type: Optional[int]


   
   Get or set the Linear shear modulus for shear angles larger than ALOC12
















   ..
       !! processed by numpydoc !!

.. py:property:: meth12
   :type: Optional[int]


   
   Get or set the Option for shear response between fiber 1 and 2 (see Remark 1):
   EQ.0:   Elastic shear response.Curve LCG12 specifies shear stress as a function of the scalar product of the fiber directions
   EQ.1 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of the normalized scalar product of the fiber directions.
   EQ.2 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.
   EQ.3 : Elasto - plastic shear response.Curve LCG12 defines yield shear stress as a function of normalized shear angle between the fibers.
   EQ.4 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.This option is a special implementation for non - crimped fabrics, where one of the fiber families corresponds to a stitching.
   EQ.5 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of normalized shear angle between the fibers.This option is a special implementation for non - crimped fabrics, where one of the fiber families corresponds to a stitching.
   EQ.10 : Elastic shear response.Curve LCG12 specifies shear stress as a function of shear angle(radians) between the fibers.This option is tailored for woven fabricsand guarantees a pure shear stress response.
   EQ.11 : Elasto - plastic shear response.Curve LCG12 specifies yield shear stress as a function of normalized shear angle.This option is tailored for woven fabricsand guarantees a pure shear stress response
















   ..
       !! processed by numpydoc !!

.. py:property:: dam12
   :type: Optional[int]


   
   Get or set the Load curve ID defining the damage parameter d_12^m for the matrix as function of shear angle (radians) between the 1st and 2nd fiber (see Remark 2). The damage parameter d_12^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_12^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
















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


   
   Get or set the Curve ID for stress as a functionof fiber elongation of 2nd fiber. With this option active, EF2 is ignored
















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

.. py:property:: daf2
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_2^ffor 2nd fiber (see Remark 2). If a curve, DAF2 specifies damage as a function of fiber strain (for compression and elongation). If DAF2 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
   The damager parameter d_2 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dam2
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_2^mfor matrix material based on the current deformation status of the 2nd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
   The damager parameter d_2 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_2^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23
   :type: Optional[float]


   
   Get or set the Linear shear modulus for shearing between fiber families 2 and 3
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg23
   :type: Optional[int]


   
   Get or set the Curve ID for shear stress as a function of shearing type as specifies with METH23 between the 2nd and 3rd fibers.
















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


   
   Get or set the Option for shear response between fibers 2 and 3:
   EQ.0: Elastic shear response, curve LCG23 defines shear stress as a function of scalar product of fibers directions. ALOC23 and GLOC23 are ignored.
   EQ.1: Elasto-plastic shear response, curve LCG23 defines shear stress as a function of the scalar product of fiber directions.
   EQ.2: Elastic shear response, curve LCG23 defines shear stress as a function of shear angle between fiber given in radians. ALOC23 and GLOC23 are ignored.
   EQ.3: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians.
   EQ.4: Elastic shear response, curve LCG23 defines shear stress vs. shear angle between fiber given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching. ALOC23 and GLOC23 are ignored.
   EQ.5: Elasto-plastic shear response, curve LCG23 defines shear stress vs. shear angle between fibers given in radians. This option is a special implementation for non-crimped fabrics, where one of the fiber families corresponds to a stitching
















   ..
       !! processed by numpydoc !!

.. py:property:: dam23
   :type: Optional[int]


   
   Get or set the Load curve ID defining the damage parameter d_23^m for the matrix as function of shear angle (in rad) between 1st and 2nd fiber. The damager parameter d_23^m ranges from 0.0 to 1.5. A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix. To initiate failure of the composite at the integration point, a matrix damage d_23^m of 1.5 must be reached. Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
















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


   
   Get or set the Curve ID for stress versus fiber elongation of 3rd fiber. With this option active, EF3 is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: g23_3
   :type: Optional[float]


   
   Get or set the Transverse shear modulus orthogonal to direction of fiber 3
















   ..
       !! processed by numpydoc !!

.. py:property:: g31_3
   :type: Optional[float]


   
   Get or set the Transverse shear modulus in direction of fiber 3
















   ..
       !! processed by numpydoc !!

.. py:property:: daf3
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_3^ffor 3rd fiber (see Remark 2). If a curve, DAF3 specifies damage as a function of fiber strain (for compression and elongation). If DAF3 refers to a table, then two different damage functions for tensile and compressive stresses are input. The values in the table are arbitrary and exist only to index the two curves. The first indexed curve is assumed to specify tensile damage as a function of fiber strains while second curve specifies compressive damage as a function of fiber strains. input different damage functions for tensile and compressive stresses. Any other curves input with the table definition are ignored.
   The damager parameter d_3 ^ f ranges from 0.0 for an undamaged fiber to 1.0 for a failed fiber family.If all families have failed, material failure at the integration point is initiated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dam3
   :type: Optional[int]


   
   Get or set the Load curve or table ID for damage parameter d_3^mfor matrix material based on the current deformation status of the 3rd fiber (see Remark 2). If a curve, it specifies damage as a function of fiber strain (for compression and elongation). If a table, then the values are fiber strain rates which index damage as a function of fiber strain curves.
   The damager parameter d_3 ^ m ranges from 0.0 to 1.5.A value of 0.0 indicates an undamaged matrix, whereas 1.0 refers to a completely damaged matrix.To initiate failure of the composite at the integration point, a matrix damage d_3^ m of 1.5 must be reached.Naturally, the mechanical behavior of the matrix does not change for damage values between 1.0 and 1.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: postv
   :type: Optional[float]


   
   Get or set the Parameter for outputting additional history variables that might be useful for post-processing.
















   ..
       !! processed by numpydoc !!

.. py:property:: viscs
   :type: Optional[float]


   
   Get or set the Portion of viscous relaxation moduli VGk that is accounted for in time step size calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: ihis
   :type: Optional[float]


   
   Get or set the Flag for material properties initialization :
   EQ.0 : Material properties defined in Cards 1 - 9 are used
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
   :value: '249_CRASH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





