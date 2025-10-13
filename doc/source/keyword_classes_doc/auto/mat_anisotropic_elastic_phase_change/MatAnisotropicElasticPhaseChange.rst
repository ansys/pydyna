





:class:`MatAnisotropicElasticPhaseChange`
=========================================


.. py:class:: mat_anisotropic_elastic_phase_change.MatAnisotropicElasticPhaseChange(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ANISOTROPIC_ELASTIC_PHASE_CHANGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAnisotropicElasticPhaseChange

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density
          * - :py:attr:`~c111`
            - Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 1 corresponds to the a material direction
          * - :py:attr:`~c121`
            - Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 2 corresponds to the b material direction
          * - :py:attr:`~c221`
            - Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c131`
            - Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c231`
            - Get or set the The 2,3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c331`
            - Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c141`
            - Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
          * - :py:attr:`~c241`
            - Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c341`
            - Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c441`
            - Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c151`
            - Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c251`
            - Get or set the The 2, 5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c351`
            - Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c451`
            - Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c551`
            - Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
          * - :py:attr:`~c161`
            - Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c261`
            - Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c361`
            - Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c461`
            - Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c561`
            - Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c661`
            - Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~aopt1`
            - Get or set the Material axes option for phase i, see Figure M2-1.
          * - :py:attr:`~xp1`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
          * - :py:attr:`~yp1`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
          * - :py:attr:`~zp1`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
          * - :py:attr:`~a11`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2.
          * - :py:attr:`~a21`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
          * - :py:attr:`~a31`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for brick elements in phase i:
          * - :py:attr:`~ihis`
            - Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only).
          * - :py:attr:`~v11`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
          * - :py:attr:`~v21`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
          * - :py:attr:`~v31`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
          * - :py:attr:`~d11`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~d21`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~d31`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~beta1`
            - Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor for the i th phase.
          * - :py:attr:`~c112`
            - Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c122`
            - Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c222`
            - Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c132`
            - Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c232`
            - Get or set the The 2, 3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c332`
            - Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c142`
            - Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
          * - :py:attr:`~c242`
            - Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c342`
            - Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c442`
            - Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c152`
            - Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c252`
            - Get or set the The 2,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c352`
            - Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c452`
            - Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c552`
            - Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
          * - :py:attr:`~c162`
            - Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c262`
            - Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c362`
            - Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c462`
            - Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c562`
            - Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~c662`
            - Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
          * - :py:attr:`~xp2`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
          * - :py:attr:`~yp2`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
          * - :py:attr:`~zp2`
            - Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
          * - :py:attr:`~a12`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2.
          * - :py:attr:`~a22`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
          * - :py:attr:`~a32`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
          * - :py:attr:`~v12`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
          * - :py:attr:`~v22`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
          * - :py:attr:`~v32`
            - Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
          * - :py:attr:`~d12`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~d22`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~d32`
            - Get or set the Define components of the i th phase's vector d for AOPT = 2
          * - :py:attr:`~beta2`
            - Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
          * - :py:attr:`~x1`
            - Get or set the Coordinates of a point on the phase transition page.
          * - :py:attr:`~y1`
            - Get or set the Coordinates of a point on the phase transition page.
          * - :py:attr:`~z1`
            - Get or set the Coordinates of a point on the phase transition page.
          * - :py:attr:`~x2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point.
          * - :py:attr:`~y2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point
          * - :py:attr:`~z2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point
          * - :py:attr:`~thkfac`
            - Get or set the Scale factor applied to the shell thickness after the phase transformation.
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

    from mat_anisotropic_elastic_phase_change import MatAnisotropicElasticPhaseChange

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density
















   ..
       !! processed by numpydoc !!

.. py:property:: c111
   :type: Optional[float]


   
   Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 1 corresponds to the a material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: c121
   :type: Optional[float]


   
   Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i. Note that 2 corresponds to the b material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: c221
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c131
   :type: Optional[float]


   
   Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c231
   :type: Optional[float]


   
   Get or set the The 2,3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c331
   :type: Optional[float]


   
   Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c141
   :type: Optional[float]


   
   Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: c241
   :type: Optional[float]


   
   Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c341
   :type: Optional[float]


   
   Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c441
   :type: Optional[float]


   
   Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c151
   :type: Optional[float]


   
   Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c251
   :type: Optional[float]


   
   Get or set the The 2, 5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c351
   :type: Optional[float]


   
   Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c451
   :type: Optional[float]


   
   Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c551
   :type: Optional[float]


   
   Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: c161
   :type: Optional[float]


   
   Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c261
   :type: Optional[float]


   
   Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c361
   :type: Optional[float]


   
   Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c461
   :type: Optional[float]


   
   Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c561
   :type: Optional[float]


   
   Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c661
   :type: Optional[float]


   
   Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt1
   :type: Optional[float]


   
   Get or set the Material axes option for phase i, see Figure M2-1.
   EQ.0.0: locally orthotropic with material axes determined by element nodes as shown in part (a) of Figure M2-1. The
   a-direction is from node 1 to node 2 of the element. The b-direction is orthogonal to the a-direction and is in the
   plane formed by nodes 1, 2, and 4. When this option is used in two-dimensional planar and axisymmetric analysis,
   it is critical that the nodes in the element definition be numbered counterclockwise for this option to work correctly.
   EQ.1.0: locally orthotropic with material axes determined by a
   point in space and the global location of the element center; this is the a-direction. This option is for solid elements only.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.
   The plane of a solid element is the midsurface between the inner surface and outer surface defined by the
   first four nodes and the last four nodes of the connectivity of the element, respectively.
   EQ.4.0: locally orthotropic in cylindrical coordinate system with
   the material axes determined by a vector v, and an originating point, P, which define the centerline axis. This option is for solid elements only.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR). Available in R3 version of 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp1
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp1
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: zp1
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a11
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a21
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a31
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for brick elements in phase i:
   EQ.1: No change, default,
   EQ.2: switch material axes a and b,
   EQ.3: switch material axes a and c,
   EQ.4: switch material axes b and c.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihis
   :type: int


   
   Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only).
   EQ.0: C11, C12, … from Cards 1, 2, and 3 are used.
   EQ.1: C11, C12, … are initialized by *INITIAL_STRESS_SOLID's    history data.
















   ..
       !! processed by numpydoc !!

.. py:property:: v11
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v21
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: v31
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d11
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d21
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d31
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: beta1
   :type: Optional[float]


   
   Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
   overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the stress tensor for the i th phase.
   The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
   EQ.0.0: off,
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: c112
   :type: Optional[float]


   
   Get or set the The 1,1 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c122
   :type: Optional[float]


   
   Get or set the The 1,2 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c222
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c132
   :type: Optional[float]


   
   Get or set the The 1,3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c232
   :type: Optional[float]


   
   Get or set the The 2, 3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c332
   :type: Optional[float]


   
   Get or set the The 3,3 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c142
   :type: Optional[float]


   
   Get or set the The 1,4 term in the 6x6 anisotropic constitutive matrix for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: c242
   :type: Optional[float]


   
   Get or set the The 2,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c342
   :type: Optional[float]


   
   Get or set the The 3,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c442
   :type: Optional[float]


   
   Get or set the The 4,4 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c152
   :type: Optional[float]


   
   Get or set the The 1,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c252
   :type: Optional[float]


   
   Get or set the The 2,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c352
   :type: Optional[float]


   
   Get or set the The 3,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c452
   :type: Optional[float]


   
   Get or set the The 4,5 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c552
   :type: Optional[float]


   
   Get or set the The 5,5 term in the 6x6 anisotropic constitutive matrix for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: c162
   :type: Optional[float]


   
   Get or set the The 1,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c262
   :type: Optional[float]


   
   Get or set the The 2,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c362
   :type: Optional[float]


   
   Get or set the The 3,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c462
   :type: Optional[float]


   
   Get or set the The 4,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c562
   :type: Optional[float]


   
   Get or set the The 5,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: c662
   :type: Optional[float]


   
   Get or set the The 6,6 term in the 6x6 anisotropic constitutive matrix for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: xp2
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp2
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: zp2
   :type: Optional[float]


   
   Get or set the Define coordinates of the i th phase's point P for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a12
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a22
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a32
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: v12
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v22
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: v32
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d12
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d22
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d32
   :type: Optional[float]


   
   Get or set the Define components of the i th phase's vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: Optional[float]


   
   Get or set the Material angle of i th phase in degrees for AOPT = 3, may be
   overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition page.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition page.
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition page.
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point
















   ..
       !! processed by numpydoc !!

.. py:property:: thkfac
   :type: float


   
   Get or set the Scale factor applied to the shell thickness after the phase transformation.
















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
   :value: 'ANISOTROPIC_ELASTIC_PHASE_CHANGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





