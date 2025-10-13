





:class:`MatAnisotropicElastic`
==============================


.. py:class:: mat_anisotropic_elastic.MatAnisotropicElastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ANISOTROPIC_ELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAnisotropicElastic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~c11`
            - Get or set the The 1,1 term in the 6 x 6 anisotropic constitutive matrix. Note that 1 corresponds to the a material direction
          * - :py:attr:`~c12`
            - Get or set the The 1,2 term in the 6 x 6 anisotropic constitutive matrix. Note that 2 corresponds to the b material direction
          * - :py:attr:`~c22`
            - Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c13`
            - Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c23`
            - Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c33`
            - Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c14`
            - Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c24`
            - Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c34`
            - Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c44`
            - Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c15`
            - Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c25`
            - Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c35`
            - Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c45`
            - Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c55`
            - Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c16`
            - Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c26`
            - Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c36`
            - Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c46`
            - Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c56`
            - Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c66`
            - Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see Figure 0-1 and the Material Directions section):
          * - :py:attr:`~xp`
            - Get or set the x-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the y-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the z-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~ihis`
            - Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only):
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
          * - :py:attr:`~ref`
            - Get or set the Flag to use reference geometry specified with *INITIAL_FOAM_REFERENCE_GEOMETRY to initialize the stress tensor.
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

    from mat_anisotropic_elastic import MatAnisotropicElastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: c11
   :type: Optional[float]


   
   Get or set the The 1,1 term in the 6 x 6 anisotropic constitutive matrix. Note that 1 corresponds to the a material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: c12
   :type: Optional[float]


   
   Get or set the The 1,2 term in the 6 x 6 anisotropic constitutive matrix. Note that 2 corresponds to the b material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: c22
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c13
   :type: Optional[float]


   
   Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: Optional[float]


   
   Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c33
   :type: Optional[float]


   
   Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c14
   :type: Optional[float]


   
   Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c24
   :type: Optional[float]


   
   Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c34
   :type: Optional[float]


   
   Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c44
   :type: Optional[float]


   
   Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c15
   :type: Optional[float]


   
   Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c25
   :type: Optional[float]


   
   Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c35
   :type: Optional[float]


   
   Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c45
   :type: Optional[float]


   
   Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c55
   :type: Optional[float]


   
   Get or set the The 1,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c16
   :type: Optional[float]


   
   Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c26
   :type: Optional[float]


   
   Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c36
   :type: Optional[float]


   
   Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c46
   :type: Optional[float]


   
   Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c56
   :type: Optional[float]


   
   Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c66
   :type: Optional[float]


   
   Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see Figure 0-1 and the Material Directions section):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes as shown in Figure 0 - 1.  The a - direction is from node 1 to node 2 of the element.The b - direction is orthogonal to the a - direction and is in the plane formed by nodes 1, 2,and 4. When this option is used in two - dimensional planar and axisymmetric analysis, it is critical that the nodes in the element definition be numbered counterclockwise for this option to work correctly.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: Globally orthotropic with material axes determined by vectors a and d input below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element(see Figure 0 - 1).The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v, and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : |AOPT| is a coordinate system ID(see * DEFINE_COORDINATE_OPTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ.-4:  Switch material axes b and c before BETA rotation
   EQ. - 3: Switch material axes a and c before BETA rotation
   EQ. - 2: Switch material axes a and b before BETA rotation
   EQ.1:  No change, default,
   EQ.2:  switch material axes a and b,
   EQ.3:  switch material axes a and c,
   EQ.4:  switch material axes b and c.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihis
   :type: int


   
   Get or set the Flag for anisotropic stiffness terms initialization (for solid elements only):
   EQ.0:   C11, C12, … from Cards 1b.1, 1b.2,and 1.b3 are used.
   EQ.1 : C11, C12, … are initialized with history data from* INITIAL_‌STRESS_‌SOLID
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Flag to use reference geometry specified with *INITIAL_FOAM_REFERENCE_GEOMETRY to initialize the stress tensor.
   EQ.0.0: Off
   EQ.1.0 : On
















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
   :value: 'ANISOTROPIC_ELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





