





:class:`MatOrthotropicElasticPhaseChange`
=========================================


.. py:class:: mat_orthotropic_elastic_phase_change.MatOrthotropicElasticPhaseChange(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ORTHOTROPIC_ELASTIC_PHASE_CHANGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatOrthotropicElasticPhaseChange

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
          * - :py:attr:`~ea1`
            - Get or set the Ea , Young's modulus in a-direction for phase i.
          * - :py:attr:`~eb1`
            - Get or set the Eb , Young's modulus in b-direction for phase i
          * - :py:attr:`~ec1`
            - Get or set the Ec , Young's modulus in c-direction for phase i
          * - :py:attr:`~prba1`
            - Get or set the Vba , Poisson's ratio in the ba direction for phase i.
          * - :py:attr:`~prca1`
            - Get or set the Vca , Poisson's ratio in the ca direction for phase i
          * - :py:attr:`~prcb1`
            - Get or set the Vcb , Poisson's ratio in the cb direction for phase i
          * - :py:attr:`~gab1`
            - Get or set the Gab , shear modulus in the ab direction for phase i..
          * - :py:attr:`~gbc1`
            - Get or set the Gbc , shear modulus in the bc direction for phase i
          * - :py:attr:`~gca1`
            - Get or set the Gca , shear modulus in the ab direction for phase i
          * - :py:attr:`~aopt1`
            - Get or set the Material axes option for phase i, see Figure M2-1.
          * - :py:attr:`~a11`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2.
          * - :py:attr:`~a21`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
          * - :py:attr:`~a31`
            - Get or set the Define components of the i th phase's vector a for AOPT = 2
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
          * - :py:attr:`~r02`
            - Get or set the Mass density for phase
          * - :py:attr:`~ea2`
            - Get or set the Ea , Young's modulus in a-direction for phase i.
          * - :py:attr:`~eb2`
            - Get or set the Eb , Young's modulus in b-direction for phase i
          * - :py:attr:`~ec2`
            - Get or set the Ec , Young's modulus in c-direction for phase i
          * - :py:attr:`~prba2`
            - Get or set the Vba , Poisson's ratio in the ba direction for phase i.
          * - :py:attr:`~prca2`
            - Get or set the Vca , Poisson's ratio in the ca direction for phase i
          * - :py:attr:`~prcb2`
            - Get or set the Vcb , Poisson's ratio in the cb direction for phase i
          * - :py:attr:`~gab2`
            - Get or set the Gab , shear modulus in the ab direction for phase i..
          * - :py:attr:`~gbc2`
            - Get or set the Gbc , shear modulus in the bc direction for phase i
          * - :py:attr:`~gca2`
            - Get or set the Gca , shear modulus in the ab direction for phase i
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

    from mat_orthotropic_elastic_phase_change import MatOrthotropicElasticPhaseChange

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

.. py:property:: ea1
   :type: Optional[float]


   
   Get or set the Ea , Young's modulus in a-direction for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb1
   :type: Optional[float]


   
   Get or set the Eb , Young's modulus in b-direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: ec1
   :type: Optional[float]


   
   Get or set the Ec , Young's modulus in c-direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: prba1
   :type: Optional[float]


   
   Get or set the Vba , Poisson's ratio in the ba direction for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca1
   :type: Optional[float]


   
   Get or set the Vca , Poisson's ratio in the ca direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb1
   :type: Optional[float]


   
   Get or set the Vcb , Poisson's ratio in the cb direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: gab1
   :type: Optional[float]


   
   Get or set the Gab , shear modulus in the ab direction for phase i..
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc1
   :type: Optional[float]


   
   Get or set the Gbc , shear modulus in the bc direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: gca1
   :type: Optional[float]


   
   Get or set the Gca , shear modulus in the ab direction for phase i
















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

.. py:property:: r02
   :type: Optional[float]


   
   Get or set the Mass density for phase
















   ..
       !! processed by numpydoc !!

.. py:property:: ea2
   :type: Optional[float]


   
   Get or set the Ea , Young's modulus in a-direction for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb2
   :type: Optional[float]


   
   Get or set the Eb , Young's modulus in b-direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: ec2
   :type: Optional[float]


   
   Get or set the Ec , Young's modulus in c-direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: prba2
   :type: Optional[float]


   
   Get or set the Vba , Poisson's ratio in the ba direction for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca2
   :type: Optional[float]


   
   Get or set the Vca , Poisson's ratio in the ca direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb2
   :type: Optional[float]


   
   Get or set the Vcb , Poisson's ratio in the cb direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: gab2
   :type: Optional[float]


   
   Get or set the Gab , shear modulus in the ab direction for phase i..
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc2
   :type: Optional[float]


   
   Get or set the Gbc , shear modulus in the bc direction for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: gca2
   :type: Optional[float]


   
   Get or set the Gca , shear modulus in the ab direction for phase i
















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
   :value: 'ORTHOTROPIC_ELASTIC_PHASE_CHANGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





