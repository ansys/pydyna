





:class:`MatTransverselyAnisotropicCrushableFoam`
================================================


.. py:class:: mat_transversely_anisotropic_crushable_foam.MatTransverselyAnisotropicCrushableFoam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_TRANSVERSELY_ANISOTROPIC_CRUSHABLE_FOAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatTransverselyAnisotropicCrushableFoam

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
          * - :py:attr:`~e11`
            - Get or set the Elastic modulus in axial direction.
          * - :py:attr:`~e22`
            - Get or set the Elastic modulus in transverse direction (E22=E33).
          * - :py:attr:`~e12`
            - Get or set the Elastic shear modulus (E12=E31).
          * - :py:attr:`~e23`
            - Get or set the Elastic shear modulus in transverse plane.
          * - :py:attr:`~g`
            - Get or set the Shear modulus.
          * - :py:attr:`~k`
            - Get or set the Bulk modulus for contact stiffness.
          * - :py:attr:`~i11`
            - Get or set the Load curve ID for nominal axial stress versus volumetric strain.
          * - :py:attr:`~i22`
            - Get or set the Load curve ID for nominal transverse stresses versus volumetric strain (I22=I33).
          * - :py:attr:`~i12`
            - Get or set the Load curve ID for shear stress component 12 and 31 versus volumetric strain.(I12=I31).
          * - :py:attr:`~i23`
            - Get or set the Load curve ID for shear stress component 23 versus volumetric strain.
          * - :py:attr:`~iaa`
            - Get or set the Load curve ID (optional) for nominal stress versus volumetric strain for load at angle, ANG, relative to the material axis.
          * - :py:attr:`~nsym`
            - Get or set the Set to unity for a symmetric yield surface in volumetric compression and tension direction.
          * - :py:attr:`~ang`
            - Get or set the Angle corresponding to load curve ID, IAA
          * - :py:attr:`~mu`
            - Get or set the Damping coefficient for tensor viscosity which acts in both tension and compression. Recommended values vary between 0.05 to 0.10. If zero, tensor viscosity is not used, but bulk viscosity is used instead. Bulk viscosity creates a pressure as the element compresses that is added to the normal stresses, which can have the effect of creating transverse deformations when none are expected.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~iscl`
            - Get or set the Load curve ID for the strain rate scale factor versus the volumetric strain rate. The yield stress is scaled by the value.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4
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

    from mat_transversely_anisotropic_crushable_foam import MatTransverselyAnisotropicCrushableFoam

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

.. py:property:: e11
   :type: Optional[float]


   
   Get or set the Elastic modulus in axial direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22
   :type: Optional[float]


   
   Get or set the Elastic modulus in transverse direction (E22=E33).
















   ..
       !! processed by numpydoc !!

.. py:property:: e12
   :type: Optional[float]


   
   Get or set the Elastic shear modulus (E12=E31).
















   ..
       !! processed by numpydoc !!

.. py:property:: e23
   :type: Optional[float]


   
   Get or set the Elastic shear modulus in transverse plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus for contact stiffness.
















   ..
       !! processed by numpydoc !!

.. py:property:: i11
   :type: Optional[int]


   
   Get or set the Load curve ID for nominal axial stress versus volumetric strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: i22
   :type: Optional[int]


   
   Get or set the Load curve ID for nominal transverse stresses versus volumetric strain (I22=I33).
















   ..
       !! processed by numpydoc !!

.. py:property:: i12
   :type: Optional[int]


   
   Get or set the Load curve ID for shear stress component 12 and 31 versus volumetric strain.(I12=I31).
















   ..
       !! processed by numpydoc !!

.. py:property:: i23
   :type: Optional[int]


   
   Get or set the Load curve ID for shear stress component 23 versus volumetric strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: iaa
   :type: Optional[int]


   
   Get or set the Load curve ID (optional) for nominal stress versus volumetric strain for load at angle, ANG, relative to the material axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsym
   :type: Optional[int]


   
   Get or set the Set to unity for a symmetric yield surface in volumetric compression and tension direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang
   :type: Optional[float]


   
   Get or set the Angle corresponding to load curve ID, IAA
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Damping coefficient for tensor viscosity which acts in both tension and compression. Recommended values vary between 0.05 to 0.10. If zero, tensor viscosity is not used, but bulk viscosity is used instead. Bulk viscosity creates a pressure as the element compresses that is added to the normal stresses, which can have the effect of creating transverse deformations when none are expected.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: iscl
   :type: Optional[int]


   
   Get or set the Load curve ID for the strain rate scale factor versus the volumetric strain rate. The yield stress is scaled by the value.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).
   This angle may be overridden on the element card, see *ELEMENT_SHELL_BETA,
   *ELEMENT_TSHELL_BETA, and *ELEMENT_SOLID_ORTHO
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
   EQ. - 4 : Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4
















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
   :value: 'TRANSVERSELY_ANISOTROPIC_CRUSHABLE_FOAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





