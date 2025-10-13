





:class:`MatOrthotropicThermal`
==============================


.. py:class:: mat_orthotropic_thermal.MatOrthotropicThermal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ORTHOTROPIC_THERMAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatOrthotropicThermal

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
          * - :py:attr:`~ea`
            - Get or set the Ea , Young's modulus in a-direction.
          * - :py:attr:`~eb`
            - Get or set the Eb , Young's modulus in b-direction.
          * - :py:attr:`~ec`
            - Get or set the Ec , Young's modulus in c-direction.
          * - :py:attr:`~prba`
            - Get or set the PRbc , Poisson's ratio, ba.
          * - :py:attr:`~prca`
            - Get or set the PRca , Poisson's ratio, ca.
          * - :py:attr:`~prcb`
            - Get or set the PRcb , Poisson's ratio, cb.
          * - :py:attr:`~gab`
            - Get or set the G ab , Shear modulus, ab.
          * - :py:attr:`~gbc`
            - Get or set the G bc , Shear modulus, bc.
          * - :py:attr:`~gca`
            - Get or set the G ca , Shear modulus, ca.
          * - :py:attr:`~aa`
            - Get or set the Alpha a,coefficients of thermal expansion in the a-direction.
          * - :py:attr:`~ab`
            - Get or set the Alpha b,coefficients of thermal expansion in the b-direction.
          * - :py:attr:`~ac`
            - Get or set the Alpha c,coefficients of thermal expansion in the c-direction.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the X-Coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the Y-Coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the Z-Coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the X-Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Y-Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Z-Components of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Component 1 of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component 2 of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component 3 of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component 1 of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component 2 of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component 3 of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).  It may be overridden on the element card; see *ELEMENT_‌SHELL_‌BETA, *ELEMENT_TSHELL_BETA, or *ELEMENT_‌SOLID_‌ORTHO
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor. The reference geometriy is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY. This option is currently restricted to 8-noded solid elements with one point integration.
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

    from mat_orthotropic_thermal import MatOrthotropicThermal

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

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Ea , Young's modulus in a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb , Young's modulus in b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Ec , Young's modulus in c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the PRbc , Poisson's ratio, ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the PRca , Poisson's ratio, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the PRcb , Poisson's ratio, cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the G ab , Shear modulus, ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the G bc , Shear modulus, bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the G ca , Shear modulus, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: aa
   :type: Optional[float]


   
   Get or set the Alpha a,coefficients of thermal expansion in the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ab
   :type: Optional[float]


   
   Get or set the Alpha b,coefficients of thermal expansion in the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ac
   :type: Optional[float]


   
   Get or set the Alpha c,coefficients of thermal expansion in the c-direction.
















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


   
   Get or set the X-Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Y-Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Z-Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the X-Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Y-Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Z-Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component 1 of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component 2 of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component 3 of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component 1 of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component 2 of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component 3 of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).  It may be overridden on the element card; see *ELEMENT_‌SHELL_‌BETA, *ELEMENT_TSHELL_BETA, or *ELEMENT_‌SOLID_‌ORTHO
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: int


   
   Get or set the Use reference geometry to initialize the stress tensor. The reference geometriy is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY. This option is currently restricted to 8-noded solid elements with one point integration.
   EQ.0.0: off,
   EQ.1.0: on.
















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
   :value: 'ORTHOTROPIC_THERMAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





