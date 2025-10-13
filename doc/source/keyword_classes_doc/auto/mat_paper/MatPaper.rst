





:class:`MatPaper`
=================


.. py:class:: mat_paper.MatPaper(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PAPER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPaper

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
            - Get or set the Mass density.
          * - :py:attr:`~e1`
            - Get or set the Young's modulus in direction E1.
          * - :py:attr:`~e2`
            - Get or set the Young's modulus in direction E2.
          * - :py:attr:`~e3`
            - Get or set the Young's modulus in direction E3
          * - :py:attr:`~pr21`
            - Get or set the Elastic Poisson's ratio V21
          * - :py:attr:`~pr32`
            - Get or set the Elastic Poisson's ratio V32
          * - :py:attr:`~pr31`
            - Get or set the Elastic Poisson's ratio V31
          * - :py:attr:`~g12`
            - Get or set the Elastic shear modulus in direction G12.
          * - :py:attr:`~g23`
            - Get or set the Elastic shear modulus in direction G23.
          * - :py:attr:`~g13`
            - Get or set the Elastic shear modulus in direction G13.
          * - :py:attr:`~e3c`
            - Get or set the Elastic compression parameter.
          * - :py:attr:`~cc`
            - Get or set the Elastic compression exponent
          * - :py:attr:`~twok`
            - Get or set the Exponent in in-plane yield surface
          * - :py:attr:`~s01`
            - Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
          * - :py:attr:`~a01`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~b01`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~c01`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~s02`
            - Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
          * - :py:attr:`~a02`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~b02`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~c02`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~s03`
            - Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
          * - :py:attr:`~a03`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~b03`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~c03`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~s04`
            - Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
          * - :py:attr:`~a04`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~b04`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~c04`
            - Get or set the Ith in-plane plasticity hardening parameter
          * - :py:attr:`~s05`
            - Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
          * - :py:attr:`~a05`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~b05`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~c05`
            - Get or set the Ith in-plane plasticity hardening parameter.
          * - :py:attr:`~prp1`
            - Get or set the Tensile plastic Poisson's ratio in direction 1
          * - :py:attr:`~prp2`
            - Get or set the Tensile plastic Poisson's ratio in direction 2
          * - :py:attr:`~prp4`
            - Get or set the Compressive plastic Poisson's ratio in direction 1
          * - :py:attr:`~prp5`
            - Get or set the Compressive plastic Poisson's ratio in direction 2
          * - :py:attr:`~asig`
            - Get or set the Out-of-plane plasticity yield parameter.
          * - :py:attr:`~bsig`
            - Get or set the Out-of-plane plasticity hardening parameter.
          * - :py:attr:`~csig`
            - Get or set the Out-of-plane plasticity hardening parameter.
          * - :py:attr:`~tau0`
            - Get or set the Transverse shear plasticity yield parameter.
          * - :py:attr:`~atau`
            - Get or set the Transverse shear plasticity hardening parameter
          * - :py:attr:`~btau`
            - Get or set the Transverse shear plasticity hardening parameter
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Define coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~a1`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Define components of vector d for AOPT = 2
          * - :py:attr:`~d3`
            - Get or set the Define components of vector d for AOPT = 2
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO
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

    from mat_paper import MatPaper

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Young's modulus in direction E1.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Young's modulus in direction E2.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[float]


   
   Get or set the Young's modulus in direction E3
















   ..
       !! processed by numpydoc !!

.. py:property:: pr21
   :type: Optional[float]


   
   Get or set the Elastic Poisson's ratio V21
















   ..
       !! processed by numpydoc !!

.. py:property:: pr32
   :type: Optional[float]


   
   Get or set the Elastic Poisson's ratio V32
















   ..
       !! processed by numpydoc !!

.. py:property:: pr31
   :type: Optional[float]


   
   Get or set the Elastic Poisson's ratio V31
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the Elastic shear modulus in direction G12.
















   ..
       !! processed by numpydoc !!

.. py:property:: g23
   :type: Optional[float]


   
   Get or set the Elastic shear modulus in direction G23.
















   ..
       !! processed by numpydoc !!

.. py:property:: g13
   :type: Optional[float]


   
   Get or set the Elastic shear modulus in direction G13.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3c
   :type: Optional[float]


   
   Get or set the Elastic compression parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: Optional[float]


   
   Get or set the Elastic compression exponent
















   ..
       !! processed by numpydoc !!

.. py:property:: twok
   :type: Optional[float]


   
   Get or set the Exponent in in-plane yield surface
















   ..
       !! processed by numpydoc !!

.. py:property:: s01
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: a01
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: b01
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: c01
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: s02
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
















   ..
       !! processed by numpydoc !!

.. py:property:: a02
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: b02
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: c02
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: s03
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: a03
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: b03
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: c03
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: s04
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks
















   ..
       !! processed by numpydoc !!

.. py:property:: a04
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: b04
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: c04
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: s05
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity yield parameter. If S0i < 0 the absolute value of S0i is a curve number, see remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: a05
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: b05
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: c05
   :type: Optional[float]


   
   Get or set the Ith in-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: prp1
   :type: float


   
   Get or set the Tensile plastic Poisson's ratio in direction 1
















   ..
       !! processed by numpydoc !!

.. py:property:: prp2
   :type: float


   
   Get or set the Tensile plastic Poisson's ratio in direction 2
















   ..
       !! processed by numpydoc !!

.. py:property:: prp4
   :type: float


   
   Get or set the Compressive plastic Poisson's ratio in direction 1
















   ..
       !! processed by numpydoc !!

.. py:property:: prp5
   :type: float


   
   Get or set the Compressive plastic Poisson's ratio in direction 2
















   ..
       !! processed by numpydoc !!

.. py:property:: asig
   :type: Optional[float]


   
   Get or set the Out-of-plane plasticity yield parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsig
   :type: Optional[float]


   
   Get or set the Out-of-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: csig
   :type: Optional[float]


   
   Get or set the Out-of-plane plasticity hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau0
   :type: Optional[float]


   
   Get or set the Transverse shear plasticity yield parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: atau
   :type: Optional[float]


   
   Get or set the Transverse shear plasticity hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: btau
   :type: Optional[float]


   
   Get or set the Transverse shear plasticity hardening parameter
















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


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















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


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO
















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
   :value: 'PAPER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





