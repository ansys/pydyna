





:class:`MatBarlatYld2004`
=========================


.. py:class:: mat_barlat_yld2004.MatBarlatYld2004(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_BARLAT_YLD2004 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatBarlatYld2004

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~cp12`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp13`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp21`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp23`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp31`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp32`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cpp12`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp13`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp21`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp23`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp31`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp32`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cp44`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp55`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cp66`
            - Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
          * - :py:attr:`~cpp44`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp55`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~cpp66`
            - Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~a`
            - Get or set the Flow potential exponent a.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID or table ID for (isotropic) hardening:
          * - :py:attr:`~xp`
            - Get or set the Define coordinates of point  for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Define coordinates of point  for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Define coordinates of point  for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3.  It may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.and *ELEMENT_TSHELL_BETA..
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

    from mat_barlat_yld2004 import MatBarlatYld2004

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
   LT.0.0: -E is either a load curve ID for Young’s modulus as a function of plastic strain or a table ID for Young’s modulus as a function of plastic strain and temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp12
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp13
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp21
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp23
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp31
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp32
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp12
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp13
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp21
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp23
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp31
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp32
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp44
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp55
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp66
   :type: Optional[float]


   
   Get or set the 9 coefficients C'ij of the first linear transformation matrix C'.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp44
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp55
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpp66
   :type: Optional[float]


   
   Get or set the 9 coefficients C''ij of the second linear transformation matrix C''.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
   EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface betwen the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Flow potential exponent a.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID or table ID for (isotropic) hardening:
   GT.0:   If LCSS is a load curve, then yield stress σ ̅ is a function of plastic strain.If LCSS is a table, then yield stress σ ̅ is a function of plastic strainand plastic strain rate.
   LT.0 : If - LCSS is a load curve, then yield stress σ ̅ is a function of plastic strain.If - LCSS is a table, then yield stress σ ̅ is a function of plastic strainand temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Define coordinates of point  for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinates of point  for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinates of point  for AOPT = 1 and 4.
















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

.. py:property:: macf
   :type: float


   
   Get or set the Material axes change flag for solid elements:
   EQ. - 4:        Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 6 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















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

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3.  It may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.and *ELEMENT_TSHELL_BETA..
















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
   :value: 'BARLAT_YLD2004'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





