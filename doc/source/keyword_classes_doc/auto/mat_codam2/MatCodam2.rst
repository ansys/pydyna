





:class:`MatCodam2`
==================


.. py:class:: mat_codam2.MatCodam2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CODAM2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCodam2

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
          * - :py:attr:`~ea`
            - Get or set the Young's modulus in a-direction = Modulus along the direction of fibers.
          * - :py:attr:`~eb`
            - Get or set the Young's modulus in b-direction = Modulus transverse to fibers.
          * - :py:attr:`~prba`
            - Get or set the Poisson's ratio, ba (minor in-plane Poisson's ratio).
          * - :py:attr:`~prcb`
            - Get or set the Poisson's ratio, cb (Poisson's ratio in the plane of isotropy).
          * - :py:attr:`~gab`
            - Get or set the Shear modulus, ab (in-plane shear modulus).
          * - :py:attr:`~nlayer`
            - Get or set the Number of layers in the sub-laminate excluding symmetry.
          * - :py:attr:`~r1`
            - Get or set the Non-local averaging radius.
          * - :py:attr:`~r2`
            - Get or set the Currently not used.
          * - :py:attr:`~nfreq`
            - Get or set the Number of time steps between update of neighbor list for nonlocal smoothing.
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~angle1`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle2`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle3`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle4`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle5`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle6`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle7`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~angle8`
            - Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
          * - :py:attr:`~imatt`
            - Get or set the Initiation strain for damage in matrix (transverse) under tensile condition.
          * - :py:attr:`~ifibt`
            - Get or set the Initiation strain for damage in the fiber (longitudinal) under tensile condition.
          * - :py:attr:`~iloct`
            - Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
          * - :py:attr:`~idelt`
            - Get or set the Not working in the current version. Can be used for visualization purpose only.
          * - :py:attr:`~smatt`
            - Get or set the Saturation strain for damage in matrix (transverse) under tensile condition.
          * - :py:attr:`~sfibt`
            - Get or set the Saturation strain for damage in the fiber (longitudinal) under tensile condition..
          * - :py:attr:`~sloct`
            - Get or set the Saturation strain for the anti-locking mechanism under tensile condition.
          * - :py:attr:`~sdelt`
            - Get or set the Not working in the current version. Can be used for visualization purpose only.
          * - :py:attr:`~imatc`
            - Get or set the Initiation strain for damage in matrix (transverse) under compressive condition.
          * - :py:attr:`~ifibc`
            - Get or set the Initiation strain for damage in the fiber (longitudinal) under compressive condition.
          * - :py:attr:`~ilocc`
            - Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
          * - :py:attr:`~idelc`
            - Get or set the Initiation strain for delamination. Not working in the current version. Can be used for visualization purpose only.
          * - :py:attr:`~smatc`
            - Get or set the Saturation strain for damage in matrix (transverse) under compressive condition.
          * - :py:attr:`~sfibc`
            - Get or set the Saturation strain for damage in the fiber (longitudinal) under compressive condition.
          * - :py:attr:`~slocc`
            - Get or set the Saturation strain for the anti-locking mechanism under compressive condition.
          * - :py:attr:`~sdelc`
            - Get or set the Delamination strain. Not working in the current version. Can be used for visualization purpose only.
          * - :py:attr:`~erode`
            - Get or set the Erosion Flag (see remarks)
          * - :py:attr:`~erpar1`
            - Get or set the The erosion parameter #1 used in ERODE types 1 and 3. ERPAR1>=1.0        and the recommended value is ERPAR1 = 1.2.
          * - :py:attr:`~erpar2`
            - Get or set the The erosion parameter #2 used in ERODE types 2 and 3. The recommended
          * - :py:attr:`~resids`
            - Get or set the Residual strength for layer damage.
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

    from mat_codam2 import MatCodam2

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

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Young's modulus in a-direction = Modulus along the direction of fibers.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Young's modulus in b-direction = Modulus transverse to fibers.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Poisson's ratio, ba (minor in-plane Poisson's ratio).
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Poisson's ratio, cb (Poisson's ratio in the plane of isotropy).
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus, ab (in-plane shear modulus).
















   ..
       !! processed by numpydoc !!

.. py:property:: nlayer
   :type: int


   
   Get or set the Number of layers in the sub-laminate excluding symmetry.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Non-local averaging radius.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the Currently not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of time steps between update of neighbor list for nonlocal smoothing.
   EQ.0: Do only one search at the start of the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4.
















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

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3 and 4.
















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


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















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

.. py:property:: angle1
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle2
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle3
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle4
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle5
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle6
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle7
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle8
   :type: Optional[float]


   
   Get or set the Rotation angle in degrees of layers with respect to the material axes. Input one for each layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: imatt
   :type: Optional[float]


   
   Get or set the Initiation strain for damage in matrix (transverse) under tensile condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifibt
   :type: Optional[float]


   
   Get or set the Initiation strain for damage in the fiber (longitudinal) under tensile condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: iloct
   :type: Optional[float]


   
   Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
   equal to the saturation strain for the fiber damage mechanism under tensile condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: idelt
   :type: Optional[float]


   
   Get or set the Not working in the current version. Can be used for visualization purpose only.
















   ..
       !! processed by numpydoc !!

.. py:property:: smatt
   :type: Optional[float]


   
   Get or set the Saturation strain for damage in matrix (transverse) under tensile condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfibt
   :type: Optional[float]


   
   Get or set the Saturation strain for damage in the fiber (longitudinal) under tensile condition..
















   ..
       !! processed by numpydoc !!

.. py:property:: sloct
   :type: Optional[float]


   
   Get or set the Saturation strain for the anti-locking mechanism under tensile condition.
   The recommended value for this parameter is (ILOCT+0.02).
















   ..
       !! processed by numpydoc !!

.. py:property:: sdelt
   :type: Optional[float]


   
   Get or set the Not working in the current version. Can be used for visualization purpose only.
















   ..
       !! processed by numpydoc !!

.. py:property:: imatc
   :type: Optional[float]


   
   Get or set the Initiation strain for damage in matrix (transverse) under compressive condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifibc
   :type: Optional[float]


   
   Get or set the Initiation strain for damage in the fiber (longitudinal) under compressive condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilocc
   :type: Optional[float]


   
   Get or set the Initiation strain for the anti-locking mechanism. This parameter should be
   equal to the saturation strain for the fiber damage mechanism under compressive condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: idelc
   :type: Optional[float]


   
   Get or set the Initiation strain for delamination. Not working in the current version. Can be used for visualization purpose only.
















   ..
       !! processed by numpydoc !!

.. py:property:: smatc
   :type: Optional[float]


   
   Get or set the Saturation strain for damage in matrix (transverse) under compressive condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfibc
   :type: Optional[float]


   
   Get or set the Saturation strain for damage in the fiber (longitudinal) under compressive condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: slocc
   :type: Optional[float]


   
   Get or set the Saturation strain for the anti-locking mechanism under compressive condition.
   The recommended value for this parameter is (ILOCC+0.02).
















   ..
       !! processed by numpydoc !!

.. py:property:: sdelc
   :type: Optional[float]


   
   Get or set the Delamination strain. Not working in the current version. Can be used for visualization purpose only.
















   ..
       !! processed by numpydoc !!

.. py:property:: erode
   :type: int


   
   Get or set the Erosion Flag (see remarks)
   EQ.0: Erosion is turned off.
   EQ.1: Non-local strain based erosion criterion.
   EQ.2: Local strain based erosion criterion.
   EQ.3: Use both ERODE = 1 and ERODE = 2 criteria.
















   ..
       !! processed by numpydoc !!

.. py:property:: erpar1
   :type: Optional[float]


   
   Get or set the The erosion parameter #1 used in ERODE types 1 and 3. ERPAR1>=1.0        and the recommended value is ERPAR1 = 1.2.
















   ..
       !! processed by numpydoc !!

.. py:property:: erpar2
   :type: Optional[float]


   
   Get or set the The erosion parameter #2 used in ERODE types 2 and 3. The recommended
   value is five times SLOC defined in cards 7 and 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: resids
   :type: Optional[float]


   
   Get or set the Residual strength for layer damage.
















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
   :value: 'CODAM2'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





