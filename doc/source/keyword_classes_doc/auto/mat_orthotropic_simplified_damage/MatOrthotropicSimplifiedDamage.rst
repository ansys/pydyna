





:class:`MatOrthotropicSimplifiedDamage`
=======================================


.. py:class:: mat_orthotropic_simplified_damage.MatOrthotropicSimplifiedDamage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ORTHOTROPIC_SIMPLIFIED_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatOrthotropicSimplifiedDamage

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
            - Get or set the Ea, Young's modulus in a-direction.
          * - :py:attr:`~eb`
            - Get or set the Eb, Young's modulus in b-direction.
          * - :py:attr:`~ec`
            - Get or set the Eb, Young's modulus in c-direction.
          * - :py:attr:`~prba`
            - Get or set the Vba, Poisson's ratio, ba.
          * - :py:attr:`~prca`
            - Get or set the Vca, Poisson's ratio, ca.
          * - :py:attr:`~prcb`
            - Get or set the Vcb, Poisson's ratio, cb.
          * - :py:attr:`~gab`
            - Get or set the Gab, Shear modulus, ab.
          * - :py:attr:`~gbc`
            - Get or set the Gbc, Shear modulus, bc.
          * - :py:attr:`~gca`
            - Get or set the Gca, Shear modulus, ca.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
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
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~nerode`
            - Get or set the Failure and erosion flag:
          * - :py:attr:`~ndam`
            - Get or set the Damage flag:
          * - :py:attr:`~eps1tf`
            - Get or set the Failure strain in tension along the a-direction.
          * - :py:attr:`~eps2tf`
            - Get or set the Failure strain in tension along the b-direction.
          * - :py:attr:`~eps3tf`
            - Get or set the Failure strain in tension along the c-direction.
          * - :py:attr:`~eps1cf`
            - Get or set the Failure strain in compression along the a-direction.
          * - :py:attr:`~eps2cf`
            - Get or set the Failure strain in compression along the b-direction.
          * - :py:attr:`~eps3cf`
            - Get or set the Failure strain in compression along the c-direction.
          * - :py:attr:`~eps12f`
            - Get or set the Failure shear strain in the ab-plane.
          * - :py:attr:`~eps23f`
            - Get or set the Failure shear strain in the bc-plane.
          * - :py:attr:`~eps13f`
            - Get or set the Failure shear strain in the ac-plane.
          * - :py:attr:`~epsd1t`
            - Get or set the Damage threshold in tension along the a-direction.
          * - :py:attr:`~epsc1t`
            - Get or set the Critical damage threshold in tension along the a-direction.
          * - :py:attr:`~cdam1t`
            - Get or set the Critical damage in tension along the a-direction.
          * - :py:attr:`~epsd2t`
            - Get or set the Damage threshold in tension along the b-direction.
          * - :py:attr:`~epsc2t`
            - Get or set the Critical damage threshold in tension along the b-direction.
          * - :py:attr:`~cdam2t`
            - Get or set the Critical damage in tension along the b-direction.
          * - :py:attr:`~epsd3t`
            - Get or set the Damage threshold in tension along the c-direction.
          * - :py:attr:`~epsc3t`
            - Get or set the Critical damage threshold in tension along the c-direction.
          * - :py:attr:`~cdam3t`
            - Get or set the Critical damage in tension along the c-direction.
          * - :py:attr:`~epsd1c`
            - Get or set the Damage threshold in compression along the a-direction.
          * - :py:attr:`~epsc1c`
            - Get or set the Critical damage threshold in compression along the a-direction.
          * - :py:attr:`~cdam1c`
            - Get or set the Critical damage in compression along the a-direction.
          * - :py:attr:`~epsd2c`
            - Get or set the Damage threshold in compression along the b-direction.
          * - :py:attr:`~epsc2c`
            - Get or set the Critical damage threshold in compression along the b-direction.
          * - :py:attr:`~cdam2c`
            - Get or set the Critical damage in compression along the b-direction.
          * - :py:attr:`~epsd3c`
            - Get or set the Damage threshold in compression along the c-direction.
          * - :py:attr:`~epsc3c`
            - Get or set the Critical damage threshold in compression along the c-direction.
          * - :py:attr:`~cdam3c`
            - Get or set the Critical damage in compression along the c-direction.
          * - :py:attr:`~epsd12`
            - Get or set the Damage threshold for shear in the ab-plane.
          * - :py:attr:`~epsc12`
            - Get or set the Critical damage threshold for shear in the ab-plane.
          * - :py:attr:`~cdam12`
            - Get or set the Critical damage for shear in the ab-plane.
          * - :py:attr:`~epsd23`
            - Get or set the Damage threshold for shear in the bc-plane.
          * - :py:attr:`~epsc23`
            - Get or set the Critical damage threshold for shear in the bc-plane.
          * - :py:attr:`~cdam23`
            - Get or set the Critical damage for shear in the bc-plane.
          * - :py:attr:`~epsd31`
            - Get or set the Damage threshold for shear in the ac-plane.
          * - :py:attr:`~epsc31`
            - Get or set the Critical damage threshold for shear in the ac-plane.
          * - :py:attr:`~cdam31`
            - Get or set the Critical damage for shear in the ac-plane.
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

    from mat_orthotropic_simplified_damage import MatOrthotropicSimplifiedDamage

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


   
   Get or set the Ea, Young's modulus in a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus in b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus in c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Vba, Poisson's ratio, ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Vca, Poisson's ratio, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Vcb, Poisson's ratio, cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Gab, Shear modulus, ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Gbc, Shear modulus, bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Gca, Shear modulus, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION)
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ. - 4:        Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 4 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
















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


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: nerode
   :type: int


   
   Get or set the Failure and erosion flag:
   EQ. 0: No failure (default)
   EQ. 1: Failure as soon as one failure criterion is reached in all
   integration points
   EQ. 2: Failure as soon as one failure criterion is reached in at least
   one integration point
   EQ. 3: Failure as soon as a tension or compression failure criterion
   in the a-direction is reached for one integration point
   EQ. 4: Failure as soon as a tension or compression failure criterion
   in the b-direction is reached for one integration point
   EQ. 5: Failure as soon as a tension or compression failure criterion
   in the c-direction is reached for one integration point
   EQ. 6: Failure as soon as tension or compression failure criteria in
   both the a- and b-directions are reached at a single integration
   point or at 2 different integration points
   EQ. 7: Failure as soon as tension or compression failure criteria in
   both the b- and c-directions are reached at a single integration
   point or at 2 different integration points
   EQ. 8: Failure as soon as tension or compression failure criteria in
   both the a- and c-directions are reached at a single integration
   point or at 2 different integration points
   EQ. 9: Failure as soon as tension or compression failure criteria in
   the 3 directions are reached at a single integration point or at
   different integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndam
   :type: int


   
   Get or set the Damage flag:
   EQ. 0: No damage (default)
   EQ. 1: Damage in tension only (null for compression)
   EQ. 2: Damage in tension and compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1tf
   :type: float


   
   Get or set the Failure strain in tension along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2tf
   :type: float


   
   Get or set the Failure strain in tension along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3tf
   :type: float


   
   Get or set the Failure strain in tension along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1cf
   :type: float


   
   Get or set the Failure strain in compression along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2cf
   :type: float


   
   Get or set the Failure strain in compression along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3cf
   :type: float


   
   Get or set the Failure strain in compression along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps12f
   :type: float


   
   Get or set the Failure shear strain in the ab-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps23f
   :type: float


   
   Get or set the Failure shear strain in the bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps13f
   :type: float


   
   Get or set the Failure shear strain in the ac-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd1t
   :type: Optional[float]


   
   Get or set the Damage threshold in tension along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc1t
   :type: Optional[float]


   
   Get or set the Critical damage threshold in tension along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam1t
   :type: Optional[float]


   
   Get or set the Critical damage in tension along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd2t
   :type: Optional[float]


   
   Get or set the Damage threshold in tension along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc2t
   :type: Optional[float]


   
   Get or set the Critical damage threshold in tension along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam2t
   :type: float


   
   Get or set the Critical damage in tension along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd3t
   :type: float


   
   Get or set the Damage threshold in tension along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc3t
   :type: Optional[float]


   
   Get or set the Critical damage threshold in tension along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam3t
   :type: Optional[float]


   
   Get or set the Critical damage in tension along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd1c
   :type: Optional[float]


   
   Get or set the Damage threshold in compression along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc1c
   :type: Optional[float]


   
   Get or set the Critical damage threshold in compression along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam1c
   :type: Optional[float]


   
   Get or set the Critical damage in compression along the a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd2c
   :type: Optional[float]


   
   Get or set the Damage threshold in compression along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc2c
   :type: Optional[float]


   
   Get or set the Critical damage threshold in compression along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam2c
   :type: Optional[float]


   
   Get or set the Critical damage in compression along the b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd3c
   :type: Optional[float]


   
   Get or set the Damage threshold in compression along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc3c
   :type: Optional[float]


   
   Get or set the Critical damage threshold in compression along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam3c
   :type: Optional[float]


   
   Get or set the Critical damage in compression along the c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd12
   :type: Optional[float]


   
   Get or set the Damage threshold for shear in the ab-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc12
   :type: Optional[float]


   
   Get or set the Critical damage threshold for shear in the ab-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam12
   :type: Optional[float]


   
   Get or set the Critical damage for shear in the ab-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd23
   :type: Optional[float]


   
   Get or set the Damage threshold for shear in the bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc23
   :type: Optional[float]


   
   Get or set the Critical damage threshold for shear in the bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam23
   :type: Optional[float]


   
   Get or set the Critical damage for shear in the bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd31
   :type: Optional[float]


   
   Get or set the Damage threshold for shear in the ac-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsc31
   :type: Optional[float]


   
   Get or set the Critical damage threshold for shear in the ac-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdam31
   :type: Optional[float]


   
   Get or set the Critical damage for shear in the ac-plane.
















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
   :value: 'ORTHOTROPIC_SIMPLIFIED_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





