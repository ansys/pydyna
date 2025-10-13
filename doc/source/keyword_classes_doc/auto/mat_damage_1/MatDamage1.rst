





:class:`MatDamage1`
===================


.. py:class:: mat_damage_1.MatDamage1(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DAMAGE_1 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDamage1

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
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress, sigma-0.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID defining efective stress versus effective plastic strain. For FLAG = -1.
          * - :py:attr:`~lcds`
            - Get or set the Load curve ID defining nonlinear damage curve. For FLAG = -1.
          * - :py:attr:`~q1`
            - Get or set the Isotropic hardening parameter Q1.
          * - :py:attr:`~c1`
            - Get or set the Isotropic hardening parameter C1.
          * - :py:attr:`~q2`
            - Get or set the Isotropic hardening parameter Q2.
          * - :py:attr:`~c2`
            - Get or set the Isotropic hardening parameter C2.
          * - :py:attr:`~epsd`
            - Get or set the Damage threshold rd. Damage effective plastic strain when material softening begin (default = 0.0).
          * - :py:attr:`~espr_s`
            - Get or set the Damage material constant S (default = sigma0/200) For FLAG>=0.
          * - :py:attr:`~dc`
            - Get or set the Critical damage value Dc. When the damage value D reaches this value, the element is deleted from the calculation (default=0.5)For FLAG>=0.
          * - :py:attr:`~flag`
            - Get or set the Flag
          * - :py:attr:`~vk`
            - Get or set the Viscous material parameter Vk.
          * - :py:attr:`~vm`
            - Get or set the Viscous material parameter Vm.
          * - :py:attr:`~r00_f`
            - Get or set the R00 for shell (default = 1.0).
          * - :py:attr:`~r45_g`
            - Get or set the R45 for shell (default = 1.0).
          * - :py:attr:`~r90_h`
            - Get or set the R90 for shell (default = 1.0).
          * - :py:attr:`~l`
            - Get or set the L for brick (default = 3/2).
          * - :py:attr:`~m`
            - Get or set the M for brick (default =3/2).
          * - :py:attr:`~n`
            - Get or set the N for brick (default =3/2).
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Define component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Define component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Define component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Define component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Define component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Define component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Define component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Define component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Define component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
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

    from mat_damage_1 import MatDamage1

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
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress, sigma-0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID defining efective stress versus effective plastic strain. For FLAG = -1.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcds
   :type: Optional[int]


   
   Get or set the Load curve ID defining nonlinear damage curve. For FLAG = -1.
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Q1.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter C1.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Q2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter C2.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd
   :type: Optional[float]


   
   Get or set the Damage threshold rd. Damage effective plastic strain when material softening begin (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: espr_s
   :type: Optional[float]


   
   Get or set the Damage material constant S (default = sigma0/200) For FLAG>=0.
   Or
   Plastic strain at which material ruptures (logarithmic).
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Critical damage value Dc. When the damage value D reaches this value, the element is deleted from the calculation (default=0.5)For FLAG>=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: int


   
   Get or set the Flag
   EQ.-1. Anisotropic damage
   EQ.0. No calculation of localization due to damage(default),
   EQ.1:The model flags element where strain localization occur.
















   ..
       !! processed by numpydoc !!

.. py:property:: vk
   :type: Optional[float]


   
   Get or set the Viscous material parameter Vk.
















   ..
       !! processed by numpydoc !!

.. py:property:: vm
   :type: Optional[float]


   
   Get or set the Viscous material parameter Vm.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00_f
   :type: Optional[float]


   
   Get or set the R00 for shell (default = 1.0).
   F for brick (default = 1/2).
















   ..
       !! processed by numpydoc !!

.. py:property:: r45_g
   :type: Optional[float]


   
   Get or set the R45 for shell (default = 1.0).
   G for brick (default = 1/2).
















   ..
       !! processed by numpydoc !!

.. py:property:: r90_h
   :type: Optional[float]


   
   Get or set the R90 for shell (default = 1.0).
   H for brick (default = 1/2).
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: float


   
   Get or set the L for brick (default = 3/2).
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the M for brick (default =3/2).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the N for brick (default =3/2).
















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


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















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
   :value: 'DAMAGE_1'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





