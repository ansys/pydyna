





:class:`MatAnisotropicElasticPlastic`
=====================================


.. py:class:: mat_anisotropic_elastic_plastic.MatAnisotropicElasticPlastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ANISOTROPIC_ELASTIC_PLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAnisotropicElasticPlastic

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
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress.
          * - :py:attr:`~lcss`
            - Get or set the
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter Qr1.
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter Cr1.
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter Qr2.
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter Cr2.
          * - :py:attr:`~c11`
            - Get or set the The 1,1 term in the 6 x 6 anisotropic constitutive matrix. Note that 1 corresponds to the a material direction
          * - :py:attr:`~c12`
            - Get or set the The 1,2 term in the 6 x 6 anisotropic constitutive matrix. Note that 2 corresponds to the b material direction
          * - :py:attr:`~c13`
            - Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c14`
            - Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c15`
            - Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c16`
            - Get or set the The 1,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c22`
            - Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c23`
            - Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c24`
            - Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c25`
            - Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c26`
            - Get or set the The 2,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c33`
            - Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c34`
            - Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c35`
            - Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c36`
            - Get or set the The 3,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c44`
            - Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c45`
            - Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c46`
            - Get or set the The 4,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c55`
            - Get or set the The 5,5 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c56`
            - Get or set the The 5,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~c66`
            - Get or set the The 6,6 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~r00_f`
            - Get or set the The 0,0 term in the 6 x 6 anisotropic constitutive matrix.
          * - :py:attr:`~r45_g`
            - Get or set the R45, Lankford parmeter determined from experiments
          * - :py:attr:`~r90_h`
            - Get or set the R90 , Lankford parmeter determined from experiments
          * - :py:attr:`~s11_l`
            - Get or set the Yield stress in local x-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s22_m`
            - Get or set the Yield stress in local y-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s33_n`
            - Get or set the Yield stress in local z-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s12`
            - Get or set the Yield stress in local xy-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~vp`
            - Get or set the Formulation for rate effects:
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
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
          * - :py:attr:`~id3upd`
            - Get or set the Flag for transverse through thickness strain update (thin shells only):
          * - :py:attr:`~extra`
            - Get or set the Flag to input further data:
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
          * - :py:attr:`~ihis`
            - Get or set the Flag for material properties initialization.
          * - :py:attr:`~xt`
            - Get or set the Longitudinal tensile strength, a-axis.
          * - :py:attr:`~xc`
            - Get or set the Longitudinal compressive strength, a-axis (positive value).
          * - :py:attr:`~yt`
            - Get or set the Transverse tensile strength, b-axis.
          * - :py:attr:`~yc`
            - Get or set the Transverse compressive strength, b-axis (positive value).
          * - :py:attr:`~sxy`
            - Get or set the Shear strength, ab-plane.
          * - :py:attr:`~ff12`
            - Get or set the Scale factor between -1 and +1 for interaction term F12, see Remarks.
          * - :py:attr:`~ncfail`
            - Get or set the Number of timesteps to reduce stresses until element deletion.The default is NCFAIL=10..
          * - :py:attr:`~zt`
            - Get or set the Transverse tensile strength, c-axis (solid elements only).
          * - :py:attr:`~zc`
            - Get or set the Transverse compressive strength, c-axis (positive value) (solid elements only).
          * - :py:attr:`~syz`
            - Get or set the Shear strength, bc-plane (solid elements only).
          * - :py:attr:`~szx`
            - Get or set the Shear strength, ca-plane (solid elements only).
          * - :py:attr:`~ff23`
            - Get or set the Scale factor between -1 and +1 for interaction term F23, see Remarks (solid elements only).
          * - :py:attr:`~ff31`
            - Get or set the Scale factor between -1 and +1 for interaction term F31, see Remarks (solid elements only).
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

    from mat_anisotropic_elastic_plastic import MatAnisotropicElasticPlastic

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

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: qr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Qr1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Cr1.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Qr2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter Cr2.
















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

.. py:property:: c13
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c14
   :type: Optional[float]


   
   Get or set the The 1,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c15
   :type: Optional[float]


   
   Get or set the The 1,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c16
   :type: Optional[float]


   
   Get or set the The 1,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c22
   :type: Optional[float]


   
   Get or set the The 2,2 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: Optional[float]


   
   Get or set the The 2,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c24
   :type: Optional[float]


   
   Get or set the The 2,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c25
   :type: Optional[float]


   
   Get or set the The 2,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c26
   :type: Optional[float]


   
   Get or set the The 2,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c33
   :type: Optional[float]


   
   Get or set the The 3,3 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c34
   :type: Optional[float]


   
   Get or set the The 3,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c35
   :type: Optional[float]


   
   Get or set the The 3,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c36
   :type: Optional[float]


   
   Get or set the The 3,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c44
   :type: Optional[float]


   
   Get or set the The 4,4 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c45
   :type: Optional[float]


   
   Get or set the The 4,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c46
   :type: Optional[float]


   
   Get or set the The 4,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c55
   :type: Optional[float]


   
   Get or set the The 5,5 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c56
   :type: Optional[float]


   
   Get or set the The 5,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c66
   :type: Optional[float]


   
   Get or set the The 6,6 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00_f
   :type: Optional[float]


   
   Get or set the The 0,0 term in the 6 x 6 anisotropic constitutive matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45_g
   :type: Optional[float]


   
   Get or set the R45, Lankford parmeter determined from experiments
















   ..
       !! processed by numpydoc !!

.. py:property:: r90_h
   :type: Optional[float]


   
   Get or set the R90 , Lankford parmeter determined from experiments
















   ..
       !! processed by numpydoc !!

.. py:property:: s11_l
   :type: Optional[float]


   
   Get or set the Yield stress in local x-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s22_m
   :type: Optional[float]


   
   Get or set the Yield stress in local y-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s33_n
   :type: Optional[float]


   
   Get or set the Yield stress in local z-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s12
   :type: Optional[float]


   
   Get or set the Yield stress in local xy-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.2.0 : Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: vp
   :type: float


   
   Get or set the Formulation for rate effects:
   EQ.0.0: scale yield stress (default),
   EQ.1.0: viscoplastic formulation.
















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

.. py:property:: id3upd
   :type: Optional[float]


   
   Get or set the Flag for transverse through thickness strain update (thin shells only):
   EQ.0.0: reflects R - values by splitting the strain tensor into elastic and plastic components
   EQ.1.0 : elastic update using total strain tensor
















   ..
       !! processed by numpydoc !!

.. py:property:: extra
   :type: Optional[float]


   
   Get or set the Flag to input further data:
   EQ.1.0:Tsai-Wu failure criterion parameters (cards 8 and 9)
















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

.. py:property:: ihis
   :type: Optional[float]


   
   Get or set the Flag for material properties initialization.
   EQ.0:   material properties defined in Cards 1-5 are used
   GE.1:   Use *INITIAL_STRESS_SOLID/SHELL to initialize material properties on an element-by-element basis for solid or shell elements, respectively (see Remarks below).
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength, a-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the Longitudinal compressive strength, a-axis (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, b-axis (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: sxy
   :type: Optional[float]


   
   Get or set the Shear strength, ab-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff12
   :type: Optional[float]


   
   Get or set the Scale factor between -1 and +1 for interaction term F12, see Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncfail
   :type: float


   
   Get or set the Number of timesteps to reduce stresses until element deletion.The default is NCFAIL=10..
















   ..
       !! processed by numpydoc !!

.. py:property:: zt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, c-axis (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, c-axis (positive value) (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: syz
   :type: Optional[float]


   
   Get or set the Shear strength, bc-plane (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: szx
   :type: Optional[float]


   
   Get or set the Shear strength, ca-plane (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ff23
   :type: Optional[float]


   
   Get or set the Scale factor between -1 and +1 for interaction term F23, see Remarks (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ff31
   :type: Optional[float]


   
   Get or set the Scale factor between -1 and +1 for interaction term F31, see Remarks (solid elements only).
















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
   :value: 'ANISOTROPIC_ELASTIC_PLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





