





:class:`MatAnisotropicViscoplastic`
===================================


.. py:class:: mat_anisotropic_viscoplastic.MatAnisotropicViscoplastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ANISOTROPIC_VISCOPLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAnisotropicViscoplastic

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
            - Get or set the Initial yield stress.
          * - :py:attr:`~flag`
            - Get or set the Flag
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain. Card 2 is ignored with this option. The table ID, see Figure 20.7, defines for each strain rate value a load curve ID giving the stress versus effectiveplastic strain for that rate. If the load curve only is used, then the coefficients Vk and Vm must be given if viscoplastice behavior is desired. If a Table ID is given these coefficients are determined internally during initialization.
          * - :py:attr:`~alpha`
            - Get or set the Distribution of hardening used in the curve-fitting. ALPHA = 0 provides pure kinematic hardening, ALPHA = 1 provides pure isotropic hardening.
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter Qr1.
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter Cr1.
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter Qr2.
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter Cr2.
          * - :py:attr:`~qx1`
            - Get or set the Kinematic hardening parameter Qc1.
          * - :py:attr:`~cx1`
            - Get or set the Kinematic hardening parameter Cc1.
          * - :py:attr:`~qx2`
            - Get or set the Kinematic hardening parameter Qc2.
          * - :py:attr:`~cx2`
            - Get or set the Kinematic hardening parameter Cc2.
          * - :py:attr:`~vk`
            - Get or set the Viscous material parameter Vk.
          * - :py:attr:`~vm`
            - Get or set the Viscous material parameter Vm.
          * - :py:attr:`~r00_f`
            - Get or set the R00 for shell, default=1.0, F for brick default =1/2.
          * - :py:attr:`~r45_g`
            - Get or set the R45 for shell, default=1.0, G for brick default =1/2.
          * - :py:attr:`~r90_h`
            - Get or set the R90 for shell, default=1.0, H for brick default =1/2.
          * - :py:attr:`~l`
            - Get or set the L for brick, default =3/2.
          * - :py:attr:`~m`
            - Get or set the M for brick, default =3/2.
          * - :py:attr:`~n`
            - Get or set the N for brick, default =3/2.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~fail`
            - Get or set the Failure flag.
          * - :py:attr:`~numint`
            - Get or set the Number of integration points which must fail before element deletion. If zero, all points must fail. This option applies to shell elements only. For the case of one point shells, NUMINT should be set to a value that is less than the number of through thickness integration points. Nonphysical stretching can sometimes appear in the results if all integration points have failed except for one point away from the midsurface. This is due to the fact that unconstrained nodal rotations will prevent strains from developing at the remaining integration point. In fully integrated shells, similar problems can occur.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
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

    from mat_anisotropic_viscoplastic import MatAnisotropicViscoplastic

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


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: int


   
   Get or set the Flag
   EQ.0 Give all material parameters (default),
   EQ.1 Material parameters are fit in LS-DYNA to Load curve. The parameters Qr1, Cr1, Qr2, and Cr2 for isotropic hardening are determined by the fit and those for kinematic hardening are found by scaling those for isotropic hardening by (1-α) where α is defined.
   EQ.2: Use load curve directly, i.e., no fitting is required for the parameters Q-r1, C-r1, Q-r2, and C-r2.EQ.4: Use table definition directly, no fitting is required and the values for Qr1, Cr1, Qr2, Cr2, Vk and Vm are ignored. Only
   isotropic hardening is implemented, and this option is only available for solids
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain. Card 2 is ignored with this option. The table ID, see Figure 20.7, defines for each strain rate value a load curve ID giving the stress versus effectiveplastic strain for that rate. If the load curve only is used, then the coefficients Vk and Vm must be given if viscoplastice behavior is desired. If a Table ID is given these coefficients are determined internally during initialization.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Distribution of hardening used in the curve-fitting. ALPHA = 0 provides pure kinematic hardening, ALPHA = 1 provides pure isotropic hardening.
















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

.. py:property:: qx1
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter Qc1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cx1
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter Cc1.
















   ..
       !! processed by numpydoc !!

.. py:property:: qx2
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter Qc2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cx2
   :type: Optional[float]


   
   Get or set the Kinematic hardening parameter Cc2.
















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


   
   Get or set the R00 for shell, default=1.0, F for brick default =1/2.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45_g
   :type: Optional[float]


   
   Get or set the R45 for shell, default=1.0, G for brick default =1/2.
















   ..
       !! processed by numpydoc !!

.. py:property:: r90_h
   :type: Optional[float]


   
   Get or set the R90 for shell, default=1.0, H for brick default =1/2.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the L for brick, default =3/2.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the M for brick, default =3/2.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the N for brick, default =3/2.
















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

.. py:property:: fail
   :type: Optional[float]


   
   Get or set the Failure flag.
   LT.0.0: User defined failure subroutine is called to determine failure. This is subroutine named, MATUSR_103, in DYN21.F.
   EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many calculations will be saved.
   GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: Optional[float]


   
   Get or set the Number of integration points which must fail before element deletion. If zero, all points must fail. This option applies to shell elements only. For the case of one point shells, NUMINT should be set to a value that is less than the number of through thickness integration points. Nonphysical stretching can sometimes appear in the results if all integration points have failed except for one point away from the midsurface. This is due to the fact that unconstrained nodal rotations will prevent strains from developing at the remaining integration point. In fully integrated shells, similar problems can occur.
















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


   
   Get or set the x-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
















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
   :value: 'ANISOTROPIC_VISCOPLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





