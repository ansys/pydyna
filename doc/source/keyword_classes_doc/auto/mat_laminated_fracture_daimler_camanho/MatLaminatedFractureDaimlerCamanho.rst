





:class:`MatLaminatedFractureDaimlerCamanho`
===========================================


.. py:class:: mat_laminated_fracture_daimler_camanho.MatLaminatedFractureDaimlerCamanho(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_LAMINATED_FRACTURE_DAIMLER_CAMANHO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatLaminatedFractureDaimlerCamanho

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~ea`
            - Get or set the Ea, Young's modulus in a-direction (longitudinal).
          * - :py:attr:`~eb`
            - Get or set the Eb, Young's modulus in b-direction (transverse).
          * - :py:attr:`~ec`
            - Get or set the Ec, Young's modulus in c-direction.
          * - :py:attr:`~prba`
            - Get or set the Vba, Poisson's ratio ba.
          * - :py:attr:`~prca`
            - Get or set the Vca, Poisson's ratio ca.
          * - :py:attr:`~prcb`
            - Get or set the Vcb, Poisson's ratio cb.
          * - :py:attr:`~gab`
            - Get or set the Gab, shear modulus ab.
          * - :py:attr:`~gbc`
            - Get or set the Gbc, shear modulus bc.
          * - :py:attr:`~gca`
            - Get or set the Gca, shear modulus ca.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~daf`
            - Get or set the Flag to control failure of an integration point based on longitudinal (fiber) tensile failure:
          * - :py:attr:`~dkf`
            - Get or set the Flag to control failure of an integration point based on longitudinal (fiber) compressive failure:
          * - :py:attr:`~dmf`
            - Get or set the Flag to control failure of an integration point based on transverse (matrix) failure:
          * - :py:attr:`~efs`
            - Get or set the Maximum effective strain for element layer failure. A value of unity
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Define components of vector a for AOPT = 2.
          * - :py:attr:`~dsf`
            - Get or set the Flag to control failure of an integration point based on in-plane shear failure:
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Define components of vector d for AOPT = 2.
          * - :py:attr:`~mangle`
            - Get or set the Material angle in degrees for AOPT = 0 (shells only) and 3.
          * - :py:attr:`~msg`
            - Get or set the Flag to control the output of warning messages:
          * - :py:attr:`~gxc`
            - Get or set the Fracture toughness for longitudinal (fiber) compressive failure mode.
          * - :py:attr:`~gxt`
            - Get or set the Fracture toughness for longitudinal (fiber) tensile failure mode.
          * - :py:attr:`~gyc`
            - Get or set the Fracture toughness for transverse compressive failure mode.
          * - :py:attr:`~gyt`
            - Get or set the Fracture toughness for transverse tensile failure mode.
          * - :py:attr:`~gsl`
            - Get or set the Fracture toughness for in-plane shear failure mode.
          * - :py:attr:`~gxco`
            - Get or set the Fracture toughness for longitudinal (fiber) compressive failure mode
          * - :py:attr:`~gxto`
            - Get or set the Fracture toughness for longitudinal (fiber) tensile failure mode to
          * - :py:attr:`~xc`
            - Get or set the Longitudinal compressive strength, a-axis (positive value).
          * - :py:attr:`~xt`
            - Get or set the Longitudinal tensile strength, a-axis.
          * - :py:attr:`~yc`
            - Get or set the Transverse compressive strength, b-axis (positive value).
          * - :py:attr:`~yt`
            - Get or set the Transverse tensile strength, b-axis.
          * - :py:attr:`~sl`
            - Get or set the Shear strength, ab plane.
          * - :py:attr:`~xco`
            - Get or set the Longitudinal compressive strength at inflection point (positive value).
          * - :py:attr:`~xto`
            - Get or set the Longitudinal tensile strength at inflection point.
          * - :py:attr:`~fio`
            - Get or set the Fracture angle in pure transverse compression (in degrees, default = 53.0).
          * - :py:attr:`~sigy`
            - Get or set the In-plane shear yield stress.
          * - :py:attr:`~etan`
            - Get or set the Tangent modulus for in-plane shear plasticity.
          * - :py:attr:`~beta`
            - Get or set the Hardening parameter for in-plane shear plasticity (0.0 <= BETA <=        1.0).
          * - :py:attr:`~pfl`
            - Get or set the Percentage of layers which must fail until crashfront is initiated. E.g.
          * - :py:attr:`~puck`
            - Get or set the Flag for evaluation and post-processing of Puck's inter-fiber-failure
          * - :py:attr:`~soft`
            - Get or set the Softening reduction factor for material strength in crashfront   elements (default = 1.0).
          * - :py:attr:`~dt`
            - Get or set the Strain rate averaging option:
          * - :py:attr:`~epsf23`
            - Get or set the Damage initiation transverse shear strain (23-plane)
          * - :py:attr:`~epsr23`
            - Get or set the Final rupture transverse shear strain (23-plane)
          * - :py:attr:`~tsmd23`
            - Get or set the Transverse shear maximum damage; default‌ = 0.90 (23-plane).
          * - :py:attr:`~epsf31`
            - Get or set the Damage initiation transverse shear strain (31-plane)
          * - :py:attr:`~epsr31`
            - Get or set the Final rupture transverse shear strain (31-plane)
          * - :py:attr:`~tsmd31`
            - Get or set the Transverse shear maximum damage; default‌ = 0.90 (31-plane)
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

    from mat_laminated_fracture_daimler_camanho import MatLaminatedFractureDaimlerCamanho

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Ea, Young's modulus in a-direction (longitudinal).
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus in b-direction (transverse).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Ec, Young's modulus in c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Vba, Poisson's ratio ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Vca, Poisson's ratio ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Vcb, Poisson's ratio cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Gab, shear modulus ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Gbc, shear modulus bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Gca, shear modulus ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
   and then, for shells only, rotated about the shell element normal by an angle MANGLE
   EQ.1.0: locally orthotropic with material axes determined by a
   point in space and the global location of the element center; this is the a-direction.
   This option is for solid elements only.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   EQ.4.0: locally orthotropic in cylindrical coordinate system with
   the material axes determined by a vector v, and an originating point, p, which define the centerline axis. This option is for solid elements only
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: daf
   :type: float


   
   Get or set the Flag to control failure of an integration point based on longitudinal (fiber) tensile failure:
   EQ.0.0: Integration point fails if any damage variable reaches 1.0.
   EQ.1.0: No failure of integration point due to fiber tensile failure (da(i)=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dkf
   :type: float


   
   Get or set the Flag to control failure of an integration point based on longitudinal (fiber) compressive failure:
   EQ.0.0: integration point fails if any damage variable reaches 1.0.
   EQ.1.0: no failure of integration point due to fiber compressive failure        (dkink(i)=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dmf
   :type: float


   
   Get or set the Flag to control failure of an integration point based on transverse (matrix) failure:
   EQ.0.0: integration point fails if any damage variable reaches 1.0.
   EQ.1.0: no failure of integration point due to matrix failure (dmat(i)=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: efs
   :type: Optional[float]


   
   Get or set the Maximum effective strain for element layer failure. A value of unity
   would equal 100% strain.
   GT.0.0: fails when effective strain calculated assuming material is volume preserving exceeds EFS.
   LT.0.0: fails when effective strain calculated from the full strain tensor exceeds |EFS|.
















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


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsf
   :type: float


   
   Get or set the Flag to control failure of an integration point based on in-plane shear failure:
   EQ. 0.0: integration point fails if any damage variable reaches 1.0.
   EQ. 1.0: No failure of integration point due to in-plane shear failure, dls(i)=1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: mangle
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 (shells only) and 3.
   MANGLE may be overridden on the element card, see
   *ELEMENT_SHELL_BETA and *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: msg
   :type: Optional[float]


   
   Get or set the Flag to control the output of warning messages:
   EQ.0:   Nnly one warning message will be written per part.
   GT.0 : All warnings are written.
   LT.0 : No warnings are written.
















   ..
       !! processed by numpydoc !!

.. py:property:: gxc
   :type: Optional[float]


   
   Get or set the Fracture toughness for longitudinal (fiber) compressive failure mode.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GXC) which defines the fracture
   toughness for fiber compressive failure mode as a
   function of characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gxt
   :type: Optional[float]


   
   Get or set the Fracture toughness for longitudinal (fiber) tensile failure mode.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GXT) which defines the fracture
   toughness for fiber tensile failure mode as a function of
   characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gyc
   :type: Optional[float]


   
   Get or set the Fracture toughness for transverse compressive failure mode.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GYC)) which defines the fracture
   toughness for intralaminar matrix tensile failure as a
   function of characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gyt
   :type: Optional[float]


   
   Get or set the Fracture toughness for transverse tensile failure mode.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GYT)) which defines the fracture
   toughness for intralaminar matrix transverse shear failure
   as a function of characteristic element length. No further      regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gsl
   :type: Optional[float]


   
   Get or set the Fracture toughness for in-plane shear failure mode.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GSL)) which defines the fracture
   toughness for intralaminar matrix longitudinal shear
   failure as a function of characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gxco
   :type: Optional[float]


   
   Get or set the Fracture toughness for longitudinal (fiber) compressive failure mode
   to define bi-linear damage evolution..
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GXCO)) which defines the fracture
   toughness for intralaminar matrix longitudinal shear
   failure as a function of characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: gxto
   :type: Optional[float]


   
   Get or set the Fracture toughness for longitudinal (fiber) tensile failure mode to
   define bi-linear damage evolution.
   GT.0.0: The given value will be regularized with the characteristic element length.
   LT.0.0: Load curve ID=(-GXTO)) which defines the fracture
   toughness for intralaminar matrix longitudinal shear
   failure as a function of characteristic element length. No further regularization.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the Longitudinal compressive strength, a-axis (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength, a-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, b-axis (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sl
   :type: Optional[float]


   
   Get or set the Shear strength, ab plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: xco
   :type: Optional[float]


   
   Get or set the Longitudinal compressive strength at inflection point (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: xto
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength at inflection point.
















   ..
       !! processed by numpydoc !!

.. py:property:: fio
   :type: float


   
   Get or set the Fracture angle in pure transverse compression (in degrees, default = 53.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the In-plane shear yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Tangent modulus for in-plane shear plasticity.
   GT.0.0: constant value
   LT.0.0: Load curve ID = (-ETAN) which defines the tangent modulus for in-plane shear plasticity vs. strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Hardening parameter for in-plane shear plasticity (0.0 <= BETA <=        1.0).
   EQ.0.0: Pure kinematic hardening
   EQ.1.0: Pure isotropic hardening        0.0<BETA<1.0: mixed hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfl
   :type: Optional[float]


   
   Get or set the Percentage of layers which must fail until crashfront is initiated. E.g.
   |PFL|=80.0, then 80 % of layers must fail until strengths are reduced
   in neighboring elements. Default: all layers must fail. A single layer
   fails if 1 in-plane IP fails (PFL>0) or if 4 in-plane IPs fail (PFL<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: puck
   :type: float


   
   Get or set the Flag for evaluation and post-processing of Puck's inter-fiber-failure
   criterion (IFF, see Puck, Kopp and Knops [2002]).
   EQ.0.0: no evaluation of Puck's IFF-criterion.
   EQ.1.0: Puck's IFF-criterion will be evaluated.
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: float


   
   Get or set the Softening reduction factor for material strength in crashfront   elements (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Strain rate averaging option:
   EQ.0.0: strain rate is evaluated using a running average.
   LT.0.0 : strain rate is evaluated using average of last 11 time steps.
   GT.0.0 : strain rate is averaged over the last DT time units.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsf23
   :type: Optional[float]


   
   Get or set the Damage initiation transverse shear strain (23-plane)
















   ..
       !! processed by numpydoc !!

.. py:property:: epsr23
   :type: Optional[float]


   
   Get or set the Final rupture transverse shear strain (23-plane)
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmd23
   :type: Optional[float]


   
   Get or set the Transverse shear maximum damage; default‌ = 0.90 (23-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsf31
   :type: Optional[float]


   
   Get or set the Damage initiation transverse shear strain (31-plane)
















   ..
       !! processed by numpydoc !!

.. py:property:: epsr31
   :type: Optional[float]


   
   Get or set the Final rupture transverse shear strain (31-plane)
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmd31
   :type: Optional[float]


   
   Get or set the Transverse shear maximum damage; default‌ = 0.90 (31-plane)
















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
   :value: 'LAMINATED_FRACTURE_DAIMLER_CAMANHO'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





