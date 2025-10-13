





:class:`Mat058Solid`
====================


.. py:class:: mat_058_solid.Mat058Solid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_058_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat058Solid

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
            - Get or set the GT.0.0:  E_a, Young’s modulus - longitudinal direction
          * - :py:attr:`~eb`
            - Get or set the GT.0.0:  E_b, Young’s modulus - transverse direction
          * - :py:attr:`~ec`
            - Get or set the E_c, Young’s modulus - normal direction (used only by thick shells and solids).  See Remark 6.
          * - :py:attr:`~prba`
            - Get or set the Poisson's ratio ba.
          * - :py:attr:`~tau1`
            - Get or set the tau-1, stress limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve. The values tau-1 and gamma-1 are used to define a curve of shear stress versus shear strain. These values are input if FS, defined below, is set to a value of -1.
          * - :py:attr:`~gamma1`
            - Get or set the gamma-1, strain limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve.
          * - :py:attr:`~gab`
            - Get or set the GT.0.0:  G_ab, shear modulus in the ab-direction
          * - :py:attr:`~gbc`
            - Get or set the GT.0.0:  G_bc, shear modulus in the cb-direction
          * - :py:attr:`~gca`
            - Get or set the GT.0.0:  G_ca, shear modulus in the ca-direction
          * - :py:attr:`~slimt1`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (fiber tension).
          * - :py:attr:`~slimc1`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (fiber compression).
          * - :py:attr:`~slimt2`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension).
          * - :py:attr:`~slimc2`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression).
          * - :py:attr:`~slims`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (shear).
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~tsize`
            - Get or set the Time step for automatic element deletion.
          * - :py:attr:`~erods`
            - Get or set the Maximum effective strain for element layer failure. A value of unity would equal 100% strain.
          * - :py:attr:`~soft`
            - Get or set the Softening reduction factor for strength in the crashfront.
          * - :py:attr:`~fs`
            - Get or set the Failure surface type:
          * - :py:attr:`~epsf`
            - Get or set the Damage initiation transverse shear strain
          * - :py:attr:`~epsr`
            - Get or set the Final rupture transverse shear strain
          * - :py:attr:`~tsmd`
            - Get or set the Transverse shear maximum damage, default = 0.90.
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~prca`
            - Get or set the Poisson's ratio ca
          * - :py:attr:`~prcb`
            - Get or set the Poisson's ratio cb
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
          * - :py:attr:`~lcdfail`
            - Get or set the Load-Curve ID, that defines orientation dependent failure strains.
          * - :py:attr:`~e11c`
            - Get or set the Strain at longitudinal compressive strength, a-axis.
          * - :py:attr:`~e11t`
            - Get or set the Strain at longitudinal tensile strength, a-axis.
          * - :py:attr:`~e22c`
            - Get or set the Strain at transverse compressive strength, b-axis.
          * - :py:attr:`~e22t`
            - Get or set the Strain at transverse tensile strength, b-axis.
          * - :py:attr:`~gms`
            - Get or set the Strain at shear strength, ab plane.
          * - :py:attr:`~xc`
            - Get or set the Longitudinal compressive strength.
          * - :py:attr:`~xt`
            - Get or set the Longitudinal tensile strength.
          * - :py:attr:`~yc`
            - Get or set the Transverse compressive strength, b-axis.
          * - :py:attr:`~yt`
            - Get or set the Transverse tensile strength, b-axis.
          * - :py:attr:`~sc`
            - Get or set the Shear strength, ab plane.
          * - :py:attr:`~e33c`
            - Get or set the Strain at transverse compressive strength, c-axis.
          * - :py:attr:`~e33t`
            - Get or set the Strain at transverse tensile strength, c-axis.
          * - :py:attr:`~gm23`
            - Get or set the Engineering shear strain at shear strength, bc-plane.
          * - :py:attr:`~gm31`
            - Get or set the Engineering shear strain at shear strength, ca-plane.
          * - :py:attr:`~zc`
            - Get or set the Transverse compressive strength, c-axis (positive value).
          * - :py:attr:`~zt`
            - Get or set the Transverse tensile strength, c-axis.
          * - :py:attr:`~sc23`
            - Get or set the Shear strength, bc-plane.
          * - :py:attr:`~sc31`
            - Get or set the Shear strength, ca-plane.
          * - :py:attr:`~slimt3`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension, c-axis).
          * - :py:attr:`~slimc3`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression, c-axis).
          * - :py:attr:`~slims23`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (shear, bc-plane
          * - :py:attr:`~lsims31`
            - Get or set the Factor to determine the minimum stress limit after stress maximum (shear, ca-plane).
          * - :py:attr:`~tau2`
            - Get or set the τ_2, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_2 and γ_2 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (bc-plane).
          * - :py:attr:`~gamma2`
            - Get or set the γ_2, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
          * - :py:attr:`~tau3`
            - Get or set the τ_3, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_3 and γ_3 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (ca-plane).
          * - :py:attr:`~gamma3`
            - Get or set the γ_3, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
          * - :py:attr:`~lcxc`
            - Get or set the Load curve ID defining longitudinal compressive strength XC vs. strain rate (XC is ignored with that option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcxt`
            - Get or set the Load curve ID defining longitudinal tensile strength XT vs. strain rate (XT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcyc`
            - Get or set the Load curve ID defining transverse compressive strength YC vs. strain rate (YC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcyt`
            - Get or set the Load curve ID defining transverse tensile strength YT vs. strain rate (YT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcsc`
            - Get or set the Load curve ID defining shear strength SC vs. strain rate (SC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lctau`
            - Get or set the Load curve ID defining TAU1 vs. strain rate (TAU1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcgam`
            - Get or set the Load curve ID defining GAMMA1 vs. strain rate (GAMMA1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~dt`
            - Get or set the Strain rate averaging option.
          * - :py:attr:`~lce11c`
            - Get or set the Load curve ID defining E11C vs. strain rate (E11C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lce11t`
            - Get or set the Load curve ID defining E11T vs. strain rate (E11T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lce22c`
            - Get or set the Load curve ID defining E22C vs. strain rate (E22C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lce22t`
            - Get or set the Load curve ID defining E22T vs. strain rate (E22T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcgms`
            - Get or set the Load curve ID defining GMS vs. strain rate (GMS is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lcefs`
            - Get or set the Load curve ID defining ERODS as a function of strain rate (ERODS is ignored with this option). The full strain tensor is used to compute the equivalent strain (new option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
          * - :py:attr:`~lczc`
            - Get or set the Load curve ID defining transverse tensile strength ZT as a function of strain rate (ZT is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lczt`
            - Get or set the Load curve ID defining shear strength SC23 as a function of strain rate (SC23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lcsc23`
            - Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lcsc31`
            - Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lctau2`
            - Get or set the Load curve ID defining TAU2 as a function of strain rate (TAU2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
          * - :py:attr:`~lcgam2`
            - Get or set the Load curve ID defining GAMMA2 as a function of strain rate (GAMMA2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
          * - :py:attr:`~lctau3`
            - Get or set the Load curve ID defining TAU3 as a function of strain rate (TAU3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
          * - :py:attr:`~lcgam3`
            - Get or set the Load curve ID defining GAMMA3 as a function of strain rate (GAMMA3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
          * - :py:attr:`~lce33c`
            - Get or set the Load curve ID defining E33C as a function of strain rate (E33C is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lce33t`
            - Get or set the Load curve ID defining E33T as a function of strain rate (E33T is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
          * - :py:attr:`~lcgms23`
            - Get or set the Load curve ID defining GMS23 as a function of strain rate (GMS23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
          * - :py:attr:`~lcgms31`
            - Get or set the Load curve ID defining GMS31 as a function of strain rate (GMS31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
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

    from mat_058_solid import Mat058Solid

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


   
   Get or set the GT.0.0:  E_a, Young’s modulus - longitudinal direction
   LT.0.0: Load Curve ID or Table ID = (-EA).See Remark 8.
   Load Curve.When - EA is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the longitudinal direction.Negative data
   points correspond to compression and positive values to tension.
   Tabular Data.When - EA is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the longitudinal direction.
   Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the GT.0.0:  E_b, Young’s modulus - transverse direction
   LT.0.0: Load Curve ID or Table ID = (-EB).See Remark 8.
   Load Curve.When - EB is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the transverse direction.Negative data points correspond to compression and positive values to tension.
   Tabular Data.When - EB is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the transverse direction.
   Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the E_c, Young’s modulus - normal direction (used only by thick shells and solids).  See Remark 6.
   GT.0.0: E_c, Young’s modulus - normal direction
   LT.0.0 : Load Curve ID or Table ID = (-EC) (solids only).See Remark 8.
   Load Curve.When - EC is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the transverse direction.Negative data points correspond to compression and positive values to tension.
   Tabular Data.When - EC is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the transverse direction.
   Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Poisson's ratio ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau1
   :type: Optional[float]


   
   Get or set the tau-1, stress limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve. The values tau-1 and gamma-1 are used to define a curve of shear stress versus shear strain. These values are input if FS, defined below, is set to a value of -1.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma1
   :type: Optional[float]


   
   Get or set the gamma-1, strain limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the GT.0.0:  G_ab, shear modulus in the ab-direction
   LT.0.0: Load Curve ID or Table ID = (-GAB)
   Load Curve.When - GAB is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of she strain behavior in the ab - direction.
   Tabular Data.When - GAB is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the ab - direction.
   Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the GT.0.0:  G_bc, shear modulus in the cb-direction
   LT.0.0: Load Curve ID or Table ID = (-GBC) (solids only)
   Load Curve.When - GBC is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of shear strain behavior in the bc - direction.
   Tabular Data.When - GBC is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the bc - direction.
   Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the GT.0.0:  G_ca, shear modulus in the ca-direction
   LT.0.0: Load Curve ID or Table ID = (-GCA) (solids only)
   Load Curve.When - GCA is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of shear strain behavior in the ca - direction.
   Tabular Data.When - GCA is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the ca - direction.
   Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: slimt1
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (fiber tension).
















   ..
       !! processed by numpydoc !!

.. py:property:: slimc1
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (fiber compression).
















   ..
       !! processed by numpydoc !!

.. py:property:: slimt2
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension).
















   ..
       !! processed by numpydoc !!

.. py:property:: slimc2
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression).
















   ..
       !! processed by numpydoc !!

.. py:property:: slims
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (shear).
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsize
   :type: Optional[float]


   
   Get or set the Time step for automatic element deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: erods
   :type: Optional[float]


   
   Get or set the Maximum effective strain for element layer failure. A value of unity would equal 100% strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: Optional[float]


   
   Get or set the Softening reduction factor for strength in the crashfront.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Failure surface type:
   EQ.1.0:smooth failure surface with a quadratic criterion for both the fiber (a) and transverse (b) directions. This option can be used with complete laminates and fabrics,
   EQ.0.0:smooth failure surface in the transverse (b) direction with a limiting value in the fiber (a) direction. This model is appropiate for unidirectional (UD) layered composites only (default),
   EQ.-1.:faceted failure surface. When the strength values are reached then damage evolves in tension and compression for both the fiber and transverse direction. Shear behavior is also considered. This option can be used with complete laminates and fabrics.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsf
   :type: Optional[float]


   
   Get or set the Damage initiation transverse shear strain
















   ..
       !! processed by numpydoc !!

.. py:property:: epsr
   :type: Optional[float]


   
   Get or set the Final rupture transverse shear strain
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmd
   :type: float


   
   Get or set the Transverse shear maximum damage, default = 0.90.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1.
















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

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Poisson's ratio ca
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Poisson's ratio cb
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















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


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdfail
   :type: Optional[int]


   
   Get or set the Load-Curve ID, that defines orientation dependent failure strains.
   The ordinate values in the load-curve define the various failure strains, in the following order:
   1. EF_11T: tensile failure strain in longitudinal a-direction
   2. EF_11C: compressive failure strain in longitudinal a-direction
   3. EF_22T: tensile failure strain in transverse b-direction
   4. EF_22C: compressive failure strain in transverse b-direction
   5. EF_12: in-plane shear failure strain in ab-plane
   6. EF_33T: tensile failure strain in transverse c-direction
   7. EF_33C: compressive failure strain in transverse c-direction
   8. EF_23: out-of plane shear failure strain in bc-plane
   9. EF_31: out-of plane shear failure strain in ca-plane
   Thus, the load-curve to define these values has to have either 5 (shells) or 9 (solids) entries in its definition.
   A load-curve definition with 9 entries may be used for shells, ignoring the last 4 entries. The abscissa values are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: e11c
   :type: Optional[float]


   
   Get or set the Strain at longitudinal compressive strength, a-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: e11t
   :type: Optional[float]


   
   Get or set the Strain at longitudinal tensile strength, a-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22c
   :type: Optional[float]


   
   Get or set the Strain at transverse compressive strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22t
   :type: Optional[float]


   
   Get or set the Strain at transverse tensile strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: gms
   :type: Optional[float]


   
   Get or set the Strain at shear strength, ab plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the Longitudinal compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, b-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc
   :type: Optional[float]


   
   Get or set the Shear strength, ab plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: e33c
   :type: Optional[float]


   
   Get or set the Strain at transverse compressive strength, c-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: e33t
   :type: Optional[float]


   
   Get or set the Strain at transverse tensile strength, c-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: gm23
   :type: Optional[float]


   
   Get or set the Engineering shear strain at shear strength, bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: gm31
   :type: Optional[float]


   
   Get or set the Engineering shear strain at shear strength, ca-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, c-axis (positive value).
















   ..
       !! processed by numpydoc !!

.. py:property:: zt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, c-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc23
   :type: Optional[float]


   
   Get or set the Shear strength, bc-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc31
   :type: Optional[float]


   
   Get or set the Shear strength, ca-plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: slimt3
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension, c-axis).
















   ..
       !! processed by numpydoc !!

.. py:property:: slimc3
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression, c-axis).
















   ..
       !! processed by numpydoc !!

.. py:property:: slims23
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (shear, bc-plane
















   ..
       !! processed by numpydoc !!

.. py:property:: lsims31
   :type: Optional[float]


   
   Get or set the Factor to determine the minimum stress limit after stress maximum (shear, ca-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau2
   :type: Optional[float]


   
   Get or set the τ_2, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_2 and γ_2 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (bc-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma2
   :type: Optional[float]


   
   Get or set the γ_2, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau3
   :type: Optional[float]


   
   Get or set the τ_3, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_3 and γ_3 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (ca-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma3
   :type: Optional[float]


   
   Get or set the γ_3, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcxc
   :type: Optional[int]


   
   Get or set the Load curve ID defining longitudinal compressive strength XC vs. strain rate (XC is ignored with that option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcxt
   :type: Optional[int]


   
   Get or set the Load curve ID defining longitudinal tensile strength XT vs. strain rate (XT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcyc
   :type: Optional[int]


   
   Get or set the Load curve ID defining transverse compressive strength YC vs. strain rate (YC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcyt
   :type: Optional[int]


   
   Get or set the Load curve ID defining transverse tensile strength YT vs. strain rate (YT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsc
   :type: Optional[int]


   
   Get or set the Load curve ID defining shear strength SC vs. strain rate (SC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctau
   :type: Optional[int]


   
   Get or set the Load curve ID defining TAU1 vs. strain rate (TAU1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgam
   :type: Optional[int]


   
   Get or set the Load curve ID defining GAMMA1 vs. strain rate (GAMMA1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Strain rate averaging option.
   EQ.0.0: Strain rate is evaluated using a running average.
   LT.0.0: Strain rate is evaluated using average of last 11 time steps.
   GT.0.0: Strain rate is averaged over the last DT time units.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce11c
   :type: int


   
   Get or set the Load curve ID defining E11C vs. strain rate (E11C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce11t
   :type: int


   
   Get or set the Load curve ID defining E11T vs. strain rate (E11T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce22c
   :type: int


   
   Get or set the Load curve ID defining E22C vs. strain rate (E22C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce22t
   :type: int


   
   Get or set the Load curve ID defining E22T vs. strain rate (E22T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgms
   :type: int


   
   Get or set the Load curve ID defining GMS vs. strain rate (GMS is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcefs
   :type: int


   
   Get or set the Load curve ID defining ERODS as a function of strain rate (ERODS is ignored with this option). The full strain tensor is used to compute the equivalent strain (new option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lczc
   :type: Optional[int]


   
   Get or set the Load curve ID defining transverse tensile strength ZT as a function of strain rate (ZT is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lczt
   :type: Optional[int]


   
   Get or set the Load curve ID defining shear strength SC23 as a function of strain rate (SC23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsc23
   :type: Optional[int]


   
   Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsc31
   :type: Optional[int]


   
   Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctau2
   :type: Optional[int]


   
   Get or set the Load curve ID defining TAU2 as a function of strain rate (TAU2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgam2
   :type: Optional[int]


   
   Get or set the Load curve ID defining GAMMA2 as a function of strain rate (GAMMA2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lctau3
   :type: Optional[int]


   
   Get or set the Load curve ID defining TAU3 as a function of strain rate (TAU3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgam3
   :type: Optional[int]


   
   Get or set the Load curve ID defining GAMMA3 as a function of strain rate (GAMMA3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lce33c
   :type: Optional[int]


   
   Get or set the Load curve ID defining E33C as a function of strain rate (E33C is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce33t
   :type: Optional[int]


   
   Get or set the Load curve ID defining E33T as a function of strain rate (E33T is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgms23
   :type: Optional[int]


   
   Get or set the Load curve ID defining GMS23 as a function of strain rate (GMS23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcgms31
   :type: Optional[int]


   
   Get or set the Load curve ID defining GMS31 as a function of strain rate (GMS31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
















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
   :value: '058_SOLID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





