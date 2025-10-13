





:class:`MatBarlatYld2000`
=========================


.. py:class:: mat_barlat_yld2000.MatBarlatYld2000(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_BARLAT_YLD2000 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatBarlatYld2000

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
          * - :py:attr:`~fit`
            - Get or set the Material parameter fit flag:
          * - :py:attr:`~beta`
            - Get or set the Hardening parameter, 0<b<1.
          * - :py:attr:`~iter`
            - Get or set the Plastic iteration flag:
          * - :py:attr:`~iscale`
            - Get or set the Yield locus scaling flag:
          * - :py:attr:`~k`
            - Get or set the material parameter.
          * - :py:attr:`~e0`
            - Get or set the epsilon-0, strain corresponding to the initial yield or b in Voce.
          * - :py:attr:`~n`
            - Get or set the n, hardening exponent for yield strength or c in Voce.
          * - :py:attr:`~c`
            - Get or set the epsilon-SR0, in powerlaw rate sensitivity.
          * - :py:attr:`~p`
            - Get or set the m, exponent for strain rate effects.
          * - :py:attr:`~hard`
            - Get or set the Hardening law:
          * - :py:attr:`~a`
            - Get or set the Flow potential exponent.
          * - :py:attr:`~crc1`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter1
          * - :py:attr:`~cra1`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc2`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra2`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc3`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra3`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc4`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra4`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~alpha1`
            - Get or set the Alpha1
          * - :py:attr:`~alpha2`
            - Get or set the Alpha2
          * - :py:attr:`~alpha3`
            - Get or set the Alpha3
          * - :py:attr:`~alpha4`
            - Get or set the Alpha4
          * - :py:attr:`~alpha5`
            - Get or set the Alpha5
          * - :py:attr:`~alpha6`
            - Get or set the Alpha6
          * - :py:attr:`~alpha7`
            - Get or set the Alpha7
          * - :py:attr:`~alpha8`
            - Get or set the Alpha8
          * - :py:attr:`~sig00`
            - Get or set the Yield stress in 00 direction
          * - :py:attr:`~sig45`
            - Get or set the Yield stress in 45 direction
          * - :py:attr:`~sig90`
            - Get or set the Yield stress in 90 direction
          * - :py:attr:`~r00`
            - Get or set the R-value in 00 direction
          * - :py:attr:`~r45`
            - Get or set the R-value in 45 direction
          * - :py:attr:`~r90`
            - Get or set the R-value in 90 direction
          * - :py:attr:`~sigxx`
            - Get or set the xx-component of stress on yield surface (see Remark 2).
          * - :py:attr:`~sigyy`
            - Get or set the yy-component of stress on yield surface (see Remark 2).
          * - :py:attr:`~sigxy`
            - Get or set the xy-component of stress on yield surface (see Remark 2).
          * - :py:attr:`~dxx`
            - Get or set the xx-component of tangent to yield surface (see Remark 2)
          * - :py:attr:`~dyy`
            - Get or set the yy-component of tangent to yield surface (see Remark 2)
          * - :py:attr:`~dxy`
            - Get or set the xy-component of tangent to yield surface (see Remark 2)
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~offang`
            - Get or set the Offset angle for AOPT = 3.
          * - :py:attr:`~p4`
            - Get or set the Material parameter:
          * - :py:attr:`~htflag`
            - Get or set the Heat treatment flag (see remarks):
          * - :py:attr:`~hta`
            - Get or set the Load curve/Table ID for postforming parameter A.
          * - :py:attr:`~htb`
            - Get or set the Load curve/Table ID for postforming parameter B.
          * - :py:attr:`~htc`
            - Get or set the Load curve/Table ID for postforming parameter C.
          * - :py:attr:`~htd`
            - Get or set the Load curve/Table ID for postforming parameter D.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
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
          * - :py:attr:`~usrfail`
            - Get or set the User defined failure flag
          * - :py:attr:`~cp`
            - Get or set the Adiabatic temperature calculation option:
          * - :py:attr:`~t0`
            - Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled.
          * - :py:attr:`~tref`
            - Get or set the Reference temperature for output of the yield stress as history variable.
          * - :py:attr:`~ta0`
            - Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
          * - :py:attr:`~b`
            - Get or set the Martensite rate equation parameter B.
          * - :py:attr:`~d`
            - Get or set the Martensite rate equation parameter D.
          * - :py:attr:`~q`
            - Get or set the Martensite rate equation parameter Q.
          * - :py:attr:`~e0mart`
            - Get or set the Martensite rate equation parameter E0(mart).
          * - :py:attr:`~vm0`
            - Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
          * - :py:attr:`~ahs`
            - Get or set the Hardening law parameter AHS.
          * - :py:attr:`~bhs`
            - Get or set the Hardening law parameter BHS.
          * - :py:attr:`~m`
            - Get or set the Hardening law parameter m.
          * - :py:attr:`~eps0`
            - Get or set the Hardening law parameter E0.
          * - :py:attr:`~hmart`
            - Get or set the Hardening law parameter.
          * - :py:attr:`~k1`
            - Get or set the Hardening law parameter K1.
          * - :py:attr:`~k2`
            - Get or set the Hardening law parameter K2.
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

    from mat_barlat_yld2000 import MatBarlatYld2000

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

.. py:property:: fit
   :type: float


   
   Get or set the Material parameter fit flag:
   EQ.0.0: Material parameters are used directly on card3.
   EQ.1.0:Material parameter are determined from test data on card3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Hardening parameter, 0<b<1.
















   ..
       !! processed by numpydoc !!

.. py:property:: iter
   :type: float


   
   Get or set the Plastic iteration flag:
   EQ.0.0:Plane stress algorithm for stress return.
   EQ.1.0:Secant iteration algorithm for stress return.
















   ..
       !! processed by numpydoc !!

.. py:property:: iscale
   :type: float


   
   Get or set the Yield locus scaling flag:
   EQ.0.0: Scaling on - reference direction=rolling direction (default)
   EQ.1.0: Scaling off - reference direction arbitrary
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the material parameter.
   HARD.EQ.1.0:k strength coefficient for exponential hardening.
   EQ.2.0: a in voce hardening law
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the epsilon-0, strain corresponding to the initial yield or b in Voce.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the n, hardening exponent for yield strength or c in Voce.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the epsilon-SR0, in powerlaw rate sensitivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the m, exponent for strain rate effects.
















   ..
       !! processed by numpydoc !!

.. py:property:: hard
   :type: float


   
   Get or set the Hardening law:
   EQ.1.0: Exponential hardening
   EQ.2.0: Voce hardening
   EQ.3.0: Hansel hardening
   EQ.4.0: Gosh hardening
   EQ.5.0: Hocket-Sherby hardening
   LT.0.0: absolute value defines load curve ID or table ID. If it is a load curve, then yield stress is a function of plastic strain.
   If it is a table, then yield stress is a function of either plastic strain and plastic strain rate in case of a 2D table,
   or, a function of plastic strain, plastic strain rate, and temperature in case of a 3D table
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Flow potential exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: crc1
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter1
















   ..
       !! processed by numpydoc !!

.. py:property:: cra1
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc2
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra2
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc3
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra3
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc4
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra4
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Alpha1
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Alpha2
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Alpha3
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha4
   :type: Optional[float]


   
   Get or set the Alpha4
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha5
   :type: Optional[float]


   
   Get or set the Alpha5
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha6
   :type: Optional[float]


   
   Get or set the Alpha6
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha7
   :type: Optional[float]


   
   Get or set the Alpha7
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha8
   :type: Optional[float]


   
   Get or set the Alpha8
















   ..
       !! processed by numpydoc !!

.. py:property:: sig00
   :type: Optional[float]


   
   Get or set the Yield stress in 00 direction
   LT.0.0: -SIG00 is load curve ID, defining this stress as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: sig45
   :type: Optional[float]


   
   Get or set the Yield stress in 45 direction
   LT.0.0: -SIG45 is load curve ID, defining this stress as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: sig90
   :type: Optional[float]


   
   Get or set the Yield stress in 90 direction
   LT.0.0: -SIG90 is load curve ID, defining this stress as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: Optional[float]


   
   Get or set the R-value in 00 direction
   LT.0.0: -R00 is load curve ID, defining this value as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: Optional[float]


   
   Get or set the R-value in 45 direction
   LT.0.0: -R45 is load curve ID, defining this value as a function of temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: Optional[float]


   
   Get or set the R-value in 90 direction
   LT.0.0: -R90 is load curve ID, defining this value as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxx
   :type: Optional[float]


   
   Get or set the xx-component of stress on yield surface (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyy
   :type: Optional[float]


   
   Get or set the yy-component of stress on yield surface (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxy
   :type: Optional[float]


   
   Get or set the xy-component of stress on yield surface (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: dxx
   :type: Optional[float]


   
   Get or set the xx-component of tangent to yield surface (see Remark 2)
















   ..
       !! processed by numpydoc !!

.. py:property:: dyy
   :type: Optional[float]


   
   Get or set the yy-component of tangent to yield surface (see Remark 2)
















   ..
       !! processed by numpydoc !!

.. py:property:: dxy
   :type: Optional[float]


   
   Get or set the xy-component of tangent to yield surface (see Remark 2)
















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

.. py:property:: offang
   :type: Optional[float]


   
   Get or set the Offset angle for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Material parameter:
   HARD.EQ.4.0: p in Gosh hardening law
   HARD.EQ:5.0: q in Hocket-Sherby hardening law.
















   ..
       !! processed by numpydoc !!

.. py:property:: htflag
   :type: int


   
   Get or set the Heat treatment flag (see remarks):
   HTFLAG.EQ.0: Preforming stage
   HTFLAG.EQ.1: Heat treatment stage
   HTFLAG.EQ.2: Postforming stage.
















   ..
       !! processed by numpydoc !!

.. py:property:: hta
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter A.
















   ..
       !! processed by numpydoc !!

.. py:property:: htb
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter B.
















   ..
       !! processed by numpydoc !!

.. py:property:: htc
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter C.
















   ..
       !! processed by numpydoc !!

.. py:property:: htd
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter D.
















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

.. py:property:: usrfail
   :type: int


   
   Get or set the User defined failure flag
   EQ.0: no user subroutine is called
   EQ.1: user subroutine matusr_24 in dyn21.f is called.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Adiabatic temperature calculation option:
   EQ.0.0: Adiabatic temperature calculation is disabled.
   GT.0.0: CP is the specific heat Cp. Adiabatic temperature calculation is enabled.
















   ..
       !! processed by numpydoc !!

.. py:property:: t0
   :type: Optional[float]


   
   Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature for output of the yield stress as history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: ta0
   :type: Optional[float]


   
   Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter B.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter D.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter Q.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0mart
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter E0(mart).
















   ..
       !! processed by numpydoc !!

.. py:property:: vm0
   :type: Optional[float]


   
   Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
   GT.0.0: Vm0 is set to VM0.
   LT.0.0: Can be used only when there are initial plastic strains Ep
   present, e.g. when using *INITIAL_STRESS_SHELL. The absolute
   value of VM0 is then the load curve ID for a function f that sets
   Vm0 = f(Ep). The function f must be a monotonically nondecreasing function of Ep.
















   ..
       !! processed by numpydoc !!

.. py:property:: ahs
   :type: Optional[float]


   
   Get or set the Hardening law parameter AHS.
















   ..
       !! processed by numpydoc !!

.. py:property:: bhs
   :type: Optional[float]


   
   Get or set the Hardening law parameter BHS.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Hardening law parameter m.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps0
   :type: Optional[float]


   
   Get or set the Hardening law parameter E0.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmart
   :type: Optional[float]


   
   Get or set the Hardening law parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Hardening law parameter K1.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Hardening law parameter K2.
















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
   :value: 'BARLAT_YLD2000'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





