





:class:`ConstrainedLagrangeInSolidEdge`
=======================================


.. py:class:: constrained_lagrange_in_solid_edge.ConstrainedLagrangeInSolidEdge(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_LAGRANGE_IN_SOLID_EDGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedLagrangeInSolidEdge

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~coupid`
            - Get or set the ID.
          * - :py:attr:`~title`
            - Get or set the Title
          * - :py:attr:`~slave`
            - Get or set the Part, part set ID or Segment set ID of slaves see *PART, *SET_PART or *SET_SEGMENT.
          * - :py:attr:`~master`
            - Get or set the Part or part set ID of master solid elements, see *PART or *SET_PART.
          * - :py:attr:`~sstyp`
            - Get or set the Slave type:
          * - :py:attr:`~mstyp`
            - Get or set the Master type:
          * - :py:attr:`~nquad`
            - Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
          * - :py:attr:`~ctype`
            - Get or set the Coupling type
          * - :py:attr:`~direc`
            - Get or set the Coupling direction (CTYPE 4 and 5).
          * - :py:attr:`~mcoup`
            - Get or set the Multi-material option (CTYPE 4 and 5).
          * - :py:attr:`~start`
            - Get or set the Start time for coupling (default=0.0).
          * - :py:attr:`~end`
            - Get or set the End time for coupling (default=1.0E+10).
          * - :py:attr:`~pfac`
            - Get or set the Penalty factor (CTYPE 4 and 5 only).
          * - :py:attr:`~fric`
            - Get or set the Coefficient of friction (DIREC 2 only).
          * - :py:attr:`~frcmin`
            - Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
          * - :py:attr:`~norm`
            - Get or set the Shell and segment normal orientation:
          * - :py:attr:`~normtyp`
            - Get or set the Penality spring direction(DIREC 1 and 2 ):
          * - :py:attr:`~damp`
            - Get or set the Damping factor for penalty coupling. This is a coupling-damping
          * - :py:attr:`~cq`
            - Get or set the Heat transfer coefficient.
          * - :py:attr:`~hmin`
            - Get or set the Minmum air gap in heat transfer
          * - :py:attr:`~hmax`
            - Get or set the Maximum air gap in heat transfer. there is no heat transfer above this value.
          * - :py:attr:`~ileak`
            - Get or set the Leakage control:
          * - :py:attr:`~pleak`
            - Get or set the Leakage control penalty factor
          * - :py:attr:`~lcidpor`
            - Get or set the A load curve ID(LCID) defining porours flow through coupling segment.
          * - :py:attr:`~nvent`
            - Get or set the Number of vents defined below
          * - :py:attr:`~blockage`
            - Get or set the Blockage consideration flag.
          * - :py:attr:`~iboxid`
            - Get or set the A box ID defining a box region in space in which ALE coupling is activated.  At time=0.0, the number of Lagrangian segments inside this box is remembered. In subsequent coupling computation steps, there is no need to search for the Lagrangian segments again.
          * - :py:attr:`~ipenchk`
            - Get or set the Initial penetration check flag (only for CTYPE=4, Remark 13):    EQ.0: Do not check for initial penetration.EQ.1: Check and save initial ALE material penetration across a Lagrangian surface (d0), but do not activate coupling at t=0.  In subsequent steps (t>0) the actual penetration is computed as follows actual penetration     = total penetration ¨C initial penetration da=dT ¨C d0
          * - :py:attr:`~intforc`
            - Get or set the A flag to turn on or off (0=OFF or 1=ON) the output of ALE coupling pressure and forces on the slave Lagrangian segments (or surfaces).  Note that the coupling pressures and forces are computed based on the ALE fluid penetrations and coupling stiffness of the system.  When (1) INTFORC=1 and (2) a *DATABASE_BINARY_FSIFOR (DBF) card is defined, LS-DYNA writes out the segment coupling pressure and forces to the binary interface force file for contour plotting.  This interface force file is activated by executing ls971 as follows (3):  ls971 i=inputfilename.k   h=interfaceforcefilename The time interval between output is defined by  dt  in the DBF card.  To plot the binary data in this file: lsprepost interfaceforcefilename.
          * - :py:attr:`~ialesof`
            - Get or set the An integer flag to turn ON/OFF a supplemental Lagrange multiplier FSI constraint which provides a coupling force in addition to the basic penalty coupling contribution.  This is a hybrid coupling method.EQ.0: OFF (default).EQ.1: Turn ON the hybrid Lagrange-multiplier method.  LAGMUL multiplier factor is read.
          * - :py:attr:`~lagmul`
            - Get or set the A Lagrange multiplier factor with a range between 0.0 and 0.05 may be defined.  A typical value may be 0.01.  This should never be greater than 0.1.     EQ.0: OFF (default).GT.0: Turn ON the Lagrange-multiplier method and use LAGMUL as a coefficient for scaling the penalty factor
          * - :py:attr:`~pfacmm`
            - Get or set the Mass-based penalty stiffness factor computational options.  This works in conjunction with PFAC=constant (not a load curve).  The coupling penalty stiffness (CPS) is computed based on an estimated effective coupling mass.
          * - :py:attr:`~thkf`
            - Get or set the (For all CTYPE choices except 11) A flag to account for the coupling thickness of the Lagrangian shell (slave) part.  LT.0: Use positive value of |THKF| for coupling segment thickness.EQ.0: Do not consider coupling segment thickness.GT.0: Coupling segment thickness scale factor.          For CTYPE=11 case (see Remark 14):  This thickness is required for volume calculation.GT.0: (Fabric) Thickness scale factor.  The base shell thickness is taken from the *PART definition.LT.0: User-defined (Fabric) thickness.  The fabric thickness is set to |THKF|.
          * - :py:attr:`~a1`
            - Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=11, A1 = An = coefficient for normal-to-segment direction.For CTYPE=12: A1 = Ax = coefficient for global X-direction
          * - :py:attr:`~b1`
            - Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14). For CTYPE=11, B1 = Bn = coefficient for normal-to-segment direction.            For CTYPE=12: B1 = Bx = coefficient for global X-direction
          * - :py:attr:`~a2`
            - Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A2 = Ay = coefficient for global Y-direction
          * - :py:attr:`~b2`
            - Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: B2 = By = coefficient for global Y-direction
          * - :py:attr:`~a3`
            - Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A3 = Az = coefficient for global Z-direction
          * - :py:attr:`~b3`
            - Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14). For CTYPE=12: B3 = Bz = coefficient for global Z-direction
          * - :py:attr:`~ventsid`
            - Get or set the sid
          * - :py:attr:`~ventyp`
            - Get or set the EQ.0 partset
          * - :py:attr:`~vtcoef`
            - Get or set the Flow coefficient for each vent surface area
          * - :py:attr:`~poppres`
            - Get or set the sid
          * - :py:attr:`~coeflc`
            - Get or set the EQ.0 partset


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from constrained_lagrange_in_solid_edge import ConstrainedLagrangeInSolidEdge

Property detail
---------------

.. py:property:: coupid
   :type: Optional[int]


   
   Get or set the ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Title
















   ..
       !! processed by numpydoc !!

.. py:property:: slave
   :type: Optional[int]


   
   Get or set the Part, part set ID or Segment set ID of slaves see *PART, *SET_PART or *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: master
   :type: Optional[int]


   
   Get or set the Part or part set ID of master solid elements, see *PART or *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: sstyp
   :type: int


   
   Get or set the Slave type:
   EQ.0: part set ID,
   EQ.1: part ID,
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: mstyp
   :type: int


   
   Get or set the Master type:
   EQ.0: part set ID,
   EQ.1: part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nquad
   :type: int


   
   Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
   EQ.0: at nodes only,
   EQ.n: use a rectangular grid of n*n points,
   EQ.-n: at nodes and a rectangular grid of n*n points.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Coupling type
   EQ.1: constrained acceleration,
   EQ.2: constrained acceleration and velocity (default),
   EQ.3: constrained acceleration and velocity, normal direction only,
   EQ.4: penalty coupling (Shell and solid Elements),
   EQ.5: penalty coupling allowing erosion in the lagrangian entities (Solid Elements).
   EQ.6: Penalty coupling designed for airbag modeling(testing).DIREC is automatically reset to DIREC=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: direc
   :type: int


   
   Get or set the Coupling direction (CTYPE 4 and 5).
   EQ.1: normal direction, compression and tension (default),
   EQ.2: normal direction, compression only,
   EQ.3: all directions.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcoup
   :type: int


   
   Get or set the Multi-material option (CTYPE 4 and 5).
   EQ.0: couple with all multi-material groups,
   EQ.1: couple with material with highest density.
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for coupling (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the End time for coupling (default=1.0E+10).
















   ..
       !! processed by numpydoc !!

.. py:property:: pfac
   :type: float


   
   Get or set the Penalty factor (CTYPE 4 and 5 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Coefficient of friction (DIREC 2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: frcmin
   :type: float


   
   Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
















   ..
       !! processed by numpydoc !!

.. py:property:: norm
   :type: int


   
   Get or set the Shell and segment normal orientation:
   EQ.0: right hand rule (default)
   EQ.1: left hand rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: normtyp
   :type: int


   
   Get or set the Penality spring direction(DIREC 1 and 2 ):
   EQ.0: interpolated from node normals(default),
   EQ.1: segment normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Damping factor for penalty coupling. This is a coupling-damping
   scaling factor. Typically it may be between 0 and 1 (see Remark 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: cq
   :type: float


   
   Get or set the Heat transfer coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmin
   :type: Optional[float]


   
   Get or set the Minmum air gap in heat transfer
















   ..
       !! processed by numpydoc !!

.. py:property:: hmax
   :type: Optional[float]


   
   Get or set the Maximum air gap in heat transfer. there is no heat transfer above this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: ileak
   :type: int


   
   Get or set the Leakage control:
   EQ.0: none(default),
   EQ.1: weak,
   EQ.2: strong.
















   ..
       !! processed by numpydoc !!

.. py:property:: pleak
   :type: float


   
   Get or set the Leakage control penalty factor
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidpor
   :type: Optional[int]


   
   Get or set the A load curve ID(LCID) defining porours flow through coupling segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: nvent
   :type: int


   
   Get or set the Number of vents defined below
















   ..
       !! processed by numpydoc !!

.. py:property:: blockage
   :type: int


   
   Get or set the Blockage consideration flag.
   EQ.0 blockage is not considered.
   EQ blockage is considered for venting and porosity
















   ..
       !! processed by numpydoc !!

.. py:property:: iboxid
   :type: int


   
   Get or set the A box ID defining a box region in space in which ALE coupling is activated.  At time=0.0, the number of Lagrangian segments inside this box is remembered. In subsequent coupling computation steps, there is no need to search for the Lagrangian segments again.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipenchk
   :type: int


   
   Get or set the Initial penetration check flag (only for CTYPE=4, Remark 13):    EQ.0: Do not check for initial penetration.EQ.1: Check and save initial ALE material penetration across a Lagrangian surface (d0), but do not activate coupling at t=0.  In subsequent steps (t>0) the actual penetration is computed as follows actual penetration     = total penetration ¨C initial penetration da=dT ¨C d0
















   ..
       !! processed by numpydoc !!

.. py:property:: intforc
   :type: int


   
   Get or set the A flag to turn on or off (0=OFF or 1=ON) the output of ALE coupling pressure and forces on the slave Lagrangian segments (or surfaces).  Note that the coupling pressures and forces are computed based on the ALE fluid penetrations and coupling stiffness of the system.  When (1) INTFORC=1 and (2) a *DATABASE_BINARY_FSIFOR (DBF) card is defined, LS-DYNA writes out the segment coupling pressure and forces to the binary interface force file for contour plotting.  This interface force file is activated by executing ls971 as follows (3):  ls971 i=inputfilename.k   h=interfaceforcefilename The time interval between output is defined by  dt  in the DBF card.  To plot the binary data in this file: lsprepost interfaceforcefilename.
















   ..
       !! processed by numpydoc !!

.. py:property:: ialesof
   :type: int


   
   Get or set the An integer flag to turn ON/OFF a supplemental Lagrange multiplier FSI constraint which provides a coupling force in addition to the basic penalty coupling contribution.  This is a hybrid coupling method.EQ.0: OFF (default).EQ.1: Turn ON the hybrid Lagrange-multiplier method.  LAGMUL multiplier factor is read.
















   ..
       !! processed by numpydoc !!

.. py:property:: lagmul
   :type: float


   
   Get or set the A Lagrange multiplier factor with a range between 0.0 and 0.05 may be defined.  A typical value may be 0.01.  This should never be greater than 0.1.     EQ.0: OFF (default).GT.0: Turn ON the Lagrange-multiplier method and use LAGMUL as a coefficient for scaling the penalty factor
















   ..
       !! processed by numpydoc !!

.. py:property:: pfacmm
   :type: int


   
   Get or set the Mass-based penalty stiffness factor computational options.  This works in conjunction with PFAC=constant (not a load curve).  The coupling penalty stiffness (CPS) is computed based on an estimated effective coupling mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: thkf
   :type: float


   
   Get or set the (For all CTYPE choices except 11) A flag to account for the coupling thickness of the Lagrangian shell (slave) part.  LT.0: Use positive value of |THKF| for coupling segment thickness.EQ.0: Do not consider coupling segment thickness.GT.0: Coupling segment thickness scale factor.          For CTYPE=11 case (see Remark 14):  This thickness is required for volume calculation.GT.0: (Fabric) Thickness scale factor.  The base shell thickness is taken from the *PART definition.LT.0: User-defined (Fabric) thickness.  The fabric thickness is set to |THKF|.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=11, A1 = An = coefficient for normal-to-segment direction.For CTYPE=12: A1 = Ax = coefficient for global X-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14). For CTYPE=11, B1 = Bn = coefficient for normal-to-segment direction.            For CTYPE=12: B1 = Bx = coefficient for global X-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A2 = Ay = coefficient for global Y-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: B2 = By = coefficient for global Y-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Viscous coefficient for the porous flow Ergun equation (see Remark 14).For CTYPE=12: A3 = Az = coefficient for global Z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the Inertial coefficient for the porous flow Ergun equation (see Remark 14). For CTYPE=12: B3 = Bz = coefficient for global Z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ventsid
   :type: Optional[int]


   
   Get or set the sid
















   ..
       !! processed by numpydoc !!

.. py:property:: ventyp
   :type: int


   
   Get or set the EQ.0 partset
   EQ .1 part
   EQ.2 segmentset
















   ..
       !! processed by numpydoc !!

.. py:property:: vtcoef
   :type: int


   
   Get or set the Flow coefficient for each vent surface area
















   ..
       !! processed by numpydoc !!

.. py:property:: poppres
   :type: float


   
   Get or set the sid
















   ..
       !! processed by numpydoc !!

.. py:property:: coeflc
   :type: int


   
   Get or set the EQ.0 partset
   EQ .1 part
   EQ.2 segmentset
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'LAGRANGE_IN_SOLID_EDGE'






