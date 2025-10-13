





:class:`ConstrainedLagrangeInSolid`
===================================


.. py:class:: constrained_lagrange_in_solid.ConstrainedLagrangeInSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_LAGRANGE_IN_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedLagrangeInSolid

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
          * - :py:attr:`~lstrsid`
            - Get or set the Set ID defining a part, part set, or segment set ID of the Lagrangian structure (see *PART, *SET_‌PART or *SET_‌SEGMENT).  See Remark 1
          * - :py:attr:`~alesid`
            - Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART). See Remark 1
          * - :py:attr:`~lstrstyp`
            - Get or set the LSTRSID set type:
          * - :py:attr:`~alestyp`
            - Get or set the ALESID set type:
          * - :py:attr:`~nquad`
            - Get or set the Number of coupling points distributed over each coupled Lagrangian surface segment. (see Remark 2)
          * - :py:attr:`~ctype`
            - Get or set the Fluid-Structure coupling method. CTYPEs(1, and 2) are not supported in MPP.
          * - :py:attr:`~direc`
            - Get or set the Coupling direction (CTYPE 4 and 5).
          * - :py:attr:`~mcoup`
            - Get or set the Multi-material option (CTYPE 4 and 5).
          * - :py:attr:`~start`
            - Get or set the Start time for coupling (default=0.0).
          * - :py:attr:`~end`
            - Get or set the End time for coupling (default=1.0E+10).
          * - :py:attr:`~pfac`
            - Get or set the For Ctype 4,5 or 6.Penalty factor.  PFAC is a scale factor for scaling the estimated stiffness of the interacting (coupling) system.  It is used to compute the coupling forces to be distributed on the Lagrangian and ALE parts
          * - :py:attr:`~fric`
            - Get or set the Coefficient of friction (DIREC 2 only).
          * - :py:attr:`~frcmin`
            - Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
          * - :py:attr:`~norm`
            - Get or set the Shell and segment normal orientation:
          * - :py:attr:`~normtyp`
            - Get or set the Penality spring direction(DIREC 1 and 2 ):
          * - :py:attr:`~damp`
            - Get or set the damping factor for coupling type4.
          * - :py:attr:`~k`
            - Get or set the Thermal conductivity of a virtual fluid between the Lagrangian structure surface and the ALE material.  See Remark 8
          * - :py:attr:`~hmin`
            - Get or set the The absolute value is minimum air gap in heat transfer, h_min (See Remark 8).
          * - :py:attr:`~hmax`
            - Get or set the Maximum air gap in heat transfer. there is no heat transfer above this value.
          * - :py:attr:`~ileak`
            - Get or set the Coupling leakage control flag :
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
            - Get or set the A flag to turn on or off (0=OFF or 1=ON) the output of ALE coupling pressure and forces on the Lagrangian segments (or surfaces).  Note that the coupling pressures and forces are computed based on the ALE fluid penetrations and coupling stiffness of the system.  When (1) INTFORC=1 and (2) a *DATABASE_BINARY_FSIFOR (DBF) card is defined, LS-DYNA writes out the segment coupling pressure and forces to the binary interface force file for contour plotting.  This interface force file is activated by executing ls971 as follows (3):        ls971 i=inputfilename.k   h=interfaceforcefilename The time interval between output is defined by  dt  in the DBF card.  To plot the binary data in this file: lsprepost interfaceforcefilename.
          * - :py:attr:`~ialesof`
            - Get or set the An integer flag to turn ON/OFF a supplemental Lagrange multiplier FSI constraint which provides a coupling force in addition to the basic penalty coupling contribution.  This is a hybrid coupling method.EQ.0: OFF (default).EQ.1: Turn ON the hybrid Lagrange-multiplier method.  LAGMUL multiplier factor is read.
          * - :py:attr:`~lagmul`
            - Get or set the A Lagrange multiplier factor with a range between 0.0 and 0.05 may be defined.  A typical value may be 0.01.  This should never be greater than 0.1.     EQ.0: OFF (default).GT.0: Turn ON the Lagrange-multiplier method and use LAGMUL as a coefficient for scaling the penalty factor
          * - :py:attr:`~pfacmm`
            - Get or set the Mass-based penalty stiffness factor computational options.  This works in conjunction with PFAC=constant (not a load curve).  The coupling penalty stiffness (CPS) is computed based on an estimated effective coupling mass.
          * - :py:attr:`~thkf`
            - Get or set the (For all CTYPE choices except 11) A flag to account for the coupling thickness of the Lagrangian shell part.  LT.0: Use positive value of |THKF| for coupling segment thickness.EQ.0: Do not consider coupling segment thickness.GT.0: Coupling segment thickness scale factor.          For CTYPE=11 case (see Remark 14):  This thickness is required for volume calculation.GT.0: (Fabric) Thickness scale factor.  The base shell thickness is taken from the *PART definition.LT.0: User-defined (Fabric) thickness.  The fabric thickness is set to |THKF|.
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
          * - :py:attr:`~poreini`
            - Get or set the For CTYPE=11 or CTYPE=12: Initial volume ratio of pores in an element. The current volume ratio is PORE=POREINI*vol/volini, where vol and volini are the current and initial element volumes respectively
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

    from constrained_lagrange_in_solid import ConstrainedLagrangeInSolid

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

.. py:property:: lstrsid
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set, or segment set ID of the Lagrangian structure (see *PART, *SET_‌PART or *SET_‌SEGMENT).  See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART). See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: lstrstyp
   :type: int


   
   Get or set the LSTRSID set type:
   EQ.0: Part set ID(PSID),
   EQ.1: Part ID(PID),
   EQ.2: Segment set ID (SSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: alestyp
   :type: int


   
   Get or set the ALESID set type:
   EQ.0: Part set ID(PSID),
   EQ.1: Part ID(PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: nquad
   :type: int


   
   Get or set the Number of coupling points distributed over each coupled Lagrangian surface segment. (see Remark 2)
   EQ.0: NQUAD will be set by default to 2,
   GT.0: An NQUAD x NQUAD coupling points distribution over each Lagrangian segment is defined,
   LT.0: NQUAD is reset to a positive value. Coupling at nodes is obsolete.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Fluid-Structure coupling method. CTYPEs(1, and 2) are not supported in MPP.
   EQ.1:   Constrained acceleration.
   EQ.2:   Constrained acceleration and velocity (default, see Remark 3).
   EQ.3:   Constrained acceleration and velocity, normal direction only.
   EQ.4:   Penalty coupling for shell  and solid elements (without erosion).
   NOTE:   For RIGID Lagrangian Structure PARTS a penalty coupling method (CTYPE = 4) must be used.
   EQ.5:   Penalty coupling allowing erosion in the Lagrangian entities.
   EQ.6:   Penalty coupling designed for airbag modeling which
   automatically controls the DIREC parameter internally.
   It is equivalent to setting {CTYPE = 4; DIREC = 1} for unfolded region;
   and {CTYPE = 4; DIREC = 2} for folded region.
   For both cases: {ILEAK = 2; FRCMIN = 0.3}.
   EQ.11:  Coupling designed to couple Lagrangian porous shell to ALE material.
   When this option is used, THKF, the 7th column parameter of optional Card 4a
   and the first 2 parameters of optional Card 4b must be defined.
   See *LOAD_‌BODY_‌POROUS and Remark 13 below.
   EQ.12:  Coupling designed to couple Lagrangian porous solid to ALE material.
   When this option is used, Ai & Bi parameters of optional Card 4b must be defined (Card 4a must be defined but can be blank).
   See *LOAD_‌BODY_‌POROUS and Remark 14 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: direc
   :type: int


   
   Get or set the Coupling direction (CTYPE 4 and 5).
   EQ.1: Normal direction, compression and tension (default),
   EQ.2: Normal direction, compression only,
   EQ.3: All directions.
   For CTYPE=12: Flag to activate an element coordinate system.
   EQ.0: The forces are applied in the global directions
   EQ.1: The forces are applied in a local system attached to the Lagrangian solid. If n1,n2,...,n8 are the nodes in the order set by *ELEMENT_SOLID, the X-direction passes through the centroids of the faces n1,n4,n8,n5 and n2,n3,n7,n6. The Y-direction is perpendicular to the X-direction and nearly parallel to an axis going through the centroids of the faces n1,n2,n6,n5 and n4,n3,n7,n8. The Z-direction is the vector cross product of X and Y-directions. The system is consistent with AOPT=1 in *LOAD_BODY_POROUS
















   ..
       !! processed by numpydoc !!

.. py:property:: mcoup
   :type: int


   
   Get or set the Multi-material option (CTYPE 4 and 5).
   EQ.0: Couple with all multi-material groups,
   EQ.1: Couple with material with highest density.
















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


   
   Get or set the For Ctype 4,5 or 6.Penalty factor.  PFAC is a scale factor for scaling the estimated stiffness of the interacting (coupling) system.  It is used to compute the coupling forces to be distributed on the Lagrangian and ALE parts
   GT.0:   Fraction of estimated critical stiffness.
   LT.0 : PFAC must be an integer,and PFAC is a load curve ID.The curve defines the coupling pressure on the y - axis as a function of the penetration along the x - axis.  (See How to Correct Leakage)
   For CTYPE = 11 or 12
   Time step factor
















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
   EQ.0:Normal vectors are interpolated from nodal normals. (default).
   EQ.1:   Normal vectors are interpolated from segment normals.This is sometimes a little more robust for sharp Lagrangian corners,and folds.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the damping factor for coupling type4.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: float


   
   Get or set the Thermal conductivity of a virtual fluid between the Lagrangian structure surface and the ALE material.  See Remark 8
















   ..
       !! processed by numpydoc !!

.. py:property:: hmin
   :type: Optional[float]


   
   Get or set the The absolute value is minimum air gap in heat transfer, h_min (See Remark 8).
   LT.0.0: Turn on constraint based thermal nodal coupling between LAG structure and ALE fluids.
   GE.0.0 : Minimum air gap.If zero, default to 10 - 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmax
   :type: Optional[float]


   
   Get or set the Maximum air gap in heat transfer. there is no heat transfer above this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: ileak
   :type: int


   
   Get or set the Coupling leakage control flag :
   EQ.0: None(default),
   EQ.1: Weak,leakage control is turned off if penetrating volume fraction > FRCMIN + 0.2
   EQ.2: Strong.with improved energy consideration.  Leakage control is turned off if penetrating volume fraction > FRCMIN + 0.4
















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


   
   Get or set the A flag to turn on or off (0=OFF or 1=ON) the output of ALE coupling pressure and forces on the Lagrangian segments (or surfaces).  Note that the coupling pressures and forces are computed based on the ALE fluid penetrations and coupling stiffness of the system.  When (1) INTFORC=1 and (2) a *DATABASE_BINARY_FSIFOR (DBF) card is defined, LS-DYNA writes out the segment coupling pressure and forces to the binary interface force file for contour plotting.  This interface force file is activated by executing ls971 as follows (3):        ls971 i=inputfilename.k   h=interfaceforcefilename The time interval between output is defined by  dt  in the DBF card.  To plot the binary data in this file: lsprepost interfaceforcefilename.
















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


   
   Get or set the (For all CTYPE choices except 11) A flag to account for the coupling thickness of the Lagrangian shell part.  LT.0: Use positive value of |THKF| for coupling segment thickness.EQ.0: Do not consider coupling segment thickness.GT.0: Coupling segment thickness scale factor.          For CTYPE=11 case (see Remark 14):  This thickness is required for volume calculation.GT.0: (Fabric) Thickness scale factor.  The base shell thickness is taken from the *PART definition.LT.0: User-defined (Fabric) thickness.  The fabric thickness is set to |THKF|.
















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

.. py:property:: poreini
   :type: Optional[float]


   
   Get or set the For CTYPE=11 or CTYPE=12: Initial volume ratio of pores in an element. The current volume ratio is PORE=POREINI*vol/volini, where vol and volini are the current and initial element volumes respectively
















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
   :value: 'LAGRANGE_IN_SOLID'






